unit uStrUtil;

interface
uses
  Math, Windows, Dialogs, SysUtils, Classes, uHashTable, StrUtils;

const
  UNIT_NAME = 'StrUtil';
  RANDCHARS = '0123456789ABCDEFGHJKLMNPQRSTUVWXYZ';
  TRUE_VALUE_DEFAULT = '1';
  FALSE_VALUE_DEFAULT = '0';

type
  CStrUtil = class(TObject)
  private
    Fws : integer; //=3;                        //  weight for substitution
    Fwi : integer; //=1;                        //  weight for insertion
    Fwd : integer; //=6;                        //  weight for deleting
    Fth : integer;
  protected
    function DamerauLevenshteinLike(const s1,s2:string;ws,wi,wd:integer):Integer;
  public
    function ExtractInt(str: string): integer;
    procedure WriteStreamStr(Stream : TStream; Str : string);
    function ReadStreamStr(Stream : TStream) : string;
    procedure WriteStreamInt(Stream : TStream; Num : integer);
    function ReadStreamInt(Stream : TStream) : integer;
    function strEOL(str: string): string;
    function IsHex(str: string): boolean;
    function AlphaCharCount(str: string): integer;
    function NonAlphaCharCount(str: string): integer;
    function SimilarCharCount(str, str2: string): integer;
    function SimilarCharPct(str, str2: string): extended;
    function folderNameFriendly(str: string): string;
    function PadL(StringToPad: string; Size: Integer): String;
    function IsChrAlpha( Character : Char ) : boolean;
    function IsChrUpperAlpha( Character : Char ) : boolean;
    function IsChrLowerAlpha( Character : Char ) : boolean;
    function IsChrNumeric( Character : Char ) : boolean;
    function IsNumeric( StringToCheck : string ) : boolean;
    function IsStrictNumeric( StringToCheck : string ) : boolean;
    function extNumDbl(str: string): double;
    function extNumInt(str: string): integer;
    function ParseString(const vS : string; ParseChar : Char; var ParsedString : TStringList; TrimOnInsert : boolean ) : cardinal;
    function StringsAreLike( const s1,s2:string ): boolean;
    property DLWeightForSubstitution : integer read Fws write Fws;
    property DLWeightForInsertion : integer read Fwi write Fwi;
    property DLWeightForDeleting : integer read Fwd write Fwd;
    property DLWeightForMisc : integer read Fth write Fth;

    constructor Create();
    destructor Destroy; override;
  end;

  TCharSet = Set of Char;

   TStringObject = class(TObject)
   private
     fStr: String;
   public
     constructor Create(const AStr: String) ;
     property AsString: String read FStr write FStr;
   end;

  TStringList2 = class(TStringList)
    public
      function asString() : string; overload;
      function asString(delimiter : string) : string; overload;
      function fromString(str : string) : boolean; overload;
      function fromString(str, delimiter : string) : boolean; overload;
  end;

  TXMLCharXlate = record
     Char : string;
     XML : string;
   end;

  TKeyValueExtended = record
     Key : string;
     Val : Extended;
   end;
  TKeyValueExtendedList = array of TKeyValueExtended;
  TPtrAccuracy = record
     Ptr : Pointer;
     Val : Extended;
   end;
  TPtrAccuracyList = array of TPtrAccuracy;

function RandomString( num : smallint = 4 ) : string; overload;
procedure RandomString( const num : smallint; var outStr : string  ); overload;
function BoolToStr( val : boolean; trueValue : string = TRUE_VALUE_DEFAULT; falseValue : string = FALSE_VALUE_DEFAULT ) : string;
function StrToBool( val : string; trueValue : string = TRUE_VALUE_DEFAULT; falseValue : string = FALSE_VALUE_DEFAULT ) : boolean;
function Split(const input: string; const schar: Char; const  s: Word ): string;
Function StringCount(const input, stringToCount : string ): Cardinal;
Function XMLEnc(const str : string ): string;
Function XMLDec(const str : string ): string;
function MaxValue(const Data: TKeyValueExtendedList): TKeyValueExtended;
function MaxValueP(const Data: TPtrAccuracyList): TPtrAccuracy;
function uDateTimeToStr( const val : TDateTime ) : string;
function uStrToDateTime( const val : string ) : TDateTime;
function sNow() : String;
function IsIn( const Val : string; const Data : array of string ) : boolean;

var
  XMLXlateTable : array[0..3] of TXMLCharXlate =
   (
     (Char : '&'; XML : '&amp;'),
     (Char : '<'; XML : '&lt;'),
     (Char : '>'; XML : '&gt;'),
     (Char : '"'; XML : '&quot;')
   );

implementation

function sNow(): string;
var
  today : TDateTime;
begin
  today := Now;
  Result := DateToStr(today) + ' ' + TimeToStr(today);
End;

function uDateTimeToStr( const val : TDateTime ): string;
Begin
  if ( val = MinDouble ) then
    Result := ''
  else
    Result := DateTimeToStr( val );
End;

function uStrToDateTime( const val : string ) : TDateTime;
Begin
  try
    Result := StrToDateTime( val );
  except
    Result := MinDouble;
  end;
End;

function IsIn( const Val : string; const Data : array of string ) : boolean;
var
  I: Integer;
begin
  Result := false;
  for I := Low(Data) to High(Data) do begin
    if LowerCase(Val) = LowerCase(Data[I]) then begin
      Result := true;
      Exit;
    end;
  end;
end;

function MaxValue(const Data: TKeyValueExtendedList): TKeyValueExtended;
var
  I: Integer;
  res : Extended;
begin
  Result := Data[Low(Data)];
  res := Data[Low(Data)].Val;
  for I := Low(Data) + 1 to High(Data) do
    if res < Data[I].Val then begin
      res := Data[I].Val;
      Result := Data[I];
    end;
end;

function MaxValueP(const Data: TPtrAccuracyList): TPtrAccuracy;
var
  I: Integer;
  res : Extended;
begin
  Result := Data[Low(Data)];
  res := Data[Low(Data)].Val;
  for I := Low(Data) + 1 to High(Data) do
    if res < Data[I].Val then begin
      res := Data[I].Val;
      Result := Data[I];
    end;
end;

Function XMLEnc(const str : string ): string;
var
  i : integer;
Begin
  Result := str;
  for i  := 0 to Length( XMLXlateTable ) - 1 do
    if ( Pos( XMLXlateTable[i].Char, Result ) > 0 ) then
      Result := StringReplace( Result, XMLXlateTable[i].Char, XMLXlateTable[i].XML, [rfReplaceAll]);
End;

Function XMLDec(const str : string ): string;
var
  i : integer;
Begin
  Result := str;
  for i  := 0 to Length( XMLXlateTable ) - 1 do
    if ( Pos( XMLXlateTable[i].XML, Result ) > 0 ) then
      Result := StringReplace( Result, XMLXlateTable[i].XML, XMLXlateTable[i].Char, [rfReplaceAll]);
End;

Function Split(const input: string; const schar: Char; const s: Word ): string;
// Thanks to http://www.swissdelphicenter.ch/torry/showcode.php?id=2064
Var
  c: array of Integer;
  b, t: Integer;
Begin
  if ( not ContainsText(input, schar) ) then begin
    if (s = 0) then
      Result := input
    else
      Result := '';
  end else begin
    t := Length( input );     // variable T needs to be initialized...
    setlength(c, Length(input));
    for b := 0 to pred(High(c)) do
    begin
      c[b + 1] := posex( schar, input, succ(c[b]) );
      if ( c[b + 1] < c[b] ) then begin
        c[b + 1] := succ( t );
        break;
      end else if ( succ(s) < b ) then
        break;
    end;
    Result := Copy( input, succ(c[s]), pred(c[s + 1] - c[s]));
  end;
End;

Function StringCount(const input, stringToCount : string ): Cardinal;
Var
  cRes : Cardinal;
  pos: Integer;
Begin
  cRes := 0;
  pos := 1;
  while pos <= Length( input ) do begin
    pos := posex( stringToCount, input, pos );
    if ( pos = 0 ) then break;
    Inc( cRes );
    Inc(pos, Length( stringToCount ));
  end;
  Result := cRes;
End;

function BoolToStr( val : boolean; trueValue : string = TRUE_VALUE_DEFAULT; falseValue : string = FALSE_VALUE_DEFAULT ) : string;
Begin
  if ( val ) then
    Result := trueValue
  else
    Result := falseValue;
End;

function StrToBool( val : string; trueValue : string = TRUE_VALUE_DEFAULT; falseValue : string = FALSE_VALUE_DEFAULT ) : boolean;
Begin
  Result := ( Trim(val) = Trim(trueValue) );
End;

function RandomString(num : smallint) : string;
var
  S: string;
  i, N: integer;
Begin
  Randomize;
  S := '';
  for i := 1 to num do begin
    N := Random(Length(RANDCHARS)) + 1;
    S := S + RANDCHARS[N];
  end;
  Result := S;
End;

procedure RandomString(const num : smallint ; var outStr : string);
var
  S: string;
  i, N: integer;
Begin
  Randomize;
  S := '';
  for i := 1 to num do begin
    N := Random(Length(RANDCHARS)) + 1;
    S := S + RANDCHARS[N];
  end;
  outStr := S;
End;

{ CStrUtil }

function CStrUtil.AlphaCharCount(str: string): integer;
var i, nCnt : integer;
Begin
  nCnt := 0;
  for i := 1 to Length(str) do begin
    case str[i] of
      'A'..'Z': Inc(nCnt);
      'a'..'z': Inc(nCnt);
      '0'..'9': Inc(nCnt);
      ' ': Inc(nCnt);
    end; {case}
  end; {for}
  Result := nCnt;
End;

constructor CStrUtil.Create;
begin
    Fws :=3;                        //  weight for substitution
    Fwi :=1;                        //  weight for insertion
    Fwd :=6;                        //  weight for deleting
    Fth :=4;
end;

function CStrUtil.DamerauLevenshteinLike(const s1, s2: string; ws, wi,
  wd: integer): Integer;
VAR
  i,j:Integer;

function Pp(x,y:Integer):Integer;
begin
  if AnsiUpperCase(s1[x])=AnsiUpperCase(s2[y]) then Pp:=0 else Pp:=ws;
end; 

var 
  Wmax:integer; 
  d:array of array of integer; 

begin 
  Wmax:=Max(length(s1),length(s2))+1;
  SetLength(d,Wmax,Wmax); 
  dec(Wmax); 
  d[0,0]:=0; 
  for j:=1 TO Wmax DO d[0,j]:=d[0,Pred(j)]+wi; 
  for i:=1 TO Wmax DO d[i,0]:=d[Pred(i),0]+wd; 
  for i:=1 TO Length(s1) DO 
    for j:=1 TO Length(s2) DO 
      d[i,j]:=MinIntValue([ d[Pred(i),Pred(j)]+Pp(i,j),      //substitution 
                            d[     i ,Pred(j)]+wi,            //insertion
                            d[Pred(i),     j ]+wd             //deletion 
                         ]); 
  result:=d[Length(s1),Length(s2)]; 
  SetLength(d,0); 
end{DamerauLevenshteinLike};

destructor CStrUtil.Destroy;
begin

  inherited;
end;

function CStrUtil.strEOL(str: string): string;
var
  nPos : integer;
begin

  result := str;
  nPos := Pos(#10, str);
  if ( nPos > 0 ) then begin
    result := Copy(str, 1, nPos - 1);
  end;
  nPos := Pos(#13, str);
  if ( nPos > 0 ) then begin
    result := Copy(str, 1, nPos - 1);
  end;
End;

function CStrUtil.StringsAreLike(const s1, s2: string): boolean;
begin
  result:= DamerauLevenshteinLike(s1,s2,Fws,Fwi,Fwd)<=Fth; 
end;

procedure CStrUtil.WriteStreamInt(Stream: TStream; Num: integer);
{writes an integer to the stream}
begin
   Stream.Write(Num, SizeOf(Integer));
end;

procedure CStrUtil.WriteStreamStr(Stream: TStream; Str: string);
{writes a string to the stream}
var
   StrLen : integer;
begin
   {get length of string}
   StrLen := Length(Str);
   {write length of string}
   WriteStreamInt(Stream, StrLen);
   {write characters}
   if StrLen > 0 then
   Stream.Write(Str[1], StrLen);
   Stream.Position := 0;
end;

function CStrUtil.IsChrAlpha(Character: Char): boolean;
begin
  case Character of
    'a'..'z': Result := True;
    'A'..'Z': Result := True;
   else
    Result := False;
  end; {case}
End;

function CStrUtil.IsChrLowerAlpha(Character: Char): boolean;
begin
  case Character of
    'a'..'z': Result := True;
   else
    Result := False;
  end; {case}
End;

function CStrUtil.IsChrNumeric(Character: Char): boolean;
begin
  case Character of
    '0'..'9': Result := True;
   else
    Result := False;
  end; {case}
End;

function CStrUtil.IsChrUpperAlpha(Character: Char): boolean;
begin
  case Character of
    'A'..'Z': Result := True;
   else
    Result := False;
  end; {case}
End;

function CStrUtil.IsHex(str: string): boolean;
var i : integer;
begin
  for i := 1 to Length(str) do begin
    case str[i] of
      'A'..'F': Result := True;
      'a'..'f': Result := True;
      '0'..'9': Result := True;
    else
      Result := False;
      break;
    end; {case}
  end; {for}
  Result := False;
End;

function CStrUtil.IsNumeric(StringToCheck: string): boolean;
var
  i : integer;
begin
  result := true;
  for i := 1 to Length(StringToCheck) do begin
    if ( StringToCheck[i] = ',' )  and ( StringToCheck[i] = '$' ) then StringToCheck[i] := '0';
    //if ( i = Length(StringToCheck)) and ( StringToCheck[i] = '-' ) then StringToCheck[i] := '0';
    if ( i = 1) and ( StringToCheck[i] = '-' ) then StringToCheck[i] := '0';
    if not IsChrNumeric( StringToCheck[i] ) then begin
      result := false;
      break;
    end;
  end; {for}
End;

function CStrUtil.extNumDbl(str: string): double;
var
  i : integer;
begin
  for i := 1 to Length(str) do begin
    if ( str[i] = ',' )  and ( str[i] = '$' ) then str[i] := ' ';
  end; {for}
  result := StrToFloat(StringReplace(str, ' ', '', [rfReplaceAll, rfIgnoreCase] ));
End;

function CStrUtil.extNumInt(str: string): integer;
var
  i : integer;
begin
  for i := 1 to Length(str) do begin
    if ( str[i] = ',' )  and ( str[i] = '$' ) then str[i] := ' ';
  end; {for}
  result := StrToInt(StringReplace(str, ' ', '', [rfReplaceAll, rfIgnoreCase] ));
End;

function CStrUtil.ExtractInt(str: string): integer;
var
  i : integer;
begin
  for i := 1 to Length(str) do begin
    case str[i] of
      '0'..'9':
    else
      str[i] := ' ';
    end; {case}
  end; {for}
  result := StrToInt(StringReplace(str, ' ', '', [rfReplaceAll, rfIgnoreCase] ));
End;

function CStrUtil.folderNameFriendly(str: string): string;
begin
  result := StringReplace(str   , '/', '-', [rfReplaceAll, rfIgnoreCase] );
  result := StringReplace(result, '\', '-', [rfReplaceAll, rfIgnoreCase] );
  result := StringReplace(result, '*', '-', [rfReplaceAll, rfIgnoreCase] );
  result := StringReplace(result, '?', '-', [rfReplaceAll, rfIgnoreCase] );
  result := StringReplace(result, ':', '_', [rfReplaceAll, rfIgnoreCase] );
  result := StringReplace(result, '.', ' ', [rfReplaceAll, rfIgnoreCase] );
End;

function CStrUtil.IsStrictNumeric(StringToCheck: string): boolean;
var
  i : integer;
  bIsStrNumeric : boolean;
begin
  bIsStrNumeric := true;
  for i := 1 to Length(StringToCheck) do begin
    bIsStrNumeric := IsChrNumeric( StringToCheck[i] );
    if not bIsStrNumeric then break;
  end; {for}
  result := bIsStrNumeric;
End;

function CStrUtil.NonAlphaCharCount(str: string): integer;
var i, nCnt : integer;
Begin
  nCnt := 0;
  for i := 1 to Length(str) do begin
    case str[i] of
      'A'..'Z': ;
      'a'..'z': ;
      '0'..'9': ;
      ' ': ;
    else
      Inc( nCnt );
    end; {case}
  end; {for}
  Result := nCnt;
End;

function CStrUtil.PadL(StringToPad: string; Size: Integer): String;
var
  nStPos : integer;
  sNew, sPadString : string;
begin
    sPadString := '0';
    nStPos := (Size - Length(StringToPad)) + 1;
    if nStPos < 2 then nStPos := 1;
    sNew := StringOfChar(sPadString[1], nStPos - 1);
    Result := sNew + StringToPad;
End;

function CStrUtil.ParseString(const vS: string; ParseChar: Char;
  var ParsedString: TStringList; TrimOnInsert: boolean) : cardinal;
Var
  i, StPos : integer;
  s, newStr : String;
Begin
  if ( ParsedString=nil) then
    ParsedString := TStringList.Create;
  s := vS + ParseChar;
  i := 1;  StPos := 1;
  While i <= Length(s) do begin
    if ( s[i] = ParseChar ) and ( i > 1 ) then begin
      if TrimOnInsert then
        newStr := Trim(Copy(s, StPos, i - StPos))
      else
        newStr := Copy(s, StPos, i - StPos);
      if ( Length(newStr) > 0 ) then ParsedString.Add(newStr);
      StPos := i + 1;
    end;{if}
    Inc(i);
  end; {while}
  Result := ParsedString.Count;
End;

function CStrUtil.ReadStreamInt(Stream: TStream): integer;
{returns an integer from stream}
begin
   if Stream.Read(Result, SizeOf(Integer)) < SizeOf(Integer) then
       Result := -1;
end;

function CStrUtil.ReadStreamStr(Stream: TStream): string;
{returns a string from the stream}
var
   StrLen : integer;
begin
  Stream.Position := 0;
   {get length of string}
   StrLen := ReadStreamInt(Stream);
   if StrLen > 0 then
   begin
       {set string to get memory}
       SetLength(Result, StrLen);
       {read characters}
       Stream.Read(Result[1], StrLen);
   end else
       Result := '';
   {end; if StrLen > -1 else}
end;

function CStrUtil.SimilarCharCount(str, str2: string): integer;
var i, nCnt : integer;
Begin
  nCnt := 0;
  for i := 1 to Length(str) do
    if (( Length( str2 ) > i ) and
        ( str[i] = str2[i] ) ) then
      Inc( nCnt );
  Result := nCnt;
End;

function CStrUtil.SimilarCharPct(str, str2: string): extended;
begin
  if (( str='' ) or ( str2='' )) then
    Result := 0
  else
    Result := SimilarCharCount(str, str2) / MAX(Length(str), Length(str2));
end;

{ TStringList2 }

function TStringList2.asString(delimiter: string): string;
var
  i : integer;
  res : string;
begin
  res := '';
  for i := 0 to self.Count - 1 do begin
    if ( i = 0 )  then
      res := self[i]
    else
      res := res + delimiter + self[i]
  end;
  result := res;
end;

function TStringList2.asString: string;
begin
  result := asString('|');
end;

function TStringList2.fromString(str, delimiter: string): boolean;
var
  oStrUtil : CStrUtil;
  sl : TStringList;
begin
  try
    try
      oStrUtil := CStrUtil.Create;
      sl := TStringList.Create;
      oStrUtil.ParseString( str, delimiter[1],  sl, true );
      self.Clear;
      self.AddStrings( sl );
      result := true;
    except
      result := false;
    end;
  finally
    FreeAndNil( sl );
    FreeAndNil( oStrUtil );
  end;
end;

function TStringList2.fromString(str: string): boolean;
begin
  result := self.fromString(str, '|');
end;

{ TString }
constructor TStringObject.Create(const AStr: String) ;
begin
   inherited Create;
   FStr := AStr;
end;

initialization

finalization

END.
