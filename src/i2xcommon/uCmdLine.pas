unit uCmdLine;

interface
uses
  SysUtils, uHashTable;

const
  UNIT_NAME = 'uCmdLine';

type

  TCommandLine = class(TObject)
    private
      fs:string;
      FCmdLineAsStringHash : THashStringTable;
      function CommandLineToStringHash(sCmdLine : string;  ht : THashStringTable ) : boolean;
    protected
    public
      constructor Create(var Commandline:PChar); // copies the commandline
      destructor  Destroy; override;
      function GetParameter(N:string) : string; overload;
      function GetParameter(N : string; StripQuotes : boolean ) : string; overload;
      function Exists(N:string) : boolean;
    published
  end;

implementation

{ TCommandline }
function TCommandLine.CommandLineToStringHash(sCmdLine : string;
      ht : THashStringTable ): boolean;
var
  MyStr, str, sTokenName : string;
  i : integer;
  inComment, isTokenName : boolean;
begin
//  ClearLog;
  result := false;
  sTokenName := 'EXEName';
  try
    str := '';
    inComment := false;
    MyStr := sCmdLine;
    for i := 1 to Length( MyStr ) do begin
      if ( MyStr[i] = '"' ) then begin
        if ( not inComment )  then
          inComment := true
        else begin
          inComment := false;
          if ( Length(sTokenName) > 0 ) then ht.Add( sTokenName, str );
          str := '';
          sTokenName := '';
        end;
      end else begin
        if ( inComment ) then begin
          str := str + MyStr[i];
        end else begin
          if (( MyStr[i] = '-' ) or ( MyStr[i] = '/' )) then begin
            isTokenName := true;
            if ( Length(sTokenName) > 0 ) then ht.Add( sTokenName, str );
            str := '';
            sTokenName := '';
          end else if ( MyStr[i] = ' ' ) then begin
            if (isTokenName) then begin
              if ( Length(sTokenName) > 0 ) then ht.Add( sTokenName, str );
              isTokenName := false;
              sTokenName := str;
              str := '';
            end;
          end else begin
            str := str + MyStr[i];
          end;
        end;
      end;
    end;
    if ( Length( str ) > 0 )  then begin
      if (isTokenName) then begin
        sTokenName := str;
        str := '';
      end;
      ht.Add( sTokenName, str );
      //ht.setValue( sTokenName, TStringObject.Create(str));
    end;
    result := ( ht.Count > 0 );
  finally
  end;
end;

constructor TCommandline.Create(var Commandline:PChar);
var i:integer;
begin
  inherited create;
  fs:=Commandline;    // get the PCHAR into the Delphi string
  self.FCmdLineAsStringHash := THashStringTable.Create;
  CommandLineToStringHash( fs, self.FCmdLineAsStringHash );
end; //create

destructor TCommandline.Destroy;
begin
 setlength(fs,0);    // get rid of the commandline string
 self.FCmdLineAsStringHash.clear;
 FreeAndNil( self.FCmdLineAsStringHash );
 inherited;
end;

function TCommandLine.Exists( N: string ): boolean;
begin
  Result := self.FCmdLineAsStringHash.containsKey(N);
end;

function TCommandLine.GetParameter(N: string; StripQuotes: boolean): string;
var
  sParmValue : string;
begin
  sParmValue := GetParameter( N );
  if ( StripQuotes ) then begin
    if ( sParmValue[1] = '''' ) then
      sParmValue := Copy(sParmValue, 2, Length( sParmValue ));
    if ( sParmValue[Length( sParmValue )] = '''' ) then
      sParmValue := Copy(sParmValue, 1, Length( sParmValue ) - 1);
  end;
  Result := sParmValue;
end;

function TCommandline.GetParameter( N:string ) : string;
Begin
  if ( not self.FCmdLineAsStringHash.containsKey(N) )  then
    result := ''
  else
    result := FCmdLineAsStringHash.Get(N);
End;

end.
