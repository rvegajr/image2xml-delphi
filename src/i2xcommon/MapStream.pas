//*****************************************************************//
//                                                                 //
//  TMapStream                                                     //
//  Copyright© BrandsPatch LLC                                     //
//  http://www.explainth.at                                        //
//                                                                 //
//  All Rights Reserved                                            //
//                                                                 //
//  Permission is granted to use, modify and redistribute          //
//  the code in this Delphi unit on the condition that this        //
//  notice is retained unchanged.                                  //
//                                                                 //
//  BrandsPatch declines all responsibility for any losses,        //
//  direct or indirect, that may arise  as a result of using       //
//  this code.                                                     //
//                                                                 //
//*****************************************************************//
unit MapStream;

interface

uses Windows,
    SysUtils,
    JclStrings,
    Classes,
    uFileDir,
    SyncObjs,
    uHashTable;

const
  //MAXCARDINALMINUS = (4294967295 - 1);
  MAXINTMINUS = MAXINT - 1;
  INVALID_SIZE = -1;
  MAP_DEBUG='MAP.LOG';

type


  TMapBytes = array[0..MAXINTMINUS] of Byte;
  PMapBytes = ^TMapBytes;


  TMapStream = class(TObject)
  private
    FHandle : THandle;
    FPosition:Cardinal;
    FSize:Cardinal;
    FTimeOut:Integer;
    FName : string;
    //This is the object that created the the Stream.  This is because
    //  the only object that should delete the stream should be object that created it
    procedure SetPosition(Value:Cardinal);
    function CountGood(Count:Cardinal):Boolean;
    function GrabEvent:Boolean;
    procedure ReleaseEvent;
    constructor CreateEx(const AName: String; ASize: Cardinal;
      ATimeOut: Integer);
    function CopyFrom(AStream:TStream;Count:Cardinal):Boolean;
    function CopyTo(AStream: TStream ): Boolean;
  protected
    FEvent:TEvent;
    FMemory:PMapBytes;
    property Name : string read FName write FName;
  public
    property Position:Cardinal read FPosition write SetPosition;
    property MMHandle:THandle read FHandle write FHandle;
    constructor Create( const AName:String;ASize:Cardinal;ATimeOut:Integer); overload;
    constructor Create( const AName:String;ASize:Cardinal); overload;
    constructor Create( const AName:String); overload;
    destructor Destroy;override;
    function Clear : Boolean;
    function ReadBuffer(P:Pointer;Count:Cardinal):Boolean;
    function WriteBuffer(P:Pointer;Count:Cardinal):Boolean;

    function Read( var str : string ):Boolean; overload;
    function Read( streamToWriteTo : TStream ):Boolean; overload;
    function Write( const str : string; DoClearFirst : boolean = false ):Boolean; overload;
    function Write( streamToReadFrom : TStream; DoClearFirst : boolean = false ):Boolean; overload;
  end;

  TMSDataType = ( dtString );
  TMSData = record
    DataType: TMSDataType;

    StringValue: AnsiString;

    class operator Implicit( const aValue: AnsiString ): TMSData;
  end;

  TMemoryStreamPlus = class( TMemoryStream )
    private
    public
      function Read( const DataTypeToReturn : TMSDataType = dtString ) : TMSData;
      function Write( const Value: TMSData ) : boolean;
      destructor Destroy; override;
      constructor Create( Value: TMSData ); overload; dynamic;
      constructor Create(); overload; dynamic;
  end;

type
  ENoMapping = class(Exception);

implementation

{ TMSData }

class operator TMSData.Implicit(const aValue: AnsiString): TMSData;
begin
  Result.DataType := dtString;
  Result.StringValue := aValue;
end;

{ TMapStream }
//....................................................MapStream Create & Destroy
constructor TMapStream.Create( const AName: String; ASize: Cardinal;
  ATimeOut: Integer);
begin
  CreateEx(AName, ASize, ATimeOut );
end;

constructor TMapStream.Create( const AName: String; ASize: Cardinal);
begin
  self.CreateEx( AName, ASize, 2000 );
end;

constructor TMapStream.Create( const AName: String);
begin
  self.CreateEx( AName, MAXWORD, 2000 );
end;

constructor TMapStream.CreateEx(const AName:String; ASize:Cardinal; ATimeOut:Integer);
Begin
  inherited Create;
  FHandle := 0;
  FSize:=ASize;
  FTimeOut:=ATimeOut;
  FName := AName;
  // This code will make sure that the entry has been added into the memory stream manager
  
  //if (FSize < 1) or (FSize > MAXCARDINALMINUS) then FSize:=MAXWORD;
  if ((FSize < 1) or (FSize > MAXINTMINUS)) then
    raise Exception.Create( 'Size specified is out of range (1 - ' + IntToStr(MAXINTMINUS) + ')' );
  //2000ms timeout for safety
  if ((FTimeOut < 1) or (FTimeOut > 5000)) then FTimeOut:=2000;
  //if (( ActiveMemoryMaps <> nil ) and (ActiveMemoryMaps.Exists( AName ))) then
  //  FTimeOut := FTimeOut * 1;
    //raise Exception.Create( 'A Memory Map of name ' + AName + ' has already been allocated.' );

  FHandle:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,FSize,PChar(AName));
  //See the Windows Kernel32 CreateFileMapping function for information
  if (FHandle = 0) then
    ENoMapping.Create(Format('%s file mapping failed.',[AName]))
  else begin
    //if ( ActiveMemoryMaps <> nil ) then ActiveMemoryMaps.Add( self );

    FMemory:=MapViewOfFile( FHandle,FILE_MAP_ALL_ACCESS,0,0,0 );
    if ( FMemory = nil ) then
      raise Exception.Create( 'MapViewOfFile Failed. ' + SysErrorMessage(GetLastError));
    //WriteString( uFileDir.GetModuleName() + '| MAP Allocated... Name=' + AName + '  Size=' + IntToStr(ASize) + '   Views Open=' + IntToStr(nOpenView), MAP_DEBUG, true );
    FEvent:=TEvent.Create(nil,True,True,Format('ExplainThat_%s_MAP',[AName]));
  end;
End;

destructor TMapStream.Destroy;
Begin
  //if (( ActiveMemoryMaps <> nil ) and (ActiveMemoryMaps.Exists( self.FName ))) then
   // ActiveMemoryMaps.Delete( self.FName );
  //WriteString(uFileDir.GetModuleName() + '| MAP CLOSE... Name=' + self.FName + '  Size=' + IntToStr(self.FSize) + '   Views Open=' + IntToStr(nOpenView), MAP_DEBUG, true );
  UnMapViewOfFile(FMemory);
  CloseHandle(FHandle);
  FEvent.Free;
  inherited;
End;
//.........................................................
function TMapStream.CountGood(Count:Cardinal):Boolean;
Begin
  Result:=(FPosition + Count < FSize);
End;

function TMapStream.GrabEvent:Boolean;
Begin
  Result:=True;
  with FEvent do
  begin
    case WaitFor(FTimeOut) of
      wrSignaled:ReSetEvent;
      {locks the event for exclusive use by this app. Funny name, ReSetEvent, not
       our choice!}
      else Result:=False;
    end;
  end;
End;

procedure TMapStream.ReleaseEvent;
Begin
  FEvent.SetEvent;//unlock the event so other apps can use it
End;
//========================================================MapStream Manipulation
function TMapStream.Clear:Boolean;
Begin
  if GrabEvent then
    try
      //FillChar(FMemory^[0],FSize,0);
      FillChar( FMemory^, FSize, 0 );
      FPosition:=0;
      Result:=True;
    finally
      ReleaseEvent
    end
  else
    Result:=False;
End;

function TMapStream.CopyFrom(AStream:TStream;Count:Cardinal):Boolean;

  function SizeGood:Boolean;
  var i,ASize:Int64;
  begin
    ASize:=AStream.Size;
    if (Count = 0) or (Count > ASize) then
    begin
      Count:=ASize;
      AStream.Position:=0;
    end;
    Result:=(FPosition + Count < FSize);
    {make sure the copy block is not too big. Incidentally, also make Count = 0
     as in Delphi.TStream}
  end;

Begin
  if SizeGood and GrabEvent then
  try
    AStream.ReadBuffer(Byte(FMemory^[FPosition]),Count);
    Result:=True;
  finally ReleaseEvent end else Result:=False;
End;

function TMapStream.CopyTo(AStream:TStream):Boolean;
var
  Count : Int64;
  ASize : Int64;
  P:PChar;
Begin
  if ( AStream = nil ) then
    raise Exception.Create( 'Passed Stream must not be nil');
  self.Position := 0;
  ReadBuffer(@ASize,sizeof(Integer));//read the mapped text size
  //AStream.Size := ASize;
  AStream.WriteBuffer( Byte(FMemory^[FPosition]), ASize );
End;

function TMapStream.Read(var str: string): Boolean;
var ASize, iMemSize:Integer;
    P : PChar;
begin
  Result := false;
  with Self do
  begin
    Position:=0;
    ReadBuffer(@ASize,sizeof(Integer));//read the mapped text size
    inc(ASize);
    iMemSize := ASize;
    P:=AllocMem( iMemSize );//create a buffer big enough to hold it
    P[iMemSize - 1]:=#0;//so we don't show garbage beyond the last character
    dec(ASize);
    try
    //WriteString(uFileDir.GetModuleName() + '|   ' + FName + ' Read(str):  DataLen=' + IntToStr(ASize), MAP_DEBUG, true );
      ReadBuffer( P, ASize );//read the mapped text
      //not doing this results in a memory leak which can bring down the app!
      str:= string( P );
      Result := true;
    finally
      FreeMem( P, iMemSize );
      iMemSize := 0;
    end;
  end;
end;

function TMapStream.Read(streamToWriteTo: TStream): Boolean;
var
  ASize, iMemSize:Integer;
begin
  Result := false;
  with Self do
  begin
    Position:=0;
    ASize := streamToWriteTo.Size;
    ReadBuffer(@ASize,sizeof(Integer));//read the mapped text size
    inc(ASize);
    iMemSize := ASize;
    dec(ASize);
    try
      //WriteString(uFileDir.GetModuleName() + '|   ' + FName + ' Read(streamToWriteTo):  DataLen=' + IntToStr(ASize), MAP_DEBUG, true );
      streamToWriteTo.Position := 0;
      streamToWriteTo.Write( Byte(FMemory^[FPosition]), ASize );
      inc(FPosition,ASize);
      streamToWriteTo.Position := 0;
      Result := true;
    finally
      iMemSize := 0;
    end;
  end;
end;

function TMapStream.ReadBuffer(P:Pointer;Count:Cardinal):Boolean;
Begin
  if CountGood(Count) and GrabEvent then
  try
    //WriteString(uFileDir.GetModuleName() + '|   ' + FName + ' ReadBuffer(P):  DataLen=' + IntToStr(Count), MAP_DEBUG, true );
    Move(FMemory^[FPosition],P^,Count);
    inc(FPosition,Count);
    Result:=True;
  finally ReleaseEvent end else Result:=False;
End;

function TMapStream.Write(const str: string; DoClearFirst : boolean ): Boolean;
Var
  i,ASize:Integer;
  AText:String;
Begin
  //WriteString(uFileDir.GetModuleName() + '|   ' + FName + ' Write(str):  DataLen=' + IntToStr(Length(str)), MAP_DEBUG, true );
  Result := false;
  AText:=str;
  ASize:=Length(AText);
  with Self do
  begin
    if DoClearFirst then Clear;
    WriteBuffer(@ASize,sizeof(Integer));//first write the text length
    WriteBuffer(PChar(AText),ASize);//then write the text itself
    Result := true;
  end;
  //i:=BSM_APPLICATIONS;
  //BroadcastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
  //                       @i,FMessage,0,0);
End;

function TMapStream.Write(streamToReadFrom: TStream; DoClearFirst : boolean ): Boolean;
Var
  i,ASize:Integer;
Begin
  Result := false;
  ASize:=streamToReadFrom.Size;
  FPosition := 0;
  if (FPosition + sizeof(Integer) + ASize > FSize) then
    raise Exception.Create('Size of stream to write into Memory map (' + IntToStr(ASize) + ') is too big (Max=(' + IntToStr(FSize) + '))');
  with Self do
  begin
    if DoClearFirst then Clear;
    try
      //WriteString(uFileDir.GetModuleName() + '|   ' + FName + ' Write(streamToReadFrom):  DataLen=' + IntToStr(ASize), MAP_DEBUG, true );
      WriteBuffer(@ASize,sizeof(Integer));//first write the text length
      streamToReadFrom.Position := 0;
      streamToReadFrom.ReadBuffer(Byte(FMemory^[FPosition]),ASize);
    except
      raise;
    end;
    Result := true;
  end;
  //i:=BSM_APPLICATIONS;
  //BroadcastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
  //                       @i,FMessage,0,0);
End;

function TMapStream.WriteBuffer(P:Pointer;Count:Cardinal):Boolean;
Begin
  if CountGood(Count) and GrabEvent then
  try
    //WriteString(uFileDir.GetModuleName() + '|   ' + FName + ' WriteBuffer(P):  DataLen=' + IntToStr(Count), MAP_DEBUG, true );
    Move(P^,FMemory^[FPosition],Count);
    inc(FPosition,Count);
    Result:=True;
  finally ReleaseEvent end else Result:=False;
End;

procedure TMapStream.SetPosition(Value:Cardinal);
Begin
  if (Value < FSize) and (Value >= 0) then FPosition:=Value;
End;

{ TMemoryStreamPlus }

constructor TMemoryStreamPlus.Create(Value: TMSData);
begin
  self.Write( Value );
end;

constructor TMemoryStreamPlus.Create;
begin
end;

destructor TMemoryStreamPlus.Destroy;
begin
  inherited;
end;

function TMemoryStreamPlus.Read(const DataTypeToReturn: TMSDataType): TMSData;
var
  lng : integer;
  s:string;
Begin
  if ( DataTypeToReturn = dtString ) then begin
    self.Position := 0;
    self.ReadBuffer(lng, SizeOf(lng));
    SetLength(s, lng);
    self.ReadBuffer(s[1], lng);
    Result := s;
  end;
End;

function TMemoryStreamPlus.Write(const Value: TMSData): boolean;
var
  lng : integer;
Begin
  Result := false;
  try
    if ( Value.DataType = dtString )  then begin
      lng := Length( Value.StringValue );
      self.WriteBuffer(lng, SizeOf(lng));
      // Write the string to the stream. We want to write from SourceString's
      // data, starting at a pointer to SourceString (this returns a pointer to
      // the first character), dereferenced (this gives the actual character).
      self.WriteBuffer(Value.StringValue[1], Length(Value.StringValue));
      Result := true;
    end;
  finally

  end;
End;


initialization
finalization
END.
