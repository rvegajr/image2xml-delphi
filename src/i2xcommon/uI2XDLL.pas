unit uI2XDLL;

interface
uses
  Graphics,
  SysUtils,
  Classes,
  Windows,
  uStrUtil,
  uFileDir,
  OmniXML,
  GR32,
  GR32_Image,
  MCMImage,
  uHashTable,
  MapStream,
  uDWImage,
  uI2XConstants,
  uI2XMemMap;

const
  MAX_BUFFER=10240;
  AUTO_CLEANUP_DEFAULT_VALUE = 1;   // 0 = off, 1 = on

type
  TI2XImgParmType = ( ptText, ptCombo, ptSlider );
  TProcessImage = function ( lpMemoryMapName : PChar; pInstructionList : PChar ) : integer; stdcall;
  TI2XDLLCleanUp = function () : integer; stdcall;
  TOCRImage = function ( lpImageMMID, lpOCRDataMMID : PChar ) : integer; stdcall;
  TSetOptions = function ( lpOptions : PChar ) : integer; stdcall;
  TGetCapabilities = function (pInstructionList : PChar; lMaxLen : integer ) : integer;  stdcall;
  TGetVersion = function ( lpVersion : PChar; lMaxLen : integer ) :  integer; stdcall;
  TGetGUID = function ( lpVersion : PChar; lMaxLen : integer ) :  integer; stdcall;
  TGetDescription = function ( lpVersion : PChar; lMaxLen : integer ) :  integer; stdcall;
  TGetLongName = function ( lpVersion : PChar; lMaxLen : integer ) :  integer; stdcall;
  TGetShortName = function ( lpVersion : PChar; lMaxLen : integer ) :  integer; stdcall;
  TDLLInitialize = function () :  integer; stdcall;
  TDLLTerminate = function () :  integer; stdcall;
  TGetLastErrorMessage = function ( lpVersion : PChar; lMaxLen : integer ) :  integer; stdcall;
  TGetLastErrorCode = function ( ) :  integer; stdcall;
  //TI2XJobInfoEvent = procedure (Sender: TObject; totalProcessed : Cardinal) of object;

  TOnInstructionStartEvent = procedure( Sender : TObject; Instruction : string; InstructionOrder : integer ) ;
  TOnCancelEvent = procedure( Sender : TObject; TaskID : string );
  TOnInstructionEndEvent = procedure( Sender : TObject; TaskID : string; Instruction : string; InstructionOrder : integer );
  TOnOCRProcJobStartEvent = procedure( Sender : TObject; TaskID : string; ImagesToOCR : integer ) of object;
  TOnOCRProcJobEndEvent = procedure( Sender : TObject; TaskID : string; ImagesOCRd : integer; OCRDataMM: string ) of object;
  TOnJobStartEvent = procedure( Sender : TObject; TaskID : string; TaskCount : integer ) of object;
  TOnJobEndEvent = procedure( Sender : TObject; TaskID : string; TasksCompleted : integer; ImageMemoryMap: string ) of object;
  TOnReturnXMLEvent = procedure( Sender : TObject; TaskID : string; ResultXML: string ) of object;
  TOnJobErrorEvent = procedure( Sender : TObject; TaskID : string; ErrorNo : integer; ErrorString: string ) of object;

  TOnImageProcJobEndEvent = procedure( Sender : TObject; TaskID : string; InstructionsCompleted : integer; Image: TDWImage ) of object;
  TOnImageProcJobStartEvent = procedure( Sender : TObject; TaskID : string; InstructionsCount : integer ) of object;
  TOnStatusChangeEvent = procedure( Sender : TObject; TaskID : string; StatusMessage : string ) of object;

  TI2XImgParm = class(TObject)
    private
      FID : string;
      FLabel : string;
    public
    published
      property ID : string read FID write FID;
      property LabelText : string read FLabel write FLabel;
      constructor Create();
      destructor Destroy; override;
  end;

  TI2XImageInstruction = class(TObject)
    private
      FID : string;
      FNeumonic : string;
      FName : string;
      FDescrName : string;
      FDescription : string;
      FImgParms : THashTable;
      FParent : TObject;
      FDLLName : string;
    public
    published
      property ID : string read FID write FID;
      property Neumonic : string read FNeumonic write FNeumonic;
      property Name : string read FName write FName;
      property DescrName : string read FDescrName write FDescrName;
      property Description : string read FDescription write FDescription;
      property ImgParms : THashTable read FImgParms write FImgParms;
      property Parent : TObject read FParent write FParent;
      property DLLName : string read FDLLName write FDLLName;
      procedure AddParm( parmToAdd : TI2XImgParm );
      constructor Create();
      destructor Destroy; override;
  end;

  TI2XImageInstructions = class(TObject)
    private
      FInstructions : THashTable;
    public
      property Instructions : THashTable read FInstructions write FInstructions;
      function ReadParmFile( const I2XFileName, DLLName : TFileName ) : boolean;
      procedure AddInstruction( instToAdd : TI2XImageInstruction );
      constructor Create();
      destructor Destroy; override;
  end;

  TI2XImgParmText = class(TI2XImgParm)
    private
      FDataType : string;
    public
    published
      property DataType : string read FDataType write FDataType;
      constructor Create();
      destructor Destroy; override;
  end;
  TI2XImgParmSlider = class(TI2XImgParm)
    private
      FMin : integer;
      FMax : integer;
      FDefault : integer;
    public
    published
      property Min : integer read FMin write FMin;
      property Max : integer read FMax write FMax;
      property DefaultValue : integer read FDefault write FDefault;
      constructor Create();
      destructor Destroy; override;
  end;

  TI2XImgParmCombo = class(TI2XImgParm)
    private
      FOptions : THashStringTable;
    public
    published
      procedure AddOption( const optionToAdd : string );
      property Options : THashStringTable read FOptions write FOptions;
      constructor Create();
      destructor Destroy; override;
  end;

  TDLLObject = class(TObject)
    private
      FMemoryMapManager : TI2XMemoryMapManager;
      FDLLName : string;
      FDLLHandle : THandle;
      FVersion : string;
      FGetVersionDLL : TGetVersion;
      FCleanUp : TI2XDLLCleanUp;
      FDLLInitialize : TDLLInitialize;
      FDLLTerminate : TDLLTerminate;
      FDLLGetGUID : TGetGUID;
      FDLLGetDescription : TGetDescription;
      FDLLGetLongName : TGetLongName;
      FDLLGetShortName : TGetShortName;
	    FDLLGetLastErrorCode: TGetLastErrorCode;
	    FDLLGetLastErrorMessage : TGetLastErrorMessage;
      FGUID : string;
      FAutoCleanUp : boolean;
      property GetVersionDLL : TGetVersion read FGetVersionDLL write FGetVersionDLL;
      property CleanUpDLL : TI2XDLLCleanUp read FCleanUp write FCleanUp;
      property DLLInitialize : TDLLInitialize read FDLLInitialize write FDLLInitialize;
      property DLLTerminate : TDLLTerminate read FDLLTerminate write FDLLTerminate;
      property DLLGetGUID : TGetGUID read FDLLGetGUID write FDLLGetGUID;
      property DLLGetDescription : TGetDescription read FDLLGetDescription write FDLLGetDescription;
      property DLLGetLongName : TGetLongName read FDLLGetLongName write FDLLGetLongName;
      property DLLGetShortName : TGetShortName read FDLLGetShortName write FDLLGetShortName;
      property DLLGetLastErrorCode : TGetLastErrorCode read FDLLGetLastErrorCode write FDLLGetLastErrorCode;
      property DLLGetLastErrorMessage : TGetLastErrorMessage read FDLLGetLastErrorMessage write FDLLGetLastErrorMessage;
      //procedure setDLLNameDyn( DLLFileName : string ); dynamic;
      procedure setDLLName( DLLFileName : string ); Virtual;
      function getVersionInfo() : string;
      function getGUID() : string;
      function getShortName() : string;
      function getLongName() : string;
      function getDescription() : string;
      function getLastErrorCode() : integer;
      function getLastErrorMessage() : string;
      function isDLLLoaded() : boolean;
      function isValidProc( procName : string ) : boolean;
    public
      property AutoCleanUp : boolean read FAutoCleanUp write FAutoCleanUp;
      property MemoryMapManager : TI2XMemoryMapManager read FMemoryMapManager write FMemoryMapManager;
      property DLLName : string read FDLLName write setDLLName;
      property DLLHandle : THandle read FDLLHandle;
      property Version : string read getVersionInfo;
      property GUID : string read getGUID;
      property ShortName : string read getShortName;
      property Name : string read getLongName;
      property Description : string read getDescription;
      property LastErrorCode : integer read getLastErrorCode;
      property LastErrorMessage : string read getLastErrorMessage;
      procedure CleanUp;
      constructor Create( DLLFileName : string ); overload; virtual;
      constructor Create(); overload; virtual;
      destructor Destroy; override;
  end;

  TDLLImageProc = class(TDLLObject)
    private
      FGUID : string;
      FProcessImageDLL : TProcessImage;
      FGetCapabilitiesDLL : TGetCapabilities;
      FXMLParm : TI2XImageInstructions;
      FParmFileLoaded : boolean;
      FImage: TDWImage;

      procedure setDLLName( DLLFileName : string ); override;
      function getImageInstructions() : TI2XImageInstructions;
      function CallDLLAndLoadParmFile() : boolean;
      property ProcessImageDLL : TProcessImage read FProcessImageDLL write FProcessImageDLL;
      property GetCapabilitiesDLL : TGetCapabilities read FGetCapabilitiesDLL write FGetCapabilitiesDLL;
      function GetCapabilities( InstructionList : TStringList2 ) : integer;
    public
      function ProcessImage( Image : TDWImage; InstructionList : TStringList2 ) : integer; overload;
      function ProcessImage( Image : TDWImage; InstructionList : string ) : integer; overload;
      function ProcessImage( const MemoryMapID : string ; InstructionList : string ) : integer; overload;
      property ImageInstructions : TI2XImageInstructions read getImageInstructions write FXMLParm;
      procedure ReloadImageInstructions;
      constructor Create( DLLFileName : string ); overload; override;
      constructor Create(); overload; override;
      destructor Destroy; override;
  end;

  TDLLOCREngine = class(TDLLObject)
    private
      FOCRImageDLL : TOCRImage;
      FSetOptionsDLL : TSetOptions;
      procedure setDLLName( DLLFileName : string ); override;
    public
      function OCRImage( const ImageMMID, OCRDataMMID: TMemoryMapID; invertColors : boolean = false ) : boolean;
      constructor Create( DLLFileName : string ); overload; override;
      constructor Create(); overload; override;
      destructor Destroy; override;
  end;

procedure LoadLibaryTest(DLLFileName: string);

implementation
{ TDLLObject }

constructor TDLLObject.Create(DLLFileName: string);
begin
  inherited Create();
  FGUID := '';
  FAutoCleanUp := (AUTO_CLEANUP_DEFAULT_VALUE <> 0);
  self.setDLLName( DLLFileName );
end;

procedure TDLLObject.CleanUp;
begin
  self.FCleanUp();
end;

constructor TDLLObject.Create;
begin
  inherited Create();
  FAutoCleanUp := (AUTO_CLEANUP_DEFAULT_VALUE <> 0);
  FGUID := '';
end;

destructor TDLLObject.Destroy;
var
  ret : integer;
  exitCode : Cardinal;
begin
  if FDLLHandle <> 0 then begin
    ret := FDLLTerminate();
    FreeLibrary( FDLLHandle );
  end;
  inherited;
end;

function TDLLObject.getDescription: string;
var
  len : integer;
  Buffer: array[0..MAX_BUFFER] of Char;
begin
  Result := '';
  FillChar( Buffer, SizeOf(Buffer),#0);
  if ( self.isDLLLoaded ) then begin
    len := FDLLGetDescription(Buffer, MAX_BUFFER );
    if ( len > 0 ) then
      Result := string( Buffer )
    else
      Result := '';
  end;
end;

function TDLLObject.getGUID: string;
var
  len : integer;
  Buffer: array[0..MAX_BUFFER] of Char;
begin
  Result := '';
  FillChar( Buffer, SizeOf(Buffer),#0);
  if ( self.isDLLLoaded ) then begin
    len := self.FDLLGetGUID(Buffer, SizeOf(Buffer) );
    if ( len > 0 ) then
      Result := string( Buffer )
    else
      Result := '';
  end;
end;

function TDLLObject.getLastErrorCode: integer;
begin
  Result := FDLLGetLastErrorCode();
end;

function TDLLObject.getLastErrorMessage: string;
var
  len : integer;
  Buffer: array[0..MAX_BUFFER] of Char;
begin
  Result := '';
  FillChar( Buffer, SizeOf(Buffer),#0);
  if ( self.isDLLLoaded ) then begin
    len := FDLLGetLastErrorMessage(Buffer, MAX_BUFFER );
    if ( len > 0 ) then
      Result := string( Buffer )
    else
      Result := '';
  end;
end;

function TDLLObject.getLongName: string;
var
  len : integer;
  Buffer: array[0..MAX_BUFFER] of Char;
begin
  Result := '';
  FillChar( Buffer, SizeOf(Buffer),#0);
  if ( self.isDLLLoaded ) then begin
    len := FDLLGetLongName(Buffer, MAX_BUFFER );
    if ( len > 0 ) then
      Result := string( Buffer )
    else
      Result := '';
  end;
end;

function TDLLObject.getShortName: string;
var
  len : integer;
  Buffer: array[0..MAX_BUFFER] of Char;
begin
  Result := '';
  FillChar( Buffer, SizeOf(Buffer),#0);
  if ( self.isDLLLoaded ) then begin
    len := FDLLGetShortName(Buffer, MAX_BUFFER );
    if ( len > 0 ) then
      Result := string( Buffer )
    else
      Result := '';
  end;
end;

function TDLLObject.getVersionInfo: string;
var
  len : integer;
  Buffer: array[0..MAX_BUFFER] of Char;
begin
  Result := '';
  FillChar( Buffer, SizeOf(Buffer),#0);
  if ( self.isDLLLoaded ) then begin
    len := FGetVersionDLL(Buffer, MAX_BUFFER );
    if ( len > 0 ) then
      Result := string( Buffer )
    else
      Result := '';
  end;
end;

function TDLLObject.isDLLLoaded: boolean;
begin
  if ( FDLLHandle = 0 ) then begin
    raise Exception.Create('External DLL "' + self.FDLLName + '" does not exist or could not be loaded...  Does this DLL exist in the path?');
    Result := false;
  end else
    Result := true;
end;

function TDLLObject.isValidProc(procName: string): boolean;
var
  ptr : Pointer;
begin
  Result := false;
  if FDLLHandle <> 0 then begin
    ptr := getProcAddress(  FDLLHandle, PChar( procName ) );
    if (ptr = nil) then
      raise Exception.Create('External Function "' + procName + '" is nil!  Does this function exist?')
    else
      Result := true;
  end else
    raise Exception.Create('External DLL "' + self.FDLLName + '" does not exist or could not be loaded...  Does this DLL exist in the path?');
end;

procedure TDLLObject.setDLLName(DLLFileName: string);
var
  lpGUID : PChar;
  DLLFullFileName: string;
  Buffer: array[0..MAX_PATH] of Char;
  val : integer;
  oFileDir : CFileDir;
begin
  try
    FDLLHandle := loadLibrary ( PChar( DLLFileName ) );
    if FDLLHandle <> 0 then begin
      @FDLLInitialize := getProcAddress ( FDLLHandle, 'Initialize' );
      @FGetVersionDLL := getProcAddress ( FDLLHandle, 'GetVersion' );
      @FCleanUp := getProcAddress ( FDLLHandle, 'CleanUp' );
      @FDLLTerminate  := getProcAddress ( FDLLHandle, 'Terminate' );
      @FDLLGetGUID  := getProcAddress ( FDLLHandle, 'GetGUID' );
      @FDLLGetDescription  := getProcAddress ( FDLLHandle, 'GetDescription' );
      @FDLLGetLongName  := getProcAddress ( FDLLHandle, 'GetName' );
      @FDLLGetShortName  := getProcAddress ( FDLLHandle, 'GetShortName' );
      @FDLLGetLastErrorMessage  := getProcAddress ( FDLLHandle, 'GetLastErrorMessage' );
      @FDLLGetLastErrorCode  := getProcAddress ( FDLLHandle, 'GetLastErrorCode' );

      if addr( FGetVersionDLL ) = nil then
        raise Exception.Create('External Function "GetVersion" is nil!  Does this function exist?');
      if addr( FCleanUp ) = nil then
        raise Exception.Create('External Function "CleanUp" is nil!  Does this function exist?');
      if addr( FDLLInitialize ) = nil then
        raise Exception.Create('External Function "Initialize" is nil!  Does this function exist?');
      if addr( FDLLTerminate ) = nil then
        raise Exception.Create('External Function "Terminate" is nil!  Does this function exist?');
      if addr( FDLLGetGUID ) = nil then
        raise Exception.Create('External Function "GetGUID" is nil!  Does this function exist?');
      if addr( FDLLGetDescription ) = nil then
        raise Exception.Create('External Function "GetDescription" is nil!  Does this function exist?');
      if addr( FDLLGetLongName ) = nil then
        raise Exception.Create('External Function "GetName" is nil!  Does this function exist?');
      if addr( FDLLGetShortName ) = nil then
        raise Exception.Create('External Function "GetShortName" is nil!  Does this function exist?');
      if addr( FDLLGetLastErrorMessage ) = nil then
        raise Exception.Create('External Function "GetLastErrorMessage" is nil!  Does this function exist?');
      if addr( FDLLGetLastErrorCode ) = nil then
        raise Exception.Create('External Function "GetLastErrorCode" is nil!  Does this function exist?');
      val := FDLLInitialize();
    end else
      raise Exception.Create('External DLL "' + DLLFileName + '" does not exist or could not be loaded...  Does this DLL exist in the path?');
    self.FDLLName := DLLFileName;
    oFileDir := CFileDir.Create;
    if ( Length(ExtractFilePath(self.FDLLName)) = 0 ) then begin
      FillChar( Buffer, SizeOf(Buffer),#0);
      GetModuleFileName(FDLLHandle, Buffer, MAX_PATH);
      DLLFullFileName := string(Buffer);
      self.FDLLName := oFileDir.ChangePath( self.FDLLName, ExtractFilePath(DLLFullFileName));
    end;
  finally
    FreeAndNil( oFileDir );
  end;
end;

{ TDLLImageProc }

function TDLLImageProc.CallDLLAndLoadParmFile: boolean;
var
  InstructionList: TStringList2;
begin
  try
    InstructionList := TStringList2.Create;
    Result := GetCapabilities( InstructionList ) > 0;
  finally
    FreeAndNil( InstructionList );
  end;
end;

constructor TDLLImageProc.Create;
begin
  inherited Create();
  if ( FXMLParm = nil ) then FXMLParm := TI2XImageInstructions.Create;
  FParmFileLoaded := false;
end;

constructor TDLLImageProc.Create(DLLFileName: string);
begin
  inherited Create( DLLFileName );
  if ( FXMLParm = nil ) then FXMLParm := TI2XImageInstructions.Create;
  FParmFileLoaded := false;
end;

destructor TDLLImageProc.Destroy;
begin
  if ( FXMLParm <> nil ) then FreeAndNil( FXMLParm );
  inherited;
end;

function TDLLImageProc.GetCapabilities(InstructionList: TStringList2): integer;
var
  sInstStr, sParmFile : string;
  InstStrLen : Integer;
  Buffer: array[0..MAX_BUFFER] of Char;
begin
  Result := -1;
  if (( self.isDLLLoaded() ) and ( self.isValidProc('GetCapabilities'))) then begin
    FillChar( Buffer, SizeOf(Buffer),#0);
    InstStrLen := Self.FGetCapabilitiesDLL( Buffer, MAX_BUFFER );
    if ( InstStrLen > 0 )  then begin
      sInstStr := string( Buffer );
      if ( Pos('I2XCFG:', sInstStr ) > 0 )  then begin
        sParmFile := StringReplace( sInstStr, 'I2XCFG:', '', [rfReplaceAll]);
        if ( not FileExists(sParmFile) ) then
          raise Exception.Create('Called I2X Image Plugin passed a parm file that did not exist! ParmFile=' + sParmFile)
        else
          if not self.FXMLParm.ReadParmFile( sParmFile, self.DLLName ) then
            raise Exception.Create('Called I2X Image Plugin passed a parm file that could not be parsed! ParmFile=' + sParmFile);
        Result := 1;
      end else begin
        InstructionList.fromString( sInstStr );
        Result := InstructionList.Count;
      end;

    end;
  end;
end;

function TDLLImageProc.getImageInstructions: TI2XImageInstructions;
begin
  if ( not FParmFileLoaded ) then begin
    CallDLLAndLoadParmFile();
    FParmFileLoaded := true;
  end;

  Result := self.FXMLParm;
end;

function TDLLImageProc.ProcessImage(Image: TDWImage;
  InstructionList: string ): integer;
var
  sInstStr, sMapName : string;
  pInstStr, pMapName : PChar;
  InstStrLen : Integer;
  LastStreamSize : cardinal;
  FMapNameBuffer : array[0..MAX_BUFFER] of Char;
  FInstStrBuffer : array[0..MAX_BUFFER] of Char;
begin
  if (( self.isDLLLoaded() ) and ( self.isValidProc('ProcessImage'))) then begin

      FillChar( FMapNameBuffer, SizeOf(FMapNameBuffer),#0);
      FillChar( FInstStrBuffer, SizeOf(FInstStrBuffer),#0);
      StrLCopy( FInstStrBuffer, PCHar( InstructionList ), MAX_BUFFER );

    //Image.SaveToMemoryMap( sMapName );

      FMemoryMapManager.DebugMessageText := 'TDLLImageProc[' + self.DLLName + '].ProcessImage() - Before Image Process';
      FMemoryMapManager.Write( sMapName, Image );
      StrLCopy( FMapNameBuffer, PCHar( sMapName ), MAX_BUFFER );
      LastStreamSize := Image.LastStreamSize;

      try
        InstStrLen := Self.FProcessImageDLL( FMapNameBuffer, FInstStrBuffer );
        //Read the image that has been processed by the DLL from the memory map
        FMemoryMapManager.DebugMessageText := 'TDLLImageProc[' + self.DLLName + '].ProcessImage() - After Image Process.. recieve from DLL';
        FMemoryMapManager.Read( sMapName, Image );
      finally
        if ( FAutoCleanUp ) then Self.CleanUp();
      end;
    //Image.LoadFromMemoryMap( sMapName, LastStreamSize + 4 ); //add 4 for the stream size integer at the beginning of the MapStream
  end;
end;

Function TDLLImageProc.ProcessImage(Image: TDWImage;
  InstructionList: TStringList2 ): integer;
Var
  sInstStr, sMapName : string;
  pInstStr, pMapName : PChar;
  InstStrLen : Integer;
  LastStreamSize : Cardinal;
  FMapNameBuffer : array[0..MAX_BUFFER] of Char;
  FInstStrBuffer : array[0..MAX_BUFFER] of Char;
Begin
  if (( self.isDLLLoaded() ) and ( self.isValidProc('ProcessImage'))) then begin
    pInstStr := PChar( InstructionList.asString() );
    FMemoryMapManager.Write( sMapName, Image );
    //Image.SaveToMemoryMap( sMapName );
    LastStreamSize := Image.LastStreamSize;
    StrLCopy( FMapNameBuffer, PCHar( sMapName ), MAX_BUFFER );
    InstStrLen := Self.FProcessImageDLL( pMapName, pInstStr );
    try
      //Image.LoadFromMemoryMap( sMapName, LastStreamSize + 4 ); //add 4 for the stream size integer at the beginning of the MapStream
      FMemoryMapManager.Read( sMapName, Image );
      //Image.SaveToFile('C:\Dark\pascal\Image2XML\templates\TEST.bmp');
    finally
      if ( FAutoCleanUp ) then Self.CleanUp();
    end;
  end;
End;

Function TDLLImageProc.ProcessImage(const MemoryMapID: string;
  InstructionList: string): integer;
Var
  sInstStr, sMapName : string;
  pInstStr, pMapName : PChar;
  InstStrLen : Integer;
  FMapNameBuffer : array[0..MAX_BUFFER] of Char;
  FInstStrBuffer : array[0..MAX_BUFFER] of Char;
Begin
  sMapName := MemoryMapID;
  if (( self.isDLLLoaded() ) and ( self.isValidProc('ProcessImage'))) then begin
    FillChar( FMapNameBuffer, SizeOf(FMapNameBuffer),#0);
    FillChar( FInstStrBuffer, SizeOf(FInstStrBuffer),#0);
    StrLCopy( FInstStrBuffer, PCHar( InstructionList ), MAX_BUFFER );
    StrLCopy( FMapNameBuffer, PCHar( sMapName ), MAX_BUFFER );

    try
      InstStrLen := Self.FProcessImageDLL( FMapNameBuffer, FInstStrBuffer );
    finally
      if ( FAutoCleanUp ) then Self.CleanUp();
    end;
    Result := Self.LastErrorCode;
  end;
End;

procedure TDLLImageProc.ReloadImageInstructions;
begin
  self.CallDLLAndLoadParmFile;
end;

procedure TDLLImageProc.setDLLName(DLLFileName: string);
Begin
  inherited setDLLName( DLLFileName );
  if FDLLHandle <> 0 then begin
    @FGetCapabilitiesDLL  := getProcAddress ( FDLLHandle, 'GetCapabilities' );
    @FProcessImageDLL := getProcAddress ( FDLLHandle, 'ProcessImage' );
    if addr( FDLLGetDescription ) = nil then
      raise Exception.Create('External Function "GetDescription" is nil!  Does this function exist?');
  end;
End;

procedure LoadLibaryTest(DLLFileName: string);
var
  lpGUID : PChar;
  Buffer: array[0..254] of Char;
  val : integer;
  FDLLHandle : cardinal;
      FGetVersionDLL : TGetVersion;
      FDLLInitialize : TDLLInitialize;
      FDLLTerminate : TDLLTerminate;
      FGUID : string;
begin
  FDLLHandle := loadLibrary ( PChar( DLLFileName ) );
  if FDLLHandle <> 0 then begin

    @FDLLInitialize := getProcAddress ( FDLLHandle, 'Initialize' );
    @FGetVersionDLL := getProcAddress ( FDLLHandle, 'GetVersion' );
    @FDLLTerminate  := getProcAddress ( FDLLHandle, 'Terminate' );
    if addr( FGetVersionDLL ) = nil then
      raise Exception.Create('External Function "GetVersion" is nil!  Does this function exist?');
    if addr( FDLLInitialize ) = nil then
      raise Exception.Create('External Function "Initialize" is nil!  Does this function exist?');
    if addr( FDLLTerminate ) = nil then
      raise Exception.Create('External Function "Terminate" is nil!  Does this function exist?');
    val := FDLLInitialize();
  end else
    raise Exception.Create('External DLL "' + DLLFileName + '" does not exist or could not be loaded...  Does this DLL exist in the path?');
  if ( FreeLibrary( FDLLHandle ) ) then
    FGUID := 'YES'
  else
    FGUID := 'NO';

end;

{ TI2XImageInstructions }

procedure TI2XImageInstructions.AddInstruction(instToAdd: TI2XImageInstruction);
begin
  instToAdd.Parent := self;
  self.FInstructions.Add( instToAdd.FNeumonic, instToAdd );
end;

constructor TI2XImageInstructions.Create;
begin
  FInstructions := THashTable.Create;
end;

destructor TI2XImageInstructions.Destroy;
begin
  FreeAndNil( FInstructions );
  inherited;
end;

function TI2XImageInstructions.ReadParmFile(
  const I2XFileName, DLLName : TFileName): boolean;
var
  xmlDoc: IXMLDocument;
  nod, nodParmData : IXMLNode;
  ele, eleParm : IXMLElement;
  xmlNodeList, xmlNodeListParm, xmlNodeListSelect: IXMLNodeList;
  i, iParm, iOptions : integer;
  imgInst : TI2XImageInstruction;
  imgParm : TI2XImgParm;
  ParmText : TI2XImgParmText;
  ParmCbo : TI2XImgParmCombo;
  ParmSlider : TI2XImgParmSlider;
begin
  Result := false;
  xmlDoc := CreateXMLDoc;
  if not xmlDoc.Load( I2XFileName ) then
    raise Exception.Create( 'Could not parse I2X Plugin Configuration file "' + I2XFileName + '"!' );

  xmlNodeList := xmldoc.DocumentElement.ChildNodes;
  if ( xmlNodeList.length > 0 ) then begin
    for i := 0 to xmlNodeList.length - 1 do begin
      nod := IXMLNode(xmlNodeList.Item[i]);
      if ( nod.NodeType = ELEMENT_NODE ) then begin
        if ( nod.NodeName='instruction') then begin
          ele := IXMLElement( nod );
          imgInst := TI2XImageInstruction.Create;
          imgInst.DLLName := DLLName;
          imgInst.Name :=  nod.Attributes.GetNamedItem('name').Text;
          imgInst.Neumonic :=  nod.Attributes.GetNamedItem( 'neumonic' ).Text;
          imgInst.FDescrName :=  nod.Attributes.GetNamedItem( 'name_in_list' ).Text;
          if ( ele.HasChildNodes ) then begin
            ele.SelectSingleNode( 'description', nod );
            if nod = nil then
              raise Exception.Create('Description was not found for instruction ' + imgInst.Name )
            else
              imgInst.Description := nod.Text;

            ele.SelectSingleNode( 'parameters', nod );
            if nod <> nil then begin
              xmlNodeListParm := nod.SelectNodes( 'parameter' );
              for iParm := 0 to xmlNodeListParm.length - 1 do begin
                eleParm := IXMLElement(xmlNodeListParm.Item[iParm]);
                if ( not eleParm.HasChildNodes ) then
                  raise Exception.Create( 'Parameters MUST have child elements of select, slider or text.' )
                else begin

                  eleParm.SelectSingleNode( 'text', nodParmData );
                  if ( nodParmData <> nil ) then begin
                    ParmText := TI2XImgParmText.Create;
                    ParmText.ID :=  xmlNodeListParm.Item[iParm].Attributes.GetNamedItem('id').text;
                    ParmText.LabelText := xmlNodeListParm.Item[iParm].Attributes.GetNamedItem('label').text;
                    ParmText.DataType := nodParmData.Attributes.GetNamedItem('type').Text;
                    imgInst.AddParm( ParmText );
                  end;
                  eleParm.SelectSingleNode( 'slider', nodParmData );
                  if ( nodParmData <> nil ) then begin
                    ParmSlider := TI2XImgParmSlider.Create;
                    ParmSlider.ID :=  xmlNodeListParm.Item[iParm].Attributes.GetNamedItem('id').text;
                    ParmSlider.LabelText := xmlNodeListParm.Item[iParm].Attributes.GetNamedItem('label').text;
                    ParmSlider.Min := StrToInt( nodParmData.Attributes.GetNamedItem('min').Text );
                    ParmSlider.Max := StrToInt( nodParmData.Attributes.GetNamedItem('max').Text );
                    ParmSlider.DefaultValue := StrToInt( nodParmData.Attributes.GetNamedItem('default').Text );
                    imgInst.AddParm( ParmSlider );
                    //imgInst.FImgParms.Add( ParmSlider );
                  end;
                  eleParm.SelectSingleNode( 'select', nodParmData );
                  if ( nodParmData <> nil ) then begin
                    ParmCbo := TI2XImgParmCombo.Create;
                    ParmCbo.ID :=  xmlNodeListParm.Item[iParm].Attributes.GetNamedItem('id').text;
                    ParmCbo.LabelText := xmlNodeListParm.Item[iParm].Attributes.GetNamedItem('label').text;
                    xmlNodeListSelect := nodParmData.SelectNodes('option');
                    if xmlNodeListSelect.Length > 0 then begin
                      for iOptions := 0 to xmlNodeListSelect.length - 1 do begin
                        ParmCbo.AddOption( IXMLElement( xmlNodeListSelect.Item[iOptions] ).Text );
                      end;
                    end;
                    //imgInst.FImgParms.Add( ParmCbo );
                    imgInst.AddParm( ParmCbo );
                  end;
                end;

              end;
            end;
          end;
          self.AddInstruction( imgInst );
        end else if ( nod.NodeName = 'debug' ) then begin
          //debug is ignored on the UI level,  this debug is used in the
          //  individual DLL,  the UI app debug is triggered via the command line
        end else
          raise Exception.Create('Expected "instruction" node!');
      end;
    end;
  end;
  if self.Instructions.ContainsKey( 'GREY_THRESH' ) then
    ParmSlider := TI2XImgParmSlider(self.Instructions.Get( 'GREY_THRESH' ));
  Result := true;
end;

{ TI2XImageInstruction }

procedure TI2XImageInstruction.AddParm(parmToAdd: TI2XImgParm);
begin
  self.FImgParms.Add( parmToAdd.ID, parmToAdd );
end;

constructor TI2XImageInstruction.Create;
begin
  self.FImgParms := THashTable.Create;
end;

destructor TI2XImageInstruction.Destroy;
begin
  FreeAndNil( FImgParms );
  inherited;
end;

{ TI2XImgParm }

constructor TI2XImgParm.Create;
begin
end;

destructor TI2XImgParm.Destroy;
begin
  inherited;
end;

{ TI2XImgParmText }

constructor TI2XImgParmText.Create;
begin
  self.FDataType := 'string';
end;

destructor TI2XImgParmText.Destroy;
begin

  inherited;
end;

{ TI2XImgParmSlider }

constructor TI2XImgParmSlider.Create;
begin
  self.FMin := 0;
  self.FMax := 255;
  self.FDefault := 120;
end;

destructor TI2XImgParmSlider.Destroy;
begin

  inherited;
end;

{ TI2XImgParmCombo }

procedure TI2XImgParmCombo.AddOption(const optionToAdd: string);
begin
  FOptions.Add( optionToAdd, optionToAdd );
end;

constructor TI2XImgParmCombo.Create;
begin
  self.FOptions := THashStringTable.Create;
end;

destructor TI2XImgParmCombo.Destroy;
begin
  FreeAndNil( self.FOptions );
  inherited;
end;

{ TDLLOCREngine }

constructor TDLLOCREngine.Create(DLLFileName: string);
begin
  inherited Create( DLLFileName );

end;

constructor TDLLOCREngine.Create;
begin
  inherited;

end;

destructor TDLLOCREngine.Destroy;
begin

  inherited;
end;

function TDLLOCREngine.OCRImage(const ImageMMID, OCRDataMMID: TMemoryMapID; invertColors : boolean ): boolean;
var
  FImageMMIDBuffer : array[0..MAX_BUFFER] of Char;
  FOCRDataMMIDBuffer : array[0..MAX_BUFFER] of Char;
  FOptionsStr : array[0..MAX_BUFFER] of Char;
  sOptionStr : string;
  returnValue : integer;
begin
  if (( self.isDLLLoaded() ) and ( self.isValidProc('OCRImage'))) then begin
    sOptionStr := '';
    FillChar( FImageMMIDBuffer, SizeOf(FImageMMIDBuffer),#0);
    FillChar( FOCRDataMMIDBuffer, SizeOf(FOCRDataMMIDBuffer),#0);

    StrLCopy( FImageMMIDBuffer, PCHar( ImageMMID ), MAX_BUFFER );
    StrLCopy( FOCRDataMMIDBuffer, PCHar( OCRDataMMID ), MAX_BUFFER );
    if ( invertColors ) then begin
      sOptionStr := 'invert';
      FillChar( FOptionsStr, SizeOf( FOptionsStr ), #0 );
      StrLCopy( FOptionsStr, PCHar( sOptionStr ), MAX_BUFFER );
      returnValue := Self.FSetOptionsDLL( FOptionsStr );
    end;

    try
      returnValue := Self.FOCRImageDLL( FImageMMIDBuffer, FOCRDataMMIDBuffer );
    finally
      if ( FAutoCleanUp ) then Self.CleanUp();
    end;
    Result := (Self.LastErrorCode = 0);
  end;
end;

procedure TDLLOCREngine.setDLLName(DLLFileName: string);
Begin
  inherited setDLLName( DLLFileName );
  if FDLLHandle <> 0 then begin
    @FOCRImageDLL  := getProcAddress ( FDLLHandle, 'OCRImage' );
    @FSetOptionsDLL  := getProcAddress ( FDLLHandle, 'SetOptions' );
    if addr( FOCRImageDLL ) = nil then
      raise Exception.Create('External Function "OCRImage" is nil!  Does this function exist?');
    if addr( FSetOptionsDLL ) = nil then
      raise Exception.Create('External Function "SetOptions" is nil!  Does this function exist?');
  end;
End;

End.
