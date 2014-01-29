unit uI2XPlugin;

interface
uses
  Windows,
  Classes,
  MapStream,
  uStrUtil,
  Graphics,
  SysUtils,
  OmniXML,
  uDWImage,
  uI2XConstants,
  uFileDir,
  uHashTable,
  uI2XMemMap;

const
  UNIT_NAME = 'uI2XPlugin';

type
  TI2XPlugin = class(TObject)
    private
      FDebugLevel : TDebugLevel;
      FDebugPath : string;
      FDebugLog : string;
      FFileDir : CFileDir;
      FGUID : string;
      FPath: string;
      FXMLParmFile : TFileName;
      FXMLParmData : IXMLDocument;
      FLogWritten : boolean;
      FLastErrorCode : integer;
      FLastErrorString : string;
    protected
      FMemoryMapManager : TI2XMemoryMapManager;
      FStrUtil : CStrUtil;
      function ReadParmFile( const FileName : TFileName ) : boolean ; dynamic;
      property DebugLevel : TDebugLevel read FDebugLevel write FDebugLevel;
      property DebugPath : string read FDebugPath write FDebugPath;
      property DebugLog : string read FDebugLog write FDebugLog;
      procedure DebugWrite( const msg : string );
      property XMLParmFile : TFileName read FXMLParmFile write FXMLParmFile;
      procedure ErrorUpdate( const msg : string; const code : integer );
    public
      property GUID : string read FGUID write FGUID;
      property Path : string read FPath write FPath;
      property LastErrorCode : integer read FLastErrorCode write FLastErrorCode;
      property LastErrorString : string read FLastErrorString write FLastErrorString;
      property MemoryMapManager : TI2XMemoryMapManager read FMemoryMapManager write FMemoryMapManager;
      procedure CleanUp;
      constructor Create( const GUID, ParmFileName : string ); dynamic;
      destructor Destroy; override; 
  end;

  PluginException = class(Exception)
    public
      constructor Create( const Msg : string; const ReturnCode : integer; plugin : TI2XPlugin ); overload;
  end;

function ThisDLLPath() : string;

implementation

function ThisDLLPath() : string;
var
  TheFileName : array[0..MAX_PATH] of char;
begin
  FillChar(TheFileName, sizeof(TheFileName), #0);
  GetModuleFileName(hInstance, TheFileName, sizeof(TheFileName));
  Result := String( TheFileName );
end;

{ TI2XPlugin }

procedure TI2XPlugin.CleanUp;
begin
  FMemoryMapManager.Flush;
end;

constructor TI2XPlugin.Create(const GUID, ParmFileName: string);
begin
  FLastErrorCode := ERROR_OK;
  FLastErrorString := '';
  FLogWritten := false;
  FFileDir := CFileDir.Create;
  FDebugLog := 'I2XPluginDebug.log';
  FXMLParmData := CreateXMLDoc;
  FXMLParmFile := ParmFileName;
  self.FGUID := GUID;
  FStrUtil := CStrUtil.Create;
  self.ReadParmFile( FXMLParmFile );
  FMemoryMapManager := TI2XMemoryMapManager.Create();
end;

destructor TI2XPlugin.Destroy;
begin
  FreeAndNil( FStrUtil );
  FreeAndNil( FFileDir );
  FreeAndNil( FMemoryMapManager );
end;

procedure TI2XPlugin.ErrorUpdate(const msg: string; const code: integer);
begin
  self.FLastErrorString := msg;
  self.FLastErrorCode := code;
end;

function TI2XPlugin.ReadParmFile(const FileName: TFileName): boolean;
var
  oNod : IXMLNode;
  sDebugLevel, sDebugPath, sTempDir : string;
  oFileDir : CFileDir;
begin
  try
    if not FileExists( FileName ) then
      raise PluginException.Create('Could not load parm file "' + FileName + '".  Is it properly formed?', ERROR_PARM_FILE_INVALID, self );
    if not FXMLParmData.Load( FileName ) then
      raise PluginException.Create('Could not load parm file "' + FileName + '".  Is it properly formed?', ERROR_PARM_FILE_INVALID, self );
    FXMLParmData.SelectSingleNode( '//debug', oNod );
    if ( oNod <> nil ) then begin
      try
        oFileDir := CFileDir.Create;
        sTempDir := oFileDir.GetSystemTempDir;
        sDebugLevel := oNod.attributes.getNamedItem( 'level' ).text;
        if FStrUtil.IsNumeric( sDebugLevel ) then
          FDebugLevel := TDebugLevel( StrToInt( sDebugLevel ));
        FDebugPath := oNod.attributes.getNamedItem( 'path' ).text;
        FDebugPath := StringReplace(FDebugPath, '%temp%\', sTempDir, [rfReplaceAll, rfIgnoreCase]);
        FDebugPath := StringReplace(FDebugPath, '%temp%', sTempDir, [rfReplaceAll, rfIgnoreCase]);
        ForceDirectories( FDebugPath );
        if ( not DirectoryExists( FDebugPath ) ) then
          FDebugPath := ExtractFilePath( FileName );
      finally
        FreeAndNil( oFileDir );
      end;
    end;
  except
    result := false;
  end;
End;


procedure TI2XPlugin.DebugWrite(const msg: string);
Var
  txtfLog : TextFile;
  fildir : uFileDir.CFileDir;
Begin
  if ( Length(FDebugPath) > 0 ) then begin
    if ( not FLogWritten ) then begin
      FFileDir.WriteString( self.ClassName +
                    ' (I2X Plugin) started on ' + FormatDateTime('c', now) +
                    ' - - - - - - - - - - ' + #13, (FDebugPath + FDebugLog), true );
      FLogWritten := true;
    end;
    FFileDir.WriteString( msg + #13, (FDebugPath + FDebugLog), true );
  end;
End;

{ PluginException }

constructor PluginException.Create(const Msg: string; const ReturnCode: integer; plugin : TI2XPlugin);
begin
  plugin.LastErrorCode := ReturnCode;
  plugin.LastErrorString := Msg;
  self.Create( Msg );
end;

END.
