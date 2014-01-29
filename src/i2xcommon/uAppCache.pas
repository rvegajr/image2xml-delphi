unit uAppCache;

interface
uses
  SysUtils,
  Classes,
  OmniXML,
  uI2XConstants,
  uHashTable,
  uHashTableXML,
  JclStrings,
  uFileDir,
  uStrUtil
;

const
  UNIT_NAME = 'uAppCache';

type
  TApplicationCacheBase = class(TObject)
    private
    protected
      FOnChange : TNotifyEvent;
      FIsDirty : boolean;
      FSaveAfterEachUpdate : boolean;
      procedure OnChangeHandler();
    public
      property OnChange : TNotifyEvent read FOnChange write FOnChange;
      property AutoSave : boolean read FSaveAfterEachUpdate write FSaveAfterEachUpdate;
      property IsDirty : boolean read FIsDirty write FIsDirty;
      function AsXML() : string; virtual; abstract;
      procedure ApplyChanges(); virtual; abstract;
      constructor Create(); virtual;
      destructor Destroy(); override;
  end;
  TApplicationCache = class(TApplicationCacheBase)
    private
      FFileName : TFileName;
      FRootName : string;
      oFileDir : CFileDir;
      FVarCache : THashStringTableXML;
      function getMinMessageDisplayed: boolean;
      procedure setMinMessageDisplayed(const Value: boolean);
      function getAutoScan: boolean;
      function getOutputFolder: string;
      function getSubPath: string;
      procedure setAutoScan(const Value: boolean);
      procedure setOutputFolder(const Value: string);
      procedure setSubPath(const Value: string);
      function getLastFolderSearched: string;
      procedure setLastFolderSearched(const Value: string);
      function getLastUserFolderSearched: string;
      procedure setLastUserFolderSearched(const Value: string);
    published
      property MinMessageDisplayed : boolean read getMinMessageDisplayed write setMinMessageDisplayed;
      property OutputFolder : string read getOutputFolder write setOutputFolder;
      property SubPath : string read getSubPath write setSubPath;
      property AutoScan : boolean read getAutoScan write setAutoScan;
      property LastFolderSearched : string read getLastFolderSearched write setLastFolderSearched;
      property LastUserFolderSearched : string read getLastUserFolderSearched write setLastUserFolderSearched;
      function getVar( const varName : string ) : string;
      procedure setVar( const varName : string; const varValue : string );

    protected
      function getVarCache( const varName : string ) : string; overload;
      function getVarCache( const varName : string; const DefaultValue : integer ) : integer; overload;
      function getVarCache( const varName : string; const DefaultValue : boolean ) : boolean; overload;
      function getVarCache( const varName : string; const DefaultValue : extended ) : extended; overload;
      procedure setVarCache( const varName : string; const varValue : string ); overload;
      procedure setVarCache( const varName : string; const varValue : integer ); overload;
      procedure setVarCache( const varName : string; const varValue : boolean ); overload;
      procedure setVarCache( const varName : string; const varValue : extended ); overload;

    public
      property FileName : TFileName read FFileName write FFileName;
      property RootName : string read FRootName write FRootName;
      function AsXML() : string; override;
      procedure ApplyChanges(); override;
      constructor Create(); override;
      destructor Destroy(); override;
  end;

var
  AppCache : TApplicationCache;

implementation

{ TApplicationCacheBase }

constructor TApplicationCacheBase.Create;
begin
  FIsDirty := false;
end;

destructor TApplicationCacheBase.Destroy;
begin

end;

procedure TApplicationCacheBase.OnChangeHandler;
begin
  self.FIsDirty := true;
  if ( Assigned( self.FOnChange ))  then
    self.FOnChange( self );
  if ( FSaveAfterEachUpdate ) then
    self.ApplyChanges();
end;

{ TApplicationCache }

procedure TApplicationCache.ApplyChanges;
begin
  self.FVarCache.SaveToFile( self.FFileName );
end;

function TApplicationCache.AsXML: string;
var
  sarr : TStringBuilder;
begin
  try
    Result := FVarCache.AsXML();
  finally
  end;
end;

constructor TApplicationCache.Create;
begin
  oFileDir := CFileDir.Create;
  FRootName := 'AppCache';
  self.FFileName := oFileDir.GetUserDir + I2XFULL_DIR_QUAL + 'appcache.dat';
  if ( not DirectoryExists( ExtractFilePath( self.FFileName ) ) ) then
    ForceDirectories( ExtractFilePath( self.FFileName ) );
  self.FVarCache := THashStringTableXML.Create;
  self.FVarCache.RootNode := FRootName;
  if ( FileExists( FFileName ) ) then begin
    FVarCache.LoadFromFile( FFileName );
  end;
end;

destructor TApplicationCache.Destroy;
begin
  FreeAndNil( FVarCache );
  FreeAndNil( oFileDir );
  inherited;
end;

function TApplicationCache.getVarCache(const varName: string;
  const DefaultValue: integer): integer;
var
  sRes : string;
begin
  sRes := getVarCache( varName );
  try
    Result := StrToInt( sRes )
  except
    Result := DefaultValue;
  end;
end;

function TApplicationCache.getVarCache(const varName: string;
  const DefaultValue: boolean): boolean;
var
  sRes : string;
begin
  sRes := getVarCache( varName );
  try
    Result := StrToBool( sRes )
  except
    Result := DefaultValue;
  end;
end;

function TApplicationCache.getAutoScan: boolean;
begin
  Result := self.getVarCache( 'auto_scan', false );
end;

function TApplicationCache.getLastFolderSearched: string;
begin
  Result := self.getVarCache( 'last_folder_searched' );
end;

function TApplicationCache.getLastUserFolderSearched: string;
begin
  Result := self.getVarCache( 'last_user_folder_searched' );
  if ( Length( Result ) = 0 ) then begin
    Result := oFileDir.GetUserDir + I2XFULL_DIR_QUAL;
    self.setLastFolderSearched( Result );
  end;
end;

function TApplicationCache.getMinMessageDisplayed: boolean;
begin
  Result := self.getVarCache( 'min_msg_displayed', false );
end;

function TApplicationCache.getOutputFolder: string;
begin
  Result := self.getVarCache( 'output_folder' );
end;

function TApplicationCache.getSubPath: string;
begin
  Result := self.getVarCache( 'sub_path' );
end;

function TApplicationCache.getVar(const varName: string): string;
begin
  Result := self.getVarCache( varName );
end;

function TApplicationCache.getVarCache(const varName: string;
  const DefaultValue: extended): extended;
var
  sRes : string;
begin
  sRes := getVarCache( varName );
  try
    Result := StrToFloat( sRes )
  except
    Result := DefaultValue;
  end;
end;

function TApplicationCache.getVarCache(const varName: string): string;
begin
  if (FVarCache.ContainsKey( varName )) then
    Result := FVarCache.Items[ varName ]
  else
    Result := '';
end;

procedure TApplicationCache.setVarCache( const varName : string; const varValue: integer);
begin
  self.setVarCache( varName, IntToStr( varValue ) );
end;

procedure TApplicationCache.setVarCache( const varName : string; const varValue: boolean);
begin
  self.setVarCache( varName, BoolToStr( varValue ) );
end;

procedure TApplicationCache.setAutoScan(const Value: boolean);
begin
  self.setVarCache( 'auto_scan', Value );
end;

procedure TApplicationCache.setLastFolderSearched(const Value: string);
begin
  self.setVarCache( 'last_folder_searched', Value );
end;

procedure TApplicationCache.setLastUserFolderSearched(const Value: string);
begin
  self.setVarCache( 'last_user_folder_searched', Value );
end;

procedure TApplicationCache.setMinMessageDisplayed(const Value: boolean);
begin
  self.setVarCache( 'min_msg_displayed', Value );
end;

procedure TApplicationCache.setOutputFolder(const Value: string);
begin
  self.setVarCache( 'output_folder', Value );
end;

procedure TApplicationCache.setSubPath(const Value: string);
begin
  self.setVarCache( 'sub_path', Value );
end;

procedure TApplicationCache.setVar(const varName, varValue: string);
begin
  self.setVarCache( varName, varValue );
end;

procedure TApplicationCache.setVarCache( const varName : string; const varValue: extended);
begin
  self.setVarCache( varName, FloatToStr( varValue ) );
end;

procedure TApplicationCache.setVarCache( const varName : string; const varValue: string);
begin
  if ( FVarCache.ContainsKey( varName ) ) then begin
    if ( FVarCache.Items[ varName ] <> varValue ) then begin
      FVarCache.Items[ varName ] := varValue;
      self.OnChangeHandler();
    end;
  end else begin
    FVarCache.Add( varName, varValue );
    self.OnChangeHandler();
  end;
end;

initialization
  AppCache := TApplicationCache.Create;
finalization
  FreeAndNil( AppCache );

END.
