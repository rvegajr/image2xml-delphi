unit uI2XOptions;

interface
uses
  SysUtils,
  Classes,
  UIOptionsBase,
  OmniXML,
  uI2XConstants,
  uHashTable,
  uHashTableXML,
  JclStrings,
  uFileDir,
  uI2XOCR,
  uStrUtil,
  uI2XPluginManager,
  uI2XDLL
;

const
  UNIT_NAME = 'uI2XOptions';

type
  TI2XOptions = class(TOptionsController)
    private
      FDLLOCRTestsComplete : TDLLOCRTests;
      FDLLOCRTestsSelect : TDLLOCRTests;
      FTWAINDevices : TStringList;
      FFileName : TFileName;
      oFileDir : CFileDir;
      FVarCache : THashStringTableXML;
      FEnableGridSnap : boolean;
      FGridSize : Integer;
      FJobRootPath : string;
      function getEnableGridSnap: boolean;
      function getGridSize: Integer;
      procedure setEnableGridSnap(const Value: boolean);
      procedure setGridSize(const Value: Integer);
      function getDLLOCRTestsComplete: TDLLOCRTests;
      procedure setDLLOCRTestsComplete(const Value: TDLLOCRTests);
      function getDLLOCRTestsSelect: TDLLOCRTests;
      procedure setDLLOCRTestsSelect(const Value: TDLLOCRTests);
      function MergeCacheTests(const CacheEntryName : string;
        var DLLOCRTests: TDLLOCRTests): boolean;
      function getTWAINDevices: TStringList;
      procedure setTWAINDevices(const Value: TStringList);
      function getDefaultTWAINDevice: string;
      procedure setDefaultTWAINDevice(const Value: string);
      function getJobRootPath: string;
      procedure setJobRootPath(const Value: string);
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
      property EnableGridSnap : boolean read getEnableGridSnap write setEnableGridSnap;
      property GridSize : Integer read getGridSize write setGridSize;
      property JobRootPath : string read getJobRootPath write setJobRootPath;
      property DLLOCRTestsComplete : TDLLOCRTests read getDLLOCRTestsComplete write setDLLOCRTestsComplete;
      property DLLOCRTestsSelect : TDLLOCRTests read getDLLOCRTestsSelect write setDLLOCRTestsSelect;
      property TWAINDevices : TStringList read getTWAINDevices write setTWAINDevices;
      property TWAINDeviceDefault : string read getDefaultTWAINDevice write setDefaultTWAINDevice;
      function SetupDefaultTests( PluginManager : TI2XPluginManager ) : boolean;
      function AsXML() : string; override;
      procedure ApplyChanges(); override;
      constructor Create(); override;
      destructor Destroy(); override;
  end;

var
  Options : TI2XOptions;

implementation

{ TI2XOptions }

procedure TI2XOptions.ApplyChanges;
begin
  self.setVarCache('dlltests_complete', FDLLOCRTestsComplete.AsXML() );
  self.setVarCache('dlltests_select', FDLLOCRTestsSelect.AsXML() );
  self.FVarCache.SaveToFile( self.FFileName );
end;

function TI2XOptions.AsXML: string;
var
  sarr : TStringBuilder;
  iEntryIdx, iTestIdx : integer;
  entry : TDLLOCRTestEntry;
  test : TDLLOCRTest;
begin
  try
    sarr := TStringBuilder.Create;
    sarr.Append('<Options>');

    sarr.Append('<Application eventkey="Application" />');
    sarr.Append('<Plugins>');
      sarr.Append('<Image eventkey="PluginsImg"/>');
      sarr.Append('<OCR eventkey="PluginsOCR">');
        sarr.Append('<Complete>');
        for iEntryIdx := 0 to self.FDLLOCRTestsComplete.Count - 1 do begin
          entry := FDLLOCRTestsComplete.Items[iEntryIdx];
          sarr.Append('<' + entry.Name + ' type="checkbox" default="' );
          sarr.Append( uStrUtil.BoolToStr( entry.Enabled, 'true', 'false' ) );
          sarr.Append( '" ptr="' );
          sarr.Append( inttostr(integer(pointer(entry))) );
          sarr.Append( '">' );
          for iTestIdx := 0 to entry.Count - 1 do begin
            test := entry.Items[iTestIdx];
            sarr.Append('<' );
            sarr.Append( test.Name );
            sarr.Append( ' type="checkbox" desc="' );
            sarr.Append( test.Description );
            sarr.Append( '" default="' );
            sarr.Append( uStrUtil.BoolToStr( test.Enabled, 'true', 'false' ) );
            sarr.Append( '" ptr="' );
            sarr.Append( inttostr(integer(pointer(test))) );
            sarr.Append( '" />' );
          end;
          sarr.Append( '</' + entry.Name + '>' );
        end;
        sarr.Append('</Complete>');
        sarr.Append('<Partial>');
        for iEntryIdx := 0 to self.FDLLOCRTestsSelect.Count - 1 do begin
          entry := FDLLOCRTestsSelect.Items[iEntryIdx];
          sarr.Append('<' + entry.Name + ' type="checkbox" default="' );
          sarr.Append( uStrUtil.BoolToStr( entry.Enabled, 'true', 'false' ) );
          sarr.Append( '" ptr="' );
          sarr.Append( inttostr(integer(pointer(entry))) );
          sarr.Append( '">' );
          for iTestIdx := 0 to entry.Count - 1 do begin
            test := entry.Items[iTestIdx];
            sarr.Append('<' );
            sarr.Append( test.Name );
            sarr.Append( ' type="checkbox" desc="' );
            sarr.Append( test.Description );
            sarr.Append( '" default="' );
            sarr.Append( uStrUtil.BoolToStr( test.Enabled, 'true', 'false' ) );
            sarr.Append( '" ptr="' );
            sarr.Append( inttostr(integer(pointer(test))) );
            sarr.Append( '" />' );
          end;
          sarr.Append( '</' + entry.Name + '>' );
        end;
        sarr.Append('</Partial>');
      sarr.Append('</OCR>');
    sarr.Append('</Plugins>');
    sarr.Append('<TWAIN eventkey="Twain" />');
    sarr.Append('</Options>');

    Result := sarr.ToString();
  finally
    FreeAndNil( sarr );
  end;
end;

constructor TI2XOptions.Create;
begin
  oFileDir := CFileDir.Create;
  FDLLOCRTestsComplete := TDLLOCRTests.Create( 'dlltests_complete' );
  FDLLOCRTestsSelect := TDLLOCRTests.Create( 'dlltests_select' );
  self.FFileName := oFileDir.GetUserDir + I2XFULL_DIR_QUAL + 'appcache.i2xcfg';
  if ( not DirectoryExists( ExtractFilePath( self.FFileName ) ) ) then
    ForceDirectories( ExtractFilePath( self.FFileName ) );
  self.FVarCache := THashStringTableXML.Create;
  self.FVarCache.RootNode := 'I2XAppCache';
  if ( FileExists( FFileName ) ) then begin
    FVarCache.LoadFromFile( FFileName );
  end;
  FTWAINDevices := TStringList.Create;
end;

destructor TI2XOptions.Destroy;
begin
  FreeAndNil( FVarCache );
  FreeAndNil( oFileDir );
  FreeAndNil( self.FDLLOCRTestsComplete );
  FreeAndNil( self.FDLLOCRTestsSelect );
  FreeAndNil( FTWAINDevices );
  inherited;
end;

function TI2XOptions.getDefaultTWAINDevice: string;
begin
  Result := self.getVarCache( 'TWAINDefault' );
end;

function TI2XOptions.getDLLOCRTestsComplete: TDLLOCRTests;
begin
  Result := self.FDLLOCRTestsComplete;
end;

function TI2XOptions.getDLLOCRTestsSelect: TDLLOCRTests;
begin
  Result := self.FDLLOCRTestsSelect;
end;

function TI2XOptions.getEnableGridSnap: boolean;
begin
  //Result := self.getVarCache( 'EnableGridSnap', true );
  Result := false;
end;

function TI2XOptions.getGridSize: Integer;
begin
  Result := self.getVarCache( 'GridSize', 5 );
end;

function TI2XOptions.getJobRootPath: string;
begin
  Result := self.getVarCache( 'JobRootPath' );
  if ( Length( Result ) = 0 ) then
    Result := oFileDir.GetMyDocDir + I2XFULL_DIR_QUAL;
end;

function TI2XOptions.getTWAINDevices: TStringList;
begin
  Result := self.FTWAINDevices;
end;

function TI2XOptions.getVarCache(const varName: string;
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

function TI2XOptions.getVarCache(const varName: string;
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

function TI2XOptions.getVarCache(const varName: string;
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

function TI2XOptions.getVarCache(const varName: string): string;
begin
  if (FVarCache.ContainsKey( varName )) then
    Result := FVarCache.Items[ varName ]
  else
    Result := '';
end;

procedure TI2XOptions.setDefaultTWAINDevice(const Value: string);
begin
  self.setVarCache( 'TWAINDefault', Value );
end;

procedure TI2XOptions.setDLLOCRTestsComplete(const Value: TDLLOCRTests);
begin
  self.FDLLOCRTestsComplete.CopyFrom( Value );
end;

procedure TI2XOptions.setDLLOCRTestsSelect(const Value: TDLLOCRTests);
begin
  self.FDLLOCRTestsSelect.CopyFrom( Value );
end;

procedure TI2XOptions.setEnableGridSnap(const Value: boolean);
begin
  self.setVarCache( 'EnableGridSnap', Value );
end;

procedure TI2XOptions.setGridSize(const Value: Integer);
begin
  self.setVarCache( 'GridSize', Value );
end;

procedure TI2XOptions.setJobRootPath(const Value: string);
begin
  self.setVarCache( 'JobRootPath', Value );
end;

procedure TI2XOptions.setTWAINDevices(const Value: TStringList);
begin
  FTWAINDevices.Clear;
  FTWAINDevices.AddStrings( Value );
end;

function TI2XOptions.MergeCacheTests( const CacheEntryName : string;
  var DLLOCRTests : TDLLOCRTests ) : boolean;
var
  DLLOCRTestsTemp : TDLLOCRTests;
  entry, newEntry : TDLLOCRTestEntry;
  iTestEntryIdx : integer;
  s : string;
Begin
  try
    DLLOCRTestsTemp := TDLLOCRTests.Create();
    s := self.getVarCache( CacheEntryName );
    DLLOCRTestsTemp.LoadFromXML( self.getVarCache( CacheEntryName ) );
    for iTestEntryIdx := 0 to DLLOCRTestsTemp.Count - 1 do begin
      entry := DLLOCRTestsTemp.Items[ iTestEntryIdx ];
      if ( DLLOCRTests.Exists( entry.Name ) ) then begin
        DLLOCRTests.Items[ entry.Name ].CopyFrom( entry );
      end else begin
        newEntry := TDLLOCRTestEntry.Create();
        newEntry.CopyFrom( entry );
        DLLOCRTests.Add( newEntry );
      end;
    end;
  finally
    FreeAndNil( DLLOCRTestsTemp );
  end;
End;

function TI2XOptions.SetupDefaultTests(
  PluginManager: TI2XPluginManager): boolean;
var
  iOCRDLLIdx : integer;
  ocre : TDLLOCREngine;
  s : string;
begin
  Result := false;
  try
    for iOCRDLLIdx := 0 to PluginManager.OCREngineDLLCount - 1 do begin
      ocre := PluginManager.OCREngineDLL[ iOCRDLLIdx ];
      if ( not self.FDLLOCRTestsComplete.Exists( ocre.ShortName )) then
        self.FDLLOCRTestsComplete.Add( ocre.ShortName, true, true, true, true );
      if ( not self.FDLLOCRTestsSelect.Exists( ocre.ShortName )) then
        self.FDLLOCRTestsSelect.Add( ocre.ShortName, false, true, false, true );
    end;
    //s := FDLLOCRTestsComplete.AsXML;
    //s := FDLLOCRTestsSelect.AsXML;
    MergeCacheTests( FDLLOCRTestsComplete.Name, FDLLOCRTestsComplete ); //'dlltests_complete'
    MergeCacheTests( FDLLOCRTestsSelect.Name, FDLLOCRTestsSelect ); //'dlltests_select'
    Result := true;
  finally

  end;
end;

procedure TI2XOptions.setVarCache( const varName : string; const varValue: integer);
begin
  self.setVarCache( varName, IntToStr( varValue ) );
end;

procedure TI2XOptions.setVarCache( const varName : string; const varValue: boolean);
begin
  self.setVarCache( varName, BoolToStr( varValue ) );
end;

procedure TI2XOptions.setVarCache( const varName : string; const varValue: extended);
begin
  self.setVarCache( varName, FloatToStr( varValue ) );
end;

procedure TI2XOptions.setVarCache( const varName : string; const varValue: string);
begin
  if ( FVarCache.ContainsKey( varName ) ) then begin
    if ( FVarCache.Items[ varName ] <> varValue ) then begin
      self.OnChangeHandler();
      FVarCache.Items[ varName ] := varValue;
    end;
  end else begin
    FVarCache.Add( varName, varValue );
    self.OnChangeHandler();
  end;
end;

initialization
  Options := TI2XOptions.Create;

finalization
  FreeAndNil( Options );

END.
