unit uI2XPluginManager;

interface
uses
  SysUtils,
  UIOptionsBase,
  OmniXML,
  uI2XConstants,
  uHashTable,
  uHashTableXML,
  uI2XOCR,
  uI2XDLL,
  uImage2XML,
  Classes
  ;

type
  TOnPluginDebugEvent = procedure( Sender : TObject; StatusMessage : string ) of object;
  TI2XImageDLLs = class(THashTable)
  end;
  TI2XOCREngineDLLs = class(THashTable)
  end;
  TI2XPluginManager = class(TObject)
    private
      FSearchPath : string;
      _ImageDLLs : TI2XImageDLLs;
      _OCREngineDLLs : TI2XOCREngineDLLs;
      FOnDebugEvent : TOnPluginDebugEvent;
      function GetImageDLLCount: integer;
      function GetImageDLL(idx: TIndex): TDLLImageProc;
      function GetOCREngineCount: integer;
      function GetOCREngineDLL(idx: TIndex): TDLLOCREngine;
      procedure PutImageDLL(idx: TIndex; const Value: TDLLImageProc);
      procedure PutOCREngineDLL(idx: TIndex; const Value: TDLLOCREngine);
      function ImageDLLExistCheck(dllName: string; ht: THashTable): TDLLImageProc;
      function OCREngineDLLExistCheck(dllName: string; ht: THashTable): TDLLOCREngine;
      procedure Debug( const msg : string );
    function getSearchPath: string;
    procedure setSearchPath(const Value: string);
    public
      //property SearchPath : string read FSearchPath Write FSearchPath;
      property SearchPath : string read getSearchPath Write setSearchPath;
      Function ImageDLLByShortName( ShortName : string ): TDLLImageProc;
      Function OCRDLLByName( PluginName : string ): TDLLOCREngine;
      Function SearchAndLoadPlugins(): integer; overload;
      Function SearchAndLoadPlugins( const NewSearchPath : string ): integer; overload;
      Function SearchAndLoadImagePlugins(): integer;
      Function SearchAndLoadOCRPlugins(): integer;
      property OCREngineDLLCount : integer read GetOCREngineCount;
      property ImageDLLCount : integer read GetImageDLLCount;
      property OCREngineDLL[ idx : TIndex ] : TDLLOCREngine read GetOCREngineDLL Write PutOCREngineDLL;
      property ImageDLL[ idx : TIndex ] : TDLLImageProc read GetImageDLL Write PutImageDLL;
      function ImageDLLExists( key : string  ) : boolean;
      function OCREngineDLLExists( key : string ) : boolean ;
      property ImageDLLs : TI2XImageDLLs read _ImageDLLs Write _ImageDLLs;
      property OCREngineDLLs : TI2XOCREngineDLLs read _OCREngineDLLs Write _OCREngineDLLs;
      property OnDebug : TOnPluginDebugEvent read FOnDebugEvent Write FOnDebugEvent;

      constructor Create(); overload;
      constructor Create( InitialSearchPath : string ); overload;
      destructor Destroy; override;
  end;

var
  PluginManager : TI2XPluginManager;

implementation
{ TI2XPluginManager }

constructor TI2XPluginManager.Create;
begin
  _ImageDLLs := TI2XImageDLLs.Create;
  _OCREngineDLLs := TI2XOCREngineDLLs.Create;
  self.FSearchPath := AppPath;
end;

constructor TI2XPluginManager.Create(InitialSearchPath: string);
begin
  Create();
  Self.FSearchPath := InitialSearchPath;
end;

procedure TI2XPluginManager.Debug(const msg: string);
begin
  if ( Assigned( self.FOnDebugEvent) ) then
    FOnDebugEvent( self, msg );
end;

destructor TI2XPluginManager.Destroy;
begin
  FreeAndNil( _ImageDLLs );
  FreeAndNil( _OCREngineDLLs );
end;

function TI2XPluginManager.GetImageDLL(idx: TIndex): TDLLImageProc;
begin
  Result := TDLLImageProc( self._ImageDLLs.Items[ idx ] );
end;

function TI2XPluginManager.GetImageDLLCount: integer;
begin
  Result := self._ImageDLLs.Count;
end;

function TI2XPluginManager.GetOCREngineCount: integer;
begin
  Result := self._OCREngineDLLs.Count;
end;

function TI2XPluginManager.GetOCREngineDLL(idx: TIndex): TDLLOCREngine;
var
  i : Cardinal;
  o : TDLLOCREngine;
begin
  Result := TDLLOCREngine( self._OCREngineDLLs.Items[ idx ] );
  if (( Result = nil ) and ( idx.DataType = idtString ))  then begin
    for i := 0 to self.OCREngineDLLCount - 1 do begin
      o := self.OCREngineDLL[i];
      if (
          ( LowerCase( o.Name ) = LowerCase( idx.StringValue ) ) or
          ( LowerCase( o.DLLName ) = LowerCase( idx.StringValue ) ) or
          ( LowerCase( o.ShortName ) = LowerCase( idx.StringValue) )
         ) then begin
        Result := self.OCREngineDLL[i];
        break;
      end;
    end;
  end;
end;

function TI2XPluginManager.getSearchPath: string;
begin
  result := self.FSearchPath;
end;

function TI2XPluginManager.ImageDLLByShortName(
  ShortName: string): TDLLImageProc;
var
  i : Cardinal;
  o : TDLLImageProc;
  arrKeyList : TKeyList;
  ht : THashTable;
Begin
  Result := nil;
  ht := THashTable( _ImageDLLs );
  if ( ht.count > 0 ) then begin
    arrKeyList := SortKeyList(ht.Keys);
    for i := 0 to Length(arrKeyList) - 1 do begin
      o := TDLLImageProc( ht.get( arrKeyList[i] ));
      if ( o.ShortName = ShortName ) then
        Result := o;
    end;
  end;
End;

function TI2XPluginManager.OCRDLLByName(PluginName: string): TDLLOCREngine;
var
  i : Cardinal;
  o : TDLLOCREngine;
  arrKeyList : TKeyList;
  ht : THashTable;
Begin
  Result := nil;
  ht := THashTable( self._OCREngineDLLs );
  if ( ht.count > 0 ) then begin
    arrKeyList := SortKeyList(ht.Keys);
    for i := 0 to Length(arrKeyList) - 1 do begin
      o := TDLLOCREngine( ht.get( arrKeyList[i] ));
      //if ( o.Name = PluginName ) then
      if (( o.Name = PluginName ) or
          ( o.ShortName = PluginName )) then
        Result := o;
    end;
  end;
End;

procedure TI2XPluginManager.PutImageDLL(idx: TIndex;
  const Value: TDLLImageProc);
begin
  self._ImageDLLs.Items[ idx ] := TObject( Value );
end;

procedure TI2XPluginManager.PutOCREngineDLL(idx: TIndex;
  const Value: TDLLOCREngine);
begin
  self._OCREngineDLLs.Items[ idx ] := TObject( Value );
end;

Function TI2XPluginManager.OCREngineDLLExistCheck( dllName : string; ht : THashTable ): TDLLOCREngine;
Begin
  if not ( ht.ContainsKey( dllName ) ) then begin
    ht.Add( dllName, TDLLOCREngine.Create( dllName ) );
  end;
  Result := TDLLOCREngine( ht.Items[ dllName ] );
End;

function TI2XPluginManager.OCREngineDLLExists(key: string): boolean;
begin
  Result := Self._OCREngineDLLs.ContainsKey( key );
end;

Function TI2XPluginManager.ImageDLLExistCheck( dllName : string; ht : THashTable ): TDLLImageProc;
Begin
  if not ( ht.ContainsKey( dllName ) ) then begin
    ht.Add( dllName, TDLLImageProc.Create( dllName ) );
  end;
  Result := TDLLImageProc( ht.Items[ dllName ] );
End;

function TI2XPluginManager.ImageDLLExists(key: string): boolean;
begin
  Result := Self._ImageDLLs.ContainsKey( key );
end;

function TI2XPluginManager.SearchAndLoadImagePlugins(): integer;
var
  DirList, ImgDLLFileList, FileList : TStringList;
  iDirList, iFileList : integer;
Begin
  try
    Debug( 'Calling TI2XPluginManager.SearchAndLoadImagePlugins()' );
    Debug( '  Searching "' + SearchPath + '"' );
    if ( not DirectoryExists( FSearchPath ) ) then
      raise Exception.Create( 'Cannot Search for Image Plugins because directory "' + FSearchPath + '" does not exist.' );

    ImgDLLFileList := TStringList.Create;

    DirList := _FileDir.GetSubDirList( SearchPath );
    DirList.Add( SearchPath ); //you should add the search path because files may exist in this 'root' of the search path
    for iDirList := 0 to DirList.Count - 1 do begin
      try
        FileList := _FileDir.GetFileList( DirList[ iDirList ] + '\i2ximg*.dll' );
        Debug( '  Searching "' + DirList[ iDirList ] + '\i2ximg*.dll"... File Count=' + IntToStr( FileList.Count ) );
        if ( FileList.Count > 0 ) then
          ImgDLLFileList.AddStrings( FileList );
      finally
        FileList.Free;
      end;
    end;

    for iFileList := 0 to ImgDLLFileList.Count - 1 do begin
      ImageDLLExistCheck( ImgDLLFileList[ iFileList ], self._ImageDLLs );
    end;

    Result := ImgDLLFileList.Count;
    Debug( 'Completed: TI2XPluginManager.SearchAndLoadImagePlugins() returning ' + IntToStr(Result) );
  finally
    FreeAndNil( ImgDLLFileList );
    FreeAndNil( DirList );
  end;
End;

function TI2XPluginManager.SearchAndLoadOCRPlugins(): integer;
var
  DirList, OCRDLLFileList, FileList : TStringList;
  iDirList, iFileList : integer;
Begin
  try
    Debug( 'Calling TI2XPluginManager.SearchAndLoadOCRPlugins()' );
    Debug( '  Searching "' + SearchPath + '"' );
    if ( not DirectoryExists( FSearchPath ) ) then
      raise Exception.Create( 'Cannot Search for Image Plugins because directory "' + FSearchPath + '" does not exist.' );

    OCRDLLFileList := TStringList.Create;

    DirList := _FileDir.GetSubDirList( SearchPath );
    DirList.Add( SearchPath ); //you should add the search path because files may exist in this 'root' of the search path
    for iDirList := 0 to DirList.Count - 1 do begin
      try
        FileList := _FileDir.GetFileList( DirList[ iDirList ] + '\i2xocr*.dll' );
        Debug( '  Searching "' + DirList[ iDirList ] + '\i2xocr*.dll"... File Count=' + IntToStr( FileList.Count ) );
        if ( FileList.Count > 0 )  then
          OCRDLLFileList.AddStrings( FileList );
      finally
        FileList.Free;
      end;
    end;

      for iFileList := 0 to OCRDLLFileList.Count - 1 do begin
        OCREngineDLLExistCheck( OCRDLLFileList[ iFileList ], self._OCREngineDLLs );
      end;

    Result := OCRDLLFileList.Count;
    Debug( 'Completed: TI2XPluginManager.SearchAndLoadOCRPlugins() returning ' + IntToStr(Result) );
  finally
    FreeAndNil( OCRDLLFileList );
    FreeAndNil( DirList );
  end;
End;

function TI2XPluginManager.SearchAndLoadPlugins(
  const NewSearchPath: string): integer;
begin
  self.FSearchPath := NewSearchPath;
  Result := self.SearchAndLoadPlugins();
end;

procedure TI2XPluginManager.setSearchPath(const Value: string);
begin
  self.FSearchPath := Value;
end;

function TI2XPluginManager.SearchAndLoadPlugins(): integer;
Begin
  Result := 0;
  Result := Result + SearchAndLoadImagePlugins();
  Result := Result + SearchAndLoadOCRPlugins();
End;
initialization
  PluginManager := TI2XPluginManager.Create;

finalization
  FreeAndNil( PluginManager );
end.
