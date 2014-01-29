unit uI2XMemMap;

interface

uses Windows,
    SysUtils,
    JclStrings,
    Classes,
    uFileDir,
    uStrUtil,
    SyncObjs,
    uHashTable,
    MapStream,
    uI2XConstants;

type
  TI2XMemoryMapManager = class(TObject)
    private
      FList : THashTable;
      FTokenFilePath : string;
      FWriteFileToken : boolean;
      FDebugMessage : string;
      FFileDir : CFileDir;
      FStrUtil : CStrUtil;

      FTokenFileName : string;
      FLogFileName : string;
      function GetItem(Index: TIndex): TMapStream;
      procedure SetItem(Index: TIndex; const Value: TMapStream);
      function GetCount: Cardinal;
      function Add( const MapLabel : string; const ItemToAdd : TMapStream ) : TMapStream;
      function GetTokenFilePath: string;
      procedure SetTokenFilePath(const Value: string);
      function GenTokenFileName( const MapLabel : string ) : string;
      function IncTokenFile( const MapLabel : string ) : bool;
      function DecTokenFile( const MapLabel : string; const ForceDelete : boolean = false ) : bool;
      procedure Debug(const Value: string);
   published
      //Notice that sometimes that map name can be different but point to the same map
      //  This condition is encountered when the same map is called in different threads
      //function Add( const ItemToAdd : TMapStream ) : TMapStream; overload;
      //procedure Remove( MapToRemove : string );
      property Count : Cardinal read GetCount;
      function Exists( const MapStreamName : string ) : boolean;
   public
      procedure Flush();
      procedure Delete( MapToDelete : string ); overload;
      property Items[ Index: TIndex ]: TMapStream read GetItem write SetItem; default;
      property TokenFilePath : string read GetTokenFilePath write SetTokenFilePath;
      property DebugMessageText : string read FDebugMessage write FDebugMessage;
      function Stream( const AName: String; ASize:Cardinal; ATimeOut:Integer ): TMapStream; overload;
      function Stream( const AName: String; ASize:Cardinal ): TMapStream; overload;
      function Stream( const AName: String ): TMapStream; overload;

      function Write( var MapName : string; ObjectToStreamFrom : TObject ) : boolean;
      function Read( const MapName : string; ObjectToStreamTo : TObject; ReleaseAfterRead : boolean = true ) : boolean;

      function OpenMapsAsString() : string ;
      constructor Create();
      destructor Destroy; override;
  end;

var
  I2XMMManager : TI2XMemoryMapManager;

implementation

uses
    Graphics,
    uDWImage,
    uI2XOCR,
    GR32;
{ TI2XMemoryMapManager }

function TI2XMemoryMapManager.Add(const MapLabel : string; const ItemToAdd: TMapStream): TMapStream;
begin
  if ( Length( MapLabel ) = 0 ) then
    raise Exception.Create('You must specify an image source');
  FList.Add( MapLabel, ItemToAdd );
  if ( self.FWriteFileToken ) then
    self.IncTokenFile( MapLabel );
  Result := ItemToAdd;
end;
{*
function TI2XMemoryMapManager.Add(const ItemToAdd: TMapStream): TMapStream;
begin
  if ( Length( ItemToAdd.Name ) = 0 ) then
    raise Exception.Create('You must specify an image source');
  ItemToAdd.Parent := TObject( self );
  FList.Add( ItemToAdd.Name, ItemToAdd );
  Result := ItemToAdd;
end;
*}
constructor TI2XMemoryMapManager.Create;
begin
  FList := THashTable.Create;
  FFileDir := CFileDir.Create;
  FStrUtil := CStrUtil.Create;
  self.TokenFilePath := 'C:\Users\Administrator\AppData\Local\Temp\i2x\';
end;

function TI2XMemoryMapManager.DecTokenFile(const MapLabel: string; const ForceDelete : boolean = false): bool;
var
  sTokenFileName, sTokenFileContents : string;
  nTokenMapCount : integer;
begin
  sTokenFileName := GenTokenFileName( MapLabel );
  if ( FileExists( sTokenFileName ) ) then begin
    sTokenFileContents := FFileDir.TextFileToString( sTokenFileName );
    nTokenMapCount := FStrUtil.ExtractInt( sTokenFileContents );
    Dec( nTokenMapCount );
    if ( ForceDelete ) then
      nTokenMapCount := 0;
    if ( nTokenMapCount = 0 ) then begin
      DeleteFile( sTokenFileName );
      Debug('CLOSE:' + MapLabel + ' ' + self.FDebugMessage );
      FDebugMessage := '';
    end else begin
      FFileDir.WriteString( IntToStr( nTokenMapCount ), sTokenFileName );
      Debug('DEC [' + IntToStr(nTokenMapCount) + ']:' + MapLabel + ' ' + self.FDebugMessage );
      FDebugMessage := '';
    end;
  end else begin
    //FFileDir.WriteString( sTokenFileName, '1' );
  end;
end;

procedure TI2XMemoryMapManager.Delete( MapToDelete : string );
begin
  FList.Delete( MapToDelete );
  if ( self.FWriteFileToken ) then
    self.DecTokenFile( MapToDelete );
end;

destructor TI2XMemoryMapManager.Destroy;
var
  kl : TKeyList;
  i : integer;
begin
  kl := self.FList.Keys;
  for i := 0 to Length( kl ) - 1 do begin
    self.Delete( kl[i] );
  end;
  FList.FreeObjectsOnDestroy := false;
  FreeAndNil( FList );
  FreeAndNil( FFileDir );
  FreeAndNil( FStrUtil );
  inherited;
end;

function TI2XMemoryMapManager.Exists(const MapStreamName: string): boolean;
begin
  Result := FList.ContainsKey( MapStreamName );
end;

procedure TI2XMemoryMapManager.Flush;
var
  kl : TKeyList;
  i : integer;
begin
  kl := self.FList.Keys;
  for i := 0 to Length( kl ) - 1 do begin
    self.Delete( kl[i] );
  end;
end;

function TI2XMemoryMapManager.GenTokenFileName(const MapLabel : string ): string;
begin
  Result := FFileDir.Slash( FTokenFilePath ) + MapLabel + '._map';
end;

function TI2XMemoryMapManager.GetCount: Cardinal;
begin
  Result := FList.Count;
end;

function TI2XMemoryMapManager.GetItem(Index: TIndex): TMapStream;
begin                                   
  if ( Index.DataType = idtString ) then begin
    if ( FList.ContainsKey( Index.StringValue ) ) then
      Result := TMapStream( FList.Items[ Index ] )
    else begin
      Result := TMapStream.Create( Index.StringValue );
      FList.Add( Index.StringValue, Result );
    end;
  end else begin
    Result := TMapStream( FList.Items[ Index ] );
  end;
end;

function TI2XMemoryMapManager.GetTokenFilePath: string;
begin
  Result := self.FTokenFilePath;
end;

function TI2XMemoryMapManager.IncTokenFile(const MapLabel: string): bool;
var
  sTokenFileName, sTokenFileContents : string;
  nTokenMapCount : integer;
begin
  sTokenFileName := GenTokenFileName( MapLabel );
  if ( FileExists( sTokenFileName ) ) then begin
    sTokenFileContents := FFileDir.TextFileToString( sTokenFileName );
    nTokenMapCount := FStrUtil.ExtractInt( sTokenFileContents );
    Inc( nTokenMapCount );
    FFileDir.WriteString( IntToStr( nTokenMapCount ), sTokenFileName );
    Debug('INC [' + IntToStr(nTokenMapCount) + ']:' + MapLabel + ' ' + self.FDebugMessage );
    FDebugMessage := '';
  end else begin
    FFileDir.WriteString( '1', sTokenFileName );
    Debug('OPEN:' + MapLabel + ' ' + self.FDebugMessage );
    FDebugMessage := '';
  end;

end;

function TI2XMemoryMapManager.OpenMapsAsString: string;
var
  sb : TStringBuilder;
  i : integer;
  kl : TKeyList;
Begin
  try
    sb :=  TStringBuilder.Create();
    kl := self.FList.Keys;
    for i := 0 to Length( kl ) - 1 do begin
      sb.Append( kl[i] + #13);
    end;
    Result := sb.ToString();
  finally
    FreeAndNil( sb );
  end;
End;

function TI2XMemoryMapManager.Read(const MapName: string;
  ObjectToStreamTo: TObject; ReleaseAfterRead : boolean ): boolean;
var
  DWImageToStream : TDWImage;
  Bitmap32ToStream : TBitmap32;
  BitmapToStream : TBitmap;
  OCRResultsToStream : TI2XOCRResults;
  FMemoryMap : TMapStream;
  ms : TMemoryStreamPlus;
  FLastStreamSize, FInitialSize : integer;
  bmp : TBitmap;
begin
  try
    Result := false;
      //FMemoryMap := self.Items[ MapName ];
    //try
      ms := TMemoryStreamPlus.Create();
      if ( ObjectToStreamTo.ClassName = 'TDWImage' ) then begin
        FMemoryMap := self.Stream( MapName, IMAGE_MEMMAP_INIT_SIZE );
        DWImageToStream := TDWImage(ObjectToStreamTo);
        DWImageToStream.Bitmap.FreeImage;
        if ( not FMemoryMap.Read( ms ) ) then
          raise Exception.Create('Error while copying BITMAP to Memory Map');
        DWImageToStream.Bitmap.LoadFromStream( ms );
        FLastStreamSize := ms.Size;
        DWImageToStream.BMPHeaderUpdate;
        Result := true;

      end else if ( ObjectToStreamTo.ClassName = 'TBitmap32' ) then begin
        FMemoryMap := self.Stream( MapName, IMAGE_MEMMAP_INIT_SIZE );
        Bitmap32ToStream := TBitmap32( ObjectToStreamTo );
        //if ( not FMemoryMap.Read( ms ) ) then
        //  raise Exception.Create('Error while copying BITMAP to Memory Map');
        try
          bmp := TBitmap.Create;
          if ( not FMemoryMap.Read( ms ) ) then
            raise Exception.Create('Error while copying BITMAP from Memory Map');
          bmp.LoadFromStream( ms );
          Bitmap32ToStream.Assign( bmp );
        finally
          FreeAndNil( bmp );
        end;
        Result := true;

      end else if ( ObjectToStreamTo.ClassName = 'TBitmap' ) then begin
        FMemoryMap := self.Stream( MapName, IMAGE_MEMMAP_INIT_SIZE );
        BitmapToStream := TBitmap( ObjectToStreamTo );
        if ( not FMemoryMap.Read( ms ) ) then
          raise Exception.Create('Error while copying BITMAP from Memory Map');
        BitmapToStream.LoadFromStream( ms );
        Result := true;

      end else if ( ObjectToStreamTo.ClassName = 'TI2XOCRResults' ) then begin
        FMemoryMap := self.Stream( MapName, OCR_MEMMAP_INIT_SIZE );
        OCRResultsToStream := TI2XOCRResults( ObjectToStreamTo );
        if ( not FMemoryMap.Read( ms ) ) then
          raise Exception.Create('Error while copying OCR Results from Memory Map');
        OCRResultsToStream.LoadXML( ms.Read().StringValue );
        Result := true;

      end else
        raise Exception.Create('Memory Map Manager does not know how to read an object of type "' + ObjectToStreamTo.ClassName + '" yet.');
      if ( ReleaseAfterRead ) then
        self.Delete( MapName );
    //except
    //  raise;
    //  on E : Exception do begin
    //    raise;
    //  end;
    //end;
  finally
    if ( ms <> nil ) then FreeAndNil( ms );
  end;
end;

//procedure TI2XMemoryMapManager.Remove( MapToRemove : string  );
//begin
//  FList.Remove( MapToRemove );
//end;

procedure TI2XMemoryMapManager.SetItem(Index: TIndex; const Value: TMapStream);
begin
  FList.Items[ Index ] := TObject( Value );
end;

procedure TI2XMemoryMapManager.Debug(const Value: string);
Begin
  if ( Length(FLogFileName) > 0 ) then begin
    self.FFileDir.WriteString( Value + #13, FLogFileName, true );
  end;
End;

procedure TI2XMemoryMapManager.SetTokenFilePath(const Value: string);
begin
  self.FTokenFilePath := Value;
  self.FWriteFileToken := ( Length( FTokenFilePath ) > 0 );
  if ( FWriteFileToken ) then
    FLogFileName := FFileDir.Slash( FTokenFilePath ) + 'I2XMemoryManager.log'
  else
    FLogFileName := '';
end;

function TI2XMemoryMapManager.Stream( const AName: String;
  ASize: Cardinal; ATimeOut: Integer): TMapStream;
var
  ms : TMapStream;
begin
    if ( FList.ContainsKey( AName ) ) then
      Result := TMapStream( FList.Items[ AName ] )
    else begin
      Result := TMapStream.Create( AName, ASize, ATimeOut );
      FList.Add( AName, Result );
      if ( self.FWriteFileToken ) then
        self.IncTokenFile( AName );
    end;
end;

function TI2XMemoryMapManager.Stream( const AName: String;
  ASize: Cardinal): TMapStream;
var
  ms : TMapStream;
begin
    if ( FList.ContainsKey( AName ) ) then
      Result := TMapStream( FList.Items[ AName ] )
    else begin
      ms := TMapStream.Create( AName, ASize );
      FList.Add( AName, ms );
      if ( self.FWriteFileToken ) then
        self.IncTokenFile( AName );
      Result := ms;
    end;
end;

function TI2XMemoryMapManager.Stream( const AName: String): TMapStream;
var
  ms : TMapStream;
begin
    if ( FList.ContainsKey( AName ) ) then
      Result := TMapStream( FList.Items[ AName ] )
    else begin
      Result := TMapStream.Create( AName );
      if ( self.FWriteFileToken ) then
        self.IncTokenFile( AName );
      FList.Add( AName, Result );
    end;
end;


function TI2XMemoryMapManager.Write(var MapName: string;
  ObjectToStreamFrom: TObject): boolean;
var
  DWImageToStream : TDWImage;
  Bitmap32ToStream : TBitmap32;
  BitmapToStream : TBitmap;
  OCRResultsToStream : TI2XOCRResults;
  FMemoryMap : TMapStream;
  ms : TMemoryStreamPlus;
  FLastStreamSize, FInitialSize : integer;
  bmp : TBitmap;
begin
  result := false;
  try
    if ( ObjectToStreamFrom.ClassName = 'TDWImage' ) then begin
      DWImageToStream := TDWImage(ObjectToStreamFrom);
      if ( Length(MapName) = 0 ) then
        MapName := IMAGE_MEMMAP_HEADER + IntToStr( Integer(Pointer(ObjectToStreamFrom)) );
      FInitialSize := IMAGE_MEMMAP_INIT_SIZE;
      ms := TMemoryStreamPlus.Create();
      DWImageToStream.BITMAP.SaveToStream( ms );

    end else if ( ObjectToStreamFrom.ClassName = 'TBitmap32' ) then begin
      Bitmap32ToStream := TBitmap32( ObjectToStreamFrom );
      if ( Length(MapName) = 0 ) then
        MapName := IMAGE_MEMMAP_HEADER + IntToStr( Integer(Pointer(ObjectToStreamFrom)) );
      FInitialSize := IMAGE_MEMMAP_INIT_SIZE;
      ms := TMemoryStreamPlus.Create();
      try
        bmp := TBitmap.Create;
        bmp.Assign( Bitmap32ToStream );
        bmp.SaveToStream( ms );
      finally
        FreeAndNil( bmp );
      end;

    end else if ( ObjectToStreamFrom.ClassName = 'TBitmap' ) then begin
      BitmapToStream := TBitmap( ObjectToStreamFrom );
      if ( Length(MapName) = 0 ) then
        MapName := IMAGE_MEMMAP_HEADER + IntToStr( Integer(Pointer(ObjectToStreamFrom)) );
      FInitialSize := IMAGE_MEMMAP_INIT_SIZE;
      ms := TMemoryStreamPlus.Create();
      BitmapToStream.SaveToStream( ms );

    end else if ( ObjectToStreamFrom.ClassName = 'TI2XOCRResults' ) then begin
      OCRResultsToStream := TI2XOCRResults( ObjectToStreamFrom );
      if ( Length(MapName) = 0 ) then
        MapName := OCR_MEMMAP_HEADER + IntToStr( Integer(Pointer(ObjectToStreamFrom)) );
      FInitialSize := OCR_MEMMAP_INIT_SIZE;
      ms := TMemoryStreamPlus.Create( OCRResultsToStream.AsXML() );
  
    end else
      raise Exception.Create('Memory Map Manager does not know how to write an object of type "' + ObjectToStreamFrom.ClassName + '" yet.');

    FMemoryMap := self.Stream( MapName, FInitialSize );
    if ( not FMemoryMap.Write( ms ) ) then
      raise Exception.Create('Error while copying stream to Memory Map');
    result := true;
    
  finally
    if ( ms <> nil ) then FreeAndNil( ms );
  end;
end;
initialization

finalization
END.
