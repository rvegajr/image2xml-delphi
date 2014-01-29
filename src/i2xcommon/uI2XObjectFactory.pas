unit uI2XObjectFactory;

interface

uses Windows,
    SysUtils,
    JclStrings,
    Classes,
    uFileDir,
    SyncObjs,
    uHashTable,
    uDWImage,
    uI2XOCR;

const
  OBJECT_FACTORY_DEBUG='C:\Users\Administrator\AppData\Local\Temp\i2x\MAP.LOG';

type
  TObjectManager = class(TObject)
    protected
      FList : THashTable;
      function GetItem(Index: TIndex): TObject; virtual;
      procedure SetItem(Index: TIndex; const Value: TObject); virtual;
      function GetCount: Cardinal;
   published
      function Add( const ObjectName : string; const ObjectToAdd : TObject ) : TObject; virtual;
      procedure Delete( ObjectName : string );
      procedure Remove( ObjectName : string );
      property Count : Cardinal read GetCount;
      function Exists( const idx : TIndex ) : boolean;
      function SaveToMemoryMap( ObjectName : string ) : boolean; virtual;
      function FreeMemoryMap( ObjectName : string ) : boolean; virtual;
   public
      //property Items[ idx: TIndex ]: TObject read GetItem write SetItem;
      function AsString() : string; virtual;
      constructor Create();
      destructor Destroy; override;
  end;

  TDWImageFactory = class(TObjectManager)
    private
      function GetItem(Index: TIndex): TDWImage;
      procedure SetItem(Index: TIndex; const Value: TDWImage);
    published
      function Add( const ImageName : string; ImageToAdd : TDWImage ) : TDWImage;
      function SaveToMemoryMap( ObjectName : string ) : boolean; virtual;
      function FreeMemoryMap( ObjectName : string ) : boolean; virtual;
    public
      property Items[ idx: TIndex ]: TDWImage read GetItem write SetItem; default;
      function AsString() : string;
  end;

  TOCRResultsFactory = class(TObjectManager)
    private
      function GetItem(Index: TIndex): TI2XOCRResultsMM;
      procedure SetItem(Index: TIndex; const Value: TI2XOCRResultsMM);
    published
      function Add( const ResName : string; ResToAdd : TI2XOCRResultsMM ) : TI2XOCRResultsMM;
      function SaveToMemoryMap( ObjectName : string ) : boolean; virtual;
      function FreeMemoryMap( ObjectName : string ) : boolean; virtual;
    public
      property Items[ idx: TIndex ]: TI2XOCRResultsMM read GetItem write SetItem; default;
      function AsString() : string;
  end;

var
  DWImageFactory : TDWImageFactory;
  OCRResultsFactory : TOCRResultsFactory;

implementation

{ TObjectManager }

function TObjectManager.Add(const ObjectName: string;
  const ObjectToAdd: TObject): TObject;
begin
  FList.Add( ObjectName, ObjectToAdd );
  Result := ObjectToAdd;
end;

function TObjectManager.AsString: string;
var
  sb : TStringBuilder;
  kl : TKeyList;
  i : integer;
Begin
  try
    sb :=  TStringBuilder.Create();
    kl := FList.Keys;
    for i := 0 to Length( kl ) do begin
      sb.Append( kl[i] + #13);
    end;
    Result := sb.ToString();
  finally
    FreeAndNil( sb );
  end;
End;

constructor TObjectManager.Create;
begin
  FList := THashTable.Create();
end;

procedure TObjectManager.Delete( ObjectName : string );
var
  item : TObject;
begin
  if ( FList.ContainsKey( ObjectName ) ) then begin
    FList.Delete( ObjectName );
  end;
end;

destructor TObjectManager.Destroy;
begin
  FreeAndNil( FList );
  inherited;
end;

function TObjectManager.Exists(const idx : TIndex ): boolean;
begin
  if ( idx.DataType = idtString ) then
    Result := FList.ContainsKey( idx.StringValue )
  else if ( idx.DataType = idtInteger ) then
    Result := (( FList.Count - 1 ) < idx.IntValue );
  ;
end;

function TObjectManager.FreeMemoryMap(ObjectName: string): boolean;
begin

end;

function TObjectManager.GetCount: Cardinal;
begin
  Result := FList.Count;
end;

function TObjectManager.GetItem(Index: TIndex): TObject;
begin
  Result := FList.Items[ Index ];
end;

procedure TObjectManager.Remove( ObjectName : string );
begin
  FList.Remove( ObjectName );
end;

function TObjectManager.SaveToMemoryMap(ObjectName: string): boolean;
begin

end;

procedure TObjectManager.SetItem(Index: TIndex; const Value: TObject);
Begin
  if ( Index.DataType = idtString ) then begin
    if ( FList.ContainsKey( Index.StringValue ) ) then begin
      FList.Items[ Index.StringValue ] := Value;
    end else begin
      FList.Add( Index.StringValue, Value );
    end;
  end else begin
    raise Exception.Create('SetItem Index cannot be an integer');
  end;
End;

{ TDWImageFactory }

function TDWImageFactory.Add(const ImageName: string;
  ImageToAdd: TDWImage): TDWImage;
begin
  self.FList.Add( ImageName, TObject(ImageToAdd));
end;

function TDWImageFactory.AsString: string;
var
  sb : TStringBuilder;
  kl : TKeyList;
  item : TDWImage;
  i : integer;
Begin
  try
    sb :=  TStringBuilder.Create();
    kl := FList.Keys;
    for i := 0 to Length( kl ) - 1 do begin
      item := TDWImage( FList.Items[ kl[i] ] );
      sb.Append( kl[i] + #13);
    end;
    Result := sb.ToString();
  finally
    FreeAndNil( sb );
  end;
End;

function TDWImageFactory.FreeMemoryMap(ObjectName: string): boolean;
var
  img : TDWImage;
begin
  Result := false;
  if ( FList.ContainsKey( ObjectName ) ) then begin
    img := TDWImage( FList.Items[ ObjectName ] );
    Result := img.ClearMemoryMap;
  end;
end;

function TDWImageFactory.GetItem(Index: TIndex): TDWImage;
begin
  if ( self.Exists(Index) ) then
    Result := TDWImage( self.FList.Items[ Index ] )
  else
    if ( Index.DataType = idtString ) then begin
      Result := TDWImage.Create();
      self.Add( Index.StringValue, Result );
      //Result := self.Add( Index.StringValue, TDWImage.Create() );
    end else
      raise Exception.Create('Index cannot be integer if item does not previosuly exist.. how can we add it?');
end;

function TDWImageFactory.SaveToMemoryMap(ObjectName: string): boolean;
var
  img : TDWImage;
begin
  Result := false;
  if ( FList.ContainsKey( ObjectName ) ) then begin
    img := TDWImage( FList.Items[ ObjectName ] );
    Result := img.SaveToMemoryMap( ObjectName );
  end;
end;

procedure TDWImageFactory.SetItem(Index: TIndex; const Value: TDWImage);
begin
  if ( Index.DataType = idtString ) then begin
    if ( FList.ContainsKey( Index.StringValue ) ) then begin
      FList.Items[ Index.StringValue ] := TObject( Value );
    end else begin
      FList.Add( Index.StringValue, TObject( Value ) );
    end;
  end else begin
    raise Exception.Create('SetItem Index cannot be an integer');
  end;
end;

{ TOCRResultsFactory }

function TOCRResultsFactory.Add(const ResName: string;
  ResToAdd: TI2XOCRResultsMM): TI2XOCRResultsMM;
begin
  self.FList.Add( ResName, TObject(ResToAdd));
end;

function TOCRResultsFactory.AsString: string;
var
  sb : TStringBuilder;
  kl : TKeyList;
  item : TI2XOCRResultsMM;
  i : integer;
Begin
  try
    sb :=  TStringBuilder.Create();
    kl := FList.Keys;
    for i := 0 to Length( kl ) do begin
      item := TI2XOCRResultsMM( FList.Items[ kl[i] ] );
      sb.Append( kl[i] + #13);
    end;
    Result := sb.ToString();
  finally
    FreeAndNil( sb );
  end;
End;

function TOCRResultsFactory.FreeMemoryMap(ObjectName: string): boolean;
var
  img : TI2XOCRResultsMM;
begin
  Result := false;
  if ( FList.ContainsKey( ObjectName ) ) then begin
    img := TI2XOCRResultsMM( FList.Items[ ObjectName ] );
    Result := img.ClearMemoryMap;
  end;
end;

function TOCRResultsFactory.GetItem(Index: TIndex): TI2XOCRResultsMM;
begin
  //Result := TI2XOCRResultsMM( FList.Items[ Index ] );
  if ( self.Exists(Index) ) then
    Result := TI2XOCRResultsMM( self.FList.Items[ Index ] )
  else
    if ( Index.DataType = idtString ) then
      Result := self.Add( Index.StringValue, TI2XOCRResultsMM.Create() )
    else
      raise Exception.Create('Index cannot be integer if item does not previosuly exist.. how can we add it?');
end;

function TOCRResultsFactory.SaveToMemoryMap(ObjectName: string): boolean;
begin

end;

procedure TOCRResultsFactory.SetItem(Index: TIndex;
  const Value: TI2XOCRResultsMM);
begin
  if ( Index.DataType = idtString ) then begin
    if ( FList.ContainsKey( Index.StringValue ) ) then begin
      FList.Items[ Index.StringValue ] := Value;
    end else begin
      FList.Add( Index.StringValue, Value );
    end;
  end else begin
    raise Exception.Create('SetItem Index cannot be an integer');
  end;
end;

initialization
  DWImageFactory := TDWImageFactory.Create();
  OCRResultsFactory := TOCRResultsFactory.Create();

finalization
  FreeAndNil( DWImageFactory );
  FreeAndNil( OCRResultsFactory );
END.
