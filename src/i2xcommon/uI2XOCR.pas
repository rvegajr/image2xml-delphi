unit uI2XOCR;

interface
uses
  Windows,
  Classes,
  uStrUtil,
  Graphics,
  SysUtils,
  OmniXML,
  uOmniXML,
  uDWImage,
  uI2XConstants,
  uI2XPlugin,
  Contnrs,
  JclStrings,
  MapStream,
  uHashTable,
  Typinfo;

const
  UNIT_NAME = 'uI2XOCR';

type
  TCountArray =  array[0..100000000] of Integer;
  PCountArray = ^TCountArray;
  TOCRItemDataType = ( odtNoneSpecified = 0, odtString, odtNumeric, odtDateTime );
  TIndexValueItem = record
    idx : Integer;
    val: string;
    obj: TObject;
  end;
  TIndexValueArray = array of TIndexValueItem;

  TI2XOCRItem = class(TObject)
    private
    protected
      FX : integer;
      FY : integer;
      FWidth : integer;
      FHeight : integer;
      FAccuracy : single;
      FData : string;
      FDataType : TOCRItemDataType;
      FPtr : Pointer;
      FObj : TObject;
      function getX() : integer; virtual;
      function getY() : integer; virtual;
      function getWidth() : integer; virtual;
      function getHeight() : integer; virtual;
      function getAccuracy() : single; virtual;
      function getData() : string; virtual;
      function getDataEnc: string; virtual;
      procedure setDataEnc(const Value: string); virtual;
      function getDataType: TOCRItemDataType; virtual;
      procedure setDataType(const Value: TOCRItemDataType); virtual;
      function getAsRect: TRect;
      function getBottom: integer;
      function getRight: integer;
    published
      property X : integer read getX Write FX;
      property Y : integer read getY Write FY;
      property Width : integer read getWidth Write FWidth;
      property Height : integer read getHeight Write FHeight;
      property Right : integer read getRight;
      property Bottom : integer read getBottom;
      property Accuracy : single read getAccuracy Write FAccuracy;
      property Data : string read getData Write FData;
      property DataEnc : string read getDataEnc Write setDataEnc;
      property DataType : TOCRItemDataType read getDataType Write setDataType;
      property AsRect : TRect  read getAsRect;
    public
      property Ptr : Pointer read FPtr Write FPtr;
      property Obj : TObject read FObj Write FObj;
      procedure Clear; 
      function AsXML() : string; virtual;
      function FromXML( const XMLString : string ) : boolean;
      constructor Create(); virtual;
      destructor Destroy; override;
      procedure Assign( Source : TI2XOCRItem ); dynamic;
      procedure AssignTo( Target : TI2XOCRItem ); dynamic;
  end;

  TI2XOCRResult = class(TI2XOCRItem)
  private
    FID: string;
    //FItemList : THashTable;
    FItemList : TObjectList;
    procedure SetID(const Value: string);
    function GetItem(Index: Integer): TI2XOCRItem;
    procedure SetItem(Index: Integer; const Value: TI2XOCRItem);
    function GetCount(): Cardinal;
    function IndexListSortedByY() : TIndexValueArray;
  published
    function Add( const ItemToAdd : TI2XOCRItem ) : integer;
    procedure Delete(ItemToDelete : Integer );
    property ID : string read FID write SetID;
    property Count : Cardinal read GetCount;
    procedure Clear;
  public
    function SortByY: boolean;
    property Items[Index: Integer]: TI2XOCRItem read GetItem write SetItem; default;
    function AsXML() : string; override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign( Source : TI2XOCRItem ); override;
    procedure AssignTo( Target : TI2XOCRItem ); override;
  end;

  TI2XOCRResults = class(TObject)
    private
      FImageID : string;
      //FResultsList : THashTable;
      FResultsList : TObjectList;
      function GetItem(Index: Integer): TI2XOCRResult;
      procedure SetItem(Index: Integer; const Value: TI2XOCRResult);
      function GetCount(): Cardinal;
      function XMLToResultsObject( const XMLStringData : string; OCRResultsObject : TI2XOCRResults ) : boolean;
      function IndexListSortedByX() : TIndexValueArray;
      function GetBoundary: TRect;
      function GetFirstCharacter: TI2XOCRItem;
    published
      function Add( const ItemToAdd : TI2XOCRResult ) : integer;
      procedure Delete( ItemToDelete : Integer );
      procedure Clear;
      property ImageID : string read FImageID write FImageID;
      property Count : Cardinal read GetCount;
    public
      function SortByX() : boolean;
      property Items[Index: Integer]: TI2XOCRResult read GetItem write SetItem; default;
      property Boundary : TRect read GetBoundary;
      property FirstCharacter : TI2XOCRItem read GetFirstCharacter;
      function AsXML( const RootNodeName : string = 'ocr') : string;
      function SaveToFile( fileName : TFileName ) : boolean;
      function LoadFromFile( fileName : TFileName ) : boolean;
      function LoadXML( XMLString : string ) : boolean;
      constructor Create; dynamic;
      destructor Destroy; override;
  end;
{*
  TI2XOCRResultsMM = class(TI2XOCRResults)
    private
      FMemoryMap : TMapStream;
      function getIsMemoryMapped: boolean;
    public
      property IsMemoryMapped : boolean read getIsMemoryMapped;
      function SaveToMemoryMap( var sMapName : string ) : boolean;
      function LoadFromMemoryMap( const sMapName: string; const InitSize : Cardinal = OCR_MEMMAP_INIT_SIZE ): boolean;
      function ClearMemoryMap() : boolean;
      constructor Create();  override;
      destructor Destroy;  override;
  end;
*}
  TI2XOCR = class(TI2XPlugin)
    private
      function GetOCRData(): string;
    protected
      FImage : TDWImage;
      FOCRData : TI2XOCRResults;
    public
      property OCRData : string read GetOCRData;
      function OCRImage( const MemoryMapName, ImageList : string ) : boolean; virtual; abstract;
      function OCRImageDLL( MemoryMapName, ImageList : PChar;
        LoadImageFromMemoryMap : boolean = true ) : boolean; virtual;
      constructor Create( const GUID, ParmFileName : string ); override;
      destructor Destroy; override;
  end;

  TDWPersist = class(TObject)
    private
    protected
      FParent : TDWPersist;
      FEnabled : boolean;
    public
      property Parent : TDWPersist read FParent write FParent;
      function AsXML() : string; dynamic; abstract;
      procedure Clear(); dynamic; abstract;
      function LoadFromXML( xmlString : string ) : boolean; dynamic; abstract;
      procedure CopyTo( TargetObject : TDWPersist ); dynamic; abstract;
      procedure CopyFrom( SourceObject : TDWPersist ); dynamic; abstract;
      property Enabled : boolean read FEnabled write FEnabled;
  end;

  pTDLLOCRTest = ^TDLLOCRTest;
  TDLLOCRTest = class(TDWPersist)
    private
      FTest : TOCRTest;
      function GetName: string;
      procedure SetName(const Value: string);
    function GetDescr: string;
    published
      property Test : TOCRTest read FTest write FTest;
      property Name : string read GetName write SetName;
      property Description : string read GetDescr;
    public
      procedure Clear(); override;
      function AsXML() : string; override;
      function LoadFromXML( xmlString : string ) : boolean;  override;
      procedure CopyTo( TargetObject : TDWPersist );  override;
      procedure CopyFrom( SourceObject : TDWPersist ); override;
      function Invert : boolean;
      function Slice : boolean;
      function IsWholePageTest : boolean;
      constructor Create(); overload;
      constructor Create( TestToDefaultTo : TOCRTest ); overload;
      constructor Create( TestToDefaultTo : TOCRTest; const Enabled : boolean ); overload;
      constructor Create( TestToDefaultTo : string; const Enabled : boolean ); overload;
  end;

  TDLLOCRTestEntry = class(TDWPersist)
    private
      FName : string;
      FDescr : string;
      FItems : THashTable;
      procedure PutItem(idx: TIndex; const Value: TDLLOCRTest);
      function GetItem(idx: TIndex) : TDLLOCRTest ;
      function GetCount() : integer;
      function CheckTest(testToCheck: TOCRTest): boolean;
    function GetIndexByWholeTests: TIndexValueArray;
    public
      procedure Clear( const DeepClear : boolean = false ); overload;
      procedure SetOCRTest(testToCheck: TOCRTest; const Value: boolean); overload;
      procedure SetOCRTest(testToCheck: string; const Value: boolean); overload;
      function AsXML() : string;
      function LoadFromXML( xmlString : string ) : boolean;
      procedure CopyTo( TargetObject : TDWPersist ); override;
      procedure CopyFrom( SourceObject : TDWPersist ); override;
      property Name : string read FName write FName;
      property Descr : string read FDescr write FDescr;
      property Enabled : boolean read FEnabled write FEnabled;
		  property Items[ idx : TIndex ]: TDLLOCRTest read GetItem write PutItem; default;
		  property IsActive[ testToCheck : TOCRTest ]: boolean read CheckTest write setOCRTest;
		  property IndexByWholeTests : TIndexValueArray read GetIndexByWholeTests;
      procedure Add( itemToAdd : TDLLOCRTest ); overload;
      function Exists( const DLLNameToCheck : string ) : boolean;
      property Count : integer read GetCount;
      constructor Create(); overload;
      constructor Create( const Name : string ); overload;
      constructor Create( const Name : string;
          DoNormalWholeTest : boolean;
          DoNormalSliceTest : boolean;
          DoInvertWholeTest : boolean;
          DoInvertSliceTest : boolean
      ); overload;
      destructor Destroy(); override;
  end;

  TDLLOCRTests = class(TDWPersist)
    private
      FItems : THashTable;
      FName : string;
      procedure PutItem(idx: TIndex; const Value: TDLLOCRTestEntry);
      function GetItem(idx: TIndex) : TDLLOCRTestEntry ;
      function GetCount() : integer;
      function GetTestCount: integer;
    public
      property Name : string read FName write FName;
      procedure Clear(); override;
      function AsXML() : string; override;
      function LoadFromXML( xmlString : string ) : boolean; override;
      procedure CopyTo( TargetObject : TDWPersist ); override;
      procedure CopyFrom( SourceObject : TDWPersist );  override;
      procedure Add( itemToAdd : TDLLOCRTestEntry ); overload;
      procedure Add( DLLNameToAdd : string;
          DoNormalWholeTest : boolean = true;
          DoNormalSliceTest : boolean = true;
          DoInvertWholeTest : boolean = true;
          DoInvertSliceTest : boolean = true
      ); overload;
      function Exists( const DLLNameToCheck : string ) : boolean;
		  property Items[ idx : TIndex ]: TDLLOCRTestEntry read GetItem write PutItem; default;
      property Count : integer read GetCount;
      property TestCount : integer read GetTestCount;
      constructor Create(); overload;
      constructor Create( const Name : string ); overload;
      destructor Destroy();  override;
  end;
//  procedure CountSortIndexValueArray( var List : TIndexValueArray;
//    minValue, maxValue : integer ); overload;
//  procedure CountSortIndexValueArray( var List, SortedList : TIndexValueArray;
//    min, max : integer; minValue, maxValue : integer ); overload;
  procedure QuickSortIndexValueArray(var List : TIndexValueArray;
    min, max : integer); overload;
  procedure QuickSortIndexValueArray(var List : TIndexValueArray ); overload;

implementation
{*
procedure CountSortIndexValueArray( var List : TIndexValueArray;
    minValue, maxValue : integer ); overload;
var
  SortedList : TIndexValueArray;
Begin
  SetLength( SortedList, Length( List ) );
  CountSortIndexValueArray( List, SortedList, 0, Length ( List ), minValue, maxValue );
  List := SortedList;
End;

procedure CountSortIndexValueArray( var List, SortedList : TIndexValueArray;
    min, max : integer; minValue, maxValue : integer );
var
    i, j, next_index : integer;
    count_index      : integer;
    //count_index      : TIndexValueItem;
    counts           : PCountArray;
begin
    SetLength( SortedList, Length( List ) );
    // Create the Counts array.
    GetMem(counts, (maxValue - minValue + 1) * SizeOf(integer));

    // Initialize the counts to zero.
    for i := 0 to maxValue - minValue do
        counts[i] := 0;

    // Count the items.
    for i := min to max do
    begin
        count_index := List[i].val - minValue;
        counts[count_index] := counts[count_index] + 1;
    end;

    // Place the items in the sorted array.
    next_index := min;
    for i := minValue to maxValue do
    begin
         for j := 1 to counts[i - minValue] do
         begin
             //SortedList[next_index] := i;
             SortedList[next_index] := List[i];
             next_index := next_index + 1;
         end;
    end;

    // Free the memory allocated for the counts array.
    FreeMem(counts);
end;
*}
procedure QuickSortIndexValueArray(var List : TIndexValueArray ); overload;
Begin
  QuickSortIndexValueArray( List, 0, Length( List ) - 1);
End;

procedure QuickSortIndexValueArray(var List : TIndexValueArray;
    min, max : integer);
var
    med_value : TIndexValueItem;
    hi, lo, i : integer;
begin
    // If the list has <= 1 element, it's sorted.
    if (min >= max) then Exit;

    // Pick a dividing item randomly.
    i := min + Trunc(Random(max - min + 1));
    med_value := List[i];

    // Swap it to the front so we can find it easily.
    List[i] := List[min];

    // Move the items smaller than this into the left
    // half of the list. Move the others into the right.
    lo := min;
    hi := max;
    while (True) do
    begin
        // Look down from hi for a value < med_value.
        while (List[hi].val >= med_value.val) do
        begin
            hi := hi - 1;
            if (hi <= lo) then Break;
        end;
        if (hi <= lo) then
        begin
            // We're done separating the items.
            List[lo] := med_value;
            Break;
        end;

        // Swap the lo and hi values.
        List[lo] := List[hi];

        // Look up from lo for a value >= med_value.
        lo := lo + 1;
        while (List[lo].val < med_value.val) do
        begin
            lo := lo + 1;
            if (lo >= hi) then Break;
        end;
        if (lo >= hi) then
        begin
            // We're done separating the items.
            lo := hi;
            List[hi] := med_value;
            Break;
        end;

        // Swap the lo and hi values.
        List[hi] := List[lo];
    end; // while (True) do

    // Sort the two sublists.
    QuickSortIndexValueArray(List, min, lo - 1);
    QuickSortIndexValueArray(List, lo + 1, max);
end;
{ TI2XOCR }

constructor TI2XOCR.Create(const GUID, ParmFileName: string);
begin
  inherited Create(GUID, ParmFileName);
  FOCRData := TI2XOCRResults.Create;
  FImage := TDWImage.Create;

end;

destructor TI2XOCR.Destroy;
begin
  FreeAndNil( FOCRData );
  FreeAndNil( FImage );
  inherited;
end;

function TI2XOCR.GetOCRData: string;
begin
  Result := FOCRData.AsXML;
end;

function TI2XOCR.OCRImageDLL(MemoryMapName, ImageList: PChar;
  LoadImageFromMemoryMap: boolean): boolean;
var
  sMemoryMapName, sImageList : string;
Begin
  try
    sMemoryMapName := PChar(MemoryMapName);
    sImageList := PChar(ImageList);
    if ( LoadImageFromMemoryMap ) then
      self.FMemoryMapManager.Read( sMemoryMapName, FImage );
      //FImage.LoadFromMemoryMap( sMemoryMapName );
    Result := self.OCRImage( sMemoryMapName, sImageList );
  finally
  end;
End;

{ TI2XOCRItem }

procedure TI2XOCRItem.Assign(Source: TI2XOCRItem);
begin
  with Self do begin
    X := Source.X;
    Y := Source.Y;
    Width := Source.Width;
    Height := Source.Height;
    Accuracy := Source.Accuracy;
    Data := Source.Data;
    DataType := Source.DataType;
  end;
end;

procedure TI2XOCRItem.AssignTo(Target: TI2XOCRItem);
begin
  with Target do begin
    X := self.X;
    Y := self.Y;
    Width := self.Width;
    Height := self.Height;
    Accuracy := self.Accuracy;
    Data := self.Data;
    DataType := self.DataType;
  end;
end;

function TI2XOCRItem.AsXML: string;
var
  sb : TStringBuilder;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<item' );
    sb.Append( ' x="');
    sb.Append( IntToStr( self.X ) );
    sb.Append( '"');
    sb.Append( ' y="');
    sb.Append( IntToStr( self.Y ) );
    sb.Append( '"');
    sb.Append( ' w="');
    sb.Append( IntToStr( self.Width ) );
    sb.Append( '"');
    sb.Append( ' h="');
    sb.Append( IntToStr( self.Height ) );
    sb.Append( '"');
    sb.Append( ' a="');
    sb.Append( FloatToStrF( self.Accuracy, ffGeneral, 6, 2) );
    sb.Append( '"');
    sb.Append( ' t="');
    sb.Append( IntToStr( Integer( self.DataType )) );
    sb.Append( '"');
    sb.Append( ' d="');
    sb.Append( self.DataEnc );
    sb.Append( '"');
    sb.Append( ' />' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TI2XOCRItem.Clear;
begin
  self.FX := 0;
  self.FY := 0;
  self.FWidth := 0;
  self.FHeight := 0;
  self.FAccuracy := 0;
  self.FData := '';
  self.FDataType := odtNoneSpecified;
  self.FPtr := nil;
  self.FObj := nil;
end;

constructor TI2XOCRItem.Create;
begin
  self.Clear;
end;

destructor TI2XOCRItem.Destroy;
begin
  if ( FObj <> nil ) then FObj.Free;
end;

function TI2XOCRItem.FromXML(const XMLString: string): boolean;
var
  xmlDoc: IXMLDocument;
  nod : IXMLNode;
  xmlNodeList : IXMLNodeList;
  iOCRIdx : integer;
Begin
  try
    if ( XMLString = '' ) then exit;
    xmlDoc := CreateXMLDoc;
    if (xmlDoc.LoadXML( XMLString ) ) then begin
      xmlNodeList := xmldoc.DocumentElement.ChildNodes;
      for iOCRIdx := 0 to xmlNodeList.length - 1 do begin
        nod := xmlNodeList.Item[iOCRIdx];
        if ( nod.NodeName = 'x' ) then
          self.X := StrToInt( nod.NodeValue )
        else if ( nod.NodeName = 'y' ) then
          self.Y := StrToInt( nod.NodeValue )
        else if ( nod.NodeName = 'w' ) then
          self.Width := StrToInt( nod.NodeValue )
        else if ( nod.NodeName = 'h' ) then
          self.Height := StrToInt( nod.NodeValue )
        else if ( nod.NodeName = 'a' ) then
          self.Accuracy := StrToFloat( nod.NodeValue )
        else if ( nod.NodeName = 't' ) then
          self.DataType := TOCRItemDataType( StrToInt( nod.NodeValue ))
        else if ( nod.NodeName = 'd' ) then
          self.DataEnc := nod.NodeValue
        ;
      end;
    end else begin
      raise Exception.Create('Could not load XML.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
end;

function TI2XOCRItem.getAccuracy: single;
begin
  Result := self.FAccuracy;
end;

function TI2XOCRItem.getAsRect: TRect;
begin
  getAsRect.Left := self.X;
  getAsRect.Top := self.Y;
  getAsRect.Right := self.X + self.Width;
  getAsRect.Bottom := self.Y + self.Height;
end;

function TI2XOCRItem.getBottom: integer;
begin
  Result := self.FY + self.FHeight;
end;

function TI2XOCRItem.getData: string;
begin
  Result := self.FData;
end;

function TI2XOCRItem.getDataEnc: string;
begin
  Result := XMLEnc( self.FData );
end;

function TI2XOCRItem.getDataType: TOCRItemDataType;
begin
  Result := self.FDataType;
end;

function TI2XOCRItem.getHeight: integer;
begin
  Result := self.FHeight;
end;

function TI2XOCRItem.getRight: integer;
begin
  Result := self.FX + self.FWidth;
end;

function TI2XOCRItem.getWidth: integer;
begin
  Result := self.FWidth;
end;

function TI2XOCRItem.getX: integer;
begin
  Result := self.FX;
end;

function TI2XOCRItem.getY: integer;
begin
  Result := self.FY;
end;

procedure TI2XOCRItem.setDataEnc(const Value: string);
begin
  FData := XMLDec( Value );
end;

procedure TI2XOCRItem.setDataType(const Value: TOCRItemDataType);
begin
  FDataType := Value;
end;

{ TI2XOCRResult }

function TI2XOCRResult.Add(const ItemToAdd: TI2XOCRItem): integer;
begin
  //FItemList.Add( IntToStr(FItemList.Count),  ItemToAdd );
  //Result := FItemList.Count;
  //Result := FItemList.Add( IntToStr(FItemList.Count),  ItemToAdd );
  Result := FItemList.Add( ItemToAdd );

  //self.FTI2XOCRItem := TI2XOCRItem.Create();
  //FTI2XOCRItem.Assign( ItemToAdd );
  //Result := FItemList.Add( IntToStr(FItemList.Count),  FTI2XOCRItem );
  //FTI2XOCRItem := nil;
end;

procedure TI2XOCRResult.Assign(Source: TI2XOCRItem);
begin
  inherited;

end;

procedure TI2XOCRResult.AssignTo(Target: TI2XOCRItem);
begin
  inherited;

end;

function TI2XOCRResult.AsXML: string;
var
  sb : TStringBuilder;
  i : integer;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<slice ' );
    sb.Append( ' id="');
    sb.Append( self.ID );
    sb.Append( '"');
    sb.Append( ' x="');
    sb.Append( IntToStr( self.X ) );
    sb.Append( '"');
    sb.Append( ' y="');
    sb.Append( IntToStr( self.Y ) );
    sb.Append( '"');
    sb.Append( ' w="');
    sb.Append( IntToStr( self.Width ) );
    sb.Append( '"');
    sb.Append( ' h="');
    sb.Append( IntToStr( self.Height ) );
    sb.Append( '"');
    sb.Append( ' t="');
    sb.Append( IntToStr( integer( self.DataType ) ) );
    sb.Append( '"');
    sb.Append( ' a="');
    sb.Append( FloatToStrF( self.Accuracy, ffGeneral, 6, 2) );
    sb.Append( '"');
    sb.Append( '>' );
    sb.Append( '<items>' );
    for i := 0 to self.FItemList.Count - 1 do begin
      sb.Append( self.Items[i].AsXML() );
    end;
    sb.Append( '</items>' );
    sb.Append( '</slice>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TI2XOCRResult.Clear;
begin
  FItemList.Clear;
end;

constructor TI2XOCRResult.Create;
begin
  inherited;
  self.FItemList := TObjectList.Create;
end;

procedure TI2XOCRResult.Delete( ItemToDelete : Integer );
begin
  self.FItemList.Delete( ItemToDelete );
end;

destructor TI2XOCRResult.Destroy;
begin
  FreeAndNil( FItemList );
  inherited;
end;

function TI2XOCRResult.GetCount: Cardinal;
begin
  Result := self.FItemList.Count;
end;

function TI2XOCRResult.GetItem(Index: Integer): TI2XOCRItem;
begin
  Result := TI2XOCRItem( FItemList.Items[ Index ] );
end;

function TI2XOCRResult.IndexListSortedByY: TIndexValueArray;
var
  i : Integer;
begin
  SetLength( Result, self.Count );
  for i := 0 to self.Count - 1 do begin
    Result[i].idx := i;
    //Result[i].val := self.Items[i].Y;
    Result[i].val := FormatFloat('0000000000', self.Items[i].Y) +
        FormatFloat('0000000000', self.Items[i].X);
    Result[i].obj := self.Items[i];
  end;
  QuickSortIndexValueArray( Result );
end;

function TI2XOCRResult.SortByY: boolean;
var
  arr : TIndexValueArray;
  i : Integer;
begin
  arr := self.IndexListSortedByY();
  FItemList.OwnsObjects := false;
  for i := 0 to Length( arr ) - 1 do begin
    self.FItemList.Items[ i ] := arr[i].obj;
  end;
  FItemList.OwnsObjects := true;
end;

procedure TI2XOCRResult.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TI2XOCRResult.SetItem(Index: Integer; const Value: TI2XOCRItem);
begin
  TI2XOCRItem( FItemList.Items[ Index ] ).Assign( Value );
end;

{ TI2XOCRResults }

function TI2XOCRResults.Add(const ItemToAdd: TI2XOCRResult): integer;
begin
  Result := self.FResultsList.Add( ItemToAdd );
end;

function TI2XOCRResults.AsXML(const RootNodeName : string = 'ocr'): string;
var
  sb : TStringBuilder;
  i : integer;
begin
  try
    sb := TStringbuilder.Create;
    if ( Length(RootNodeName) > 0 ) then begin
      sb.Append( '<' );
      sb.Append( RootNodeName );
      sb.Append( '>' );
    end;
    sb.Append( '<image ' );
    sb.Append( ' id="');
    sb.Append( self.FImageID );
    sb.Append( '">');
    sb.Append( '<slices>' );
    for i := 0 to self.FResultsList.Count - 1 do begin
      sb.Append( self[i].AsXML() );
    end;
    sb.Append( '</slices>' );
    sb.Append( '</image>' );
    if ( Length(RootNodeName) > 0 ) then begin
      sb.Append( '</' );
      sb.Append( RootNodeName );
      sb.Append( '>' );
    end;
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TI2XOCRResults.Clear;
begin
  FResultsList.Clear;
end;

constructor TI2XOCRResults.Create;
begin
  self.FResultsList := TObjectList.Create;
end;

procedure TI2XOCRResults.Delete( ItemToDelete: Integer);
begin
  FResultsList.Delete( ItemToDelete );
end;

destructor TI2XOCRResults.Destroy;
Begin
  FreeAndNil( FResultsList );
End;

function TI2XOCRResults.GetBoundary: TRect;
var
  iResult : integer;
  iItem   : integer;
  oItem   : TI2XOCRItem;
  oResult : TI2XOCRResult;
  right   : integer;
  bottom  : integer;
Begin
  Initialize( Result );
  if ( self.Count = 0 ) then begin
    exit;
  end;
  
  Result.Left := MAXINT;
  Result.Top := MAXINT;
  Result.Right := 0;
  Result.Bottom := 0;
  for iResult := 0 to self.Count - 1 do begin
    oResult := self.Items[ iResult ];
    for iItem := 0 to oResult.Count - 1 do begin
      oItem := oResult.Items[ iItem ];
      if ( oItem.X < Result.Left ) then Result.Left := oItem.X;
      if ( oItem.Y < Result.Top ) then Result.Top := oItem.Y;
      if ( oItem.Right > Result.Right ) then Result.Right := oItem.Right;
      if ( oItem.Bottom > Result.Bottom ) then Result.Bottom := oItem.Bottom;
    end;
  end;
End;

function TI2XOCRResults.GetCount: Cardinal;
Begin
  Result := self.FResultsList.Count;
End;

function TI2XOCRResults.GetFirstCharacter: TI2XOCRItem;
var
  oItem   : TI2XOCRItem;
  oResult : TI2XOCRResult;
  ResultsIndexSortedByX, ItemsIndexSortedByY: TIndexValueArray;
Begin
  //This will obtain an index of the result items from left to right
  Result := nil;
  if ( self.Count = 0 ) then begin
    exit;
  end;
  self.SaveToFile( 'C:\Users\Administrator\AppData\Local\Temp\i2x\BEFORE_FIRST_CHAR.xml');
  ResultsIndexSortedByX := self.IndexListSortedByX();
  if ( Length( ResultsIndexSortedByX ) > 0 ) then begin
    //This will obtain an index of the result items from left to right
    oResult := TI2XOCRResult( ResultsIndexSortedByX[0].obj );
    ItemsIndexSortedByY := oResult.IndexListSortedByY();
    if ( Length( ItemsIndexSortedByY ) > 0 ) then begin
      oItem := TI2XOCRItem( ItemsIndexSortedByY[0].obj );
      Result := oItem;
    end;
  end;
End;

function TI2XOCRResults.GetItem(Index: Integer): TI2XOCRResult;
begin
  Result := TI2XOCRResult( self.FResultsList.Items[ Index ] );
end;

function TI2XOCRResults.IndexListSortedByX: TIndexValueArray;
var
  i : Integer;
begin
  SetLength( Result, self.Count );
  for i := 0 to self.Count - 1 do begin
    Result[i].idx := i;
    //Result[i].val := self.Items[i].Y;
    Result[i].val := FormatFloat('0000000000', self.Items[i].X) +
        FormatFloat('0000000000', self.Items[i].Y);
    Result[i].obj := self.Items[i];
  end;
  QuickSortIndexValueArray( Result );
end;

function TI2XOCRResults.LoadFromFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.Load( fileName ) ) then begin
      Result := XMLToResultsObject( xmlDoc.XML, self );
    end else begin
      raise Exception.Create('Could not load XML from file ' + fileName + '.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

function TI2XOCRResults.XMLToResultsObject( const XMLStringData : string; OCRResultsObject : TI2XOCRResults ) : boolean;
var
  xmlDoc: IXMLDocument;
  nod, nodSlice, nodItem  : IXMLElement;
  oNod : IXMLNode;
  parsed : boolean;
  xmlNodeList, xmlSubNodeList, nodItemList : IXMLNodeList;
  ocrresult: TI2XOCRResult;
  ocrresitem : TI2XOCRItem;
  iImageIdx, iSliceIdx, iItemIdx : integer;
Begin
  try
    Result := false;
    if ( OCRResultsObject = nil ) then
      OCRResultsObject := TI2XOCRResults.Create
    else
      OCRResultsObject.Clear;
    xmlDoc := CreateXMLDoc;
    parsed := xmlDoc.loadXML( XMLStringData );
    if ( not parsed ) then begin
      raise Exception.Create( 'XML Passed to function could not be parsed.' );
    end;
    xmlNodeList := xmldoc.DocumentElement.ChildNodes;
    for iImageIdx := 0 to xmlNodeList.length - 1 do begin
      nod := IXMLElement(xmlNodeList.Item[iImageIdx]);
      if ( nod.nodeType = ELEMENT_NODE ) then begin
        if (nod.nodeName = 'image' ) then begin
          OCRResultsObject.ImageID := nod.attributes.getNamedItem('id').text;
          xmlSubNodeList := nod.SelectNodes('slices/slice');
          if ( xmlSubNodeList.length  > 0 ) then begin

            for iSliceIdx := 0 to xmlSubNodeList.length - 1 do begin
              nodSlice := IXMLElement(xmlSubNodeList.Item[iSliceIdx]);
              ocrresult := TI2XOCRResult.Create();

              ocrresult.ID := GetAttr( nodSlice, 'id' );
              ocrresult.X := GetAttr( nodSlice, 'x', 0 );
              ocrresult.Y := GetAttr( nodSlice, 'y', 0);
              ocrresult.Width :=  GetAttr( nodSlice, 'w', 0);
              ocrresult.Height := GetAttr( nodSlice, 'h', 0);
              ocrresult.Accuracy := GetAttr( nodSlice, 'a', 0.0 );
              ocrresult.DataType := TOCRItemDataType( GetAttr( nodSlice, 't', 0 ) );
              nodItemList := nodSlice.SelectNodes('items/item');
              if ( nodItemList.length > 0 ) then begin
                for iItemIdx := 0 to nodItemList.length - 1 do begin
                  nodItem := IXMLElement(nodItemList.Item[iItemIdx]);
                  ocrresitem := TI2XOCRItem.Create();
                  ocrresitem.X := GetAttr( nodItem, 'x', 0 );
                  ocrresitem.Y := GetAttr( nodItem, 'y', 0 );
                  ocrresitem.Width := GetAttr( nodItem, 'w', 0 );
                  ocrresitem.Height := GetAttr( nodItem, 'h', 0 );
                  ocrresitem.Accuracy := GetAttr( nodItem, 'a', 0.0 );
                  ocrresitem.Data := GetAttr( nodItem, 'd' );
                  ocrresitem.DataType := TOCRItemDataType( GetAttr( nodItem, 't', 0 ) );
                  ocrresult.Add( ocrresitem );
                end;
              end;
              OCRResultsObject.Add( ocrresult );
            end;

          end;
        end;
      end;
    end;
  finally
  end;
  Result := true;
End;

function TI2XOCRResults.LoadXML(XMLString: string): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( XMLString ) ) then begin
      Result := XMLToResultsObject( xmlDoc.XML, self );
    end else begin
      raise Exception.Create('Could not load XML. Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

function TI2XOCRResults.SaveToFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( AsXML() ) ) then begin
      xmlDoc.Save( fileName );
      Result := true;
    end else begin
      raise Exception.Create('File ' + fileName + ' could not be loaded on into OCR Results.');
    end;
  finally
  end;
End;

procedure TI2XOCRResults.SetItem(Index: Integer; const Value: TI2XOCRResult);
begin
  TI2XOCRResult( self.FResultsList.Items[ Index ] ).Assign( Value );
end;

function TI2XOCRResults.SortByX: boolean;
var
  arr : TIndexValueArray;
  i : Integer;
begin
  arr := self.IndexListSortedByX();
  FResultsList.OwnsObjects := false;
  for i := 0 to Length( arr ) - 1 do begin
    self.FResultsList.Items[ i ] := arr[i].obj;
  end;
  FResultsList.OwnsObjects := true;
end;

{ TI2XOCRResultsMM }
{*
function TI2XOCRResultsMM.ClearMemoryMap: boolean;
begin
  Result := false;
  try
    if ( FMemoryMap <> nil ) then
      FMemoryMap.Free;
    Result := true;
  except
    raise;
  end;
end;

constructor TI2XOCRResultsMM.Create;
begin
  inherited Create();
end;

destructor TI2XOCRResultsMM.Destroy;
begin
  if ( FMemoryMap <> nil ) then FreeAndNil( FMemoryMap );
  inherited;
end;

function TI2XOCRResultsMM.getIsMemoryMapped: boolean;
begin
  Result := (self.FMemoryMap <> nil);
end;

//This will read the bitmap from the memory stream.  since it is of TBitmap,
//  it will then get converted into the FreeBitmap
function TI2XOCRResultsMM.LoadFromMemoryMap(const sMapName: string;
  const InitSize: Cardinal): boolean;
var
  ms : TMemoryStreamPlus;
  Buffer : array of Char;
  s : string;
begin
  Result := false;
  try
    if ( FMemoryMap = nil ) then
      FMemoryMap := TMapStream.Create( sMapName, InitSize );
    ms := TMemoryStreamPlus.Create;
    if ( not FMemoryMap.Read( ms ) ) then
      raise Exception.Create('Error while copying OCR Results from Memory Map');
    self.ClearMemoryMap();
    self.LoadXML( ms.Read().StringValue );
    Result := true;
  finally
    FreeAndNil( ms );
  end;
end;

function TI2XOCRResultsMM.SaveToMemoryMap( var sMapName: string): boolean;
const
  MAX_RETRY = 5;
var
  ms : TMemoryStreamPlus;
  iRetry : integer;
begin
  Result := false;
  try
    if ( Length(sMapName) = 0 ) then
      sMapName := OCR_MEMMAP_HEADER + IntToStr( integer( @self ) );
    if ( FMemoryMap = nil ) then begin
      FMemoryMap := TMapStream.Create( sMapName, OCR_MEMMAP_INIT_SIZE );
      //FMemoryMap := ActiveMemoryMaps.Stream( sMapName, OCR_MEMMAP_INIT_SIZE );
    end;
    ms := TMemoryStreamPlus.Create( self.AsXML() );
    iRetry := 0;
    while ( (not Result) and (iRetry < 5) ) do begin
      try
        if ( not FMemoryMap.Write( ms ) ) then
          raise Exception.Create('Error while copying OCR Results from Memory Map');
        Result := true;
      except
        Inc( iRetry );
        if ( MAX_RETRY < iRetry ) then
          raise;
        FMemoryMap.Free;
        sMapName := OCR_MEMMAP_HEADER + IntToStr( integer( @self ) ) + '_' + IntToStr( iRetry );
        FMemoryMap := TMapStream.Create( sMapName, OCR_MEMMAP_INIT_SIZE );
        //FMemoryMap := ActiveMemoryMaps.Stream( sMapName, OCR_MEMMAP_INIT_SIZE );
      end;
    end;
  finally
    FreeAndNil( ms );
  end;
end;
*}
{ TDLLOCRTests }
procedure TDLLOCRTests.Add(itemToAdd: TDLLOCRTestEntry);
var
  sKey : string;
begin
  itemToAdd.Parent := self;
  if self.Exists( itemToAdd.Name ) then begin
    raise Exception.Create('TDLLOCRTests.Add(DLL Entry "' + itemToAdd.Name + '" already exists!');
  end;
  FItems.Add( itemToAdd.Name, itemToAdd );
end;

procedure TDLLOCRTests.Add(DLLNameToAdd: string;
  DoNormalWholeTest,
  DoNormalSliceTest,
  DoInvertWholeTest,
  DoInvertSliceTest: boolean);
Var
  newItemToAdd : TDLLOCRTestEntry;
begin
  newItemToAdd := TDLLOCRTestEntry.Create(
    DLLNameToAdd,
    DoNormalWholeTest,
    DoNormalSliceTest,
    DoInvertWholeTest,
    DoInvertSliceTest
  );
  if self.Exists( DLLNameToAdd ) then begin
    raise Exception.Create('TDLLOCRTests.Add(DLL Entry "' + DLLNameToAdd + '" already exists!');
  end;
  self.Add( newItemToAdd );
end;

function TDLLOCRTests.AsXML: string;
var
  sb : TStringBuilder;
  i : integer;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<ocr_tests' );
    sb.Append( ' name="' );
    sb.Append( self.Name );
    sb.Append( '">' );
    for i := 0 to self.GetCount - 1 do begin
      sb.Append( self.Items[i].AsXML());
    end;
    sb.Append( '</ocr_tests>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TDLLOCRTests.Clear;
begin
  self.Name := '';
  self.FItems.Clear;
end;

procedure TDLLOCRTests.CopyFrom(SourceObject: TDWPersist);
begin
  self.Clear;
  self.LoadFromXML( SourceObject.AsXML() );
end;

procedure TDLLOCRTests.CopyTo(TargetObject: TDWPersist);
begin
  TargetObject.Clear;
  TargetObject.LoadFromXML( self.AsXML() );
end;

constructor TDLLOCRTests.Create(const Name: string);
begin
  self.Name := Name;
  self.FItems := THashTable.Create(10);
end;

constructor TDLLOCRTests.Create;
begin
  self.FItems := THashTable.Create(10);
end;

destructor TDLLOCRTests.Destroy;
begin
  FreeAndNil( FItems );
end;

function TDLLOCRTests.Exists(const DLLNameToCheck: string): boolean;
begin
  Result := FItems.ContainsKey( DLLNameToCheck );
end;

function TDLLOCRTests.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TDLLOCRTests.GetItem(idx: TIndex) : TDLLOCRTestEntry ;
begin
  Result := TDLLOCRTestEntry( FItems[ idx ] );
end;

function TDLLOCRTests.GetTestCount: integer;
var
  entry : TDLLOCRTestEntry;
  i : integer;
begin
  Result := 0;
  for i := 0 to self.Count - 1 do begin
    entry := self.Items[i];
    Inc( Result, entry.Count );
  end;
end;

function TDLLOCRTests.LoadFromXML(xmlString: string): boolean;
var
  xmlDoc: IXMLDocument;
  attr : IXMLNode;
  ele: IXMLElement;
  nod, nodTest : IXMLNode;
  xmlNodeList, testNodeList : IXMLNodeList;
  iOCRIdx, iTestIdx : integer;
  sTestName, sTestValue : string;
  entry : TDLLOCRTestEntry;
Begin
  try
    self.Clear;
    if ( XMLString = '' ) then exit;
    xmlDoc := CreateXMLDoc;
    if (xmlDoc.LoadXML( XMLString ) ) then begin
      self.Name := GetAttr(xmldoc.DocumentElement, 'name' );
      xmlNodeList := xmldoc.DocumentElement.ChildNodes;
      for iOCRIdx := 0 to xmlNodeList.length - 1 do begin
        nod := xmlNodeList.Item[iOCRIdx];
        entry := TDLLOCRTestEntry.Create( GetAttr( nod, 'name' ) );
        entry.Descr := GetAttr( nod, 'descr' );
        entry.Enabled := ( GetAttr( nod, 'enabled' ) = 'true' );
        testNodeList := nod.ChildNodes;
        for iTestIdx := 0 to testNodeList.length - 1 do begin
          nodTest := testNodeList.Item[iTestIdx];
          sTestName := nodTest.NodeName;
          sTestValue := nodTest.Text;
          //entry.Enabled := ( GetAttr( nodTest, 'enabled' ) = 'true' );
          entry.SetOCRTest( sTestName, StrToBool( sTestValue ) );
          //entry.IsActive[ TOCRTest(GetEnumValue(TypeInfo(TOCRTest),sTestName)) ] := StrToBool( sTestValue );
        end;
        self.Add( entry );
      end;

    end else begin
      raise Exception.Create('Could not load XML.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

procedure TDLLOCRTests.PutItem(idx: TIndex; const Value: TDLLOCRTestEntry);
begin
  FItems[ idx ] := TObject( Value );
end;

{ TDLLOCRTestEntry }

procedure TDLLOCRTestEntry.Add(itemToAdd: TDLLOCRTest);
begin
  itemToAdd.Parent := self;
  FItems.Add( itemToAdd.Name, itemToAdd );
end;

function TDLLOCRTestEntry.AsXML: string;
var
  sb : TStringBuilder;
  i : integer;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<test name="');
    sb.Append( self.Name );
    sb.Append( '" descr="');
    sb.Append( self.Descr );
    sb.Append( '" enabled="');
    if self.Enabled then
      sb.Append( 'true' )
    else
      sb.Append( 'false' );
    sb.Append( '">');
    for i := 0 to self.GetCount - 1 do begin
      sb.Append( self.Items[i].AsXML());
    end;
    sb.Append( '</test>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

function TDLLOCRTestEntry.CheckTest(testToCheck: TOCRTest): boolean;
begin
end;

procedure TDLLOCRTestEntry.Clear( const DeepClear : boolean = false );
var
  DelItems : boolean;
begin
  self.Name := '';
  self.Descr := '';
  DelItems := FItems.FreeObjectsOnDestroy;
  FItems.FreeObjectsOnDestroy := DeepClear;
  FItems.Clear;
  FItems.FreeObjectsOnDestroy := DelItems;
end;

procedure TDLLOCRTestEntry.CopyFrom(SourceObject: TDWPersist);
var
  i : integer;
  src : TDLLOCRTestEntry;
  test : TDLLOCRTest;
begin
  src := TDLLOCRTestEntry(SourceObject);
  //self.Clear( true );
  self.Name := src.Name;
  self.Descr := src.Descr;
  self.Enabled := src.Enabled;
  for i := 0 to src.GetCount - 1 do begin
    self.IsActive[ src.Items[i].Test ] := src.Items[i].Enabled;
  end;
end;

procedure TDLLOCRTestEntry.CopyTo(TargetObject: TDWPersist);
var
  i : integer;
  tgt : TDLLOCRTestEntry;
  test : TDLLOCRTest;
begin
  tgt := TDLLOCRTestEntry(TargetObject);
    tgt.Clear( true );
    tgt.Name := self.Name;
    tgt.Descr := self.Descr;
    tgt.Enabled := self.Enabled;
    for i := 0 to self.GetCount - 1 do begin
      test := TDLLOCRTest.Create();
      tgt.Items[i].CopyTo( test );
      tgt.Add( test );
      //Items[i].Parent := tgt;
      //tgt.Add( self.Items[i] );
    end;
End;

constructor TDLLOCRTestEntry.Create(const Name: string);
begin
  self.Name := Name;
  FEnabled := true;
  FItems := THashTable.Create(4);
  self.Add( TDLLOCRTest.Create( ocrtNormalWhole, true) );
  self.Add( TDLLOCRTest.Create( ocrtNormalSliced, true) );
  self.Add( TDLLOCRTest.Create( ocrtInvertedWhole, true) );
  self.Add( TDLLOCRTest.Create( ocrtInvertedSliced, true) );
end;

constructor TDLLOCRTestEntry.Create(const Name: string; DoNormalWholeTest,
  DoNormalSliceTest, DoInvertWholeTest, DoInvertSliceTest: boolean);
begin
  self.Name := Name;
  FEnabled := true;
  FItems := THashTable.Create(4);
  self.Add( TDLLOCRTest.Create( ocrtNormalWhole, DoNormalWholeTest) );
  self.Add( TDLLOCRTest.Create( ocrtNormalSliced, DoNormalSliceTest) );
  self.Add( TDLLOCRTest.Create( ocrtInvertedWhole, DoInvertWholeTest) );
  self.Add( TDLLOCRTest.Create( ocrtInvertedSliced, DoInvertSliceTest) );
end;

constructor TDLLOCRTestEntry.Create;
begin
  FEnabled := true;
  FItems := THashTable.Create(4);
  self.Add( TDLLOCRTest.Create( ocrtNormalWhole, true) );
  self.Add( TDLLOCRTest.Create( ocrtNormalSliced, true) );
  self.Add( TDLLOCRTest.Create( ocrtInvertedWhole, true) );
  self.Add( TDLLOCRTest.Create( ocrtInvertedSliced, true) );
end;

destructor TDLLOCRTestEntry.Destroy;
begin
  FreeAndNil( FItems );
end;

function TDLLOCRTestEntry.Exists(const DLLNameToCheck: string): boolean;
begin
  Result := FItems.ContainsKey( DLLNameToCheck );
end;

function TDLLOCRTestEntry.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TDLLOCRTestEntry.GetIndexByWholeTests: TIndexValueArray;
var
  arr : TIndexValueArray;
begin
  SetLength( arr, self.Count );
  //TOCRTest = ( ocrtNormalWhole = 0, ocrtNormalSliced, ocrtInvertedWhole, ocrtInvertedSliced );
  arr[0].idx := 0;
  arr[0].obj := self.Items[ 'ocrtNormalWhole' ];

  arr[1].idx := 1;
  arr[1].obj := self.Items[ 'ocrtInvertedWhole' ];

  arr[2].idx := 2;
  arr[2].obj := self.Items[ 'ocrtNormalSliced' ];

  arr[3].idx := 3;
  arr[3].obj := self.Items[ 'ocrtInvertedSliced' ];

  Result := arr;
end;

function TDLLOCRTestEntry.GetItem(idx: TIndex): TDLLOCRTest;
begin
  Result := TDLLOCRTest( FItems[ idx ] );
end;

function TDLLOCRTestEntry.LoadFromXML(xmlString: string): boolean;
var
  xmlDoc: IXMLDocument;
  nod, nodTest : IXMLNode;
  attr : IXMLNode;
  testNodeList : IXMLNodeList;
  iTestIdx : integer;
  sTestName, sTestValue : string;
Begin
  try
    self.Clear( true );
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( XMLString ) ) then begin
      nod := xmldoc.DocumentElement;

      attr := nod.Attributes.GetNamedItem('name');
      if ( attr <> nil ) then
        self.Name := attr.Text;
      attr := nod.Attributes.GetNamedItem('descr');
      if ( attr <> nil ) then
        self.Descr := attr.Text;
      attr := nod.Attributes.GetNamedItem('enabled');
      if ( attr <> nil ) then
        self.Enabled := (attr.Text = 'true') or (attr.Text <> '0');
      testNodeList := nod.ChildNodes;
      for iTestIdx := 0 to testNodeList.length - 1 do begin
        nodTest := testNodeList.Item[iTestIdx];
        sTestName := nodTest.NodeName;
        sTestValue := nodTest.Text;
        self.SetOCRTest( sTestName, StrToBool( sTestValue ) );
        //entry.IsActive[ TOCRTest(GetEnumValue(TypeInfo(TOCRTest),sTestName)) ] := StrToBool( sTestValue );
      end;

    end else begin
      raise Exception.Create('Could not load XML.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

procedure TDLLOCRTestEntry.PutItem(idx: TIndex; const Value: TDLLOCRTest);
begin
  FItems[ idx ] := Value;
end;

procedure TDLLOCRTestEntry.setOCRTest(testToCheck: string;
  const Value: boolean);
begin
    if ( self.Exists( testToCheck )) then
    self.Items[ testToCheck ].Enabled := Value
  else
    self.Add( TDLLOCRTest.Create( testToCheck, Value ) );
end;

procedure TDLLOCRTestEntry.setOCRTest(testToCheck: TOCRTest;
  const Value: boolean);
var
  sName : string;
  test : TDLLOCRTest;
begin
  sName := GetEnumName(TypeInfo(TOCRTest), integer(testToCheck));
  if ( self.Exists( sName )) then
    self.Items[ sName ].Enabled := Value
  else
    self.Add( TDLLOCRTest.Create( testToCheck, Value ) );
end;

{ TDLLOCRTest }

constructor TDLLOCRTest.Create;
begin
  FTest := ocrtNormalWhole;
  self.FEnabled := true;
end;

constructor TDLLOCRTest.Create(TestToDefaultTo: TOCRTest);
begin
  Self.Test := TestToDefaultTo;
  self.FEnabled := true;
end;

function TDLLOCRTest.AsXML: string;
var
  sb : TStringBuilder;
  elementName : string;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<' );
    sb.Append( self.Name );
    sb.Append( '>' );
    sb.Append( BoolToStr(self.Enabled) );
    sb.Append( '</' );
    sb.Append( self.Name );
    sb.Append( '>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TDLLOCRTest.Clear;
begin
  self.Name := '';
  self.FEnabled := false;
end;

procedure TDLLOCRTest.CopyFrom(SourceObject: TDWPersist);
begin
  self.Test := TDLLOCRTest(SourceObject).Test;
  self.Enabled := TDLLOCRTest(SourceObject).Enabled;
end;

procedure TDLLOCRTest.CopyTo(TargetObject: TDWPersist);
begin
  TDLLOCRTest(TargetObject).Test := self.Test;
  TDLLOCRTest(TargetObject).Enabled := self.Enabled;
end;

constructor TDLLOCRTest.Create(TestToDefaultTo: TOCRTest;
  const Enabled: boolean);
begin
  Self.FTest := TestToDefaultTo;
  self.FEnabled := Enabled;
end;

function TDLLOCRTest.GetDescr: string;
begin
  Case self.FTest of
    ocrtNormalWhole :
        Result := 'Normal Whole';
    ocrtNormalSliced :
        Result := 'Sliced Normal';
    ocrtInvertedWhole :
        Result := 'Whole Inverted';
    ocrtInvertedSliced :
        Result := 'Sliced Inverted';
  end;
end;

function TDLLOCRTest.GetName: string;
begin
  Result := GetEnumName(TypeInfo(TOCRTest), integer( FTest )) ;
end;

function TDLLOCRTest.Invert: boolean;
begin
  Result := ( FTest in [ocrtInvertedWhole, ocrtInvertedSliced ] );
end;

function TDLLOCRTest.IsWholePageTest: boolean;
begin
  Result := ( FTest in [ocrtInvertedWhole, ocrtNormalWhole ] );
end;

function TDLLOCRTest.LoadFromXML(xmlString: string): boolean;
var
  xmlDoc: IXMLDocument;
  ele: IXMLElement;
  nod : IXMLNode;
  xmlNodeList : IXMLNodeList;
  iImageIdx : integer;
Begin
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( XMLString ) ) then begin
      self.Name := xmldoc.DocumentElement.nodeName;
      xmlNodeList := xmldoc.DocumentElement.ChildNodes;
      for iImageIdx := 0 to xmlNodeList.length - 1 do begin
        nod := xmlNodeList.Item[iImageIdx];
        self.Enabled := StrToBool(nod.Text);
      end;

    end else begin
      raise Exception.Create('Could not load XML.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

procedure TDLLOCRTest.SetName(const Value: string);
begin
  FTest := TOCRTest(GetEnumValue(TypeInfo(TOCRTest),Value));
end;

function TDLLOCRTest.Slice: boolean;
begin
  Result := ( FTest in [ocrtNormalSliced, ocrtInvertedSliced ] );
end;

constructor TDLLOCRTest.Create(TestToDefaultTo: string; const Enabled: boolean);
begin
  self.Name := TestToDefaultTo;
  self.Enabled := Enabled;
end;

END.
