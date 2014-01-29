unit uI2XTemplate;

interface
uses
//MSXML2_TLB,
  SysUtils,
  uStrUtil,
  OmniXML,
  uHashTable,
  Classes,
  JclStrings,
  uImage2XML,
  RegExpr,
  OmniXMLUtils,
  uI2XOCR,
  uI2XConstants,
  GR32;

const
  UNIT_NAME = 'uI2XTemplate';
  NODE_TYPE_MULTI = 'multi';
  NODE_TYPE_SINGLE = 'single';
  NODE_ELEMENT = 1;

  type
  TSearchParm = type string;
  TOffsetCorrectionType = (ocNone = 0, ocFirstChar, ocBoundary, ocOffset);
  TOffsetBoundry = ( obNone = 0, obLeft, obRight, obTop, obBottom );
  TDataType = ( dtString = 0, dtNumeric, dtDateTime );
  TCopyDirection = ( cpOffset, cpRight, cpLeft, cpAbove, cpBelow );
  TXMLImageMode = ( xcNotSpecified, xcNoSpecialFields, xcComplete );
  TIntArray = array of integer;
  IOCRElement = Interface(IInterface)
    function GetX : Integer;
    procedure SetX( val : Integer);
    function GetY : Integer;
    procedure SetY( val : Integer);
    function GetWidth : Integer;
    procedure SetWidth( val : Integer);
    function GetHeight : Integer;
    procedure SetHeight( val : Integer);
    function GetDimX : Integer;
    procedure SetDimX( val : Integer);
    function GetDimY : Integer;
    procedure SetDimY( val : Integer);

    property X : Integer read GetX write SetX;
    property Y : Integer read GetY write SetY;
    property Width : Integer read GetWidth write SetWidth;
    property Height : Integer read GetHeight write SetHeight;
    property DimX : Integer read GetDimX write SetDimX;
    property DimY : Integer read GetDimY write SetDimY;
    function AsXML( rootNodeName : string ) : string;
    procedure Free;
  end;
  TOCRElement = Class(TInterfacedObject, IOCRElement)
  //TOCRElement = Class(TObject)
  private
    FX, FY, FWidth, FHeight, FDimX, FDimY : Integer;
    function GetX : Integer;
    procedure SetX( val : Integer);
    function GetY : Integer;
    procedure SetY( val : Integer);
    function GetWidth : Integer;
    procedure SetWidth( val : Integer);
    function GetHeight : Integer;
    procedure SetHeight( val : Integer);
    function GetDimX : Integer;
    procedure SetDimX( val : Integer);
    function GetDimY : Integer;
    procedure SetDimY( val : Integer);
    function EvalLocationAsRect: TRect;
  published
    property X : Integer read GetX write SetX;
    property Y : Integer read GetY write SetY;
    property Width : Integer read GetWidth write SetWidth;
    property Height : Integer read GetHeight write SetHeight;
    property LocationAsRect : TRect read EvalLocationAsRect;
    property DimX : Integer read GetDimX write SetDimX;
    property DimY : Integer read GetDimY write SetDimY;
    function AsXML( rootNodeName : string = 'ocr_element') : string;
    procedure CopyTo( Target : TOCRElement ); dynamic;
    procedure CopyFrom( Source : TOCRElement ); dynamic;
    procedure Clear; dynamic;
    constructor Create();
    destructor Destroy; override;
  end;
  IDocumentOffsetCorrection = Interface(IInterface)
    function GetOffsetX : Integer;
    procedure SetOffsetX( val : Integer);
    function GetOffsetY : Integer;
    procedure SetOffsetY( val : Integer);
    property OffsetX : Integer read GetOffsetX write SetOffsetX;
    property OffsetY : Integer read GetOffsetY write SetOffsetY;
    function AsXML(rootNodeName : string ) : string;
    procedure Free;
  end;
  TDocumentOffsetCorrection = Class(TInterfacedObject, IDocumentOffsetCorrection)
  //TDocumentOffsetCorrection = Class(TObject)
  private
    FOffsetX, FOffsetY : Integer;
    function GetOffsetX : Integer;
    procedure SetOffsetX( val : Integer);
    function GetOffsetY : Integer;
    procedure SetOffsetY( val : Integer);
  public
    property OffsetX : Integer read GetOffsetX write SetOffsetX;
    property OffsetY : Integer read GetOffsetY write SetOffsetY;
    function AsXML( rootNodeName : string ) : string; virtual;
  published
    constructor Create();
    destructor Destroy; override;
  End;
  TDOCFirstCharacter = Class(TDocumentOffsetCorrection)
    private
      FFirstChar : TOCRElement;
      function GetFirstChar : TOCRElement;
      procedure SetFirstChar( point : TOCRElement);
    public
      property FirstChar : TOCRElement read GetFirstChar write SetFirstChar;
      function AsXML( rootNodeName : string = 'doc_offset_correction' ) : string; override;
    published
      constructor Create();
     destructor Destroy; override;
  End;
  TBoundarySet = Set of TOffsetBoundry;
  TDOCBoundary = Class(TDocumentOffsetCorrection)
    private
      FBoundaries: TBoundarySet;
    public
      property Boundaries : TBoundarySet read FBoundaries write FBoundaries;
      function AsXML( rootNodeName : string  = 'doc_offset_correction' ) : string; override;
      procedure SetBoundary( boundaryString: string );
      destructor Destroy; override;
  End;
  TDOCOffset = Class(TDocumentOffsetCorrection)
  End;

  TObjectChangeNotify = class(TObject)
    private
      FOnChange : TNotifyEvent;
    public
      property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  THashTableI2X = class( THashTable )
    private
      FParent : TObject;
      FOnChange : TNotifyEvent;
      FOnItemChange : TNotifyEvent;
      FForceKeyNameSafe: boolean;
      procedure OnChangeHandler( Sender: TObject );
      function SafeKeyName( const keyName : string ) : string;
    public
      function ContainsKey(const key: string): boolean;
		  procedure Add(const key: string; value: TObjectChangeNotify);
		  procedure Put(const key: string; value: TObjectChangeNotify);
      function Get(const key: string): TObjectChangeNotify;
      procedure Delete(const key: string);
      procedure Clear(); dynamic;
      property Parent : TObject read FParent write FParent;
      property OnChange : TNotifyEvent read FOnChange write FOnChange;
      property ForceKeyNameSafe : boolean read FForceKeyNameSafe write FForceKeyNameSafe;
      constructor Create; overload;
      constructor Create( const initialCapacity : Cardinal ); overload;
      destructor Destroy; override;
  end;

  THashStringTableI2X = class( THashStringTable )
    private
      FOnChange : TNotifyEvent;
    public
      property OnChange : TNotifyEvent read FOnChange write FOnChange;
      constructor Create; overload;
      constructor Create( const initialCapacity : Cardinal ); overload;
      destructor Destroy; override;
  end;

  pCEleNodeData = ^CEleNodeData;
  CEleNodeData = class( TObjectChangeNotify )
    private
      FConfidenceAvg : real;
      Fgroupkey : boolean;
      FgroupOrder : smallint;
      Feekey : boolean;
      FeeKeyOrder : smallint;
      Fx : cardinal;
      Fy : cardinal;
      Fwidth : cardinal;
      FWidthPct : Extended;
      Fheight : cardinal;
      Fid : string;
      FDataType : TDataType;
      Fformat : string;
      FScaler : string;
      FnodeType : string;
      FSubNodes : THashTableI2X;
      FRegionType : TRegionType;
      FTreeViewNodePointer : pointer;
      FGraphicPointer : pointer;
      FParent : TObject;
      FFillColor : Cardinal;
      FNamePathShallow : string;
      procedure setNode(nod: IXMLNode; name, val: string);
      procedure setId(const Value: string);
      function getX() : cardinal;
      function getY() : cardinal;
      function getWidth() : cardinal;
      function getWidthPercent() : Extended;
      function getHeight() : cardinal;
      function getNodeType() : string;

      procedure setFillColor ( val : Cardinal );
      procedure setRegionType ( val : TRegionType );
      procedure setTreeViewNodePointer ( val : pointer );
      procedure setGraphicPointer ( val : pointer );
      procedure setGroupkey ( val : boolean );
      procedure setGroupOrder ( val : smallint );
      procedure setEekey ( val : boolean );
      procedure setEeKeyOrder ( val : smallint );
      procedure setDataType ( val : TDataType );
      procedure setFormat ( val : string );
      procedure setNodeType ( val : string );
      procedure setScaler ( val : string );
      procedure setX ( val : cardinal );
      procedure setY ( val : cardinal );
      procedure setWidth ( val : cardinal );
      procedure setWidthPercent ( val : Extended );
      procedure setHeight ( val : cardinal );
      procedure setConfidenceAvg ( val : real );
      procedure setParent ( val : TObject );
      procedure setSubNodes ( val : THashTableI2X );
      procedure FreeSubNodes;
      function getDataTypeString: string;
      procedure setDataTypeString(const Value: string);
      function getFillColor: Cardinal;
      function getIsFirstChar: boolean;
      function getIsXAxis: boolean;
      function getIsYAxis: boolean;
      function evalNamePath: string;
      function EvalLocationAsRect: TRect;
    public
      property ID : string read Fid write setId;
      property RegionType : TRegionType read FRegionType write setRegionType;
      property TreeViewNodePointer : pointer read FTreeViewNodePointer write setTreeViewNodePointer;
      property GraphicPointer : pointer read FGraphicPointer write setGraphicPointer;
      property Groupkey : boolean read Fgroupkey write setgroupkey;
      property GroupOrder : smallint read FgroupOrder write setgroupOrder;
      property Eekey : boolean read Feekey write seteekey;
      property EeKeyOrder : smallint read FeeKeyOrder write seteeKeyOrder;
      property DataType : TDataType read FDataType write setDataType;
      property DataTypeString : string read getDataTypeString write setDataTypeString;
      property Format : string read Fformat write setformat;
      property NodeType : string read getNodeType;
      property Scaler : string read FScaler write setScaler;
      property X : cardinal read getX write setX;
      property Y : cardinal read getY write setY;
      property Width : cardinal read getWidth write setWidth;
      property WidthPercent : Extended read getWidthPercent write setWidthPercent;
      property Height : cardinal read getHeight write setHeight;
      property SubNodes : THashTableI2X read FSubNodes write setSubNodes;
      property ConfidenceAvg : real read FConfidenceAvg write setConfidenceAvg;
      property Parent : TObject read FParent write setParent;
      property FillColor : Cardinal read getFillColor write setFillColor;
      property isXAxis : boolean read getIsXAxis;
      property isYAxis : boolean read getIsYAxis;
      property isFirstChar : boolean read getIsFirstChar;
      property NamePath : string read evalNamePath;
      property NamePathShallow : string read FNamePathShallow write FNamePathShallow;
      procedure GetElementData( xmlElement: IXMLNode; Deep : boolean = true );
      function AppendElement( nod: IXMLNode; tmplElementType : string = NODE_TYPE_SINGLE)  : IXMLNode;
      procedure UpdateRegion(nod: IXMLNode);
      procedure AssignTo(Dest: TObject); overload;
      procedure AssignTo(Dest: TObject; EnablePointerCopy : boolean ); overload;
      procedure Assign(Source: TObject); overload;
      procedure Assign(Source: TObject; EnablePointerCopy : boolean ); overload;
      procedure Clear;
      function AsString : string;
      function AsXML( const EvalNamePath : boolean = false ): string;
      procedure FromXML( ElementAsXMLString : string );
      constructor Create(); overload;
      constructor Create( Parent : TObject ); overload;
      destructor Destroy; override;
      property LocationAsRect : TRect read EvalLocationAsRect;
  end;

  CTemplate = class(TObjectChangeNotify)
    private
      FOnOffsetChange : TNotifyEvent;
      FXMLImageMode : TXMLImageMode;
      FXMLImageModeCache : TXMLImageMode; //used to store the old value when the use changes
      FData : THashStringTable;
      FFirstElement : THashStringTable;
      FImageProcessing : TStringList2;
      FImageProcessingUpsize : TStringList2;
      FElements : THashTableI2X;
      FDocumentOffsetCorrectionType : TOffsetCorrectionType;
      FDocumentOffsetCorrection : TDocumentOffsetCorrection;
      FTemplateName: string;
      FTemplateFileName: TFileName;
      FRootNodeName: string;
      FOnInnerChange : TNotifyEvent;
      FXOffset : integer;
      FYOffset : integer;
      FIsOffset : boolean;
      FBoundary : TRect;
      FFirstChar : TI2XOCRItem;
      procedure OnInnerChange( sender : TObject );
      //function GetDocumentOffsetCorrection : TDocumentOffsetCorrection;
      //procedure SetDocumentOffsetCorrection( val : TDocumentOffsetCorrection );
      function xmlN(const str: string): string;
      function getImageProcessingInstructionsAsString() : string;
      procedure setImageProcessingInstructionsAsString( imgProcStr : string );
      function getImageProcessingInstructionsUpsizeAsString() : string;
      procedure setImageProcessingInstructionsUpsizeAsString( imgProcStr : string );
      procedure FreeHashStringElements(ht: THashTable);
      function getNodeValue(xmlDoc: IXMLDocument; nodeName,
        defaultValue: string): string;
      function getNodeValueInt(xmlDoc: IXMLDocument; nodeName: string;
        defaultValue: integer): integer;
      function IsLike(const stringToSearch: string;
        const searchParm: TSearchParm): boolean;
      procedure Load(const fileNameOrXML : string );
      procedure ElementsHashTableToDOM(FXMLTemplate: IXMLDocument; root: IXMLNode; ht: THashTable;
        nodeFilter: TSearchParm);
      function getImageFileName: TFileName;
      procedure setImageFileName(const Value: TFileName);
      function getTemplateFileName: TFileName;
      procedure setTemplateFileName(const Value: TFileName);
      procedure Offset(ht : THashTable; const x, y: integer); overload;
      function GetIsOffset: boolean;
      function getXMLImageMode: TXMLImageMode;
      procedure setXMLImageMode(const Value: TXMLImageMode);
      function getElement( ht: THashTable; const IntegerPointer: cardinal): CEleNodeData; overload;
    public
      property Boundary : TRect read FBoundary write FBoundary;
      property FirstChar : TI2XOCRItem read FFirstChar write FFirstChar;
      property XMLImageMode : TXMLImageMode read getXMLImageMode write setXMLImageMode;
      property OnOffsetChange : TNotifyEvent read FOnOffsetChange write FOnOffsetChange;
      function FillWithRegionSlices( oOCRResults: TI2XOCRResults ): boolean;
      function RenameIsValid(const eleToCheckBeforeRename: CEleNodeData; renamedTo: string): boolean;
      procedure saveAsXML(const fileName: TFileName); overload;
      procedure saveAsXML(const fileName: TFileName; const elementSearchParm : string ); overload;
      procedure setData(const varname: string; const val : string); overload;
      procedure setData(const varname: string; const val : integer); overload;
      procedure setData(const varname: string; const val : extended); overload;
      function getData(const varname : string) : string;
      procedure setFirstElement(const varname, val : string); overload;
      procedure setFirstElement(const varname : string; val : integer); overload;
      function getFirstElement(const varname : string) : string;
      function setElement(eleData: CEleNodeData) : CEleNodeData; overload;
      function DeleteElement(eleData: CEleNodeData) : boolean;
      function setElement(const elementName: string; eleData: CEleNodeData = nil) : CEleNodeData;  overload;
      function getElement(const elementName: string; forceExist: boolean = true): CEleNodeData; overload;
      function getElement( const ht : THashTable; const x, y, width, height : cardinal ): CEleNodeData; overload;
      function getElement( const ht : THashTable; const x, y, width, height, pixelTolerance : cardinal ): CEleNodeData; overload;
      function getElement( const x, y, width, height : cardinal ): CEleNodeData; overload;
      function getElement( const IntegerPointer : cardinal ): CEleNodeData; overload;
      function MoveElement( const srcElement : CEleNodeData; ParentEleData: CEleNodeData = nil; const ForceMove : boolean = false ): CEleNodeData;
      procedure LoadFromFile(const fileName: TFileName);
      procedure LoadFromXML(const xmlToLoad: string);
      function genElementXML(FXMLTemplate: IXMLDocument; eleData: CEleNodeData): IXMLNode;
      property ImageProcessingInstructionString : string
          read getImageProcessingInstructionsAsString
          write setImageProcessingInstructionsAsString;
      property ImageProcessingInstructionUpsizeString : string
          read getImageProcessingInstructionsUpsizeAsString
          write setImageProcessingInstructionsUpsizeAsString;
      property TemplateName: string read FTemplateName write FTemplateName;
      property TemplateFileName: TFileName read getTemplateFileName write setTemplateFileName;
      //property TemplateFileName: TFileName read FTemplateFileName write FTemplateFileName;
      property ImageFileName: TFileName read getImageFileName write setImageFileName;
      property RootNodeName: string read FRootNodeName write FRootNodeName;
      property XOffset : integer read FXOffset;
      property YOffset : integer read FYOffset;
      property IsOffset : boolean read GetIsOffset;
      procedure OffsetClear();
      property Elements: THashTableI2X read FElements write FElements;
//      property DocumentOffsetCorrection : TDocumentOffsetCorrection
//        read GetDocumentOffsetCorrection write SetDocumentOffsetCorrection;
      property DocumentOffsetCorrectionType : TOffsetCorrectionType
        read FDocumentOffsetCorrectionType write FDocumentOffsetCorrectionType;
      procedure Clear;
      procedure ClearData;
      procedure ClearElements;
      procedure SetDefaultValues;
      function asXML( const XMLImageMode : TXMLImageMode = xcNotSpecified ): string; overload;
      function asXML( const fileName : TFileName; const XMLImageMode : TXMLImageMode = xcNotSpecified ): string; overload;
      function asXML( const fileName : TFileName; nodeFilter : TSearchParm; const XMLImageMode : TXMLImageMode = xcNotSpecified ): string; overload;
      constructor Create();
      destructor Destroy; override;
      function Clone(var NewObject : CTemplate) : boolean; overload;
      function Clone(var NewObject : CTemplate; nodeFilter : TSearchParm ) : boolean; overload;
      procedure Offset(const x, y: integer); overload;
  end;

function CInt( s : string ) : int64;
function StringAsXMLNode( var xmlDocWork : IXMLDocument; str : string ) : IXMLNode;
procedure EleKeyListXSort( ht : THashTable; list : TKeyList );
procedure EleKeyListYSort( ht : THashTable; list : TKeyList );
function strToBool(str : string) : boolean;
function boolToStr(b : boolean) : string;

implementation

procedure EleKeyListWidthSort( ht : THashTable; list : TKeyList );
var
  i, j: Integer;
  temp: string;
Begin
  // bubble sort
  for i := 0 to length(list) - 1 do begin
    for j := 0 to ( length( list ) - 1 ) - i do begin
      // Condition to handle i=0 & j = 9. j+1 tries to access x[10] which
      // is not there in zero based array
      if ( j + 1 = length(list) ) then
        continue;
      if ( CEleNodeData( ht.Get( list[j] ) ).Width >
              CEleNodeData( ht.Get( list[j+1] ) ).Width ) then begin
      //if ( list[j] > list[j+1] ) then begin
        temp := list[j];
        list[j]   := list[j+1];
        list[j+1] := temp;
      end; // endif
    end; // endwhile
  end; // endwhile
End;

procedure EleKeyListXSort( ht : THashTable; list : TKeyList );
var
  i, j: Integer;
  temp: string;
Begin
  // bubble sort
  for i := 0 to length(list) - 1 do begin
    for j := 0 to ( length( list ) - 1 ) - i do begin
      // Condition to handle i=0 & j = 9. j+1 tries to access x[10] which
      // is not there in zero based array
      if ( j + 1 = length(list) ) then
        continue;
      if ( CEleNodeData( ht.Get( list[j] ) ).X >
              CEleNodeData( ht.Get( list[j+1] ) ).X ) then begin
      //if ( list[j] > list[j+1] ) then begin
        temp := list[j];
        list[j]   := list[j+1];
        list[j+1] := temp;
      end; // endif
    end; // endwhile
  end; // endwhile
End;

procedure EleKeyListYSort( ht : THashTable; list : TKeyList );
var
  i, j: Integer;
  temp: string;
Begin
  // bubble sort
  for i := 0 to length(list) - 1 do begin
    for j := 0 to ( length( list ) - 1 ) - i do begin
      // Condition to handle i=0 & j = 9. j+1 tries to access x[10] which
      // is not there in zero based array
      if ( j + 1 = length(list) ) then
        continue;
      if ( CEleNodeData( ht.Get( list[j] ) ).Y >
              CEleNodeData( ht.Get( list[j+1] ) ).Y ) then begin
      //if ( list[j] > list[j+1] ) then begin
        temp := list[j];
        list[j]   := list[j+1];
        list[j+1] := temp;
      end; // endif
    end; // endwhile
  end; // endwhile
End;

function CInt( s : string ) : int64;
var
  c : Int64;
Begin
  if TryStrToInt64( s, c ) then
    result := c
  else
    result := 0;
End;

function StringAsXMLNode(var xmlDocWork : IXMLDocument; str : string) : IXMLNode;
begin
  if ( xmlDocWork = nil )  then
    xmlDocWork := CreateXMLDoc;
  xmlDocWork.loadXML( str );
  Result := xmlDocWork.documentElement.cloneNode( true );
end;

function strToBool(str : string) : boolean;
begin
  if (( str = 'Y' ) or ( str = 'y' )) then
    result := true
  else
    result := false;
end;

function boolToStr(b : boolean) : string;
begin
  if (b) then
    result := 'Y'
  else
    result := 'N';
end;

{ TOCRElement }

function TOCRElement.AsXML( rootNodeName : string ): string;
var
  sarr : TStringBuilder;
begin
  try
    sarr := TStringBuilder.Create;
    sarr.Append('<');
    sarr.Append(rootNodeName);
    sarr.Append('>');

    sarr.Append('<x>');
    sarr.Append( IntToStr(self.FX) );
    sarr.Append('</x>');

    sarr.Append('<y>');
    sarr.Append( IntToStr(self.FY) );
    sarr.Append('</y>');

    sarr.Append('<width>');
    sarr.Append( IntToStr(self.FWidth) );
    sarr.Append('</width>');

    sarr.Append('<height>');
    sarr.Append( IntToStr(self.FHeight) );
    sarr.Append('</height>');

    sarr.Append('<x_dim_min>');
    sarr.Append( IntToStr(self.FDimX) );
    sarr.Append('</x_dim_min>');

    sarr.Append('<y_dim_min>');
    sarr.Append( IntToStr(self.FDimY) );
    sarr.Append('</y_dim_min>');

    sarr.Append('</');
    sarr.Append(rootNodeName);
    sarr.Append('>');

    Result := sarr.ToString();
  finally
    FreeAndNil( sarr );
  end;
end;

procedure TOCRElement.Clear;
begin
  self.FX := 0;
  self.FY := 0;
  self.FWidth := 0;
  self.FHeight := 0;
  Self.FDimX := 0;
  Self.DimY := 0;
end;

procedure TOCRElement.CopyFrom(Source: TOCRElement);
begin
  with self do begin
    X := Source.X;
    Y := Source.Y;
    Width := Source.Width;
    Height := Source.Height;
    DimX := Source.DimX;
    DimY := Source.DimY;
  end;
end;

procedure TOCRElement.CopyTo( Target : TOCRElement);
begin
  with self do begin
    Target.X := X;
    Target.Y := Y;
    Target.Width := Width;
    Target.Height := Height;
    Target.DimX := DimX;
    Target.DimY := DimY;
  end;
end;

constructor TOCRElement.Create;
begin
  self.Clear;
end;

destructor TOCRElement.Destroy;
begin

  inherited;
end;

function TOCRElement.EvalLocationAsRect: TRect;
var
  res : TRect;
begin
  res.Top := self.Y;
  res.Left := self.X;
  res.Bottom := self.Y + self.Height;
  res.Right := self.X + self.Width;
  Result := res;
end;

function TOCRElement.GetDimX: Integer;
begin
  Result := self.FDimX;
end;

function TOCRElement.GetDimY: Integer;
begin
  Result := self.FDimY;
end;

function TOCRElement.GetHeight: Integer;
begin
  Result := self.FHeight;
end;

function TOCRElement.GetWidth: Integer;
begin
  Result := self.FWidth;
end;

function TOCRElement.GetX: Integer;
begin
  Result := self.FX;
end;

function TOCRElement.GetY: Integer;
begin
  Result := self.FY;
end;

procedure TOCRElement.SetDimX(val: Integer);
begin
  self.FDimX := val;
end;

procedure TOCRElement.SetDimY(val: Integer);
begin
  self.FDimY := val;
end;

procedure TOCRElement.SetHeight(val: Integer);
begin
  self.FHeight := val;
end;

procedure TOCRElement.SetWidth(val: Integer);
begin
  self.FWidth := val;
end;

procedure TOCRElement.SetX(val: Integer);
begin
  self.FX := val;
end;

procedure TOCRElement.SetY(val: Integer);
begin
  self.FY := val;
end;

{ TDOCFirstCharacter }

function TDOCFirstCharacter.AsXML( rootNodeName : string ) : string;
var
  sarr : TStringBuilder;
begin
  try
    sarr := TStringBuilder.Create;
    sarr.Append('<');
    sarr.Append(rootNodeName);
    sarr.Append(' type=');
    sarr.Append('"FIRSTCHAR">');

    sarr.Append( self.FirstChar.AsXML('first_element') );

    sarr.Append('<x_offset>');
    sarr.Append( IntToStr(self.OffsetX) );
    sarr.Append('</x_offset>');

    sarr.Append('<y_offset>');
    sarr.Append( IntToStr(self.OffsetY) );
    sarr.Append('</y_offset>');

    sarr.Append('</');
    sarr.Append(rootNodeName);
    sarr.Append('>');

    Result := sarr.ToString();
  finally
    FreeAndNil( sarr );
  end;
end;

constructor TDOCFirstCharacter.Create;
begin
  self.FFirstChar := TOCRElement.Create();
  self.FOffsetX := 0;
  self.FOffsetY := 0;
end;

destructor TDOCFirstCharacter.Destroy;
begin
  if ( FFirstChar <> nil ) then FreeAndNil( FFirstChar );
  inherited;
end;

function TDOCFirstCharacter.GetFirstChar: TOCRElement;
begin
  Result := self.FFirstChar;
end;

procedure TDOCFirstCharacter.SetFirstChar(point: TOCRElement);
begin
  self.FFirstChar := point;
end;

{ TDOCBoundary }

function TDOCBoundary.AsXML(rootNodeName: string): string;
var
  sarr : TStringBuilder;
begin
  try
    sarr := TStringBuilder.Create;
    sarr.Append('<');
    sarr.Append(rootNodeName);
    sarr.Append(' type=');
    sarr.Append('"BOUNDARY">');

    if ( obTop in self.FBoundaries ) then
      sarr.Append('<boundary>top</boundary>');
    if ( obBottom in self.FBoundaries ) then
      sarr.Append('<boundary>bottom</boundary>');
    if ( obLeft in self.FBoundaries ) then
      sarr.Append('<boundary>left</boundary>');
    if ( obRight in self.FBoundaries ) then
      sarr.Append('<boundary>right</boundary>');

    sarr.Append('<x_offset>');
    sarr.Append( IntToStr(self.OffsetX) );
    sarr.Append('</x_offset>');

    sarr.Append('<y_offset>');
    sarr.Append( IntToStr(self.OffsetY) );
    sarr.Append('</y_offset>');

    sarr.Append('</');
    sarr.Append(rootNodeName);
    sarr.Append('>');

    Result := sarr.ToString();
  finally
    FreeAndNil( sarr );
  end;
end;

destructor TDOCBoundary.Destroy;
begin
  inherited;
end;

procedure TDOCBoundary.SetBoundary(boundaryString: string);
begin
  if ( LowerCase(boundaryString) = 'top' ) then
    Include( self.FBoundaries, obTop );
  if ( LowerCase(boundaryString) = 'bottom' ) then
    Include( self.FBoundaries, obBottom );
  if ( LowerCase(boundaryString) = 'left' ) then
    Include( self.FBoundaries, obLeft );
  if ( LowerCase(boundaryString) = 'right' ) then
    Include( self.FBoundaries, obRight );
end;

{ TDocumentOffsetCorrection }

function TDocumentOffsetCorrection.AsXML(rootNodeName: string): string;
var
  sarr : TStringBuilder;
begin
  try
    sarr := TStringBuilder.Create();
    sarr.Append('<');
    sarr.Append(rootNodeName);
    sarr.Append(' type=');
    sarr.Append('"OFFSET">');

    sarr.Append('<x_offset>');
    sarr.Append( IntToStr(self.OffsetX) );
    sarr.Append('</x_offset>');

    sarr.Append('<y_offset>');
    sarr.Append( IntToStr(self.OffsetY) );
    sarr.Append('</y_offset>');

    sarr.Append('</');
    sarr.Append(rootNodeName);
    sarr.Append('>');

    Result := sarr.ToString();
  finally
    FreeAndNil( sarr );
  end;
end;

constructor TDocumentOffsetCorrection.Create;
begin
  self.OffsetX := 0;
  self.OffsetY := 0;
end;

destructor TDocumentOffsetCorrection.Destroy;
begin
  inherited;
end;

function TDocumentOffsetCorrection.GetOffsetX: Integer;
begin
  result := self.FOffsetX;
end;

function TDocumentOffsetCorrection.GetOffsetY: Integer;
begin
  result := self.FOffsetY;
end;

procedure TDocumentOffsetCorrection.SetOffsetX(val: Integer);
begin
  self.FOffsetX := val;
end;

procedure TDocumentOffsetCorrection.SetOffsetY(val: Integer);
begin
  self.FOffsetY := val;
end;

{* CEleNodeData *}

constructor CEleNodeData.Create;
begin
  FParent := nil;
  Clear;
end;

procedure CEleNodeData.Clear;
begin
    Fgroupkey := false;
    self.FgroupOrder := 0;
    Feekey := false;
    FeeKeyOrder := 0;
    FDataType := dtString;
    Fx := 0;
    Fy := 0;
    FConfidenceAvg := 0;
    Fwidth := 0;
    Fheight := 0;
    Initialize( Fid );
    Initialize( Fformat );
    Initialize( FScaler );
    FFillColor := 0;
    //Initialize( FNodType );
    if  (( FSubNodes <> nil ) and ( FSubNodes.count > 0 )) then
      FreeSubNodes;
    if ( FSubNodes = nil ) then
      FSubNodes := THashTableI2X.Create( 100 );
    self.FTreeViewNodePointer := nil;
    self.GraphicPointer := nil;
    self.FRegionType := rtStandard;
end;

constructor CEleNodeData.Create(Parent: TObject);
begin
  self.Create();
  self.FParent := Parent;
end;

destructor CEleNodeData.Destroy;
begin
  FreeSubNodes;
  inherited;
end;

function CEleNodeData.EvalLocationAsRect: TRect;
var
  res : TRect;
begin
  res.Left := self.X;
  res.Top := self.Y;
  res.Right := self.X + self.Width;
  res.Bottom := self.Y + self.Height;
  Result := res;
end;

function CEleNodeData.evalNamePath: string;
var
  sName : string;
  oParent : TObject;
begin
  sName := self.ID;
  oParent := self.FParent;
  while oParent <> nil do begin
    sName := CEleNodeData(oParent).ID + '.' + sName;
    oParent := CEleNodeData(oParent).FParent;
  end;
  Result := sName;
end;

Procedure CEleNodeData.FreeSubNodes();
var
  i : cardinal;
  arrKeyList : TKeyList;
  ele : CEleNodeData;
  sKeyName : string;
Begin
  try
    Initialize( sKeyName );
    try
      {*
      if ( FSubNodes.Count > 0) then begin
        arrKeyList := FSubNodes.Keys;
        for i := 0 to Length(arrKeyList) - 1 do begin
          sKeyName := arrKeyList[i];
          ele := CEleNodeData( FSubNodes.Get( arrKeyList[i] ));
          ele.Free;
        end;
      end;
      *}
      if ( FSubNodes <> nil ) then FSubNodes.Free;
    finally
    end;
  except
     on E : Exception do
     begin
       dbg('CEleNodeData.FreeSubNodes() Key =' + sKeyName);
     end;
  end;
End;

procedure CEleNodeData.setConfidenceAvg(val: real);
begin
  if (( val <> self.FConfidenceAvg ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FConfidenceAvg := val;
end;

procedure CEleNodeData.setTreeViewNodePointer(val: pointer);
begin
  if (( val <> self.FTreeViewNodePointer ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FTreeViewNodePointer := val;
end;

procedure CEleNodeData.setDataType(val: TDataType);
begin
  if (( val <> self.FDataType ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FDataType := val;
end;

procedure CEleNodeData.setDataTypeString(const Value: string);
var
  sValueLower : string;
begin
  //TDataType = ( dtString = 0, dtNumeric, dtDateTime );
  sValueLower := LowerCase(Value);
  if (( sValueLower = 'dt' ) or ( sValueLower = 'datetime' )) then
    DataType := dtDateTime
  else if (( sValueLower = 'n' ) or ( sValueLower = 'numeric' )) then
    DataType := dtNumeric
  else
    DataType := dtString;
end;

procedure CEleNodeData.setEekey(val: boolean);
begin
  if (( val <> self.Feekey ) and Assigned(FOnChange)) then FOnChange(Self);
  self.Feekey:= val;
end;

procedure CEleNodeData.setEeKeyOrder(val: smallint);
begin
  if (( val <> self.FeeKeyOrder ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FeeKeyOrder := val;
end;

procedure CEleNodeData.setFillColor(val: Cardinal);
begin
  if (( val <> self.FFillColor ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FFillColor := val;
end;

procedure CEleNodeData.setFormat(val: string);
begin
  if (( val <> self.Fformat ) and Assigned(FOnChange)) then FOnChange(Self);
  self.Fformat := val;
end;

procedure CEleNodeData.setGraphicPointer(val: pointer);
begin
  if (( val <> self.FGraphicPointer ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FGraphicPointer := val;
end;

procedure CEleNodeData.setGroupkey(val: boolean);
begin
  if (( val <> self.Fgroupkey ) and Assigned(FOnChange)) then FOnChange(Self);
  self.Fgroupkey := val;
end;

procedure CEleNodeData.setGroupOrder(val: smallint);
begin
  if (( val <> self.FgroupOrder ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FgroupOrder := val;
end;

procedure CEleNodeData.setHeight(val: cardinal);
Var
  i, cOldHeight, cHeightLeft, cNewHeight : cardinal;
  PctChg : Extended;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
  bChanged : boolean;
Begin
  bChanged := false;
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    bChanged := ( val <> self.FHeight );
    self.FHeight := val;
    if ( bChanged and Assigned(FOnChange)) then FOnChange(Self);
  end else begin
    cOldHeight := self.Height;
    if ( val <> cOldHeight ) then begin
      arrKeyList := self.SubNodes.Keys;
      EleKeyListYSort(self.SubNodes, arrKeyList);
      cHeightLeft := val;

      PctChg := val / cOldHeight;
      cNewHeight := Round(cOldHeight * PctChg);
      for i := 0 to Length(arrKeyList) - 1 do begin
        cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
        //if ( i < ( Length(arrKeyList) - 1 ) ) then begin
        //  cNewHeight := Round(cNode.Height * PctChg);
        //  Dec( cHeightLeft, cNewHeight );
        if (( not bChanged ) and (cNode.Height <> cNewHeight)) then
          bChanged := true;
        if ( cNode.Height <> cNewHeight ) then
          cNode.Height := cNewHeight;
        //end else begin
        //  cNode.Height := cHeightLeft;
        //end;
      end;
      if ( bChanged and Assigned(FOnChange)) then FOnChange(Self);
    end;
  end;
End;
//begin
//  if (( val <> self.Fheight ) and Assigned(FOnChange)) then FOnChange(Self);
//  self.Fheight := val;
//end;

procedure CEleNodeData.setId(const Value: string);
var
  sID : string;
begin
  sID := Value;
  sID := StringReplace( sID, ' ', '_', [rfReplaceAll]);
  sID := StringReplace( sID, '.', '_', [rfReplaceAll]);
  if ( sID <> self.Fid ) then begin
    if ( Assigned(FOnChange) ) then
      FOnChange(Self);
    self.Fid := sID;
  end;
end;

procedure CEleNodeData.setRegionType( val: TRegionType );
begin
  if (( val <> self.FRegionType ) and Assigned(FOnChange)) then FOnChange(Self);
  FRegionType := val;
end;

procedure CEleNodeData.getElementData(xmlElement: IXMLNode; Deep : boolean );
var
  nod, nodSub : IXMLNode;
  ele : CEleNodeData;
  sFieldName, sValue : string;
begin
  try
    self.Clear;
    nod := xmlElement.firstChild;
      while ( nod <> nil ) do begin
        sFieldName := nod.nodeName;
        sValue := nod.text;
        if ( sFieldName = 'id' ) then
          self.Fid := sValue
        else if ( sFieldName = 'NamePath' ) then
          self.FNamePathShallow := sValue
        else if ( sFieldName = 'group_key' ) then
          self.Fgroupkey := strToBool(sValue)
        else if ( sFieldName = 'group_key_order' ) then
          self.FgroupOrder := StrToInt(sValue)
        else if ( sFieldName = 'ee_key' ) then
          self.Feekey := strToBool(sValue)
        else if ( sFieldName = 'ee_key_order' ) then
          self.FeeKeyOrder := StrToInt(sValue)
        else if ( sFieldName = 'datatype' ) then
          self.DataTypeString := sValue
        else if ( sFieldName = 'format' ) then
          self.format := sValue
        else if ( sFieldName = 'x' ) then
          self.x := StrToInt(sValue)
        else if ( sFieldName = 'y' ) then
          self.y := StrToInt(sValue)
        else if ( sFieldName = 'width' ) then
          self.width := StrToInt(sValue)
        else if ( sFieldName = 'height' ) then
          self.height := StrToInt(sValue)
        else if ( sFieldName = 'scaler' ) then
          self.Scaler := sValue
        else if ( sFieldName = 'ConfidenceAvg' ) then
          self.ConfidenceAvg := StrToInt( sValue )
        else if ( sFieldName = 'Parent' ) then
          self.Parent := Pointer( (StrToInt( sValue )) )
        else if ( sFieldName = 'RegionType' ) then
          self.RegionType := TRegionType( StrToInt( sValue ))
        else if ( sFieldName = 'TreeViewNodePointer' ) then
          self.TreeViewNodePointer := Pointer( StrToInt( sValue ) )
        else if ( sFieldName = 'GraphicPointer' ) then
          self.GraphicPointer := Pointer( StrToInt( sValue ) )
        else if ( sFieldName = 'FillColor' ) then
          self.FillColor := StrToInt( sValue )
        else if (( Deep ) and ( sFieldName = 'elements' )) then begin
          nodSub := nod.firstChild;
          while ( nodSub <> nil ) do begin
            ele := CEleNodeData.Create;
            ele.GetElementData( nodSub );
            nodSub := nodSub.nextSibling;
            self.SubNodes.Add( ele.ID, ele );
          end;
        end;

        nod := nod.nextSibling;
      end;
  finally
  end;
End;

function CEleNodeData.getFillColor: Cardinal;
Begin
  if ( FFillColor = 0 ) then begin
    if ( self.NodeType = NODE_TYPE_SINGLE ) then
      Result := clBlue32
    else if ( self.NodeType = NODE_TYPE_MULTI ) then
      Result := clGreen32
  end else begin
    Result := FFillColor;
  end;
End;

function CEleNodeData.getHeight: cardinal;
Var
  i, cTop, cBottom : cardinal;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
Begin
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    Result := self.Fheight;
  end else begin
{Need to get the highest Top and the Lowest bottom,  the difference of these will
 yield the hieght of this multi element}
    cTop := 9999999;
    cBottom := 0;
    arrKeyList := self.SubNodes.Keys;
    for i := 0 to Length(arrKeyList) - 1 do begin
      cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
      if ( cNode.Y < cTop ) then
        cTop := cNode.Y;
      if (  ( cNode.Y + cNode.height ) > cBottom ) then
        cBottom := ( cNode.Y + cNode.height );
    end;
    if (( cBottom - cTop ) < 0 ) then
      raise Exception.Create( 'Height of multi element (id=' + self.id + ') was less than 0 (' + IntToStr( cBottom - cTop ) + ').' )
    else
      Result := cBottom - cTop;
  end;
End;

function CEleNodeData.getIsFirstChar: boolean;
begin
  Result := ( self.Fid = STANDARD_REGION_FIRST_CHAR );
end;

function CEleNodeData.getIsXAxis: boolean;
begin
  Result := ( self.Fid = STANDARD_REGION_X_AXIS );
end;

function CEleNodeData.getIsYAxis: boolean;
begin
  Result := ( self.Fid = STANDARD_REGION_Y_AXIS );
end;

function CEleNodeData.getNodeType: string;
begin
  if ( self.FSubNodes.Count = 0 )  then
    Result := NODE_TYPE_SINGLE
  else if ( self.FSubNodes.Count > 0 ) then
    Result := NODE_TYPE_MULTI;
end;

function CEleNodeData.getWidth: cardinal;
Var
  i, cLeft, cRight : cardinal;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
Begin
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    Result := self.Fwidth;
  end else begin
{ Need to get the left most LEFT and the right most RIGHT and subtract the difference
 in order to obtain the proper width of all the child elements }
    cLeft := 9999999;
    cRight := 0;
    arrKeyList := self.SubNodes.Keys;
    for i := 0 to Length(arrKeyList) - 1 do begin
      cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
      if ( cNode.X < cLeft ) then
        cLeft := cNode.X;
      if (  ( cNode.X + cNode.Width ) > cRight ) then
        cRight := ( cNode.X + cNode.Width );
    end;
    if (( cRight - cLeft ) < 0 ) then
      raise Exception.Create( 'Width of multi element (id=' + self.id + ') was less than 0 (' + IntToStr( cRight - cLeft ) + ').' )
    else
      Result := cRight - cLeft;
  end;
End;

function CEleNodeData.getWidthPercent: Extended;
begin
  Result := self.FWidthPct;
end;

function CEleNodeData.getX: cardinal;
Var
  i, cX : cardinal;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
Begin
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    Result := self.Fx;
  end else begin
//Need to get the left most X
    arrKeyList := self.SubNodes.Keys;
    cX := 99999999;
    for i := 0 to Length(arrKeyList) - 1 do begin
      cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
      if ( cNode.x < cX ) then
        cX := cNode.x;
    end;
    Result := cX;
  end;
End;

function CEleNodeData.getY: cardinal;
Var
  i, cY : cardinal;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
Begin
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    Result := self.Fy;
  end else begin
//Need to get the top most Y
    cY := 9999999;
    arrKeyList := self.SubNodes.Keys;
    for i := 0 to Length(arrKeyList) - 1 do begin
      cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
      if ( cNode.y < cY ) then
        cY := cNode.y;
    end;
    Result := cY;
  end;
End;

procedure CEleNodeData.AssignTo(Dest: TObject; EnablePointerCopy: boolean);
var
  odest : CEleNodeData;
begin
  odest := (Dest as CEleNodeData);
  odest.id := self.Fid;
  odest.Fgroupkey := self.Fgroupkey;
  odest.FgroupOrder := self.FgroupOrder;
  odest.Feekey := self.Feekey ;
  odest.FeeKeyOrder := self.FeeKeyOrder ;
  odest.FDataType  := self.FDataType;
  odest.Fformat := self.Fformat;
  //odest.FNodType := self.FNodType;
  odest.FScaler := self.FScaler;
  odest.Fx := self.Fx;
  odest.Fy := self.Fy;
  odest.Fwidth := self.Fwidth ;
  odest.Fheight := self.Fheight;
  odest.FConfidenceAvg := self.FConfidenceAvg;
  odest.FRegionType := self.FRegionType;
  if ( EnablePointerCopy ) then begin
    odest.FTreeViewNodePointer := self.FTreeViewNodePointer;
    odest.FGraphicPointer := self.FGraphicPointer;
    odest.FParent := self.FParent;
  end;
  odest.FFillColor := self.FFillColor;
end;

procedure CEleNodeData.AssignTo(Dest: TObject);
Begin
  self.AssignTo( Dest, true );
End;

procedure CEleNodeData.Assign(Source: TObject; EnablePointerCopy: boolean);
var
  oSource : CEleNodeData;
begin
  oSource := (Source as CEleNodeData);
  self.Fid := oSource.id;
  self.Fgroupkey := oSource.Fgroupkey;
  self.FgroupOrder := oSource.FgroupOrder;
  self.Feekey := oSource.Feekey;
  self.FeeKeyOrder := oSource.FeeKeyOrder;
  self.FDataType := oSource.FDataType;
  self.Fformat := oSource.Fformat;
  //self.FNodType := oSource.FNodType;
  self.FScaler := oSource.FScaler;
  self.Fx := oSource.Fx;
  self.Fy := oSource.Fy;
  self.Fwidth := oSource.Fwidth;
  self.Fheight := oSource.Fheight;
  self.FConfidenceAvg := oSource.FConfidenceAvg;
  self.FRegionType := oSource.FRegionType;
  if ( EnablePointerCopy ) then begin
    self.FTreeViewNodePointer := oSource.FTreeViewNodePointer;
    self.FGraphicPointer := oSource.FGraphicPointer;
    self.FParent := oSource.FParent;
  end;
  self.FFillColor := oSource.FFillColor;
end;

procedure CEleNodeData.Assign(Source: TObject);
begin
  self.Assign( Source, true );
end;

function CEleNodeData.asString: string;
begin
  result := 'id=' + self.Fid + '  datatype=' + self.DataTypeString +
             '  FNodType=' + self.nodeType +
             ' X=' + IntToStr(self.X) + ' Y=' + IntToStr(self.Y) +
             ' W=' + IntToStr(self.Width) + ' H=' + IntToStr(self.Height) +
             ' avgC=' + FloatToStr(self.FConfidenceAvg) ;
End;

procedure CEleNodeData.setNode(nod: IXMLNode; name, val : string );
var
  vnod: IXMLNode;
  ele : IXMLElement;
Begin
  vnod := nod.selectSingleNode( name );
  if ( vnod = nil ) then begin
    ele := nod.ownerDocument.createElement( name );
    ele.text := val;
    nod.appendChild( ele );
  end else
    vnod.text := val;
End;

procedure CEleNodeData.setNodeType(val: string);
begin
  if (( val <> self.FNodeType ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FNodeType := val;
end;

procedure CEleNodeData.setParent(val: TObject);
begin
  if (( val <> self.FParent ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FParent := val;
end;

procedure CEleNodeData.setScaler(val: string);
begin
  if (( val <> self.FScaler ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FScaler := val;
end;

procedure CEleNodeData.setSubNodes(val: THashTableI2X);
begin
  if (( val <> self.FSubNodes ) and Assigned(FOnChange)) then FOnChange(Self);
  self.FSubNodes := val;
end;

procedure CEleNodeData.setWidth( val : cardinal );
Var
  i, iNodes, cOldWidth : cardinal;
  nNewWidth, nWidthLeft, nDiff : integer;
  PctChg : Extended;
  arrKeyList, arrKeyListXSort : TKeyList;
  cNode, cEle : CEleNodeData;
  bChanged, bFound : boolean;
Begin
  bChanged := false;
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    bChanged := ( val <> self.FWidth );
    self.FWidth := val;
    if ( bChanged and Assigned(FOnChange)) then FOnChange(Self);
  end else begin
    arrKeyList := self.SubNodes.Keys;
    arrKeyListXSort := self.SubNodes.Keys;
    EleKeyListXSort(self.SubNodes, arrKeyListXSort);
    EleKeyListWidthSort( self.SubNodes, arrKeyList );
    cOldWidth := self.Width;

    nWidthLeft := val;
    PctChg := val / cOldWidth;
    if ( val <> cOldWidth ) then begin
      //dbg('CEleNodeData.setWidth(): cOldWidth=' + IntToStr(cOldWidth) + ' val=' + IntToStr(val) + '   PctChg=' + FloatToStr(PctChg) );
      for i := 0 to Length(arrKeyList) - 1 do begin
        cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
        if ( i < ( Length(arrKeyList) - 1 ) ) then begin
          nNewWidth := Trunc( cNode.Width * PctChg );
          //nNewWidth := Round(cNode.Width * PctChg);
          //dbg( '  id=' + cNode.ID + ' , cNode.Width=' + IntToStr(cNode.Width) + ', cNewWidth=' + IntToStr(nNewWidth) + '.. STR=' + cNode.AsString() );
          Dec( nWidthLeft, nNewWidth );
          if (( not bChanged ) and (cNode.Width <> nNewWidth)) then
            bChanged := true;
          nDiff := nNewWidth - cNode.Width;
          if ( nDiff <> 0 ) then begin
            cNode.Width := nNewWidth;
            //Move all regions to the left of this one over by the difference
            bfound := false;
            for iNodes := 0 to Length(arrKeyListXSort) - 1 do begin
              if ( not bfound ) then
                bfound := ( arrKeyListXSort[iNodes] = arrKeyList[i] )
              else begin
                cEle := CEleNodeData( self.SubNodes.get( arrKeyListXSort[iNodes] ));
                cEle.X := cEle.X + nDiff;
              end;
            end;
          end; //if ( nDiff <> 0 ) then begin
        end else begin
          //dbg( '  id=' + cNode.ID + ' , cNode.Width=' + IntToStr(cNode.Width) + ', cNewWidth=' + IntToStr(nWidthLeft) + '.. STR=' + cNode.AsString());
          cNode.Width := nWidthLeft;
        end;
      end;
      if ( bChanged and Assigned(FOnChange)) then FOnChange(Self);
    end;
  end;
End;

procedure CEleNodeData.setWidthPercent(val: Extended);
begin
  self.FWidthPct := Val;
end;

procedure CEleNodeData.setX(val: cardinal);
Var
  i : cardinal;
  cXDiff : integer;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
  bChanged : boolean;
Begin
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    bChanged := ( val <> self.FX );
    self.FX := val;
    if ( bChanged and Assigned(FOnChange) ) then FOnChange(Self);
  end else begin
//Need to get the left most X
    arrKeyList := self.SubNodes.Keys;
    EleKeyListXSort(self.SubNodes, arrKeyList);
    cXDiff := val - self.X;
    if ( cXDiff <> 0 ) then begin
      //dbg('CEleNodeData.setX(): val=' + IntToStr(val) + ' FX=' + IntToStr(FX) + '  cXDiff=' + IntToStr(cXDiff) + '  ' );
      for i := 0 to Length(arrKeyList) - 1 do begin
        cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
        //dbg(' ID:' + cNode.ID + '- old cNode.X=' + IntToStr(cNode.X) + '    new cNode=' + IntToStr(cNode.X + cXDiff) );
        cNode.X := cNode.X + cXDiff;
      end;
      if (( cXDiff <> 0 ) and Assigned(FOnChange)) then FOnChange(Self);
    end;
  end;
End;

procedure CEleNodeData.setY(val: cardinal);
Var
  i : cardinal;
  cYDiff : integer;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
  bChanged : boolean;
Begin
  if (( self.SubNodes = nil ) or ( self.SubNodes.Count = 0 )) then begin
    bChanged := ( val <> self.FY );
    self.FY := val;
    if ( bChanged and Assigned(FOnChange) ) then FOnChange(Self);
  end else begin
    arrKeyList := self.SubNodes.Keys;
    EleKeyListYSort(self.SubNodes, arrKeyList);
    cYDiff := val - self.Y;
    if ( cYDiff <> 0 ) then begin
      for i := 0 to Length(arrKeyList) - 1 do begin
        cNode := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
        cNode.Y := cNode.Y + cYDiff;
      end;
      if (( cYDiff <> 0 ) and Assigned(FOnChange)) then FOnChange(Self);
    end;
  end;
End;

function CEleNodeData.appendElement(nod: IXMLNode; tmplElementType : string ) : IXMLNode;
var
  xmlNode : IXMLNode;
  ele : IXMLElement;
begin
  try
    if ( nod.nodeName <> 'element') then
      xmlNode := nod.ownerDocument.createElement( 'element' )
    else
      xmlNode := nod;

    setNode( xmlNode, 'id', Fid);
    ele := nod.ownerDocument.createElement( 'type' );
    ele.text := tmplElementType;
    xmlNode.appendChild( ele );

    ele := nod.ownerDocument.createElement( 'x' );
    ele.text := IntToStr( Fx );
    xmlNode.appendChild( ele );

    ele := nod.ownerDocument.createElement( 'y' );
    ele.text := IntToStr( Fy );
    xmlNode.appendChild( ele );

    ele := nod.ownerDocument.createElement( 'width' );
    ele.text := IntToStr( self.width );
    xmlNode.appendChild( ele );

    ele := nod.ownerDocument.createElement( 'height' );
    ele.text := IntToStr( self.height );
    xmlNode.appendChild( ele );

    if ( tmplElementType = NODE_TYPE_MULTI ) then begin

      result := nod.ownerDocument.createElement( 'elements' );
      result := xmlNode.appendChild( result );

      //nod.appendChild( xmlNode );

    end else if ( tmplElementType = NODE_TYPE_SINGLE ) then begin
      if ( Length(DataTypeString) > 0 ) then begin
        ele := nod.ownerDocument.createElement( 'datatype' );
        ele.text := DataTypeString;
        xmlNode.appendChild( ele );
      end;

      if ( self.groupkey ) then begin
        ele := nod.ownerDocument.createElement( 'group_key' );
        ele.text := boolToStr( self.groupkey );
        xmlNode.appendChild( ele );

        ele := nod.ownerDocument.createElement( 'group_key_order' );
        ele.text := IntToStr( FgroupOrder );
        xmlNode.appendChild( ele );
      end;

      if ( self.eekey ) then begin
        ele := nod.ownerDocument.createElement( 'ee_key' );
        ele.text := boolToStr( self.eekey );
        xmlNode.appendChild( ele );

        ele := nod.ownerDocument.createElement( 'ee_key_order' );
        ele.text := IntToStr( self.eeKeyOrder );
        xmlNode.appendChild( ele );
      end;

      if Length(self.scaler) > 0 then begin
        ele := nod.ownerDocument.createElement( 'scaler' );
        ele.text := self.scaler;
        xmlNode.appendChild( ele );
      end;

      if Length( self.format ) > 0 then begin
        ele := nod.ownerDocument.createElement( 'format' );
        ele.text := self.format;
        xmlNode.appendChild( ele );
      end;

      if ( nod.nodeName <> 'element') then
        result := nod.appendChild( xmlNode )
      else
        result := nod;
    end;
  except
        //ErrorRaised( 'CEleNodeData.getElementData - Field=' + sFieldName + ' Value=' + sValue, ExceptObject );
  end;
End;

procedure CEleNodeData.FromXML(ElementAsXMLString: string);
var
  xmlDoc: IXMLDocument;
  parsed : boolean;
Begin
  try
    //xmlDoc := IXMLDocument.Create;
    XMLDoc := CreateXMLDoc;
    parsed := xmlDoc.loadXML( ElementAsXMLString );

    if ( not parsed ) then begin
      raise Exception.Create( 'CTemplate.Load - XML Passed to function could not be parsed.' );
    end;

    self.GetElementData( xmlDoc.documentElement );

  finally
    //xmlDoc := nil;
  end;
End;

function CEleNodeData.getDataTypeString: string;
begin
  //TDataType = ( dtString = 0, dtNumeric, dtDateTime );
  if ( FDataType = dtDateTime ) then
    Result := 'dt'
  else if ( FDataType = dtNumeric ) then
    Result := 'n'
  else
    Result := 's'
end;

function CEleNodeData.AsXML(const EvalNamePath : boolean = false) : string;
var
  sarr : TStringBuilder;
  arrKeyList : TKeyList;
  i : Integer;
  ele : CEleNodeData;
begin
  try
    if ( EvalNamePath ) then
      self.FNamePathShallow := self.evalNamePath;

    sarr := TStringBuilder.Create;
    sarr.Append('<element>');

    sarr.Append('<NamePath>');
    sarr.Append( self.FNamePathShallow );
    sarr.Append('</NamePath>');

    sarr.Append('<id>');
    sarr.Append( ID );
    sarr.Append('</id>');

    sarr.Append('<x>');
    sarr.Append( IntToStr(self.X) );
    sarr.Append('</x>');

    sarr.Append('<y>');
    sarr.Append( IntToStr(self.Y) );
    sarr.Append('</y>');

    sarr.Append('<width>');
    sarr.Append( IntToStr(self.Width) );
    sarr.Append('</width>');

    sarr.Append('<height>');
    sarr.Append( IntToStr(self.Height) );
    sarr.Append('</height>');

    sarr.Append('<group_key>');
    sarr.Append( BoolToStr(Groupkey) );
    sarr.Append('</group_key>');

    sarr.Append('<group_key_order>');
    sarr.Append( IntToStr(GroupOrder) );
    sarr.Append('</group_key_order>');

    sarr.Append('<ee_key>');
    sarr.Append( BoolToStr(Eekey) );
    sarr.Append('</ee_key>');

    sarr.Append('<ee_key_order>');
    sarr.Append( IntToStr(EeKeyOrder) );
    sarr.Append('</ee_key_order>');

    sarr.Append('<datatype>');
    sarr.Append( DataTypeString );
    sarr.Append('</datatype>');

    sarr.Append('<format>');
    sarr.Append( Format );
    sarr.Append('</format>');

    sarr.Append('<NodeType>');
    sarr.Append( NodeType );
    sarr.Append('</NodeType>');

    sarr.Append('<scaler>');
    sarr.Append( Scaler );
    sarr.Append('</scaler>');

    sarr.Append('<ConfidenceAvg>');
    sarr.Append( FloatToStr(ConfidenceAvg) );
    sarr.Append('</ConfidenceAvg>');

    sarr.Append('<Parent>');
    sarr.Append( IntToStr(Integer(Addr(Parent))) );
    sarr.Append('</Parent>');

    sarr.Append('<RegionType>');
    sarr.Append( IntToStr(Integer( RegionType )) );
    sarr.Append('</RegionType>');

    sarr.Append('<TreeViewNodePointer>');
    sarr.Append( IntToStr( Integer( TreeViewNodePointer) ) );
    sarr.Append('</TreeViewNodePointer>');

    sarr.Append('<GraphicPointer>');
    sarr.Append( IntToStr( Integer( GraphicPointer) ) );
    sarr.Append('</GraphicPointer>');

    if ( self.SubNodes.Count > 0 ) then begin
      sarr.Append('<elements>');
      arrKeyList := self.SubNodes.Keys;
      for i := 0 to Length(arrKeyList) - 1 do begin
        ele := CEleNodeData( self.SubNodes.get( arrKeyList[i] ));
        sarr.Append( ele.AsXML( true ) );
      end;
      sarr.Append('</elements>');
    end;

    //SubNodes : THashTableI2X read FSubNodes write setSubNodes;

    sarr.Append('</element>');

    Result := sarr.ToString();
  finally
    FreeAndNil( sarr );
  end;
end;

procedure CEleNodeData.updateRegion(nod: IXMLNode);
var
  i : integer;
  xmlNode, newNode : IXMLNode;
  ele : IXMLElement;
  sFieldName, sValue : string;
begin
  try
    setNode( nod, 'x', IntToStr( self.x ));
    setNode( nod, 'y', IntToStr( self.y ));
    setNode( nod, 'width', IntToStr( self.width ));
    setNode( nod, 'height', IntToStr( self.height ));
  except
        //ErrorRaised( 'CEleNodeData.getElementData - Field=' + sFieldName + ' Value=' + sValue, ExceptObject );
  end;
End;

{ CTemplate }
procedure CTemplate.Clear;
Begin
  self.ClearData;
  self.ClearElements;

  FFirstElement.Clear;
  FImageProcessing.Clear;
  FImageProcessingUpsize.Clear;

  self.FTemplateName := '';
  self.FTemplateFileName := '';

  self.FXOffset := 0;
  self.FYOffset := 0;
  self.FIsOffset := false;

  TemplateName := '';
  FTemplateFileName := '';
  self.FXOffset := 0;
  self.FYOffset := 0;

  Initialize( FBoundary );
  FFirstChar.Clear;

  FRootNodeName := 'ocr_template';
  FDocumentOffsetCorrectionType := ocNone;
  XMLImageMode := xcNoSpecialFields; //( xcNoSpecialFields, xcComplete )

End;

procedure CTemplate.ClearData;
Begin
  FData.Clear;
  self.DocumentOffsetCorrectionType := ocNone;
  //self.DocumentOffsetCorrection := nil;
End;

procedure CTemplate.ClearElements;
Begin
  FElements.Clear;
End;

function CTemplate.Clone(var NewObject: CTemplate; nodeFilter: TSearchParm ): boolean;
{
  this function is really useful in only focusing on specific elements we want to OCR,
  or if we pass soemthing goofy,  then we can have it do all the processing and
  give us image information
}
var
  sNewFileName : TFileName;
begin
  if ( NewObject = nil ) then
    NewObject := CTemplate.Create();
  NewObject.LoadFromXML( self.asXML('', nodeFilter ) );
  sNewFileName := AppTempDir + '~Template' + RandomString() + '.xml';
  NewObject.saveAsXML( sNewFileName, nodeFilter );
  NewObject.TemplateName := sNewFileName;
end;

function CTemplate.Clone(var NewObject: CTemplate): boolean;
var
  sNewFileName : TFileName;
begin
  if ( NewObject <> nil ) then
    NewObject.Free;
  NewObject := CTemplate.Create();
  NewObject.LoadFromXML( self.asXML() );
  sNewFileName := AppTempDir + '~Template' + RandomString() + '.xml';
  NewObject.saveAsXML( sNewFileName );
end;

constructor CTemplate.Create;
Begin
  FOnInnerChange := self.OnInnerChange;

  FData := THashStringTable.create( 40 );
  FElements := THashTableI2X.create( 50 );
  FElements.OnChange := FOnInnerChange;
  FFirstElement := THashStringTable.create( 8 );
  FImageProcessing := TStringList2.Create;
  FImageProcessingUpsize := TStringList2.Create;

  TemplateName := '';
  FTemplateFileName := '';
  self.FXOffset := 0;
  self.FYOffset := 0;

  Initialize( FBoundary );
  FFirstChar := TI2XOCRItem.Create();

  FRootNodeName := 'ocr_template';
  FDocumentOffsetCorrectionType := ocNone;
  XMLImageMode := xcNoSpecialFields; //( xcNoSpecialFields, xcComplete )
End;

function CTemplate.DeleteElement(eleData: CEleNodeData): boolean;
var
  parent : CEleNodeData;
begin
  if ( eleData.Parent = nil ) then begin
    self.FElements.Delete( eleData.id );
  end else begin
    parent := CEleNodeData( eleData.Parent );
    parent.SubNodes.Delete( eleData.id );
  end;
  Result := true;
end;

destructor CTemplate.Destroy;
Begin
  //FreeHashStringElements( FElements );

  FreeAndNil( FElements );
  FreeAndNil( FImageProcessing );
  FreeAndNil( FImageProcessingUpsize );

  FreeAndNil( FFirstElement );
  FreeAndNil( FData );
  if ( FFirstChar <> nil ) then FreeAndNil( FFirstChar );
  if ( FDocumentOffsetCorrection <> nil ) then FreeAndNil( FDocumentOffsetCorrection );
  inherited;
End;

function CTemplate.getData(const varname: string): string;
Var
  str: string;
Begin
  result := FData.Get( varname, '' );
  if ( Pos('file_name', varname ) > 0 ) then begin
    if ( Length(ExtractFilePath( result )) = 0 )  then begin
      result := ExtractFilePath( self.TemplateName ) + result;
    end;
  end;
End;


function CTemplate.xmlN(const str: string) : string;
Begin
  result := StringReplace(str, ' ', '_', [rfReplaceAll]);
End;

Procedure CTemplate.setData(const varname, val: string);
Begin
  FData.Items[ varname ] := val;
  //if ( FData.ContainsKey( varname ) ) then
  //  FData.Add( xmlN(varname), val )
  //else
  //  FData.Add( xmlN(varname), val );
End;

function CTemplate.FillWithRegionSlices(oOCRResults: TI2XOCRResults): boolean;
{*
  This function is called to prepate the OCRResults object with region slices.
  This is used for a type of OCR pass where we will break up the image and scan
  each part of it.  Sometimes this yeilds better results.
*}
var
  eleData, eleSubData : CEleNodeData;
  i, iSubEle : cardinal;
  arrElementList, arrSubEleList : TKeyList;
  ocrres : TI2XOCRResult;
Begin
  Result := false;
  if ( oOCRResults = nil ) then
    raise Exception.Create('Passed oOCRResults must be preallocated');
  oOCRResults.Clear;
  if ( self.Elements.Count > 0 ) then begin
    arrElementList := self.Elements.Keys;
    for i := 0 to Length( arrElementList ) - 1 do begin
      eleData := CEleNodeData( self.Elements.Get( arrElementList[i] ));
      if ( eleData.NodeType = NODE_TYPE_SINGLE ) then begin
        ocrres := TI2XOCRResult.Create;
        ocrres.ID := SLICE_HEADER + eleData.ID;
        ocrres.X := eleData.X;
        ocrres.Y := eleData.Y;
        ocrres.Width := eleData.Width;
        ocrres.Height := eleData.Height;
        if ( eleData.DataType = dtNumeric ) then
          ocrres.DataType := odtNumeric
        else if ( eleData.DataType = dtDateTime ) then
          ocrres.DataType := odtDateTime
        else
          ocrres.DataType := odtString;
        oOCRResults.Add( ocrres );
      end else if ( eleData.NodeType = NODE_TYPE_MULTI ) then begin
        arrSubEleList := eleData.SubNodes.Keys;
        for iSubEle := 0 to eleData.SubNodes.Count - 1 do begin
          eleSubData := CEleNodeData( eleData.SubNodes.Get( arrSubEleList[iSubEle] ));
          ocrres := TI2XOCRResult.Create;
          ocrres.ID := SLICE_HEADER + eleData.ID + '_' + eleSubData.ID;
          ocrres.X := eleSubData.X;
          ocrres.Y := eleSubData.Y;
          ocrres.Width := eleSubData.Width;
          ocrres.Height := eleSubData.Height;
          if ( eleSubData.DataType = dtNumeric ) then
            ocrres.DataType := odtNumeric
          else if ( eleSubData.DataType = dtDateTime ) then
            ocrres.DataType := odtDateTime
          else
            ocrres.DataType := odtString;
          oOCRResults.Add( ocrres );
        end;
      end;
    end;
  end;
  Result := true;
End;

Procedure CTemplate.FreeHashStringElements( ht : THashTable );
Var
  i : cardinal;
  arrKeyList : TKeyList;
  ele : CEleNodeData;
  sKeyName : string;
Begin
  try
    Initialize( sKeyName );
      if ( ht.Count > 0) then begin
        arrKeyList := ht.Keys;
        for i := 0 to Length(arrKeyList) - 1 do begin
          sKeyName := arrKeyList[i];
          ele := CEleNodeData( ht.Get( arrKeyList[i] ));
          ele.Free;
        end;
      end;
      ht.Free;
  except
     on E : Exception do
     begin
       DebugString('CEleNodeData.FreeSubNodes() : Exception!!');
       DebugString('  Class name = '+E.ClassName);
       DebugString('  Exception message = '+E.Message);
       DebugString('  Key = '+ sKeyName);
     end;
  end;
End;

function CTemplate.setElement(const elementName: string; eleData : CEleNodeData) : CEleNodeData;
var
  strKey: string;
  sParentName, sFieldName : string;
  nPos : integer;
  value: string;
  bCreateElement : boolean;
  eleDataEntry, eleDataEntrySub, oTest: CEleNodeData;
begin
  result := nil;
  bCreateElement := (eleData = nil);

  if (bCreateElement) then begin
    bCreateElement := true;
    eleData := CEleNodeData.Create;
  end;

  nPos := Pos('.', elementName);
  if ( nPos > 0 ) then begin
    sParentName := Copy(elementName, 1, nPos - 1);
    sFieldName := Copy(elementName, nPos + 1, 999);
  end else begin
    sParentName := '';
    sFieldName := elementName;
  end;
  if ( Length(sParentName) = 0 ) then begin
    if FElements.containsKey( sFieldName ) then begin
      eleDataEntry := CEleNodeData(FElements.get( sFieldName ));
    end else begin
      eleDataEntry := CEleNodeData.Create; //create copy that will be stored into hashtable
    end;

    eleDataEntry.Assign( eleData );
    FElements.put( sFieldName, eleDataEntry );
    result := eleDataEntry;
  end else begin

    if FElements.containsKey( sParentName ) then begin
      eleDataEntry := CEleNodeData(FElements.Get( sParentName ));
    end else begin
      eleDataEntry :=  CEleNodeData.Create; //create copy that will be stored into hashtable
      eleDataEntry.id := sParentName;
      FElements.Put( sParentName, eleDataEntry );
    end;
    if ( Length(sFieldName) > 0 ) then begin
      if eleDataEntry.SubNodes.containsKey( sFieldName ) then begin
        eleDataEntrySub := CEleNodeData(eleDataEntry.SubNodes.get( sFieldName ));
      end else begin
        eleDataEntrySub :=  CEleNodeData.Create(); //create copy that will be stored into hashtable
      end;
      eleDataEntrySub.Assign( eleData );
      eleDataEntry.SubNodes.Parent := TObject( FElements );
      eleDataEntrySub.Parent := TObject( eleDataEntry );
      eleDataEntry.SubNodes.put( sFieldName, eleDataEntrySub );
      result := eleDataEntrySub;
    end;
  end;
  if (bCreateElement) then begin
    eleData.Free;
  end;
End;

function CTemplate.getElement(const elementName: string; forceExist : boolean) : CEleNodeData;
var
  sParentName, sFieldName : string;
  nPos : integer;
  eleDataEntry, eleDataEntrySub: CEleNodeData;
begin
    Result := nil;
    nPos := Pos('.', elementName);
    if ( nPos > 0 ) then begin
      sParentName := Copy(elementName, 1, nPos - 1);
      sFieldName := Copy(elementName, nPos + 1, 999);
    end else begin
      sParentName := '';
      sFieldName := elementName;
    end;
    if ( Length(sParentName) = 0 ) then begin
      if FElements.containsKey( sFieldName ) then
        result := CEleNodeData(FElements.get( sFieldName ))
      else if ( forceExist )  then  begin
        result := setElement( sFieldName );
      end;
    end else begin
     //Parent key was specified
      if not FElements.containsKey( sParentName ) then begin
        if ( forceExist ) then
          eleDataEntry := setElement( sParentName )
        else
          raise Exception.create( 'Parent key ' + sParentName + ' does not exist.' );
      end else
        eleDataEntry := CEleNodeData(FElements.get( sParentName ));
      if ((eleDataEntry <> nil) and ( Length(sFieldName) > 0 )) then begin
        if eleDataEntry.SubNodes.containsKey( sFieldName ) then
          result := CEleNodeData(eleDataEntry.SubNodes.get( sFieldName ))
        else if ( forceExist )  then  begin
          result := setElement(elementName);
        end;
      end;
    end;

End;

procedure CTemplate.SetDefaultValues;
Begin
  self.setData( 'x_offset', '0' );
  self.setData( 'y_offset', '0' );
End;

//procedure CTemplate.SetDocumentOffsetCorrection(val: TDocumentOffsetCorrection);
//begin
//  if ( self.FDocumentOffsetCorrection <> nil ) then
//    self.FDocumentOffsetCorrection.Free;
//  self.FDocumentOffsetCorrection := val;
//end;

procedure CTemplate.LoadFromXML(const xmlToLoad: string);
begin
  self.Load( xmlToLoad );
end;

function CTemplate.RenameIsValid( const eleToCheckBeforeRename : CEleNodeData;
  renamedTo : string ): boolean;
var
  eleData : CEleNodeData;
Begin
  if ( eleToCheckBeforeRename.Parent = nil ) then begin
    Result := not self.FElements.ContainsKey( renamedTo );
  end else begin
    eleData := CEleNodeData( eleToCheckBeforeRename.Parent );
    Result := not eleData.SubNodes.ContainsKey( renamedTo );
  end;
End;

function CTemplate.MoveElement( const srcElement : CEleNodeData;
  ParentEleData: CEleNodeData ; const ForceMove : boolean ): CEleNodeData;
var
  eleData, newEleData : CEleNodeData;
Begin
  if ( ParentEleData = nil )  then begin
    if ( srcElement.Parent <> nil ) then begin
      eleData := CEleNodeData( srcElement.Parent );
      if ( self.FElements.ContainsKey( srcElement.id ) ) then
        raise Exception.Create( 'Cannot Move Element because an id of ' + srcElement.id + ' already.' );
      newEleData := CEleNodeData.Create;
      newEleData.Assign( srcElement );
      newEleData.Parent := nil;
      eleData.SubNodes.Delete( newEleData.id );
      self.FElements.Add( newEleData.id, newEleData );
      Result := newEleData;
    end;
  end else if ( ParentEleData <> nil )  then begin
    if ( ( srcElement.Parent <> nil ) and ( ForceMove ) ) then
      srcElement.Parent := nil;

    if ( srcElement.Parent = nil ) then begin
      if ( ParentEleData.SubNodes.ContainsKey( srcElement.id ) ) then
        if ( ForceMove ) then
          eleData.SubNodes.Delete( srcElement.id )
        else
          raise Exception.Create( 'Cannot Move Element because an id of ' + srcElement.id + ' already.' );

      newEleData := CEleNodeData.Create;
      newEleData.Assign( srcElement );
      //self.FElements.Delete( newEleData.id );
      newEleData.Parent := ParentEleData;
      ParentEleData.SubNodes.add( newEleData.id, newEleData );
      dbg( 'Contains Key ' + newEleData.id + ' ? ' + uStrUtil.BoolToStr( self.Elements.ContainsKey(newEleData.id), 'Y', 'N') );
      self.DeleteElement( srcElement );
      Result := newEleData;
    end;
  end;
End;

procedure CTemplate.Load(const fileNameOrXML: string);
var
  xmlDoc: IXMLDocument;
  nod, subNod, root, ele, curEle  : IXMLElement;
  oNod : IXMLNode;
  sType : string;
  fileName : TFileName;
  xmlNodeList, xmlSubNodeList, xmlSubNodeList2 : IXMLNodeList;
  i, j, k : integer;
  eleData, eleChildData : CEleNodeData;
  docFirstChar : TDOCFirstCharacter;
  docOffset : TDOCOffset;
  docBoundary : TDOCBoundary;
  parsed : boolean;
Begin
  try
    self.Clear;
    //xmlDoc := IXMLDocument.Create;
    xmlDoc := CreateXMLDoc;
    parsed := false;
    if ( FileExists( fileNameOrXML ) ) then begin
      parsed := xmlDoc.load( fileNameOrXML );
      fileName := fileNameOrXML;
    end else begin
      parsed := xmlDoc.loadXML( fileNameOrXML );
      if ( parsed ) then begin
        fileName := AppTempDir + '~TemplateLoad.xml';
        xmlDoc.save( fileName );
      end;
    end;

    if ( not parsed ) then begin
      raise Exception.Create( 'CTemplate.Load - XML Passed to function could not be parsed.' );
    end;
    self.TemplateFileName := fileName;
    eleData := CEleNodeData.Create;
    eleChildData := CEleNodeData.Create;
      //xmlNodeList := xmldoc.selectNodes( WideString( '/*/child::*' ) );
      xmlNodeList := xmldoc.DocumentElement.ChildNodes;
      if ( xmlNodeList.length > 0 ) then begin
        for i := 0 to xmlNodeList.length - 1 do begin
          nod := IXMLElement(xmlNodeList.Item[i]);
          if ( nod.nodeType = NODE_ELEMENT ) then begin

            if (nod.nodeName = 'xml_image_mode' ) then begin
              self.FXMLImageMode := TXMLImageMode( StrToInt( nod.text ));
            end else if (nod.nodeName = 'xml_image_mode_cache' ) then  begin
              self.FXMLImageModeCache := TXMLImageMode( StrToInt( nod.text ));
            end else if (nod.nodeName = 'template_file_name' ) then begin
              self.FTemplateFileName := nod.text;
            end else if (nod.nodeName = 'template_name' ) then begin
              self.FTemplateName := nod.text;
            end else if (nod.nodeName = 'root_node_name' ) then begin
              self.FRootNodeName := nod.text;
            end else if (nod.nodeName = 'xoffset' ) then begin
              self.FXOffset := StrToInt( nod.text );
            end else if (nod.nodeName = 'yoffset' ) then begin
              self.FYOffset := StrToInt( nod.text );
            end else if (nod.nodeName = 'boundary' ) then begin
              xmlSubNodeList := nod.childNodes;
              for j := 0 to xmlSubNodeList.length - 1 do begin
                subNod := IXMLElement(xmlSubNodeList.Item[j]);
                if (subNod.nodeName = 'top' ) then
                  self.FBoundary.Top := StrToInt( subNod.text );
                if (subNod.nodeName = 'left' ) then
                  self.FBoundary.Left := StrToInt( subNod.text );
                if (subNod.nodeName = 'right' ) then
                  self.FBoundary.Right := StrToInt( subNod.text );
                if (subNod.nodeName = 'bottom' ) then
                  self.FBoundary.Bottom := StrToInt( subNod.text );
              end;
            end
            else if (nod.nodeName = 'first_char' ) then begin
              self.FFirstChar.Clear;
              xmlSubNodeList := nod.childNodes;
              for j := 0 to xmlSubNodeList.length - 1 do begin
                subNod := IXMLElement(xmlSubNodeList.Item[j]);
                if (subNod.nodeName = 'x' ) then
                  self.FFirstChar.X := StrToInt( subNod.text );
                if (subNod.nodeName = 'y' ) then
                  self.FFirstChar.Y := StrToInt( subNod.text );
                if (subNod.nodeName = 'w' ) then
                  self.FFirstChar.Width := StrToInt( subNod.text );
                if (subNod.nodeName = 'h' ) then
                  self.FFirstChar.Height := StrToInt( subNod.text );
                if (subNod.nodeName = 'a' ) then
                  self.FFirstChar.Accuracy := strToFloat(subNod.text);
                if (subNod.nodeName = 'd' ) then
                  self.FFirstChar.DataEnc := subNod.text;
              end;
            end
            else if (nod.nodeName = 'first_element' ) then begin
              xmlSubNodeList := nod.childNodes;
              for j := 0 to xmlSubNodeList.length - 1 do begin
                subNod := IXMLElement(xmlSubNodeList.Item[j]);
                self.setFirstElement( subNod.nodeName, subNod.text);
              end;
            end else if (nod.nodeName = 'image_processing_instructions' ) then begin
              self.FImageProcessing.Clear;
              xmlSubNodeList := nod.childNodes;
              for j := 0 to xmlSubNodeList.length - 1 do begin
                subNod := IXMLElement(xmlSubNodeList.Item[j]);
                if ( subNod.nodeType = 1 ) then
                  self.FImageProcessing.Add( subNod.text );
              end;
            end else if (nod.nodeName = 'image_processing_instructions_upsize' ) then begin
              self.FImageProcessingUpsize.Clear;
              xmlSubNodeList := nod.childNodes;
              for j := 0 to xmlSubNodeList.length - 1 do begin
                subNod := IXMLElement(xmlSubNodeList.Item[j]);
                if ( subNod.nodeType = 1 ) then
                  self.FImageProcessingUpsize.Add( subNod.text );
              end;
            end else if (nod.nodeName = 'document_offset_correction' ) then begin
              self.FDocumentOffsetCorrectionType := TOffsetCorrectionType( StrToInt( nod.text ) );
            end else if (nod.nodeName = 'elements' ) then begin
              xmlSubNodeList := nod.ChildNodes;
              for j := 0 to xmlSubNodeList.length - 1  do begin
                if (IXMLNode( xmlSubNodeList.Item[j] ).NodeName='element') then begin
                  eleData.getElementData( xmlSubNodeList.Item[j], false );
                  IXMLNode( xmlSubNodeList.Item[j] ).SelectSingleNode('elements', oNod);
                  if ( oNod = nil ) then begin
                    setElement( eleData );
                  end else if ( nod.HasChildNodes ) then begin
                    xmlSubNodeList2 := oNod.ChildNodes;
                    for k := 0 to xmlSubNodeList2.length - 1 do begin
                      eleChildData.getElementData( xmlSubNodeList2.Item[k] );
                      setElement( eleData.id + '.' + eleChildData.id, eleChildData );
                    end;
                  end;
                end;
              end;

            end else begin
              self.setData( nod.nodeName, nod.text );
            end;
          end;
        end;
      end;
      self.TemplateName := fileName;
  finally
    FreeAndNil( ele );
    FreeAndNil( eleData );
    FreeAndNil( eleChildData );
    //xmlDoc := nil;
  end;
End;

procedure CTemplate.LoadFromFile(const fileName : TFileName );
Begin
  self.Load( fileName );
End;


function CTemplate.getNodeValue(xmlDoc: IXMLDocument;
          nodeName : string; defaultValue : string ) : string;
var
  nod : IXMLNode;
Begin
  nod := xmldoc.selectSingleNode( WideString( nodeName ) );
  if ( nod <> nil ) then
    result := nod.text
  else
    result := defaultValue;
End;

function CTemplate.getNodeValueInt(xmlDoc: IXMLDocument;
          nodeName : string; defaultValue : integer ) : integer;
var
  nod : IXMLNode;
Begin
  nod := xmldoc.selectSingleNode( WideString( nodeName ) );
  if ( nod <> nil ) then
    result := StrToInt( nod.text )
  else
    result := defaultValue;
End;

function CTemplate.getTemplateFileName: TFileName;
begin
  Result := FTemplateFileName;
end;

function CTemplate.getXMLImageMode: TXMLImageMode;
begin
  Result := self.FXMLImageMode;
end;

procedure CTemplate.saveAsXML(const fileName : TFileName );
Begin
  asXml( fileName, '' );
End;

procedure CTemplate.saveAsXML(const fileName : TFileName; const elementSearchParm: string);
Begin
  asXml( fileName, elementSearchParm );
End;

function CTemplate.IsLike(const stringToSearch : string; const searchParm : TSearchParm ): boolean;
var
  r : TRegExpr;
  sExp : string;
  iMatchCount : cardinal;
Begin
  Result := false;
  iMatchCount := 0;
  r := TRegExpr.Create;
  try
    sExp := searchParm;
    sExp := StringReplace(sExp, '*', '(.*)',[rfReplaceAll, rfIgnoreCase]);
    sExp := StringReplace(sExp, '%', '(.*)',[rfReplaceAll, rfIgnoreCase]);
    r.Expression := sExp;
    if r.Exec( stringToSearch ) then begin
      REPEAT
       inc(iMatchCount);
      UNTIL not r.ExecNext;
    end;
  finally
    r.Free;
  end;
  Result := (iMatchCount > 0);
End;

function CTemplate.asXML(const fileName: TFileName;
    nodeFilter: TSearchParm; const XMLImageMode : TXMLImageMode ): string;
var
  sFieldName : string;
  FXMLTemplate, odoc: IXMLDocument;
  nod, nod2, root, rootElements, rootSubElements: IXMLNode;
  ele : IXMLElement;
  eleData, eleSubData : CEleNodeData;
  eleCount, subEleCount, i : cardinal;
  arrFieldList, arrElementList : TKeyList;
Begin
  try
      if ( XMLImageMode <> xcNotSpecified ) then
        self.FXMLImageMode := XMLImageMode;

      if (Length(nodeFilter) = 0) then
        nodeFilter := '*';
      eleCount := 0;
      subEleCount := 0;
      //FXMLTemplate := IXMLDocument.Create;
      FXMLTemplate := CreateXMLDoc;
      FXMLTemplate.loadXML('<' + FRootNodeName + '></' + FRootNodeName + '>');
      //root := FXMLTemplate.documentElement;
      root := FXMLTemplate.firstChild;

      if ( FData.count > 0 ) then begin
        arrFieldList := FData.Keys;
        for i := 0 to Length(arrFieldList) - 1 do begin
           ele := FXMLTemplate.createElement( arrFieldList[i] );
           ele.text := FData.Get( arrFieldList[i] );
           root.appendChild( ele );
        end;
      end;

      if ( self.FXOffset <> 0 ) then begin
        ele := FXMLTemplate.createElement( 'xoffset' );
        ele.text := IntToStr( self.FXOffset );
        root.appendChild( ele );
      end;

      if ( self.FYOffset <> 0 ) then begin
        ele := FXMLTemplate.createElement( 'yoffset' );
        ele.text := IntToStr( self.FYOffset );
        root.appendChild( ele );
      end;

      rootElements := FXMLTemplate.createElement( 'boundary' );
        ele := FXMLTemplate.createElement( 'top' );
        ele.text := IntToStr( self.FBoundary.Top );
        rootElements.appendChild( ele );

        ele := FXMLTemplate.createElement( 'left' );
        ele.text := IntToStr( self.FBoundary.left );
        rootElements.appendChild( ele );

        ele := FXMLTemplate.createElement( 'right' );
        ele.text := IntToStr( self.FBoundary.Right );
        rootElements.appendChild( ele );

        ele := FXMLTemplate.createElement( 'bottom' );
        ele.text := IntToStr( self.FBoundary.Bottom );
        rootElements.appendChild( ele );
      root.appendChild( rootElements );

      rootElements := FXMLTemplate.createElement( 'first_char' );
        ele := FXMLTemplate.createElement( 'x' );
        ele.text := IntToStr( self.FFirstChar.X );
        rootElements.appendChild( ele );

        ele := FXMLTemplate.createElement( 'y' );
        ele.text := IntToStr( self.FFirstChar.Y );
        rootElements.appendChild( ele );

        ele := FXMLTemplate.createElement( 'w' );
        ele.text := IntToStr( self.FFirstChar.Width );
        rootElements.appendChild( ele );

        ele := FXMLTemplate.createElement( 'h' );
        ele.text := IntToStr( self.FFirstChar.Height );
        rootElements.appendChild( ele );
        root.appendChild( rootElements );

        ele := FXMLTemplate.createElement( 'a' );
        ele.text := FloatToStrF( self.FFirstChar.Accuracy, ffGeneral, 6, 2 );

        rootElements.appendChild( ele );
        root.appendChild( rootElements );

        ele := FXMLTemplate.createElement( 'd' );
        ele.text := self.FFirstChar.DataEnc;
        rootElements.appendChild( ele );
      root.appendChild( rootElements );

      //if (( self.DocumentOffsetCorrectionType <> ocNone ) and ( self.DocumentOffsetCorrection <> nil ))  then begin
      //  odoc := CreateXMLDoc;
      //  with self.DocumentOffsetCorrection do
      //    nod := StringAsXMLNode( odoc, '<root>' + AsXML('doc_offset_correction') + '</root>' );
      //  CopyNode( nod, root );
      //  //root.appendChild( nod );
      //end;

      ele := FXMLTemplate.createElement( 'document_offset_correction' );
      ele.text := IntToStr( integer( self.FDocumentOffsetCorrectionType ) );
      root.appendChild( ele );

      if (FImageProcessing.Count > 0) then begin
        rootElements := FXMLTemplate.createElement( 'image_processing_instructions' );
        for i := 0 to FImageProcessing.Count - 1 do begin
            ele := FXMLTemplate.createElement( 'task' );
            ele.text := FImageProcessing[i];
            rootElements.appendChild( ele );
        end;
        root.appendChild( rootElements );
      end;

      if (self.FImageProcessingUpsize.Count > 0) then begin
        rootElements := FXMLTemplate.createElement( 'image_processing_instructions_upsize' );
        for i := 0 to FImageProcessingUpsize.Count - 1 do begin
          ele := FXMLTemplate.createElement( 'task' );
          ele.text := FImageProcessingUpsize[i];
          rootElements.appendChild( ele );
        end;
        root.appendChild( rootElements );
      end;

      ElementsHashTableToDOM( FXMLTemplate, root, FElements, nodeFilter );

      if ( self.FXMLImageMode = xcComplete ) then begin
        ele := FXMLTemplate.createElement( 'xml_image_mode' );
        ele.text := IntToStr( integer( self.FXMLImageMode ) );
        root.appendChild( ele );

        ele := FXMLTemplate.createElement( 'xml_image_mode_cache' );
        ele.text := IntToStr( integer( self.FXMLImageModeCache ) );
        root.appendChild( ele );

        ele := FXMLTemplate.createElement( 'template_file_name' );
        ele.text := FTemplateFileName;
        root.appendChild( ele );

        ele := FXMLTemplate.createElement( 'template_name' );
        ele.text := FTemplateName;
        root.appendChild( ele );

        ele := FXMLTemplate.createElement( 'root_node_name' );
        ele.text := FRootNodeName;
        root.appendChild( ele );
      end;


      if ( Length(fileName) > 0 ) then begin
        FXMLTemplate.save( fileName );
        FTemplateFileName := fileName;
      end;
      Result := FXMLTemplate.xml;
      //restore the cached value.. this will allow us to utilize the command line just for
      //  a specifc run of this template
      self.FXMLImageMode := self.FXMLImageModeCache;
  finally
    //FreeAndNil( FXMLTemplate );
    //FXMLTemplate := nil;
  end;
End;

function CTemplate.asXML( const fileName : TFileName; const XMLImageMode : TXMLImageMode ): string;
Begin
  Result := asXml( '*', fileName, XMLImageMode );
End;

function CTemplate.asXML(const XMLImageMode : TXMLImageMode ): string;
Begin
  Result := self.asXML('', '*', XMLImageMode );
End;

procedure CTemplate.ElementsHashTableToDOM( FXMLTemplate: IXMLDocument;
    root: IXMLNode; ht : THashTable; nodeFilter: TSearchParm);
var
  baseEle : IXMLElement;
  nod : IXMLNode;
  eleData : CEleNodeData;
  i : cardinal;
  arrElementList : TKeyList;
  doCopy : boolean;
Begin
  doCopy := false;
  if ( ht.Count > 0 ) then begin
    arrElementList := ht.Keys;
    baseEle := FXMLTemplate.createElement( 'elements' );
    for i := 0 to Length( arrElementList ) - 1 do begin
      eleData := CEleNodeData( ht.Get( arrElementList[i] ));
      //if we specify a special region (such as a first character region) then we really will want to
      // copy it,  so force it to copy if it matches
      doCopy := (FXMLImageMode = xcComplete);
      if ( not doCopy ) then
        doCopy := ( ( Copy(nodeFilter, 1, 2) = '__' ) and ( IsLike( eleData.id, nodeFilter ) ));
      if ( not doCopy ) then
        doCopy := ( ( Copy(eleData.id, 1, 2) <> '__' ) and ( IsLike( eleData.id, nodeFilter ) ));
      if ( doCopy ) then begin
        //if ( ( Copy(eleData.id, 1, 2) <> '__' ) and ( IsLike( eleData.id, nodeFilter ) )) then begin
        //if (eleData.SubNodes.count > 0 ) then eleData. := NODE_TYPE_MULTI else eleData.nodeType := NODE_TYPE_SINGLE;
        nod := self.genElementXML( FXMLTemplate, eleData );
        if ( eleData.nodeType = NODE_TYPE_MULTI ) then begin
          ElementsHashTableToDOM( FXMLTemplate, nod, eleData.SubNodes, '*' );
        end;
        baseEle.appendChild( nod );
        //root.appendChild( nod );
      end;
    end;
    root.appendChild( baseEle );

  end;
End;

function CTemplate.genElementXML(FXMLTemplate:IXMLDocument;  eleData : CEleNodeData ) : IXMLNode;
var
  i : integer;
  xmlNode, nod : IXMLNode;
  ele : IXMLElement;
  sFieldName, sValue : string;
begin
    xmlNode := FXMLTemplate.createElement( 'element' );

    ele := FXMLTemplate.createElement( 'id' );
    ele.text := eleData.id;
    xmlNode.appendChild( ele );

    ele := FXMLTemplate.createElement( 'type' );
    ele.text := eleData.nodeType;
    xmlNode.appendChild( ele );

    ele := FXMLTemplate.createElement( 'x' );
    ele.text := IntToStr( eleData.x );
    xmlNode.appendChild( ele );

    ele := FXMLTemplate.createElement( 'y' );
    ele.text := IntToStr( eleData.y );
    xmlNode.appendChild( ele );

    ele := FXMLTemplate.createElement( 'width' );
    ele.text := IntToStr( eleData.width );
    xmlNode.appendChild( ele );

    ele := FXMLTemplate.createElement( 'height' );
    ele.text := IntToStr( eleData.height );
    xmlNode.appendChild( ele );

    if ( eleData.nodeType = NODE_TYPE_SINGLE ) then begin
      if ( Length(eleData.DataTypeString) > 0 ) then begin
        ele := FXMLTemplate.createElement( 'datatype' );
        ele.text := eleData.DataTypeString;
        xmlNode.appendChild( ele );
      end;

      if ( eleData.groupkey ) then begin
        ele := FXMLTemplate.createElement( 'group_key' );
        ele.text := boolToStr( eleData.groupkey );
        xmlNode.appendChild( ele );

        ele := FXMLTemplate.createElement( 'group_key_order' );
        ele.text := IntToStr( eleData.groupOrder );
        xmlNode.appendChild( ele );
      end;

      if ( eleData.eekey ) then begin
        ele := FXMLTemplate.createElement( 'ee_key' );
        ele.text := boolToStr( eleData.eekey );
        xmlNode.appendChild( ele );

        ele := FXMLTemplate.createElement( 'ee_key_order' );
        ele.text := IntToStr( eleData.eeKeyOrder );
        xmlNode.appendChild( ele );
      end;

      if Length(  eleData.scaler ) > 0 then begin
        ele := FXMLTemplate.createElement( 'scaler' );
        ele.text := eleData.scaler;
        xmlNode.appendChild( ele );
      end;

      if Length( eleData.format ) > 0 then begin
        ele := FXMLTemplate.createElement( 'format' );
        ele.text := eleData.format;
        xmlNode.appendChild( ele );
      end;
    end;

    result := xmlNode;
End;

function CTemplate.setElement(eleData: CEleNodeData): CEleNodeData;
begin
   result := self.setElement( eleData.id, eleData );
end;

function CTemplate.getElement(const ht: THashTable; const x, y, width,
  height: cardinal): CEleNodeData;
var
  eleData : CEleNodeData;
  i : cardinal;
  arrElementList : TKeyList;
Begin
  eleData := nil;
  Result := nil;
  if ( ht.Count = 0 ) then begin
    Result := nil;
  end else begin
    arrElementList := ht.Keys;
    for i := 0 to Length( arrElementList ) - 1 do begin
      eleData := CEleNodeData( ht.Get( arrElementList[i] ));
      if (
          (( x >= eleData.X ) and ( x <= ( eleData.X + eleData.Width )) ) and
          (( y >= eleData.Y ) and ( y <= ( eleData.Y + eleData.Height )) )
         ) then begin
        Result := eleData;
        Break;
      end;
    end;
  end;
End;

function CTemplate.getElement(const x, y, width,
  height: cardinal): CEleNodeData;
begin
  Result := getElement( self.Elements, x, y, width, height );
end;

function CTemplate.getElement(const ht: THashTable; const x, y, width, height,
  pixelTolerance: cardinal): CEleNodeData;
var
  eleData : CEleNodeData;
  i : cardinal;
  arrElementList : TKeyList;
Begin
  eleData := nil;
  Result := nil;
  if ( ht.Count = 0 ) then begin
    Result := nil;
  end else begin
    arrElementList := ht.Keys;
    for i := 0 to Length( arrElementList ) - 1 do begin
      eleData := CEleNodeData( ht.Get( arrElementList[i] ));
      if (
          (( x >= (eleData.X - pixelTolerance) ) and ( x <= ( eleData.X + eleData.Width + pixelTolerance )) ) and
          (( y >= (eleData.Y - pixelTolerance) ) and ( y <= ( eleData.Y + eleData.Height + pixelTolerance )) )
         ) then begin
        Result := eleData;
        Break;
      end;
    end;
  end;
End;

function CTemplate.getElement( const IntegerPointer: cardinal): CEleNodeData;
Begin
  Result := getElement( self.Elements, IntegerPointer );
End;

function CTemplate.getElement(ht : THashTable; const IntegerPointer: cardinal): CEleNodeData;
var
  eleData : CEleNodeData;
  i : cardinal;
  arrElementList : TKeyList;
Begin
  eleData := nil;
  Result := nil;
  if ( ht.Count = 0 ) then begin
    Result := nil;
  end else begin
    arrElementList := ht.Keys;
    for i := 0 to Length( arrElementList ) - 1 do begin
      eleData := CEleNodeData( ht.Get( arrElementList[i] ));
      if ( IntegerPointer = Cardinal( pointer( eleData )) ) then begin
        Result := eleData;
      end else if ( eleData.SubNodes.Count > 0 ) then begin
        Result := getElement( eleData.SubNodes, IntegerPointer );
      end;
    end;
  end;
End;

function CTemplate.getFirstElement(const varname: string): string;
Begin
  result := FFirstElement.get(varname, '');
End;

procedure CTemplate.setImageFileName(const Value: TFileName);
begin
  setData('image_file_name', Value );
end;

procedure CTemplate.setImageProcessingInstructionsAsString(imgProcStr: string);
begin
  try
    FImageProcessing.fromString( imgProcStr );
  finally
  end;
end;

procedure CTemplate.setImageProcessingInstructionsUpsizeAsString(
  imgProcStr: string);
begin
    FImageProcessingUpsize.fromString( imgProcStr );
end;

procedure CTemplate.setTemplateFileName(const Value: TFileName);
begin
  self.FTemplateFileName := Value;
end;

procedure CTemplate.setXMLImageMode(const Value: TXMLImageMode);
begin
  self.FXMLImageMode := value;
  self.FXMLImageModeCache := Value;
end;

function CTemplate.getImageFileName: TFileName;
begin
  Result := getData('image_file_name');
end;

function CTemplate.getImageProcessingInstructionsAsString() : string;
Begin
  try
    result := FImageProcessing.asString();
  finally
  end;
End;

function CTemplate.getImageProcessingInstructionsUpsizeAsString: string;
begin
  try
    result := FImageProcessingUpsize.asString();
  finally
  end;
end;

function CTemplate.GetIsOffset: boolean;
begin
  Result := self.FIsOffset;
end;

procedure CTemplate.setFirstElement(const varname, val: string);
Begin
  FFirstElement.Add( xmlN(varname), val );
End;

procedure CTemplate.setFirstElement(const varname: string; val: integer);
begin
  setFirstElement(varname, IntToStr( val ));
end;

procedure CTemplate.setData(const varname: string; const val: integer);
begin
  setData( varname, IntToStr( val ));
end;

procedure CTemplate.setData(const varname: string; const val: extended);
begin
  setData( varname, FloatToStr( val ));
end;

procedure CTemplate.Offset( ht : THashTable; const x, y : integer );
var
  baseEle : IXMLElement;
  nod : IXMLNode;
  eleData : CEleNodeData;
  i : cardinal;
  arrElementList : TKeyList;
Begin
  if ( ht.Count > 0 ) then begin
  // disable the ability for the parent to obtain update events.. since
    FElements.OnChange := nil;
    arrElementList := ht.Keys;
    for i := 0 to Length( arrElementList ) - 1 do begin
      eleData := CEleNodeData( ht.Get( arrElementList[i] ));
      if ( eleData.nodeType = NODE_TYPE_MULTI ) then
        self.offset( eleData.SubNodes, x, y )
      else begin
        eleData.x := eleData.x + x;
        eleData.y := eleData.y + y;
      end;
    end;
  // restore the ability for the parent to obtain update events.. since
    FElements.OnChange := FOnInnerChange;
  end;
End;

procedure CTemplate.Offset(const x, y: integer);
begin
  if ( self.IsOffset ) then begin
    self.OffsetClear();
  end;
  if ( not (( x = 0 ) and ( y = 0 ))) then begin
    self.FXOffset := x;
    self.FYOffset := y;
    Offset( self.Elements, x, y );
    self.FIsOffset := true;
    if ( Assigned( self.FOnOffsetChange ) ) then
      self.FOnOffsetChange( self );
  end;
end;

procedure CTemplate.OffsetClear;
begin
  Offset( self.Elements, ( -1 * self.FXOffset ), ( -1 * self.FYOffset ) );
  self.FIsOffset := false;
  if ( Assigned( self.FOnOffsetChange ) ) then
    self.FOnOffsetChange( self );
end;

procedure CTemplate.OnInnerChange(sender: TObject);
begin
  if Assigned(FOnChange) then begin
    FOnChange( sender );
  end;
end;

{ THashTableI2X }

constructor THashTableI2X.Create;
begin
  inherited Create();
  self.FOnItemChange := self.OnChangeHandler;
end;

procedure THashTableI2X.Add(const key: string; value: TObjectChangeNotify);
begin
  if Assigned(FOnItemChange) then begin
    value.OnChange := FOnItemChange;
  end;
  inherited Add( SafeKeyName( key ), value );
  if Assigned(FOnChange) then begin
    FOnChange(Self);
  end;
end;

procedure THashTableI2X.Clear;
begin
  inherited Clear;
end;

function THashTableI2X.ContainsKey(const key: string): boolean;
begin
  Result := inherited ContainsKey( SafeKeyName( key ) );
end;

constructor THashTableI2X.Create(const initialCapacity: Cardinal);
begin
  inherited Create(initialCapacity);
  self.FOnItemChange := self.OnChangeHandler;
  FForceKeyNameSafe := true;
end;

procedure THashTableI2X.Delete(const key: string);
begin
  inherited Delete( SafeKeyName( Key ) );
  if Assigned(FOnChange) then begin
    FOnChange(Self);
  end;
end;

destructor THashTableI2X.Destroy;
begin
  inherited;
end;

function THashTableI2X.Get(const key: string): TObjectChangeNotify;
begin
  Result := TObjectChangeNotify(inherited Get( SafeKeyName( key )));
end;

procedure THashTableI2X.OnChangeHandler(Sender: TObject);
begin
  if Assigned(FOnChange) then begin
    FOnChange( Sender );
  end;
end;

procedure THashTableI2X.Put(const key: string; value: TObjectChangeNotify);
begin
  if Assigned(FOnChange) then begin
    value.OnChange := FOnItemChange;
    FOnChange(Self);
  end;
  inherited Put( SafeKeyName( key ), value );
end;

function THashTableI2X.SafeKeyName(const keyName: string): string;
begin
  if ( self.FForceKeyNameSafe ) then
    Result := StringReplace( keyName, ' ', '_', [rfReplaceAll])
  else
    Result := keyName;
end;

{ THashStringTableI2X }

constructor THashStringTableI2X.Create;
begin
  inherited Create();
end;

constructor THashStringTableI2X.Create(const initialCapacity: Cardinal);
begin
  inherited Create(initialCapacity);
end;

destructor THashStringTableI2X.Destroy;
begin

  inherited;
end;

END.
