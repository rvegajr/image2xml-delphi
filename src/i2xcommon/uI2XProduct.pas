unit uI2XProduct;

interface
uses
  Types,
  uDebug,
  SysUtils,
  uStrUtil,
  OmniXML,
  Contnrs,
  JclStrings,
  uI2XConstants,
  uI2XPlugin,
  uHashTable,
  uI2XOCR,
  uI2XTemplate,
  Math;

const
  UNIT_NAME = 'uI2XProduct';
  NODE_ELEMENT = 1;

type
  TI2XProductItem = class(TI2XOCRItem)
    private
      FElement : CEleNodeData;  //This is the template element that this product item will be tied to
      FParent : TI2XProductItem;
      function getElementID: string;
    public
      property Element : CEleNodeData read FElement Write FElement;
      property ElementID : string read getElementID;
      property Parent : TI2XProductItem read FParent Write FParent;
      procedure Clear; dynamic;
      function AsXML() : string; dynamic;
      constructor Create; overload; override;
      constructor Create( ele : CEleNodeData ); overload; dynamic;
      constructor Create( const x, y, height, width : integer;
        const accuracy : single; const data : string ); overload; dynamic;
      destructor Destroy; override;
      procedure Copy( Source : TI2XProductItem ); dynamic;
      procedure CopyTo( Target : TI2XProductItem ); dynamic;
  end;

  TI2XItem = class( TI2XProductItem )
    private
      FItems :  THashTable;
      procedure PutItem(idx: TIndex; const Value: TI2XProductItem);
      function GetKeys: TKeyList;
      function GetCount: Cardinal;
    protected
      function getX() : integer;  override;
      function getY() : integer; override;
      function getWidth() : integer; override;
      function getHeight() : integer; override;
      function getAccuracy() : single; override;
    public
      function GetItem(idx: TIndex): TI2XProductItem;
      function Add( const ItemToAdd : TI2XProductItem ) : TI2XProductItem;
      procedure Delete( KeyOfItemToDelete : string );
      property Count : Cardinal read GetCount;
		  property Items[ idx : TIndex ]: TI2XProductItem read GetItem write PutItem; default;
      procedure Clear; override;
      function AsXML() : string; override;
      function Exists( const eleID: string): boolean;
		  property Keys : TKeyList read GetKeys;
      procedure Copy( Source : TI2XProductItem ); override;
      procedure CopyTo( Target : TI2XProductItem ); override;
      constructor Create( ele : CEleNodeData ) ; overload; override;
      constructor Create; overload; override;
      constructor Create( const x, y, height, width : integer;
        const accuracy : single; const data : string ); overload;
      destructor Destroy; override;
  end;

  TI2XRow = class( TI2XItem )
    public
      function AsXML() : string; override;
      constructor Create; overload; override;
  end;

  TI2XRows = class( TI2XProductItem )
    private
      FRows : TObjectList;
      function GetRow(idx: Integer): TI2XRow;
      procedure PutRow(idx: Integer; const Value: TI2XRow);
      function GetRowCount: Cardinal;
    protected
      function getX() : integer; override;
      function getY() : integer; override;
      function getWidth() : integer; override;
      function getHeight() : integer; override;
      function getAccuracy() : single; override;
    public
      function Add( const ItemToAdd : TI2XProductItem ) : TI2XProductItem;
      procedure Clear; override;
      property Count : Cardinal read GetRowCount;
		  property Rows[ idx : Integer ]: TI2XRow read GetRow write PutRow; default;
      function FindRow( const x, y : integer ) : TI2XRow;
      function AsXML() : string; override;
      procedure Delete( IndexToDelete : Integer );
      procedure Copy( Source : TI2XProductItem ); override;
      procedure CopyTo( Target : TI2XProductItem ); override;
      constructor Create; overload; override;
      constructor Create( ele : CEleNodeData ) ; overload; override;
      constructor Create( const x, y, height, width : integer;
        const accuracy : single; const data : string ); overload;
      destructor Destroy; override;
  end;

  TI2XOCRItemExt = class( TObject )
    private
      FCharTotal : integer;
      FCharSizeTotal : integer;
      FCharSizeTotalWSpaces : integer;
      FPixelsBtwCharTotal : cardinal;
      FPixelsBtwCharTotalCount : cardinal;
      FItem : TI2XProductItem;
      function getAvgCharSize: integer;
      function getAvgCharSizeSpace: integer;
      function getAvgSpaceBtwChar: integer;
    public
		  property Item : TI2XProductItem read FItem write FItem;
		  property CharTotal : integer read FCharTotal write FCharTotal;
      property CharSizeTotal : integer read FCharSizeTotal write FCharSizeTotal;
      property CharSizeTotalWSpaces : integer read FCharSizeTotalWSpaces write FCharSizeTotalWSpaces;
      property PixelsBtwCharTotal : cardinal read FPixelsBtwCharTotal write FPixelsBtwCharTotal;
      procedure AddToPixelsBtwCharTotal( const val : cardinal );
		  property AvgCharSize : integer read getAvgCharSize;
		  property AvgCharSizeSpace : integer read getAvgCharSizeSpace;
		  property AvgSpaceBtwChar : integer read getAvgSpaceBtwChar;
      constructor Create(); overload;
      constructor Create( const CharTotal, CharSizeTotal, CharSizeTotalWSpaces : integer; Item : TI2XProductItem ); overload;
  end;

  TI2XProductItems = class(TObject)
    private
      FItems: THashTable;
      FID : string;
      FSourceImageFileName : TFileName;
      FAccuracyMinimum : Single;
      function GetItem( idx : TIndex): TI2XProductItem;
      procedure PutItem( idx : TIndex; const Value: TI2XProductItem);
      function GetCount(): Cardinal;
      function XMLToProductItems(const XMLStringData: string;
            ProductItemsObject: TI2XProductItems): boolean;
      function GetKeys: TKeyList;
      function LoadFromFile( fileName : TFileName ) : boolean;
      function AppendOCRResultItem( TemplateRegions : CTemplate;
          OCRResultItem: TI2XOCRItem): boolean;
      function GetAccuracy: Single;
      function FlattenAndSortOCRResults(const OCRDataSource: TI2XOCRResults; var OCRDataTarget : TI2XOCRResults ): boolean;
      function CalcSpace(var PixeslBtwChar: integer; DataExt: TI2XOCRItemExt;
        ele: CEleNodeData): integer;
      function RoundUp(const pValue: extended): int64;
      function IsContainedWithin(ContainedInThisItem: TI2XProductItem;
        itemToTest: TI2XOCRItem): boolean;
    published
      function Add( const ItemToAdd : TI2XProductItem ) : TI2XProductItem;
      procedure Delete( KeyOfItemToDelete : string );
      procedure Clear;
      property Count : Cardinal read GetCount;
      property Accuracy : Single read GetAccuracy;
      property AccuracyMinimum : Single read FAccuracyMinimum write FAccuracyMinimum;
    public
		  property Items[ idx : TIndex ]: TI2XProductItem read GetItem write PutItem; default;
		  property Keys : TKeyList read GetKeys;
		  property ID : string read FID write FID;
		  property SourceImageFileName : TFileName read FSourceImageFileName write FSourceImageFileName;
      function ElementIDExists( const eleID : string ): boolean;
      function AsXML( const RootNodeName : string = 'i2x_xml') : string;
      function SaveToFile( fileName : TFileName ) : boolean;
      procedure Assign( Source : TI2XProductItems );
      procedure AssignTo( Target : TI2XProductItems );
      function ConsumeOCRResults( const Template : CTemplate; const OCRData : TI2XOCRResults ) : boolean;
      constructor Create( const id : string ); dynamic;
      destructor Destroy; override;
  end;

  TI2XProductItemCompare = class(TObject)
    private
      FItems: THashTable;
      FTemplate : CTemplate;
      FProductItemDefault : TI2XProductItems;
      function GetItem( idx : TIndex ): TI2XProductItems;
      procedure PutItem( idx : TIndex; const Value: TI2XProductItems );
      function GetItemsCount: integer;
      function AppendMostAccurateResultSingle(ele: CEleNodeData;
        ResultItems: TI2XProductItems): boolean;
      function GetMostAccurateResultItem(ele: CEleNodeData): TI2XOCRItem; overload;
      function GetMostAccurateResultItem(ele: CEleNodeData;
        rowSource: TI2XRow): TI2XOCRItem; overload;
      procedure InitKVList(var accList: TKeyValueExtendedList); overload;
      procedure InitKVList(var accList: TPtrAccuracyList); overload;
      function CompareRecusively(ResultItems: TI2XProductItems; ProductItem,
        Parent: TI2XProductItem): boolean; overload;
      function CompareRecusively(SourceResultItems, BestResultItems: TI2XProductItems ): boolean; overload;
    public
      function CompareRecusively( BestResultItems: TI2XProductItems ): boolean; overload;
      function Add( const ItemToAdd : TI2XProductItems ) : string;
		  property Items[ idx : TIndex ]: TI2XProductItems read GetItem write PutItem; default;
		  property ProductItemDefault : TI2XProductItems
          read FProductItemDefault write FProductItemDefault;
      function CompareResults( ResultItems : TI2XProductItems ) : boolean;
		  property Count : integer read GetItemsCount;
      constructor Create( const Template : CTemplate ); dynamic;
      destructor Destroy; override;
  end;

function OffsetCorrection(const Template: CTemplate; const OCRData: TI2XOCRResults; var Offset: TPoint): boolean;

implementation
uses
  uImage2XML;
  
function OffsetCorrection(const Template : CTemplate; const OCRData: TI2XOCRResults; var Offset: TPoint) : boolean;
var
  item : TI2XOCRItem;
  Boundary : TRect;
Begin
  Initialize( Offset );
  Result := false;
  if ( template.DocumentOffsetCorrectionType = ocFirstChar ) then begin
    //with TDOCFirstCharacter(template.DocumentOffsetCorrection) do begin
      item := OCRData.FirstCharacter;
      if ( item <> nil ) then begin
        Offset.X := item.X - Template.FirstChar.X;
        Offset.Y := item.Y - Template.FirstChar.Y;
        Result := true;
      end;
    //end;
  end else if ( template.DocumentOffsetCorrectionType = ocBoundary ) then begin
    //with TDOCBoundary(template.DocumentOffsetCorrection) do begin
      Boundary := OCRData.Boundary;
      if ( item <> nil ) then begin
        Offset.X := Boundary.Left - Template.Boundary.Left;
        Offset.Y := Boundary.Top - Template.Boundary.Top;
        Result := true;
      end;
    //end;
  end;
End;


{ TI2XProductItem }

function TI2XProductItem.AsXML: string;
var
  sb : TStringBuilder;
  elementName : string;
begin
  try
    if ( self.FElement <> nil ) then
      elementName := self.Element.id
    else
      elementName := 'product_item';
    sb := TStringbuilder.Create;
    sb.Append( '<' );
    sb.Append( elementName );
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
    sb.Append( '>' );
    sb.Append( self.DataEnc );
    sb.Append( '</' );
    sb.Append( elementName );
    sb.Append( '>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TI2XProductItem.Clear;
begin
  inherited;
  self.FElement := nil;
end;

procedure TI2XProductItem.Copy(Source: TI2XProductItem);
begin
  inherited Assign(Source);
  self.FElement := TI2XProductItem(Source).FElement;
  self.FParent := TI2XProductItem(Source).FParent;
end;

procedure TI2XProductItem.CopyTo(Target: TI2XProductItem);
begin
  inherited AssignTo(Target);
  TI2XProductItem(Target).FElement := self.FElement;
  TI2XProductItem(Target).FParent := self.FParent;
end;

constructor TI2XProductItem.Create(const x, y, height, width: integer;
  const accuracy: single; const data: string);
begin
  self.Create();
  self.X := x;
  self.Y := y;
  self.Width := width;
  self.Height := height;
  self.Accuracy := accuracy;
  self.Data := data;
  self.FParent := nil;
end;

constructor TI2XProductItem.Create(ele: CEleNodeData);
begin
  self.Create();
  self.Element := ele;
end;

constructor TI2XProductItem.Create;
begin
  inherited;
  self.FElement := nil;
end;

destructor TI2XProductItem.Destroy;
begin
  self.FElement := nil;
  inherited;
end;

function TI2XProductItem.getElementID: string;
begin
  if (( FElement = nil ) or ( Length( FElement.ID ) = 0 )) then
    Result := IntToStr( integer( @Self ))
  else
    Result := FElement.ID;
end;

{ TI2XProductItems }

function TI2XProductItems.Add(const ItemToAdd: TI2XProductItem ): TI2XProductItem;
begin
  FItems.Add( ItemToAdd.ElementID, TObject(ItemToAdd) );
  Result := TI2XProductItem( FItems[ ItemToAdd.ElementID ] );
end;

procedure TI2XProductItems.Assign( Source: TI2XProductItems );
Var
  i : integer;
  arrKeyList : TKeyList;
  sKeyName : string;
Begin
  self.FSourceImageFileName := Source.FSourceImageFileName;
  self.Clear;
  arrKeyList := Source.Keys;
  for i := 0 to Length(arrKeyList) - 1 do begin
    sKeyName := arrKeyList[i];
    self.Add( TI2XProductItem(Source[ sKeyName ]) );
  end;
End;

procedure TI2XProductItems.AssignTo( Target: TI2XProductItems);
Var
  i : integer;
  arrKeyList : TKeyList;
  sKeyName : string;
Begin
  Target.FSourceImageFileName := self.FSourceImageFileName;
  Target.Clear;
  arrKeyList := Target.Keys;
  for i := 0 to Length(arrKeyList) - 1 do begin
    sKeyName := arrKeyList[i];
    Target.Add( TI2XProductItem(self[ sKeyName ]) );
  end;
End;

function TI2XProductItems.AsXML(const RootNodeName: string): string;
var
  sb : TStringBuilder;
  i : integer;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<' );
    sb.Append( RootNodeName );
    sb.Append( ' source_image="' );
    sb.Append( self.FSourceImageFileName );
    sb.Append( '" a="' );
    sb.Append( FloatToStrF( self.Accuracy, ffGeneral, 6, 2) );
    sb.Append( '">' );

    for i := 0 to self.FItems.Count - 1 do begin
      sb.Append( TI2XProductItem(FItems[i]).AsXML() );
    end;

    sb.Append( '</' );
    sb.Append( RootNodeName );
    sb.Append( '>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TI2XProductItems.Clear;
begin
  self.FSourceImageFileName := '';
  FItems.Clear;
end;

function TI2XProductItems.IsContainedWithin( ContainedInThisItem : TI2XProductItem; itemToTest: TI2XOCRItem ): boolean;
var
  Top, Bottom : integer;
Begin
  //Result := ( ContainedInThisItem.Y + ContainedInThisItem.Height >
  //            itemToTest.Y + itemToTest.Height + SAME_LINE_PIXEL_TRESHOLD );
  //if the bottom of the item is
  Top := ((ContainedInThisItem.Y ) - SAME_LINE_PIXEL_TRESHOLD);
  Bottom := ((ContainedInThisItem.Y + ContainedInThisItem.Height ) + SAME_LINE_PIXEL_TRESHOLD);
  Result := ( Bottom >= (itemToTest.Y + itemToTest.Height));
End;

function TI2XProductItems.AppendOCRResultItem( TemplateRegions : CTemplate;
  OCRResultItem: TI2XOCRItem): boolean;
var
  item : TI2XProductItem;
  LastItem : TI2XItem;
  row : TI2XRow;
  rows : TI2XRows;
  subEleID, eleID : string;
  eleSub, ele : CEleNodeData;
  PixeslBtwChar, PixelsOfSpaceChar : integer;
  SpacePadCount, AvgPixeslBtwChar : Cardinal;
  DataExt : TI2XOCRItemExt;
  sLastAction : string;
Begin
  try
    sLastAction := 'Initialzing';
    Result := false;
    SpacePadCount := 0;
    //dbg('ItemAsXML=' + OCRResultItem.AsXML() );
    //if ( (OCRResultItem.x = 121) and (OCRResultItem.y = 214 ))  then
    //  dbg( 'VALUE', 'VALUE=STOPPING1' );

    sLastAction := 'Get element';
    ele := TemplateRegions.getElement( OCRResultItem.X, OCRResultItem.Y, OCRResultItem.Width, OCRResultItem.Height );
    if ( ele <> nil ) then begin

      eleID := ele.ID;
      sLastAction := 'eleID=' + eleID;
      if ( ele.SubNodes.Count = 0 ) then begin           //If this is a single field
        //Make sure the node exists
        sLastAction := eleID + ' is a Single Field element ';
        if ( not self.ElementIDExists( eleID ) ) then begin
          with OCRResultItem do begin
            sLastAction := eleID + ' does not exist.. creating ';
            item := TI2XItem.Create( OCRResultItem.X, OCRResultItem.Y,
                OCRResultItem.Height, OCRResultItem.Width,
                OCRResultItem.Accuracy, OCRResultItem.Data );
            item.Element := ele;
            item.Ptr := OCRResultItem;
            item.Obj := TI2XOCRItemExt.Create( 1, OCRResultItem.Width, OCRResultItem.Width, item );
            self.Add( item );
          end;
        end else begin
          sLastAction := eleID + ' does exist ';
          item := TI2XItem(self.FItems[ eleID ]);
          LastItem := item.Ptr;
          DataExt := TI2XOCRItemExt( item.Obj );
          DataExt.CharTotal := DataExt.CharTotal + 1;
        //Append the data to the node,  making sure that you honor the line breaks
          if ( not IsContainedWithin( item, OCRResultItem )) then
          //if ( item.Y + item.Height >
          //  OCRResultItem.Y + OCRResultItem.Height + SAME_LINE_PIXEL_TRESHOLD ) then
          begin
            sLastAction := eleID + ' is not contained with, so add line ';
            PixeslBtwChar := 0;
            item.Data := item.Data + #13#10 + OCRResultItem.Data;
            item.Height := item.Height + (OCRResultItem.X - item.X) -
              OCRResultItem.Height;
            //item.Height := item.Height + ((OCRResultItem.Y + OCRResultItem.Height) - (row.Y + row.Height));
          end else begin
            sLastAction := eleID + ' calculate space between characters';
        //We must figure out the average space between chars to determine if there are any spaces
        // we have to account for...
            DataExt.CharSizeTotal := DataExt.CharSizeTotal + OCRResultItem.Width;
            DataExt.CharSizeTotalWSpaces := DataExt.CharSizeTotalWSpaces  + (OCRResultItem.X - LastItem.X);
            PixeslBtwChar := OCRResultItem.X - (LastItem.X + LastItem.Width);
            SpacePadCount := CalcSpace(PixeslBtwChar, DataExt, ele );

            item.Data := item.Data + StringOfChar( ' ', SpacePadCount ) + OCRResultItem.Data;
            item.Width := item.Width + (OCRResultItem.X - LastItem.X);
            //we do not add to the pixels between char when a space is there because
            //  that space will mess up the calculation
            if ( SpacePadCount = 0 ) then DataExt.AddToPixelsBtwCharTotal( PixeslBtwChar );
          end;
          item.Ptr := OCRResultItem;
        end;
        //dbg('??  item.Height=' + IntToStr(item.Height) );

        result := true;
      end else begin
        sLastAction := eleID + ' is a multi-level field element ';
                                                //If this is a multi field
        if ( not self.ElementIDExists( eleID ) ) then begin   //If the field already exists
          sLastAction := eleID + ' does not already exist ';
          rows := TI2XRows.Create( ele );
          row := TI2XRow.Create();
          rows.Add( row );
          self.Add( rows );
        end else begin
          sLastAction := eleID + ' does exist ';
          rows := TI2XRows( self.Items[ eleID ] );
          // since typed text is usually aligned across,  we should measure my the location + height
          row := rows.FindRow( OCRResultItem.X, OCRResultItem.Y );
          if ( row = nil ) then begin
            row := TI2XRow.Create();
            rows.Add( row );
          end;
          //row := TI2XRow( rows[ rows.Count - 1 ] );
          //row.Add( item );
        end;

        sLastAction := eleID + ' is a parent,  search for sub elements ';
        eleSub := TemplateRegions.getElement( ele.SubNodes, OCRResultItem.X,
             OCRResultItem.Y, OCRResultItem.Width, OCRResultItem.Height );
        if ( eleSub = nil ) then begin
          //If we cannot find an element for this Item,  then we discard it
          Exit;
        end;
        //subEleID := IntToStr( integer( @eleSub ));
        subEleID := eleSub.ID;
        if ( not row.Exists( subEleID ) ) then begin
          sLastAction := eleID + ': ' + subEleID + ' does not previously exist ';
          item := TI2XItem.Create( OCRResultItem.X, OCRResultItem.Y,
            OCRResultItem.Height, OCRResultItem.Width,
            OCRResultItem.Accuracy, OCRResultItem.Data );
          item.Element := eleSub;
          item.Ptr := OCRResultItem;

          item.Obj := TI2XOCRItemExt.Create( 1, OCRResultItem.Width, OCRResultItem.Width, item );
          row.Add( item );
        end else begin
          sLastAction := eleID + ': ' + subEleID + ' does exist ';
  //$$
  //It looks like the row cannout be matched because we are attaching to the end
  // we need to fix it to where we are adding to the correct row..
          //row := rows.FindRow( OCRResultItem.X, OCRResultItem.Y );
          //if ( row = nil ) then begin
          if ( ( row.Y + row.Height + SAME_LINE_PIXEL_TRESHOLD )
                        < ( OCRResultItem.Y ) ) then begin
            sLastAction := eleID + ': ' + subEleID + ' row does exist';
            row := TI2XRow.Create();
            item := TI2XItem.Create( OCRResultItem.X, OCRResultItem.Y,
                OCRResultItem.Height, OCRResultItem.Width,
                OCRResultItem.Accuracy, OCRResultItem.Data );
            item.Element := eleSub;
            item.Ptr := OCRResultItem;

            item.Obj := TI2XOCRItemExt.Create( 1, OCRResultItem.Width, OCRResultItem.Width, item );
            row.Add( item );
            rows.Add( row );
          end else begin
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ';
            item := row.Items[ subEleID ];
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 01';
            LastItem := item.Ptr;
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 02';
            DataExt := TI2XOCRItemExt( item.Obj );
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 03';
            PixeslBtwChar := OCRResultItem.X - (LastItem.X + LastItem.Width);
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 04';
            SpacePadCount := CalcSpace(PixeslBtwChar, DataExt, eleSub );

            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 05';
            item.Data := item.Data + StringOfChar( ' ', SpacePadCount ) + OCRResultItem.Data;
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 06';
            item.Width := item.Width + (OCRResultItem.X - LastItem.X);
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 07';
            if ( (row.Y + row.Height) < (OCRResultItem.Y + OCRResultItem.Height) ) then
              item.Height := item.Height + ((OCRResultItem.Y + OCRResultItem.Height) - (row.Y + row.Height));

            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 08';
            DataExt.CharTotal := DataExt.CharTotal + 1;
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 09';
            DataExt.CharSizeTotal := DataExt.CharSizeTotal + OCRResultItem.Width;
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 10';
            DataExt.CharSizeTotalWSpaces := DataExt.CharSizeTotalWSpaces  + (OCRResultItem.X - LastItem.X);
            //we do not add to the pixels between char when a space is there because
            //  that space will mess up the calculation
            sLastAction := eleID + ': ' + subEleID + ' row exists,  appending ' + ' - Line 11';
            if ( SpacePadCount = 0 ) then DataExt.AddToPixelsBtwCharTotal( PixeslBtwChar );
          end;
        end;
        item.Ptr := OCRResultItem;

        result := true;
      end;
    end;
  except
    on e : Exception do begin
      raise Exception.Create( 'TI2XProductItems.AppendOCRResultItem: Error (' + e.Message + ' ). Last Action=' + sLastAction );
      result := false;
    end;
  end;
End;

function TI2XProductItems.RoundUp( const pValue: extended): int64;
begin
   result := trunc( pValue) + trunc (frac( pValue) * 2);
end;

function TI2XProductItems.CalcSpace( var PixeslBtwChar : integer; DataExt : TI2XOCRItemExt; ele : CEleNodeData ) : integer;
var
  PixelsOfSpaceChar, PixelsRemaining, SpacePadCount : integer;
Begin
  if ( ele.DataType = dtNumeric ) then begin //this assumes no spaces in numbers
    Result := 0;
    Exit;
  end;
  //if there is only 1 character, then we will obviously have no space between itself
  if ( DataExt.FCharTotal = 1 ) then begin
    Result := 0;
    Exit;
  end;
  //if there is only 1 character, then we will obviously have no space between itself
  if ( DataExt.FCharTotal < 4 ) then begin

  end;

//We must figure out the average space between chars to determine if there are any spaces
// we have to account for...
  SpacePadCount := 0;
  if ( PixeslBtwChar < 0 ) then PixeslBtwChar := 0;
  PixelsRemaining := Trunc(PixeslBtwChar - (DataExt.AvgSpaceBtwChar ));
  if ( PixelsRemaining < 0 ) then PixelsRemaining := 0;
  if ( DataExt.AvgCharSize = 0 ) then
    SpacePadCount := 0
  else begin
    //Handle the initial space block,  which we will assume is a little larger
    try
      PixelsOfSpaceChar := Trunc( DataExt.AvgCharSize * 0.55 );
      if ( PixelsRemaining >= PixelsOfSpaceChar ) then
        Inc( SpacePadCount, 1 );
      Dec(PixelsRemaining, PixelsOfSpaceChar);
      if ( PixelsRemaining < 0 ) then PixelsRemaining := 0;
      //Other space are assumed to be a little shorter...
      Inc( SpacePadCount, ( PixelsRemaining div Trunc( (DataExt.AvgCharSize ) * 0.60 ) ) );
      //SpacePadCount := SpacePadCount +
      //  ( PixelsRemaining div Trunc( (DataExt.AvgCharSize ) * 0.55 ) );
    except
      SpacePadCount := 0
    end;
  end;
    //SpacePadCount := ( PixelsOfSpaceChar div RoundUp( (DataExt.AvgCharSize ) * 0.50 ) );
  Result := SpacePadCount;
End;

function TI2XProductItems.FlattenAndSortOCRResults(const OCRDataSource: TI2XOCRResults; var OCRDataTarget : TI2XOCRResults ): boolean;
var
  iResult, iItem : integer;
  ele : CEleNodeData;
  eleID : string;
  OCRResultSource, OCRResultTarget : TI2XOCRResult;
  //OCRItem : TI2XOCRItem;
  res : boolean;
Begin
  Result := false;
  iResult := -1;
  iItem := -1;
  try
    OCRDataTarget.Clear;
    OCRResultTarget := TI2XOCRResult.Create();
    OCRResultTarget.ID :='ocr_result_composite';
    OCRDataTarget.Add( OCRResultTarget );

    for iResult := 0 to OCRDataSource.Count - 1 do begin
      OCRResultSource := OCRDataSource[iResult];
      for iItem := 0 to OCRResultSource.Count - 1 do begin
        OCRResultTarget.Add( OCRResultSource.Items[ iItem ] );
      end;
    end;
    OCRResultTarget.SortByY();
  except
    raise;
  end;
  Result := true;
End;

function TI2XProductItems.ConsumeOCRResults(const Template : CTemplate; const OCRData: TI2XOCRResults ): boolean;
var
  iResult, iItem : integer;
  ele : CEleNodeData;
  eleID : string;
  OCRResult : TI2XOCRResult;
  OCRDataFlat : TI2XOCRResults;
  //OCRItem : TI2XOCRItem;
  Offset : TPoint;
  res : boolean;
Begin
  Result := false;
  iResult := -1;
  iItem := -1;
  try
    try
      if (( OCRData = nil ) or ( OCRData.Count = 0 )) then begin
        Result := true;
        Exit;
      end;
       //if ( OffsetCorrection(Template, OCRData, Offset ) ) then
      //  Template.Offset( Offset.X, Offset.Y );
      if ( OCRData.Count > 1 ) then
        OCRData.SortByX();
      //OCRData.SaveToFile( AppTempDir + '_FLATTENED.xml' );
      for iResult := 0 to OCRData.Count - 1 do begin
        //dbg('iResult=' + IntToStr( iResult ) );
        OCRResult := OCRData[iResult];

        for iItem := 0 to OCRResult.Count - 1 do begin
          //dbg('iItem=' + IntToStr( iItem ) );
         // if (( iResult=0 ) and ( iItem=6 )) then
            //dbg('STOP');

          try
            //self.SaveToFile( AppTempDir + '_BEFOREAPPEND.xml' );
//            if ( Pos('>Geese', self.AsXML() ) > 0 ) then
//              dbg('STOP');

            if ( OCRResult.Items[ iItem ].Accuracy >= self.AccuracyMinimum ) then
              res := self.AppendOCRResultItem( template, OCRResult.Items[ iItem ] )
            else
              dbg('  Item accuracy is less than accuracy minimum,  ignoring item.')
            ;
          except
            on e : Exception do
              raise Exception.Create( 'Error (' + e.Message + ') while consuming Result Item(' + IntToStr(iItem) + '):' + OCRResult.Items[ iItem ].AsXML() );
          end;
        end;

      end;
      Result := true;
    except
      on e : Exception do
        raise Exception.Create( 'Error (' + e.Message + ') while consuming OCRData Structure (' + IntToStr(iResult) + '):' + OCRData[iResult].AsXML() );
    end;
  finally
  end;
End;

constructor TI2XProductItems.Create( const id : string );
begin
  self.FID := id;
  FItems := THashTable.Create;
  FAccuracyMinimum := 0.10;
end;


procedure TI2XProductItems.Delete( KeyOfItemToDelete : string );
begin
  FItems.Delete( KeyOfItemToDelete );
end;

destructor TI2XProductItems.Destroy;
begin
  FreeAndNil( FItems );
  inherited;
end;

function TI2XProductItems.ElementIDExists( const eleID : string ): boolean;
begin
  Result := FItems.ContainsKey( eleID );
end;

function TI2XProductItems.GetAccuracy: Single;
Var
  i : cardinal;
  AccuracyTotal : single;
  item : TI2XProductItem;
  AccuracyList : array of Double;
Begin
  if ( self.Count = 0 ) then begin
    Result := 0;
  end else begin
//Sum up all the accuracies and give us the median
    AccuracyTotal := 0;
    SetLength( AccuracyList, self.Count );
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      AccuracyList[i] := item.Accuracy;
      AccuracyTotal := AccuracyTotal + item.Accuracy;
    end;
    if ( self.Count <= 2 ) then
      Result := ( AccuracyTotal / self.Count )
    else
      Result := Mean( AccuracyList )
  end;
End;

function TI2XProductItems.GetCount: Cardinal;
begin
  Result := FItems.Count;
end;

function TI2XProductItems.GetItem( idx : TIndex): TI2XProductItem;
begin
  Result := TI2XProductItem(FItems.Items[ idx ]);
end;

function TI2XProductItems.GetKeys: TKeyList;
begin
  Result := self.FItems.Keys;
end;

function TI2XProductItems.LoadFromFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  raise Exception.Create( 'Not Implemented yet...' );
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.Load( fileName ) ) then begin
      Result := XMLToProductItems( xmlDoc.XML, self );
    end else begin
      raise Exception.Create('Could not load XML from file ' + fileName + '.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

function TI2XProductItems.XMLToProductItems( const XMLStringData : string;
    ProductItemsObject : TI2XProductItems ) : boolean;
var
  xmlDoc: IXMLDocument;
  nod, nodSlice, nodItem  : IXMLElement;
  oNod : IXMLNode;
  parsed : boolean;
  xmlNodeList, xmlSubNodeList, nodItemList : IXMLNodeList;
  item : TI2XProductItems;
  iImageIdx, iSliceIdx, iItemIdx : integer;
Begin
  try
    Result := false;
    if ( ProductItemsObject = nil ) then
      raise Exception.Create( 'ProductItemsObject must be preallocated before passing to XMLToProductItems()' )
    else
      ProductItemsObject.Clear;
    xmlDoc := CreateXMLDoc;
    parsed := xmlDoc.loadXML( XMLStringData );
    if ( not parsed ) then begin
      raise Exception.Create( 'XML Passed to function could not be parsed.' );
    end;
  finally
  end;
  Result := true;
End;

procedure TI2XProductItems.PutItem( idx : TIndex; const Value: TI2XProductItem);
begin
  FItems.Items[ idx ] := TObject(Value);
end;

function TI2XProductItems.SaveToFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
  str : string;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    str := AsXML();
    if ( xmlDoc.LoadXML( AsXML() ) ) then begin
      xmlDoc.Save( fileName );
      Result := true;
    end else begin
      raise Exception.Create('File ' + fileName + ' could not be loaded on into OCR Results.');
    end;
  finally
  end;
End;

{ TI2XItem }

function TI2XItem.Add( const ItemToAdd: TI2XProductItem ): TI2XProductItem;
begin
  FItems.Add( ItemToAdd.ElementID, TObject(ItemToAdd) );
  Result := TI2XProductItem( FItems[ ItemToAdd.ElementID ] );
  Result.Parent := self;
end;

function TI2XItem.AsXML: string;
var
  sb : TStringBuilder;
  i : cardinal;
  elementName : string;
begin
  try
    if ( self.Element <> nil ) then
      elementName := self.Element.id
    else
      elementName := 'item';
    sb := TStringbuilder.Create;
    sb.Append( '<' );
    sb.Append( elementName );
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
    sb.Append( '>' );
    if ( self.FItems.Count = 0 ) then begin
      sb.Append( self.DataEnc );
    end else begin
      for i := 0 to self.FItems.Count - 1 do begin
        sb.Append( TI2XProductItem(FItems[i]).AsXML() );
      end;
    end;
    sb.Append( '</' );
    sb.Append( elementName );
    sb.Append( '>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TI2XItem.Clear;
begin
  inherited;
  self.FItems.Clear;
end;

procedure TI2XItem.Copy(Source: TI2XProductItem);
Var
  i : integer;
  arrKeyList : TKeyList;
  sKeyName : string;
  SourceItem : TI2XItem;
Begin
  inherited Copy( Source );
  if ( Source.ClassName = 'TI2XItem') then begin
    SourceItem := TI2XItem( Source );
    with SourceItem do begin
      self.Clear;
      arrKeyList := SourceItem.Keys;
      for i := 0 to Length(arrKeyList) - 1 do begin
        sKeyName := arrKeyList[i];
        self.Add( TI2XProductItem( SourceItem[ sKeyName ] ) );
      end;
    end;
  end;
End;

procedure TI2XItem.CopyTo(Target: TI2XProductItem);
Var
  i : integer;
  arrKeyList : TKeyList;
  sKeyName : string;
  TargetItem : TI2XItem;
Begin
  Target.Clear;
  inherited CopyTo( Target );
  if ( Target.ClassName = 'TI2XItem') then begin
    TargetItem := TI2XItem( Target );
    with TargetItem do begin
      //TargetItem.Clear;
      arrKeyList := self.Keys;
      for i := 0 to Length(arrKeyList) - 1 do begin
        sKeyName := arrKeyList[i];
        TargetItem.Add( TI2XProductItem( Self[ sKeyName ] ) );
      end;
    end;
  end;
End;

constructor TI2XItem.Create(const x, y, height, width: integer;
  const accuracy: single; const data: string);
begin
  self.Create();
  self.X := x;
  self.Y := y;
  self.Width := width;
  self.Height := height;
  self.Accuracy := accuracy;
  self.Data := data;
end;

constructor TI2XItem.Create(ele: CEleNodeData);
begin
  self.Create();
  self.Element := ele;
end;

constructor TI2XItem.Create;
begin
  inherited;
  self.FItems := THashTable.Create;
end;

procedure TI2XItem.Delete(KeyOfItemToDelete: string);
begin
  FItems.Delete( KeyOfItemToDelete );
end;

destructor TI2XItem.Destroy;
begin
  FreeAndNil( FItems );
  inherited;
end;

function TI2XItem.Exists(const eleID: string): boolean;
begin
  Result := FItems.ContainsKey( eleID );
end;

function TI2XItem.getAccuracy: single;
Var
  i : cardinal;
  AccuracyTotal : single;
  item : TI2XProductItem;
  AccuracyList : array of Double;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FAccuracy;
  end else begin
//Sum up all the accuracies and give us the median
    AccuracyTotal := 0;
    SetLength( AccuracyList, self.Count );
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      AccuracyList[i] := item.Accuracy;
      AccuracyTotal := AccuracyTotal + item.Accuracy;
    end;
    if ( self.Count <= 2 ) then
      Result := ( AccuracyTotal / self.Count )
    else
      Result := Mean( AccuracyList )
  end;
End;

function TI2XItem.GetCount: Cardinal;
begin
  Result := FItems.Count;
end;

function TI2XItem.GetItem(idx: TIndex): TI2XProductItem;
begin
  Result := TI2XProductItem( FItems[ idx ] );
end;

function TI2XItem.GetKeys: TKeyList;
begin
  Result := FItems.Keys;
end;

function TI2XItem.getHeight: integer;
Var
  i, cTop, cBottom : cardinal;
  arrKeyList : TKeyList;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FHeight;
  end else begin
{Need to get the highest Top and the Lowest bottom,  the difference of these will
 yield the hieght of this multi element}
    cTop := 9999999;
    cBottom := 0;
    //arrKeyList := self.Keys;
    //for i := 0 to Length(arrKeyList) - 1 do begin
    //  item := TI2XProductItem( self[ arrKeyList[i] ]);
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.Y < cTop ) then
        cTop := item.Y;
      if (  ( item.Y + item.height ) > cBottom ) then
        cBottom := ( item.Y + item.height );
    end;
    if (( cBottom - cTop ) < 0 ) then
      raise Exception.Create( 'Height of element (id=' + self.ElementID + ') was less than 0 (' + IntToStr( cBottom - cTop ) + ').' )
    else
      Result := cBottom - cTop;
  end;
End;

function TI2XItem.getWidth: integer;
Var
  i, cLeft, cRight : cardinal;
  arrKeyList : TKeyList;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FWidth;
  end else begin
{ Need to get the left most LEFT and the right most RIGHT and subtract the difference
 in order to obtain the proper width of all the child elements }
    cLeft := 9999999;
    cRight := 0;
    //arrKeyList := self.Keys;
    //for i := 0 to Length(arrKeyList) - 1 do begin
    //  item := TI2XProductItem( self[ arrKeyList[i] ]);
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.X < cLeft ) then
        cLeft := item.X;
      if (  ( item.X + item.Width ) > cRight ) then
        cRight := ( item.X + item.Width );
    end;
    if (( cRight - cLeft ) < 0 ) then
      raise Exception.Create( 'Width of multi element (id=' + self.ElementID + ') was less than 0 (' + IntToStr( cRight - cLeft ) + ').' )
    else
      Result := cRight - cLeft;
  end;
End;

function TI2XItem.getX: integer;
Var
  i, cX : cardinal;
  arrKeyList : TKeyList;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FX;
  end else begin
//Need to get the left most X
    cX := 99999999;
    //arrKeyList := self.Keys;
    //for i := 0 to Length(arrKeyList) - 1 do begin
    //  item := TI2XProductItem( self[ arrKeyList[i] ]);
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.X < cX ) then
        cX := item.x;
    end;
    Result := cX;
  end;
End;

function TI2XItem.getY: integer;
Var
  i, cY : cardinal;
  arrKeyList : TKeyList;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.Fy;
  end else begin
//Need to get the top most Y
    cY := 9999999;
    //arrKeyList := self.Keys;
    //for i := 0 to Length(arrKeyList) - 1 do begin
    //  item := TI2XProductItem( self[ arrKeyList[i] ]);
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.y < cY ) then
        cY := item.y;
    end;
    Result := cY;
  end;
End;

procedure TI2XItem.PutItem(idx: TIndex; const Value: TI2XProductItem);
begin
  FItems[ idx ] := TObject( Value );
end;

{ TI2XRows }

function TI2XRows.Add(const ItemToAdd: TI2XProductItem): TI2XProductItem;
begin
  self.FRows.Add( ItemToAdd );
  Result := TI2XProductItem( FRows[ self.Count - 1 ] );
  Result.Parent := self;
end;

function TI2XRows.AsXML: string;
Var
  sb : TStringBuilder;
  i : cardinal;
  elementName : string;
Begin
  try
    if ( self.Element <> nil ) then
      elementName := self.Element.id
    else
      elementName := 'item';
    sb := TStringbuilder.Create;
    sb.Append( '<' );
    sb.Append( elementName );
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
    sb.Append( '>' );
    if ( self.FRows.Count = 0 ) then begin
      sb.Append( self.DataEnc );
    end else begin
      if ( self.FRows.Count > 0 ) then begin
        for i := 0 to self.FRows.Count - 1 do begin
          sb.Append( TI2XRow( FRows[i] ).AsXML() );
        end;
      end;
    end;
    sb.Append( '</' );
    sb.Append( elementName );
    sb.Append( '>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
End;

procedure TI2XRows.Clear;
begin
  inherited;
  self.FRows.Clear;
end;

procedure TI2XRows.Copy( Source: TI2XProductItem );
Var
  i : integer;
  SourceRows : TI2XRows;
Begin
  inherited Copy( Source );
  if ( Source.ClassName = 'TI2XRows') then begin
    SourceRows := TI2XRows( Source );
    with SourceRows do begin
      self.Clear;
      for i := 0 to SourceRows.Count - 1 do begin
        self.Add( TI2XProductItem( SourceRows[ i ] ) );
      end;
    end;
  end;
End;

procedure TI2XRows.CopyTo(Target: TI2XProductItem);
Var
  i : integer;
  TargetRows : TI2XRows;
Begin
  inherited CopyTo( Target );
  if ( Target.ClassName = 'TI2XRows') then begin
    TargetRows := TI2XRows( Target );
    with TargetRows do begin
      TargetRows.Clear;
      for i := 0 to TargetRows.Count - 1 do begin
        TargetRows.Add( TI2XProductItem( Self[ i ] ) );
      end;
    end;
  end;
End;

constructor TI2XRows.Create(ele: CEleNodeData);
begin
  self.Create();
  self.Element := ele;
end;

constructor TI2XRows.Create(const x, y, height, width: integer;
  const accuracy: single; const data: string);
begin
  self.Create();
  self.X := x;
  self.Y := y;
  self.Width := width;
  self.Height := height;
  self.Accuracy := accuracy;
  self.Data := data;
end;

constructor TI2XRows.Create;
begin
  inherited;
  FRows := TObjectList.Create;
end;

procedure TI2XRows.Delete(IndexToDelete: Integer);
begin
  FRows.Delete( IndexToDelete );
end;

destructor TI2XRows.Destroy;
begin
  FreeAndNil( FRows );
  inherited;
end;

function TI2XRows.FindRow( const x, y : integer ): TI2XRow;
const
  REGION_PIXEL_PADDING = 5;
Var
  i : cardinal;
  item : TI2XProductItem;
  row : TI2XRow;
Begin
  Result := nil;
  row := nil;
  if ( self.Count > 0 ) then begin
    for i := 0 to self.Count - 1 do begin
      row := TI2XRow( self[ i ]);
      {*
      dbg( 'row=' + row.AsXml());
      dbg( 'X=' + IntToStr( x ) +
            '  ( row.X - REGION_PIXEL_PADDING )=' + IntToStr(( row.X - REGION_PIXEL_PADDING )) +
            '  ( row.X + row.Width + REGION_PIXEL_PADDING )=' + IntToStr(( row.X + row.Width + REGION_PIXEL_PADDING )) +
            '  Y=' + IntToStr(Y) +
            '  ( row.Y - REGION_PIXEL_PADDING )=' + IntToStr(( row.Y - REGION_PIXEL_PADDING )) +
            '  ( row.Y + row.Height + REGION_PIXEL_PADDING )=' + IntToStr(( row.Y + row.Height + REGION_PIXEL_PADDING ))
            );
      *}
      if (
          (( x >= ( row.X - REGION_PIXEL_PADDING ) ) and
           ( x <= ( row.X + row.Width + REGION_PIXEL_PADDING )) ) and
          (( y >= ( row.Y - REGION_PIXEL_PADDING ) ) and
           ( y <= ( row.Y + row.Height + REGION_PIXEL_PADDING )) )
        )
      then begin
        Result := row;
        break;
      end;
    end;
    if (( Result = nil ) and ( self.Element <> nil )) then begin
      //usually when we are finding a row,  we want to incliude the fact that
      // the element width is what we want to search for.  This is mainly used when
      //  constructing the output of the best elements
      for i := 0 to self.Count - 1 do begin
        row := TI2XRow( self[ i ]);
        {*
        dbg( 'row=' + row.AsXml());
        dbg( 'X=' + IntToStr( x ) +
            '  ( row.X - REGION_PIXEL_PADDING )=' + IntToStr(( row.X - REGION_PIXEL_PADDING )) +
            '  ( row.X + self.Element.Width + REGION_PIXEL_PADDING )=' + IntToStr(( row.X + self.Element.Width + REGION_PIXEL_PADDING )) +
            '  Y=' + IntToStr(Y) +
            '  ( row.Y - REGION_PIXEL_PADDING )=' + IntToStr(( row.Y - REGION_PIXEL_PADDING )) +
            '  ( row.Y + row.Height + REGION_PIXEL_PADDING )=' + IntToStr(( row.Y + row.Height + REGION_PIXEL_PADDING ))
            );
        *}
        if (
            (( x >= ( row.X - REGION_PIXEL_PADDING ) ) and
             ( x <= ( row.X + self.Element.Width )) ) and
            (( y >= ( row.Y - REGION_PIXEL_PADDING ) ) and
             ( y <= ( row.Y + row.Height + REGION_PIXEL_PADDING )) )
           )
        then begin
          Result := row;
          break;
        end;
      end;
    end;

  end;
End;

function TI2XRows.GetRow(idx: Integer): TI2XRow;
begin
  Result := TI2XRow( self.FRows[ idx ] );
end;

function TI2XRows.GetRowCount: Cardinal;
begin
  Result := self.FRows.Count;
end;

procedure TI2XRows.PutRow(idx: Integer; const Value: TI2XRow);
begin
  Self.FRows[ idx ] := Value;
end;

function TI2XRows.getAccuracy: single;
Var
  i : cardinal;
  AccuracyTotal : single;
  item : TI2XProductItem;
  AccuracyList : array of Double;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FAccuracy;
  end else begin
//Sum up all the accuracies and give us the median
    AccuracyTotal := 0;
    SetLength( AccuracyList, self.Count );
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      AccuracyList[i] := item.Accuracy;
      AccuracyTotal := AccuracyTotal + item.Accuracy;
    end;
    if ( self.Count <= 2 ) then
      Result := ( AccuracyTotal / self.Count )
    else
      Result := Mean( AccuracyList )
  end;
End;

function TI2XRows.getHeight: integer;
Var
  i, cTop, cBottom : cardinal;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FHeight;
  end else begin
{Need to get the highest Top and the Lowest bottom,  the difference of these will
 yield the hieght of this multi element}
    cTop := 9999999;
    cBottom := 0;
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.Y < cTop ) then
        cTop := item.Y;
      if (  ( item.Y + item.height ) > cBottom ) then
        cBottom := ( item.Y + item.height );
    end;
    if (( cBottom - cTop ) < 0 ) then
      raise Exception.Create( 'Height of element (id=' + self.ElementID + ') was less than 0 (' + IntToStr( cBottom - cTop ) + ').' )
    else
      Result := cBottom - cTop;
  end;
End;

function TI2XRows.getWidth: integer;
Var
  i, cLeft, cRight : cardinal;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FWidth;
  end else begin
{ Need to get the left most LEFT and the right most RIGHT and subtract the difference
 in order to obtain the proper width of all the child elements }
    cLeft := 9999999;
    cRight := 0;
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.X < cLeft ) then
        cLeft := item.X;
      if (  ( item.X + item.Width ) > cRight ) then
        cRight := ( item.X + item.Width );
    end;
    if (( cRight - cLeft ) < 0 ) then
      raise Exception.Create( 'Width of element (id=' + self.ElementID + ') was less than 0 (' + IntToStr( cRight - cLeft ) + ').' )
    else
      Result := cRight - cLeft;
  end;
End;

function TI2XRows.getX: integer;
Var
  i, cX : cardinal;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.FX;
  end else begin
//Need to get the left most X
    cX := 99999999;
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.X < cX ) then
        cX := item.x;
    end;
    Result := cX;
  end;
End;

function TI2XRows.getY: integer;
Var
  i, cY : cardinal;
  item : TI2XProductItem;
Begin
  if ( self.Count = 0 ) then begin
    Result := self.Fy;
  end else begin
//Need to get the top most Y
    cY := 9999999;
    for i := 0 to self.Count - 1 do begin
      item := TI2XProductItem( self[ i ]);
      if ( item.y < cY ) then
        cY := item.y;
    end;
    Result := cY;
  end;
End;


{ TI2XRow }

function TI2XRow.AsXML: string;
Var
  sb : TStringBuilder;
  i : integer;
Begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<items' );
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
    sb.Append( '>');

    for i := 0 to self.FItems.Count - 1 do begin
      sb.Append( TI2XProductItem(FItems[i]).AsXML() );
    end;

    sb.Append( '</items>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
End;

constructor TI2XRow.Create;
begin
  inherited;

end;

{ TI2XProductItemCompare }

function TI2XProductItemCompare.Add(
  const ItemToAdd: TI2XProductItems): string;
begin
  FItems.Add( ItemToAdd.ID,  ItemToAdd );
  Result := ItemToAdd.ID;
end;

constructor TI2XProductItemCompare.Create( const Template : CTemplate );
begin
  FItems := THashTable.Create;
  FItems.FreeObjectsOnDestroy := false;
  self.FTemplate := Template;
end;

destructor TI2XProductItemCompare.Destroy;
begin
  FreeAndNil( FItems );
  inherited;
end;

function TI2XProductItemCompare.AppendMostAccurateResultSingle( ele: CEleNodeData; ResultItems : TI2XProductItems ) : boolean;
var
  item : TI2XItem;
Begin
  try
    item := TI2XItem.Create( ele );
    TI2XItem(GetMostAccurateResultItem( ele )).CopyTo( item );
    resultItems.Add( item );
    Result := true;
  except
    Result := false;
  end;
End;

procedure TI2XProductItemCompare.InitKVList(
  var accList : TKeyValueExtendedList );
var
  i : smallint;
Begin
  SetLength( accList, self.Count );
  //Initialize the accuracy list
  for i := 0 to Length(accList) - 1 do
    accList[i].Val := 0;
End;

procedure TI2XProductItemCompare.InitKVList(
  var accList : TPtrAccuracyList );
var
  i : smallint;
Begin
  SetLength( accList, self.Count );
  //Initialize the accuracy list
  for i := 0 to Length(accList) - 1 do begin
    accList[i].Ptr := nil;
    accList[i].Val := 0;
  end;
End;

function TI2XProductItemCompare.GetMostAccurateResultItem( ele: CEleNodeData ) : TI2XOCRItem;
var
  //accList : TKeyValueExtendedList;
  //kv : TKeyValueExtended;
  accList : TPtrAccuracyList;
  kv : TPtrAccuracy;

  i, iProdItems : smallint;
  prodItems : TI2XProductItems;
  rows : TI2XRows;
  row : TI2XRow;
  //item : TI2XItem;
Begin
  InitKVList( accList );

  //Go through the results see if an product of that element exists
  for iProdItems := 0 to self.Count - 1 do begin
    prodItems := self.items[ iProdItems ];
    if ( prodItems.ElementIDExists( ele.ID ) ) then begin
      //If a product does exist,  save off the 'TEST ID' and the accuracy
      //accList[iProdItems].Key := prodItems.ID;
      accList[iProdItems].Ptr := prodItems[ele.ID];
      accList[iProdItems].Val := prodItems[ele.ID].Accuracy;
    end;
  end;
  //Get the highest accuracy and return it
  kv := uStrUtil.MaxValueP( accList );
  result := kv.Ptr;
  //result := self[ kv.Key ].Items[ ele.ID ];
End;

function TI2XProductItemCompare.GetMostAccurateResultItem( ele: CEleNodeData; rowSource : TI2XRow ) : TI2XOCRItem;
var
  accList : TPtrAccuracyList;
  kv : TPtrAccuracy;
  i, iProdItems : smallint;
  prodItems : TI2XProductItems;
  rows : TI2XRows;
  row : TI2XRow;
  item : TI2XItem;
  eleParent: CEleNodeData;
Begin
  InitKVList( accList );

  rows := TI2XRows( rowSource.Parent );
  eleParent := rows.Element;
  //Go through the results see if an product of that element exists
  for iProdItems := 0 to self.Count - 1 do begin
    prodItems := self.items[ iProdItems ];

    if ( prodItems.ElementIDExists( eleParent.ID ) ) then begin
      rows := TI2XRows( prodItems[ eleParent.ID ] );
      row := rows.FindRow( rowSource.X, rowSource.Y );
      if ( row <> nil ) then begin
        if ( row.Exists( ele.ID ) ) then begin
          item := TI2XItem( row[ ele.ID ] );
          //If a product does exist,  save off the 'TEST ID' and the accuracy
          accList[iProdItems].Ptr := item;
          accList[iProdItems].Val := item.Accuracy;
        end;
      end;
    end;

  end;
  //Get the highest accuracy and return it
  kv := uStrUtil.MaxValueP( accList );
  result := kv.Ptr;
End;

function TI2XProductItemCompare.CompareRecusively(
  ResultItems : TI2XProductItems; ProductItem, Parent : TI2XProductItem ) : boolean;
var
  accList : TKeyValueExtendedList;
  kv : TKeyValueExtended;
  item, itemNew, itemMostAccurate : TI2XItem;
  row, rowNew, rowRes : TI2XRow;
  rows, rowsNew : TI2XRows;
  i, iRow, iItems : integer;
  bRes : boolean;
Begin
  bRes := false;
  if ( ProductItem.ClassNameIs('TI2XItem') ) then begin
      Item := TI2XItem( ProductItem );
      //If there is not parent control for this item,  then it is a top level item
      if ( Parent = nil ) then begin
            //if the element does not already exists in the results,
            //  then lets go accross the results to find comparable ones
            //  so we will ignore it
        if ( not ResultItems.ElementIDExists( Item.Element.ID ) ) then begin
          itemNew := TI2XItem.Create( Item.Element );
          itemMostAccurate := TI2XItem(GetMostAccurateResultItem( Item.Element ));
          itemMostAccurate.CopyTo( itemNew );
          itemNew.Element := Item.Element;
          ResultItems.Add( itemNew );
          Result := true;
        end;
      end else begin    //This must be an element with a parent of type I2XRow,
                        //  We can use this to figure out if the other tests have
                        //  text in the same location,  and compare the results
        row := TI2XRow( Parent );
        rowsNew := TI2XRows( ResultItems[ Parent.Parent.Element.ID ] );
        //rowNew := rowsNew.FindRow( row.X, row.Y );
        rowNew := rowsNew.FindRow( Item.X, ( Item.Y + Item.Height ) );
        itemMostAccurate := TI2XItem(GetMostAccurateResultItem( Item.Element, row ));
        if ( rowNew = nil ) then begin  //this row does not exist in the target results yet...
          rowNew := TI2XRow.Create();
          rowsNew.Add( rowNew );
        end;
        if ( not rowNew.Exists( Item.Element.ID ) ) then begin //this row does not contain this element yet
          itemNew := TI2XItem.Create( Item.Element );
          itemMostAccurate.CopyTo( itemNew );
          itemNew.Element := Item.Element;
          rowNew.Add( itemNew );
        end;
        Result := true;
      end;
  end else if ( ProductItem.ClassNameIs('TI2XRows') ) then begin
      Rows := TI2XRows( ProductItem );
      if ( not ResultItems.ElementIDExists( Rows.Element.ID ) ) then begin
        rowsNew := TI2XRows.Create( Rows.Element );
        ResultItems.Add( rowsNew );
      end;

      for iRow := 0 to Rows.Count - 1 do begin
        bRes := self.CompareRecusively( ResultItems, Rows[ iRow ], ProductItem );
      end;
  end else if ( ProductItem.ClassNameIs('TI2XRow') ) then begin
      Row := TI2XRow( ProductItem );

      for iItems := 0 to Row.Count - 1 do begin
        bRes := self.CompareRecusively( ResultItems, Row[ iItems ], Row );
      end;

  end;
  Result := bRes;
End;


function TI2XProductItemCompare.CompareRecusively(
  SourceResultItems, BestResultItems: TI2XProductItems ): boolean;
var
  iProdItems : cardinal;
  oProdItems : TI2XProductItems;
  iItem : integer;
  res : boolean;
begin
  for iItem := 0 to SourceResultItems.Count - 1 do begin
    res := CompareRecusively( BestResultItems, SourceResultItems[ iItem ], nil );
  end;

  Result := res;
end;

function TI2XProductItemCompare.CompareRecusively(
  BestResultItems: TI2XProductItems): boolean;
var
  iProdItems : cardinal;
begin
  for iProdItems := 0 to self.Count - 1 do begin
    result := CompareRecusively( self[iProdItems], BestResultItems );
  end;
end;

function TI2XProductItemCompare.CompareResults( ResultItems : TI2XProductItems ) : boolean;
var
  ele : CEleNodeData;
  i, iElements, iProdItems : cardinal;
  arrElementList : TKeyList;
  iKeyCount : smallint;
Begin
  if ( resultItems = nil )  then
    raise Exception.Create('resultItems must be preallocated');
  if ( FTemplate = nil ) then
    raise Exception.Create('Template must not be null!  How can we figure out what to expect in the compare?');
  if ( FItems.Count = 0 ) then
    raise Exception.Create('FItems (which really is an array of TI2XProductItems) needs to have some product items to compare');
  if ( FItems.Count = 1 ) then begin
    ResultItems.Assign( TI2XProductItems( FItems[0] ) );
  end else begin
    arrElementList := FTemplate.Elements.Keys;
    for iElements := 0 to Length( arrElementList ) - 1 do begin
      //Obtain an element from the the template
      ele := CEleNodeData( FTemplate.Elements.Get( arrElementList[ iElements ] ));
      if (( ele <> nil ) and ( Copy(ele.ID, 0, 2) <> STANDARD_REGION_PREFIX )) then begin

        if ( ele.SubNodes.Count = 0 ) then begin
          AppendMostAccurateResultSingle( ele,  ResultItems );
        end else begin
          //AppendMostAccurateResultMulti( ele,  ResultItems );
        end;

      end;
    end;
  end;
End;

function TI2XProductItemCompare.GetItem(idx: TIndex): TI2XProductItems;
begin
  Result := TI2XProductItems( FItems[ idx ] );
end;

function TI2XProductItemCompare.GetItemsCount: integer;
begin
  Result := self.FItems.Count;
end;

procedure TI2XProductItemCompare.PutItem(idx: TIndex;
  const Value: TI2XProductItems);
begin
  FItems[ idx ] := Value;
end;

{ TI2XOCRItemExt }

procedure TI2XOCRItemExt.AddToPixelsBtwCharTotal(const val: cardinal);
begin
  Inc( self.FPixelsBtwCharTotalCount );
  Inc( self.FPixelsBtwCharTotal, val );
end;

constructor TI2XOCRItemExt.Create;
begin
  FCharTotal := 0;
  FCharSizeTotal := 0;
  FCharSizeTotalWSpaces := 0;
  self.FPixelsBtwCharTotalCount := 0;
  self.FPixelsBtwCharTotal := 0;
end;

constructor TI2XOCRItemExt.Create(const CharTotal, CharSizeTotal,
  CharSizeTotalWSpaces: integer; Item : TI2XProductItem);
begin
  FCharTotal := CharTotal;
  FCharSizeTotal := CharSizeTotal;
  FCharSizeTotalWSpaces := CharSizeTotalWSpaces;
  self.FPixelsBtwCharTotalCount := 0;
  self.FPixelsBtwCharTotal := 0;
  self.FItem := Item;
end;

function TI2XOCRItemExt.getAvgCharSize: integer;
begin
  if ( FCharTotal = 0 ) then
    Result := 0
  else
    Result := Round(FCharSizeTotal / FCharTotal) + AVG_CHAR_SIZE_PADDING;
end;

function TI2XOCRItemExt.getAvgCharSizeSpace: integer;
begin
  if ( FCharTotal = 0 ) then
    Result := 0
  else
    Result := Round(FCharSizeTotalWSpaces / FCharTotal);
end;

function TI2XOCRItemExt.getAvgSpaceBtwChar: integer;
begin
  if ( FPixelsBtwCharTotal = 0 ) then
    Result := 0
  else
    Result := Round( FPixelsBtwCharTotal /  FPixelsBtwCharTotalCount );
  if (( FCharTotal < 3 ) and ( Result = 0 )) then
    //If we do not have a lot of character as a sample, and we calculate that we have
    // no space between characters,  that doesn't make sense,  we should always have
    // SOME sort of space between charcters,  so add some sort of default
    Result := 4;
end;

END.
