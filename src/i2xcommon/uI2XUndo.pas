unit uI2XUndo;

interface

uses
  SysUtils,
  Classes,
  contnrs,
  Controls,
  StdCtrls,
  GR32_RangeBars,
  GR32_Image,
  ComCtrls,
  uStrUtil,
  Types,
  Grids
  ;

const
  UNDO_FILE_PREFIX='~UNDO';

type
  TUndoAction = (
    udNone = 0,
    udControlChange,
    udFileLoad,
    udImageLoad
  );
  TRegionUndoAction = (
    ruRegionMove,
    ruRegionAdd,
    ruRegionDelete,
    ruRegionRename
  );
  TUndoItem = class( TObject )
    //private
    public
      Parent : TObject;
      Action : TUndoAction;
      Ctl : TObject;
      Descr : string;
      OldValue : string;
      procedure Undo(); dynamic;
  end;
  TUndoImageItem = class( TUndoItem )
    //private
    public CachedImageFileName : string;
    procedure Undo(); override;
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;
  TUndoRegionItem = class( TUndoItem )
    //private
    public
      Action : TRegionUndoAction;
      EleNodeDataXMLImage : string;
      NamePath : string;
      Template : TObject;
      procedure Undo(); override;
      constructor Create(); virtual;
      destructor Destroy(); override;
  end;
  TUndoStack = class(TStack)
  public
    function Push( AObject: TUndoItem ): TUndoItem;
    function Pop: TUndoItem;
    function Peek: TUndoItem;
    procedure Clear();
    destructor Destroy(); override;
  end;
  TRequestRegionActionEvent = procedure(Sender: TObject; const Action : TRegionUndoAction; const ElementXMLImage : string; const NamePath : string ) of object;
  TRequestActionGridUpdateEvent = procedure(Sender: TObject; const Data : string ) of object;
  TUndoDebugEvent = procedure( Sender: TObject; DebugMessage : string ) of object;
  TI2XUndo = class(TObject)
  private
    FTempPath : string;
    FIsUndoing : boolean;
    FIsRedoing : boolean;
    undoStack : TUndoStack;
    redoStack : TUndoStack;
    lastUndoItem : TUndoItem;
    lastRedoItem : TUndoItem;
    prepUndoItem : TUndoItem;
    FOnChange : TNotifyEvent;
    FOnRequestRegionAction : TRequestRegionActionEvent;
    FOnRequestActionGridUpdate : TRequestActionGridUpdateEvent;
    FOnDebug : TUndoDebugEvent;
    FActiveUndoItem : TUndoItem;
    function GetCount: integer;
    function GetRedoAvailable: boolean;
    function GetUndoAvailable: boolean;
    procedure DoOnChange();
    procedure dbg( const DebugMessage : string );
  public
    procedure Prep( ctl : TObject; const OldValue : string; const ForcePrepChange : boolean = false );
    procedure Add( ctl : TObject; const CurrentValue : string; const ForcePrep : boolean = false ); overload;
    procedure Add( ctl : TImgView32 ); overload;
    procedure Add( sg: TStringGrid); overload;
    procedure Add( region : TObject; action : TRegionUndoAction; template : TObject; const ForcePrep : boolean = false ); overload;
    procedure Undo();
    procedure Redo();
    procedure Clear();
    property Count : integer read GetCount;
    property UndoAvailable : boolean read GetUndoAvailable;
    property RedoAvailable : boolean read GetRedoAvailable;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnDebug : TUndoDebugEvent read FOnDebug write FOnDebug;
    property ActiveUndoItem : TUndoItem read FActiveUndoItem;
    property IsUndoing : boolean read FIsUndoing;
    property IsRedoing : boolean read FIsRedoing;
    property TempPath : string read FTempPath write FTempPath;
    procedure RequestRegionAction( const Action : TRegionUndoAction; const ElementXMLImage : string; const NamePath : string );
    procedure RequestActionGridUpdate( Grid : TObject; const Data : string );
    property OnRequestRegionAction : TRequestRegionActionEvent read FOnRequestRegionAction write FOnRequestRegionAction;
    property OnRequestActionGridUpdate : TRequestActionGridUpdateEvent read FOnRequestActionGridUpdate write FOnRequestActionGridUpdate;
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;
var
  UndoTempPath : string;
  Undo : TI2XUndo;
  UndoItemFocus : TUndoItem;

//function UpdateUndoItemFocus( ctl : TObject; val : string) : boolean;

implementation
uses
  uI2XTemplate, uImage2XML;

function SetFocusOnControl(ctl: TWinControl): boolean;
// This function will force the particular Class Types that we are interested in
//  to be active before we force a control to be active
var
  ctrl, parentCtl, topCtl : TWinControl;
  nLastActiveIndex : integer;
  //sLastActivePage
  bHasFocus : boolean;
begin
  try
    Result := false;
    topCtl := nil;
    parentCtl := ctl.Parent;
    nLastActiveIndex := -1;
    while ( parentCtl <> nil ) do begin
      if ( parentCtl.ClassName = 'TTabSheet' ) then
        nLastActiveIndex := TTabSheet(parentCtl).TabIndex;
      if (( parentCtl.ClassName = 'TPageControl' ) and ( nLastActiveIndex > -1 )) then begin
        TPageControl(parentCtl).ActivePageIndex := nLastActiveIndex;
        nLastActiveIndex := -1;
      end;
      parentCtl := parentCtl.Parent;
    end;
    ctl.SetFocus();
    Result := true;
  finally

  end;
end;


{ TUndoStack }

procedure TUndoStack.Clear;
var
  item : TUndoItem;
  i: integer;
begin
  for i := self.List.Count -1 downto 0 do begin
    TUndoItem( List.Items[i] ).Free;
    List.Delete( i );
  end;
end;

destructor TUndoStack.Destroy;
begin
  Clear;
  inherited;
end;

function TUndoStack.Peek: TUndoItem;
begin
  Result := TUndoItem(inherited Peek);
end;

function TUndoStack.Pop: TUndoItem;
begin
  Result := TUndoItem(inherited Pop);
end;

function TUndoStack.Push(AObject: TUndoItem): TUndoItem;
begin
  Result := TUndoItem(inherited Push(AObject));
end;

{ TUndoItem }

procedure TUndoItem.Undo;
var
  edt : TEdit;
  cbo : TComboBox;
  sld : TGaugeBar;
  mem : TMemo;
  sg  : TStringGrid;
  oUndoParent : TI2XUndo;
begin
  if ( ctl <> nil ) then begin
    oUndoParent := TI2XUndo( self.Parent );
    if ( TObject(ctl).ClassName = 'TEdit' ) then begin
      edt := TEdit(ctl);
      SetFocusOnControl( edt );
      edt.Text := self.OldValue;
      edt.SelectAll();
    end else 
    if ( TObject(ctl).ClassName = 'TComboBox' ) then begin
      cbo := TComboBox(ctl);
      SetFocusOnControl( cbo );
      cbo.Text := self.OldValue;
    end else 
    if ( TObject(ctl).ClassName = 'TMemo' ) then begin
      mem := TMemo(ctl);
      SetFocusOnControl( mem );
      mem.Lines.Text := self.OldValue;
      //mem.SelStart := Length( mem.Lines.Text );
      mem.SelectAll();
    end else 
    if ( TObject(ctl).ClassName = 'TGaugeBar' ) then begin
      sld := TGaugeBar(ctl);
      SetFocusOnControl( sld );
      sld.Position := StrToInt( self.OldValue );
    end else 
    if ( TObject(ctl).ClassName = 'TStringGrid' ) then begin
      sg := TStringGrid(ctl);
      SetFocusOnControl( sg );
      oUndoParent.RequestActionGridUpdate( self.Ctl, self.OldValue );
    end;
  end;

end;

{ TI2XUndo }

procedure TI2XUndo.Add(ctl: TObject; const CurrentValue : string; const ForcePrep : boolean = false);
var
  undoItem : TUndoItem;
begin
  if ( not ( self.FIsUndoing or self.FIsRedoing )) then begin
    if ( ForcePrep ) then
      self.Prep( ctl, CurrentValue );
    if ( prepUndoItem.Ctl = ctl ) then begin
      if (( lastUndoItem = nil ) or
          (  TObject(ctl) <> lastUndoItem.Ctl )) then begin
        undoItem := TUndoItem.Create();
        undoItem.Ctl := ctl;
        undoItem.Action := udControlChange;
        undoItem.OldValue := prepUndoItem.OldValue;
        lastUndoItem := self.undoStack.Push( undoItem );
        lastUndoItem.Parent := self;
        DoOnChange();
      end;
    end;
  end;
end;

procedure TI2XUndo.Add(sg: TStringGrid);
var
  undoItem : TUndoItem;
begin
  if ( not ( self.FIsUndoing or self.FIsRedoing )) then begin
    if ( prepUndoItem.Ctl = TObject( sg ) ) then begin
      //Commenting the next 2 lines out will cause the app to add undo for each action 
      //if (( lastUndoItem = nil ) or
      //    ( lastUndoItem.Ctl <> TObject( sg ) )) then begin
        undoItem := TUndoItem.Create();
        undoItem.Action := udControlChange;
        undoItem.OldValue := prepUndoItem.OldValue;
        undoItem.Ctl := sg;
        lastUndoItem := self.undoStack.Push( undoItem );
        lastUndoItem.Parent := self;
        DoOnChange();
      //end;
    end;
  end;
end;

procedure TI2XUndo.Add(ctl: TImgView32);
var
  undoItem : TUndoImageItem;
  sImageFileNameTemp : string;
begin
  if ( not ( self.FIsUndoing or self.FIsRedoing )) then begin
    sImageFileNameTemp := self.FTempPath + UNDO_FILE_PREFIX + uStrUtil.RandomString(6) + '.BMP';
    ctl.Bitmap.SaveToFile( sImageFileNameTemp );
    undoItem := TUndoImageItem.Create();
    undoItem.CachedImageFileName := sImageFileNameTemp;
    undoItem.Ctl := ctl;
    lastUndoItem := self.undoStack.Push( undoItem );
    lastUndoItem.Parent := self;
    DoOnChange();
  end;
end;

procedure TI2XUndo.Add(region: TObject; action : TRegionUndoAction; template: TObject; const ForcePrep : boolean = false);
var
  undoItem : TUndoRegionItem;
begin
  if ( not ( self.FIsUndoing or self.FIsRedoing )) then begin
    if ( ForcePrep ) then begin
      self.Prep( region, CEleNodeData(region).AsXML( true ), ForcePrep );
      lastUndoItem := nil;
    end;
    if ( prepUndoItem.Ctl = region ) then begin
      if (( lastUndoItem = nil ) or
          ( lastUndoItem.Ctl <> region )) then begin
        undoItem := TUndoRegionItem.Create();
        undoItem.Action := action;
        undoItem.Template := template;
        undoItem.EleNodeDataXMLImage := prepUndoItem.OldValue;
        undoItem.NamePath := CEleNodeData( region ).NamePath;
        undoItem.Ctl := region;
        lastUndoItem := self.undoStack.Push( undoItem );
        lastUndoItem.Parent := self;
        DoOnChange();
      end;
    end;
  end;
end;

procedure TI2XUndo.Clear;
begin
  undoStack.Clear;
  redoStack.Clear;
  lastRedoItem.Free;
  prepUndoItem.Free;
  lastRedoItem := TUndoItem.Create();
  prepUndoItem := TUndoItem.Create();
  FIsUndoing := false;
  FIsRedoing := false;
end;

constructor TI2XUndo.Create;
begin
  undoStack := TUndoStack.Create();
  redoStack := TUndoStack.Create();
  lastRedoItem := TUndoItem.Create();
  prepUndoItem := TUndoItem.Create();
  FIsUndoing := false;
  FIsRedoing := false;
end;

procedure TI2XUndo.dbg(const DebugMessage: string);
begin
  if ( Assigned( self.FOnDebug )) then
    self.FOnDebug( self, DebugMessage );
end;

destructor TI2XUndo.Destroy;
begin
  FreeAndNil( undoStack );
  FreeAndNil( redoStack );
  FreeAndNil( lastRedoItem );
  FreeAndNil( prepUndoItem );
  inherited;
end;

procedure TI2XUndo.DoOnChange;
begin
  if ( Assigned( self.FOnChange )) then begin
    self.FOnChange( self );
  end;
end;

function TI2XUndo.GetCount: integer;
begin
  Result := undoStack.Count;
end;

function TI2XUndo.GetRedoAvailable: boolean;
begin
  Result := ( self.lastRedoItem.Ctl <> nil )
end;

function TI2XUndo.GetUndoAvailable: boolean;
begin
  Result := ( self.undoStack.Count > 0 );
end;

procedure TI2XUndo.Prep(ctl: TObject; const OldValue: string; const ForcePrepChange : boolean = false);
begin
  if ( not ( self.FIsUndoing or self.FIsRedoing )) then begin
    if (( ForcePrepChange ) or
        ( prepUndoItem.Ctl <> ctl )) then begin
      prepUndoItem.Ctl := ctl;
      prepUndoItem.OldValue := OldValue;
    end;
  end;
end;

procedure TI2XUndo.Redo;
var
  redoItem : TUndoItem;
begin
  if ( self.RedoAvailable ) then begin
    try
      FIsRedoing := true;
      redoItem := redoStack.Pop;
      redoItem.Undo;
      FreeAndNil( redoItem );
      DoOnChange();
    finally
      FIsRedoing := false;
    end;
  end;
end;

procedure TI2XUndo.RequestActionGridUpdate(Grid: TObject; const Data: string);
begin
  if ( Assigned( self.FOnRequestActionGridUpdate ) ) then begin
    self.FOnRequestActionGridUpdate( Grid, Data );
  end;
end;

procedure TI2XUndo.RequestRegionAction(const Action: TRegionUndoAction;
  const ElementXMLImage: string; const NamePath : string );
begin
  if ( Assigned( self.FOnRequestRegionAction ) ) then begin
    self.FOnRequestRegionAction( self, Action, ElementXMLImage, NamePath );
  end;
end;

procedure TI2XUndo.Undo;
begin
  if ( self.UndoAvailable ) then begin
    try
      FIsUndoing := true;
      FActiveUndoItem := undoStack.Pop;
      FActiveUndoItem.Undo;
      DoOnChange();
    finally
      FreeAndNil( FActiveUndoItem );
      FIsUndoing := false;
    end;
  end;
end;

{ TUndoImageItem }

constructor TUndoImageItem.Create;
begin
  self.Action := udImageLoad;
end;

destructor TUndoImageItem.Destroy;
begin

  inherited;
end;

procedure TUndoImageItem.Undo;
var
  iv : TImgView32;
begin
  if ( ctl <> nil ) then begin
    if ( TObject(ctl).ClassName = 'TImgView32' ) then begin
      iv := TImgView32(ctl);
      iv.SetFocus();
      iv.Bitmap.LoadFromFile( self.CachedImageFileName );
    end;
  end;
end;

{ TUndoRegionItem }

constructor TUndoRegionItem.Create;
begin

end;

destructor TUndoRegionItem.Destroy;
begin

  inherited;
end;

procedure TUndoRegionItem.Undo;
var
  oUndoParent : TI2XUndo;
  ele : CEleNodeData;
begin
  try
    oUndoParent := TI2XUndo( self.Parent );
    oUndoParent.RequestRegionAction( self.Action, self.EleNodeDataXMLImage, self.NamePath );
  finally
  end;
end;

{*
function UpdateUndoItemFocus( ctl : TObject; val : string) : boolean;
Begin
  if ( TObject(UndoItemFocus.Ctl) = ctl ) then begin
    UndoItemFocus.Ctl := ctl;
  end;
  UndoItemFocus.OldValue := val;
End;
*}

initialization
  Undo := TI2XUndo.Create();
  Undo.TempPath := AppTempDir;
  UndoItemFocus := TUndoItem.Create();

finalization
  FreeAndNil( Undo );
  FreeAndNil( UndoItemFocus );
END.

