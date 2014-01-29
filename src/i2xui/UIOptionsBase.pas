unit UIOptionsBase;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  ComCtrls,
  OmniXML,
  uOmniXML,
  ActnList,
  uI2XConstants,
  //uImage2XML,
  ImgList,
  uStrUtil,
  uHashTable,
  uFileDir;

type
  TOptionsController = class(TObject)
    private
    protected
      FOnChange : TNotifyEvent;
      FIsDirty : boolean;
      procedure OnChangeHandler();
    public
      property OnChange : TNotifyEvent read FOnChange write FOnChange;
      property IsDirty : boolean read FIsDirty write FIsDirty;
      function AsXML() : string; virtual; abstract;
      procedure ApplyChanges(); virtual; abstract;
      constructor Create(); virtual;
      destructor Destroy(); override;
  end;
  TOnOptionsUpdate = procedure( Options : TOptionsController ) of object;
  TOnOptionGroupSelect = procedure( const SelectedGroup : string ) of object;
  TOnTreeNodeCheckBoxClick = procedure( Node :TTreeNode; isChecked : boolean  ) of object;
  TOptionsBaseUI = class(TForm)
    pnlLeft: TPanel;
    pnlRight: TPanel;
    splitter: TSplitter;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnApply: TBitBtn;
    tvOptions: TTreeView;
    ActionList1: TActionList;
    acApply: TAction;
    acCancel: TAction;
    ImageList1: TImageList;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure tvOptionsClick(Sender: TObject);
    procedure tvOptionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject); virtual;
  private
    FOnOptionGroupSelect : TOnOptionGroupSelect;
    FOnTreeNodeCheckBoxClick : TOnTreeNodeCheckBoxClick;
    procedure OptionsChanged( Sender : TObject );
    procedure setOptionsController(const Value: TOptionsController);
    procedure OnTreeNodeCheckBox(Node: TTreeNode; isChecked: boolean);
  protected
    FOptionsController : TOptionsController;
    FOnOptionsUpdate : TOnOptionsUpdate;
    FEventKey : THashStringTable;
    FFileDir : CFileDir;
    procedure XML2Tree(tree: TTreeView; XMLDoc: IXMLDocument);
    Procedure ToggleTreeViewCheckBoxes( Node :TTreeNode; cUnChecked, cChecked,  cRadioUnchecked,  cRadioChecked  :integer );
  public
    procedure OptionGroupSelected( const SelectedGroup : string ); virtual;
    function LoadOptionsAndDisplay( Options : TOptionsController ) : boolean; virtual;
    procedure FormCreate(Sender: TObject);   overload; virtual;
    procedure FormCreate(Sender: TObject; Options : TOptionsController );  overload; virtual;
    property OnOptionGroupSelect : TOnOptionGroupSelect read FOnOptionGroupSelect write FOnOptionGroupSelect;
    property OnOptionsUpdateEvent : TOnOptionsUpdate read FOnOptionsUpdate write FOnOptionsUpdate;
    property OnTreeNodeCheckBoxClickEvent : TOnTreeNodeCheckBoxClick read FOnTreeNodeCheckBoxClick write FOnTreeNodeCheckBoxClick;
    property OptionsController : TOptionsController read FOptionsController write setOptionsController;
  end;

var
  OptionsBaseUI: TOptionsBaseUI;

implementation

{$R *.dfm}

procedure TOptionsBaseUI.XML2Tree(tree: TTreeView;
  XMLDoc: IXMLDocument);
var
  iNode : IXMLNode;

  procedure ProcessNode(
        Node : IXMLNode;
        tn   : TTreeNode);
  var
    cNode : IXMLNode;
    sID, sType, sDesc, sDefault, sEventKey, sTestObjectPtr : string;
  begin
    if Node = nil then Exit;
    with Node do
    begin
      sID := Node.NodeName;
      sType := GetAttr( Node, 'type', '' );
      sDesc := GetAttr( Node, 'desc', '' );
      sDefault := GetAttr( Node, 'default', '' );
      sEventKey := GetAttr( Node, 'eventkey', '' );
      sTestObjectPtr := GetAttr( Node, 'ptr', '0' );

      if ( Length(sDesc) > 0 )  then
        tn := tree.Items.AddChild(tn, sDesc)
      else
        tn := tree.Items.AddChild(tn, sID);
      if ( sTestObjectPtr <> '0' ) then
        tn.Data := pointer(strToInt(sTestObjectPtr));
      if ( sType = 'checkbox' ) then
        if ( uStrUtil.StrToBool( sDefault, 'true', 'false' ) ) then
          tn.StateIndex := cFlatChecked
        else
          tn.StateIndex := cFlatUnCheck;
      self.FEventKey.Add( IntToStr(tn.AbsoluteIndex), sEventKey );

    end;


    cNode := Node.FirstChild;
    while cNode <> nil do
    begin
      ProcessNode(cNode, tn);
      cNode := cNode.NextSibling;
    end;
  end; (*ProcessNode*)
begin
  tree.Items.Clear;
  //XMLDoc.FileName := ChangeFileExt(ParamStr(0),'.XML');
  //XMLDoc.Active := True;

  iNode := XMLDoc.DocumentElement.FirstChild;

  while iNode <> nil do
  begin
    ProcessNode(iNode,nil);
    iNode := iNode.NextSibling;
  end;
End;

procedure TOptionsBaseUI.FormCreate(Sender: TObject);
begin
  FEventKey := THashStringTable.Create();
  self.OnOptionGroupSelect := OptionGroupSelected;
  FFileDir := CFileDir.Create;
end;

procedure TOptionsBaseUI.btnApplyClick(Sender: TObject);
begin
  if ( Assigned( self.FOnOptionsUpdate ) ) then
    FOnOptionsUpdate( self.FOptionsController );
  self.FOptionsController.ApplyChanges;
  self.Hide;
end;

procedure TOptionsBaseUI.btnCancelClick(Sender: TObject);
begin
  self.Hide;
end;

procedure TOptionsBaseUI.FormCreate(Sender: TObject; Options : TOptionsController );
begin
  FEventKey := THashStringTable.Create();
  self.OnOptionGroupSelect := OptionGroupSelected;
  self.LoadOptionsAndDisplay( Options );
  Options.OnChange := self.OptionsChanged;
  btnApply.Enabled := false;
  FFileDir := CFileDir.Create;
end;

procedure TOptionsBaseUI.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FEventKey );
  FreeAndNil( FFileDir );
end;

function TOptionsBaseUI.LoadOptionsAndDisplay(Options : TOptionsController) : boolean;
var
  FXmlDoc : IXMLDocument;
begin
  Result := false;
  FXmlDoc := CreateXMLDoc();
  if ( FXmlDoc.LoadXML( Options.AsXML )) then begin
    XML2Tree( self.tvOptions, FXmlDoc );
    Result := true;
  end;
  self.Refresh;
  self.Show;
End;

procedure TOptionsBaseUI.OptionGroupSelected(const SelectedGroup: string);
begin
    ///
end;

procedure TOptionsBaseUI.OptionsChanged(Sender: TObject);
begin
  btnApply.Enabled := self.FOptionsController.IsDirty;
end;

procedure TOptionsBaseUI.setOptionsController(const Value: TOptionsController);
begin
  FOptionsController := Value;
  FOptionsController.OnChange := self.OptionsChanged;
end;

Procedure TOptionsBaseUI.OnTreeNodeCheckBox( Node :TTreeNode; isChecked : boolean );
Begin
  if ( Assigned( self.FOnTreeNodeCheckBoxClick ) ) then
    self.FOnTreeNodeCheckBoxClick( Node, isChecked );
End;

// Thank you http://delphi.about.com/od/vclusing/l/aa092104a.htm
Procedure TOptionsBaseUI.ToggleTreeViewCheckBoxes( Node :TTreeNode;
  cUnChecked, cChecked, cRadioUnchecked, cRadioChecked :integer);
var
  tmp:TTreeNode;
Begin
  if Assigned(Node) then
  begin
    if (Node.StateIndex in [cUnChecked,cChecked]) then begin

      if Node.StateIndex = cUnChecked then begin
        Node.StateIndex := cChecked;
        OnTreeNodeCheckBox( Node, true );
      end else if Node.StateIndex = cChecked then begin
        Node.StateIndex := cUnChecked;
        OnTreeNodeCheckBox( Node, false );
      end;

      tmp := Node.getFirstChild;
      while Assigned(tmp) do
      begin
        if (tmp.StateIndex in
                   [cUnChecked,cChecked]) then begin
          tmp.StateIndex := Node.StateIndex;
          OnTreeNodeCheckBox( tmp, (tmp.StateIndex = cChecked) );
        end;
        tmp := tmp.getNextSibling;
      end;
    end else begin
      tmp := Node.Parent;
      if not Assigned(tmp) then
        tmp := TTreeView(Node.TreeView).Items.getFirstNode
      else
        tmp := tmp.getFirstChild;
      while Assigned(tmp) do
      begin
        if (tmp.StateIndex in
                   [cRadioUnChecked,cRadioChecked]) then begin
          tmp.StateIndex := cRadioUnChecked;
          OnTreeNodeCheckBox( tmp, false );
        end;
        tmp := tmp.getNextSibling;
      end;
      OnTreeNodeCheckBox( Node, true );
      Node.StateIndex := cRadioChecked;
    end; // if StateIndex = cRadioUnChecked
  end; // if Assigned(Node)
End;

procedure TOptionsBaseUI.tvOptionsClick(Sender: TObject);
Var
  P:TPoint;
  n : Pointer;
Begin
  GetCursorPos(P);
  P := tvOptions.ScreenToClient(P);
  if (htOnStateIcon in tvOptions.GetHitTestInfoAt(P.X,P.Y)) then begin
  //if ( tvOptions.Selected <> nil ) then begin
    //n := tvOptions.Selected.Data;
    //if ( n <> nil ) then begin
      ToggleTreeViewCheckBoxes(
        tvOptions.Selected,
        cFlatUnCheck,
        cFlatChecked,
        cFlatRadioUnCheck,
        cFlatRadioChecked);
    //end;
  end;
  if ( tvOptions.Selected <> nil ) then 
    if ( Assigned( self.FOnOptionGroupSelect ) ) then
      self.FOnOptionGroupSelect( self.FEventKey[IntToStr(tvOptions.Selected.AbsoluteIndex)] );
End;

procedure TOptionsBaseUI.tvOptionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Assigned(tvOptions.Selected) then
    ToggleTreeViewCheckBoxes(tvOptions.Selected,cFlatUnCheck,cFlatChecked,cFlatRadioUnCheck,cFlatRadioChecked);
end;

(*ToggleTreeViewCheckBoxes*)

{ TOptionsController }

constructor TOptionsController.Create;
begin
  FIsDirty := false;
end;

destructor TOptionsController.Destroy;
begin

end;

procedure TOptionsController.OnChangeHandler;
begin
  self.FIsDirty := true;
  if ( Assigned( self.FOnChange ))  then
    self.FOnChange( self );
end;

END.
