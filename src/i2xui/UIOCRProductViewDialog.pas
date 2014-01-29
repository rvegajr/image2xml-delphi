unit UIOCRProductViewDialog;

interface

uses uImage2XML,
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Menus,
  OmniXML,
  ComCtrls,
  CommCtrl,
  uReg,
  uHashTable,
  ShellAPI,
  uFileDir,
  uStrUtil;

type
  TOCRProductViewDialog = class(TForm)
    mnuMain: TMainMenu;
    pmnClose: TMenuItem;
    tv: TTreeView;
    pmnuOptions: TMenuItem;
    pmnuAlwaysOnTop: TMenuItem;
    mnuViewXML: TMenuItem;
    procedure pmnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pmnuAlwaysOnTopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure mnuViewXMLClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    reg : CReg;
    FEnableUIEvents : boolean;
    lastHintNode : TTreeNode;
    FHintHashTable : THashStringTable;
    FXMLTempFileName : string;
    FXML : string;
    procedure XMLToTreeview(DataTree: TTreeView; XMLDocument: IXMLDocument);
    procedure BoldTreeNode(treeNode: TTreeNode; Value: Boolean);
    function NodeHint(tn: TTreeNode): string;
  public
    function DisplayXML( const XMLToDisplay : string ) : boolean;
    property XMLTempFileName : String read FXMLTempFileName write FXMLTempFileName;
  end;

var
  dlgOCRProductView: TOCRProductViewDialog;

implementation

{$R *.dfm}

function TOCRProductViewDialog.DisplayXML( const XMLToDisplay : string ): boolean;
var
    FXmlDoc : IXMLDocument;
begin
  FXML := '';
  self.Show;
  Result := false;
  FXmlDoc := CreateXMLDoc();
  FXML := XMLToDisplay;
  if ( FXmlDoc.LoadXML( FXML ) ) then begin
    XMLToTreeview( self.tv, FXmlDoc );
    Result := true;
  end;
  self.Refresh;
end;

procedure TOCRProductViewDialog.FormCreate(Sender: TObject);
begin
  FEnableUIEvents := false;
  reg := CReg.Create();
  pmnuAlwaysOnTop.Checked := ( reg.RegGet( REG_KEY, pmnuAlwaysOnTop.Name, 'N' ) = 'Y' );
  FEnableUIEvents := true;
  FHintHashTable := THashStringTable.Create();
end;

procedure TOCRProductViewDialog.FormDestroy(Sender: TObject);
Begin
  FreeAndNil( Reg );
  FreeAndNil( FHintHashTable );
End;

procedure TOCRProductViewDialog.FormHide(Sender: TObject);
begin
  if ( FileExists( FXMLTempFileName ) ) then
    DeleteFile( FXMLTempFileName );
end;

procedure TOCRProductViewDialog.mnuViewXMLClick(Sender: TObject);
var
  filedir : CFileDir;
begin
  try
    filedir := CFileDir.Create();
    FXMLTempFileName := AppTempDir + '\~XMLOCRProduct' + RandomString() + '.xml';
    filedir.WriteString( FXML, FXMLTempFileName );
    ShellExecute(Handle, 'open', pchar( FXMLTempFileName ), nil,nil,SW_SHOWNORMAL);
  finally
    FreeAndNil( filedir );
  end;
end;

procedure TOCRProductViewDialog.pmnCloseClick(Sender: TObject);
Begin
  self.Hide;
End;

procedure TOCRProductViewDialog.pmnuAlwaysOnTopClick(Sender: TObject);
Begin
  if ( FEnableUIEvents ) then begin
    pmnuAlwaysOnTop.Checked := not pmnuAlwaysOnTop.Checked;
    if ( pmnuAlwaysOnTop.Checked ) then
      SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE)
    else
      SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
    reg.RegSet( REG_KEY, pmnuAlwaysOnTop.Name, BoolToStr( pmnuAlwaysOnTop.Checked ) );
  end;
End;

procedure TOCRProductViewDialog.tvMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  tree: TTreeView;
  hoverNode: TTreeNode;
  hitTest : THitTests;
  //ht : THitTest;
begin
  if (Sender is TTreeView) then
    tree := TTreeView(Sender)
  else
    Exit;

  hoverNode := tree.GetNodeAt(X, Y) ;
  hitTest := tree.GetHitTestInfoAt(X, Y) ;

(*
  //list the hitTest values
  Caption := '';
  for ht in hitTest do
  begin
    Caption := Caption + GetEnumName(TypeInfo(THitTest), integer(ht)) + ', ';
  end;
  *)

  if (lastHintNode <> hoverNode) then
  begin
    Application.CancelHint;

    if (hitTest <= [htOnItem, htOnIcon, htOnLabel, htOnStateIcon]) then
    begin
      lastHintNode := hoverNode;
      tree.Hint := NodeHint(hoverNode) ;
      Application.Hint := tree.Hint;
      Application.ActivateHint(ClientToScreen(Point(X, Y)));
    end;
  end;
end;

function TOCRProductViewDialog.NodeHint(tn: TTreeNode): string;
begin
  result := Format('%s',[ self.FHintHashTable[ IntToStr(tn.AbsoluteIndex) ] ]) ;
end;

procedure TOCRProductViewDialog.XMLToTreeview(DataTree: TTreeView; XMLDocument: IXMLDocument);
  procedure AddNode(ParentNode: TTreeNode; Node: IXMLNode);
  const
    MaxTextLen = 50;
  var
    I: Integer;
    NodeText, sNodeName, sAttrib : string;
    NewNode: TTreeNode;
    nod: IXMLNode;
    nodeResultFlag : boolean;
  begin
    case Node.NodeType of
      ELEMENT_NODE, TEXT_NODE, CDATA_SECTION_NODE:
        begin
          // Here you may want to retrieve the value
          // of the id attribute instead of Node.Text
          NodeText := Node.NodeValue;
          nodeResultFlag := true;

          if NodeText = '' then begin
            NodeText := Node.NodeName;
            nodeResultFlag := false;
          end;
        end;
      DOCUMENT_TYPE_NODE:
        //NodeText := Node.DOMNode.nodeName;
        //NodeText := Node.na
      //else
        NodeText := Node.NodeName;
    end;
    NewNode := DataTree.Items.AddChildObject(ParentNode,
                        Copy(NodeText, 1, MaxTextLen),
                        Pointer(Node));
    BoldTreeNode( NewNode, nodeResultFlag );
    NewNode.ImageIndex := Ord(Node.nodeType);
    if ( Node.Attributes.Length > 0 ) then begin
      sAttrib := '';
      for I := 0 to Node.Attributes.Length - 1 do begin
        nod := Node.Attributes.Item[I];
        with nod do
        begin
          sNodeName := NodeName;
          if ( sNodeName = 'x' ) then
            sNodeName := 'X'
          else if ( sNodeName = 'y' ) then
            sNodeName := 'Y'
          else if ( sNodeName = 'w' ) then
            sNodeName := 'Width'
          else if ( sNodeName = 'h' ) then
            sNodeName := 'Height'
          else if ( sNodeName = 'a' ) then
            sNodeName := 'Accuracy'
          ;

          sAttrib := sAttrib + Format('%s=%s', [sNodeName, NodeValue]) + '  ';
          //DataTree.Items.AddChildObject(NewNode,
          //                Copy(NodeText, 1, MaxTextLen),
          //                Pointer(Node.Attributes.Item[I]));
        end;
      end;
      FHintHashTable.Add( IntToStr(NewNode.AbsoluteIndex), sAttrib );
    end;

    for I := 0 to Node.ChildNodes.Length - 1 do
      AddNode(NewNode, Node.ChildNodes.Item[I]);
  end;

var
  I: Integer;
begin
  if XMLDocument.ChildNodes.Length = 0 then
    Exit;
  DataTree.Items.BeginUpdate;
  try
    DataTree.Items.Clear;
    for I := 0 to XMLDocument.ChildNodes.Length - 1 do
      AddNode(nil, XMLDocument.ChildNodes.Item[I]);
    DataTree.Items[0].Expand(True);
    DataTree.Selected := DataTree.Items[0];
    DataTree.TopItem := DataTree.Selected;
    //DataTree.SetFocus;
  finally
    DataTree.Items.EndUpdate;
  end;
end;

//Thanks http://delphi.about.com/od/adptips2006/qt/treeitembold.htm
procedure TOCRProductViewDialog.BoldTreeNode(treeNode: TTreeNode; Value: Boolean) ;
var
   treeItem: TTVItem;
begin
   if not Assigned(treeNode) then Exit;

   with treeItem do
   begin
     hItem := treeNode.ItemId;
     stateMask := TVIS_BOLD;
     mask := TVIF_HANDLE or TVIF_STATE;
     if Value then
       state := TVIS_BOLD
     else
       state := 0;
   end;

   TreeView_SetItem(treeNode.Handle, treeItem) ;
end;

end.
