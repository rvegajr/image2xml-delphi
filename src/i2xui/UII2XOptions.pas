unit UII2XOptions;

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
  UIOptionsBase,
  StdCtrls,
  Buttons,
  ComCtrls,
  ExtCtrls,
  OmniXML,
  ActnList,
  ImgList,
  Typinfo,
  uStrUtil,
  Grids,
  uI2XOptions,
  uI2XDLL,
  uHashTable,
  uI2XPluginManager,
  uI2XOCR
  ;

type
  TI2XOptionsEnum = (
    iAPPLICATION = 0,
    iPLUGINS,
    iPLUGINSOCR,
    iPLUGINSIMG,
    iTWAIN
  );
  TI2XOptionsUI = class(TOptionsBaseUI)
    pnlApplication: TPanel;
    pnlPlugins: TPanel;
    chkGridSnap: TCheckBox;
    txtGridSize: TEdit;
    UpDownGridSize: TUpDown;
    lblGridSize: TLabel;
    grdPlugins: TStringGrid;
    pnlTWAIN: TPanel;
    lblDeviceList: TLabel;
    lstTWAINDevices: TListBox;
    lblRootPath: TLabel;
    btnJobRootPathSelect: TSpeedButton;
    txtJobRootPath: TEdit;
    procedure FormCreate(Sender: TObject); override;
    procedure txtGridSizeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure chkGridSnapClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TreeNodeCheckBoxClickHandler( Node :TTreeNode; isChecked : boolean  );
    procedure lstTWAINDevicesClick(Sender: TObject);
    procedure btnJobRootPathSelectClick(Sender: TObject);
  private
    FI2XOptions : TI2XOptions;
    oStrUtil : CStrUtil;
    FUIEVents : boolean;
    FShowOCRPlugins : boolean;
    FShowImagePlugins : boolean;
    procedure FillOptionsList();
    procedure OptionGroupSelected( const SelectedGroup : string ); override;
    procedure ShowApplicationPanel();
    procedure ShowPluginsPanel( const OCRPlugins : boolean = true; const ImagePlugins : boolean = true);
    procedure ShowTWAINPanel();
    procedure HidePanels();
    function getI2XOptions: TI2XOptions;
    procedure setI2XOptions(const Value: TI2XOptions);
    procedure SetPluginGridSize( const OCRPlugins : boolean; const ImagePlugins : boolean );
    procedure TestUpdated(Sender: TObject);

  public
    property I2XOptions : TI2XOptions read getI2XOptions write setI2XOptions;
    function LoadOptionsAndDisplay( Options : TOptionsController ) : boolean; override;
    procedure SetCurrentPanel(const PanelDisplay: TI2XOptionsEnum);
  end;

var
  frmI2XOptions: TI2XOptionsUI;

implementation
uses uImage2XML;
{$R *.dfm}

{ TI2XOptionsUI }

procedure TI2XOptionsUI.btnJobRootPathSelectClick(Sender: TObject);
var
  sFolder : string;
begin
  inherited;
  if FFileDir.BrowseForFolder( self.txtJobRootPath.Text, sFolder ) then
    txtJobRootPath.Text := sFolder;
end;

procedure TI2XOptionsUI.chkGridSnapClick(Sender: TObject);
begin
  if ( self.FUIEVents ) then begin
    FI2XOptions.EnableGridSnap := chkGridSnap.Checked;
  end;
end;

procedure TI2XOptionsUI.FillOptionsList;
begin
end;

procedure TI2XOptionsUI.FormCreate(Sender: TObject);
Begin
  FI2XOptions := Options;
  self.FOptionsController := FI2XOptions;
  oStrUtil := CStrUtil.Create;
  HidePanels;
  FUIEVents := true;
  inherited FormCreate( Sender, FOptionsController );
  ShowApplicationPanel();
  self.OnTreeNodeCheckBoxClickEvent := TreeNodeCheckBoxClickHandler;
End;

procedure TI2XOptionsUI.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil( oStrUtil );
end;

procedure TI2XOptionsUI.FormResize(Sender: TObject);
begin
  inherited;
  if ( self.FShowOCRPlugins or self.FShowImagePlugins  ) then begin
    self.SetPluginGridSize( self.FShowOCRPlugins, self.FShowImagePlugins );
  end;
end;

function TI2XOptionsUI.getI2XOptions: TI2XOptions;
begin
  Result := TI2XOptions( self.FOptionsController );
end;

procedure TI2XOptionsUI.HidePanels;
begin
  self.pnlApplication.Visible := false;
  self.pnlPlugins.Visible := false;
  self.pnlTWAIN.Visible := false;

  FShowOCRPlugins := false;
  FShowImagePlugins := false;
end;

function TI2XOptionsUI.LoadOptionsAndDisplay(
  Options: TOptionsController): boolean;
begin
  self.FUIEVents := false;

  inherited LoadOptionsAndDisplay( Options );

  self.FUIEVents := true;
end;

procedure TI2XOptionsUI.lstTWAINDevicesClick(Sender: TObject);
begin
  inherited;
  I2XOptions.TWAINDeviceDefault :=
    lstTWAINDevices.Items[ lstTWAINDevices.ItemIndex ];
end;

procedure TI2XOptionsUI.SetCurrentPanel(const PanelDisplay : TI2XOptionsEnum );
Begin
  case PanelDisplay of
    iAPPLICATION:
      begin
        ShowApplicationPanel();
      end;
    iPLUGINS    :
      begin
        ShowPluginsPanel();
      end;
    iPLUGINSOCR :
      begin
        ShowPluginsPanel(true, false);
      end;
    iPLUGINSIMG :
      begin
        ShowPluginsPanel(false, true);
      end;
    iTWAIN :
      begin
        self.ShowTWAINPanel();
      end;
  end;
End;

procedure TI2XOptionsUI.OptionGroupSelected(const SelectedGroup: string);
Begin
  inherited OptionGroupSelected( SelectedGroup );
  SetCurrentPanel( TI2XOptionsEnum(GetEnumValue(TypeInfo(TI2XOptionsEnum),'i' + UpperCase(SelectedGroup))) );
End;

procedure TI2XOptionsUI.setI2XOptions(const Value: TI2XOptions);
Begin
  self.FOptionsController := Value;
End;

procedure TI2XOptionsUI.ShowApplicationPanel;
begin
  HidePanels;
  with self.pnlApplication do begin
    self.FUIEVents := false;
    Visible := true;

    self.txtGridSize.Text := IntToStr( FI2XOptions.GridSize );
    chkGridSnap.Checked := I2XOptions.EnableGridSnap;
    self.txtJobRootPath.Text := I2XOptions.JobRootPath;

    self.FUIEVents := true;
    Align := alClient;
  end;
end;

procedure TI2XOptionsUI.ShowPluginsPanel( const OCRPlugins : boolean; const ImagePlugins : boolean );
var
  bShowPluginType : boolean;
  i, iRow, iCol, nWidth : integer;
  oimg : TDLLImageProc;
  oOcr : TDLLOCREngine;
  arrKeyList : TKeyList;
  ht : THashTable;
begin
  HidePanels;
  bShowPluginType := (OCRPlugins and ImagePlugins);
  with self.pnlPlugins do begin
    self.FUIEVents := false;
    Visible := true;

    self.grdPlugins.RowCount := 1;
    iRow := 0;
    iCol := 0;
    FShowOCRPlugins := OCRPlugins;
    FShowImagePlugins := ImagePlugins;

    if ( (PluginManager.ImageDLLCount + PluginManager.OCREngineDLLCount) > 1) then begin
      SetPluginGridSize( OCRPlugins, ImagePlugins );
      if ( ImagePlugins ) then begin
        for i := 0 to PluginManager.ImageDLLCount - 1 do begin
          iCol := 0;
          oimg := PluginManager.ImageDLL[i];
          Inc( iRow );
          self.grdPlugins.RowCount := iRow + 1;

          if ( iRow = 1 ) then
            self.grdPlugins.FixedRows := 1;
          if ( bShowPluginType ) then begin
            self.grdPlugins.Cells[iCol, iRow] := 'IMAGE';
            Inc(iCol);
          end;
          self.grdPlugins.Cells[iCol, iRow] := oimg.ShortName;
          Inc(iCol);
          self.grdPlugins.Cells[iCol, iRow] := oimg.Description;
          Inc(iCol);
          self.grdPlugins.Cells[iCol, iRow] := oimg.Version;
          Inc(iCol);
          self.grdPlugins.Cells[iCol, iRow] := oimg.DLLName;
          Inc(iCol);
        end;
      end;
      if ( OCRPlugins ) then begin
        for i := 0 to PluginManager.OCREngineDLLCount - 1 do begin
          iCol := 0;
          oOcr := PluginManager.OCREngineDLL[i];
          Inc( iRow );
          self.grdPlugins.RowCount := iRow + 1;
          if ( iRow = 1 ) then
            self.grdPlugins.FixedRows := 1;

          if ( bShowPluginType ) then begin
            self.grdPlugins.Cells[iCol, iRow] := 'OCR';
            Inc(iCol);
          end;
          self.grdPlugins.Cells[iCol, iRow] := oOcr.ShortName;
          Inc(iCol);
          self.grdPlugins.Cells[iCol, iRow] := oOcr.Description;
          Inc(iCol);
          self.grdPlugins.Cells[iCol, iRow] := oOcr.Version;
          Inc(iCol);
          self.grdPlugins.Cells[iCol, iRow] := oOcr.DLLName;
          Inc(iCol);
        end;
      end;
    end;

    self.FUIEVents := true;
    Align := alClient;
  end;
end;

procedure TI2XOptionsUI.ShowTWAINPanel;
var
  i : integer;
begin
  HidePanels;
  with self.pnlTWAIN do begin
    self.FUIEVents := false;
    Visible := true;

    self.lstTWAINDevices.Items.Clear;
    self.lstTWAINDevices.Items.AddStrings( FI2XOptions.TWAINDevices );
    for i := 0 to lstTWAINDevices.Count - 1 do begin
      if ( lstTWAINDevices.Items[i] = FI2XOptions.TWAINDeviceDefault )then begin
        lstTWAINDevices.ItemIndex := i;
      end;
    end;

    self.FUIEVents := true;
    Align := alClient;
  end;
end;

procedure TI2XOptionsUI.TreeNodeCheckBoxClickHandler(Node: TTreeNode;
  isChecked: boolean);
var
  test : TDLLOCRTest;
  dwpersist : TDWPersist;
begin
  if ( Node.data <> nil ) then begin
    dwpersist := TDWPersist(Node.data);
    dwpersist.Enabled := isChecked;
    //dbg( 'classname=' + dwpersist.ClassName );
    //test := TDLLOCRTest(dwpersist);
    //test := TDLLOCRTest(Node.data);
    //test.Enabled := isChecked;
    FI2XOptions.IsDirty := true;
    FI2XOptions.OnChange( self );
  end;
end;

procedure TI2XOptionsUI.SetPluginGridSize( const OCRPlugins : boolean; const ImagePlugins : boolean );
var
  i, iRow, iCol, nWidth : integer;
  bShowPluginType : boolean;
Begin
  bShowPluginType := (OCRPlugins and ImagePlugins);
      nWidth := pnlRight.Width - 12;
      iRow := 0;
      iCol := 0;
      if ( bShowPluginType ) then begin
        self.grdPlugins.ColCount := 5;

        self.grdPlugins.Cells[iCol, iRow] := 'Plugin Type';
        self.grdPlugins.ColWidths[iCol] := Round(nWidth * 0.05);
        Inc(iCol);

        self.grdPlugins.Cells[iCol, iRow] := 'Short Name';
        self.grdPlugins.ColWidths[iCol] := Round(nWidth * 0.05);
        Inc(iCol);

        self.grdPlugins.Cells[iCol, iRow] := 'Description';
        self.grdPlugins.ColWidths[iCol] := Round(nWidth * 0.35);
        Inc(iCol);

        self.grdPlugins.Cells[iCol, iRow] := 'Version';
        self.grdPlugins.ColWidths[iCol] := Round(nWidth * 0.10);
        Inc(iCol);

        self.grdPlugins.Cells[iCol, iRow] := 'File Name';
        self.grdPlugins.ColWidths[iCol] := Round(nWidth * 0.45);
        Inc(iCol);

      end else begin
        self.grdPlugins.ColCount := 4;
        self.grdPlugins.ColWidths[0] := Round(nWidth * 0.10);
        self.grdPlugins.ColWidths[1] := Round(nWidth * 0.30);
        self.grdPlugins.ColWidths[2] := Round(nWidth * 0.20);
        self.grdPlugins.ColWidths[3] := Round(nWidth * 0.40);

        self.grdPlugins.Cells[0, iRow] := 'Short Name';
        self.grdPlugins.Cells[1, iRow] := 'Description';
        self.grdPlugins.Cells[2, iRow] := 'Version';
        self.grdPlugins.Cells[3, iRow] := 'File Name';
      end;
End;

procedure TI2XOptionsUI.txtGridSizeChange(Sender: TObject);
var
  GridSize : Integer;
  doChange : boolean;
Begin
  if ( FUIEVents ) then begin
    doChange := false;
    GridSize := -1;

    if ( oStrUtil.IsNumeric( txtGridSize.Text )) then begin
      I2XOptions.GridSize := StrToInt( txtGridSize.Text );
      doChange := (( I2XOptions.GridSize >= 1 ) and ( I2XOptions.GridSize <= 50 ));
    end;

    if ( doChange ) then begin
      //reg.RegSet( REG_KEY, 'grid_size', txtGridSize.Text, HKEY_CURRENT_USER );
      I2XOptions.GridSize := StrToInt( txtGridSize.Text );
    end else begin
      //txtGridSize.Text := reg.RegGet( REG_KEY, 'grid_size', '5', HKEY_CURRENT_USER );
    end;
  end;
End;

procedure TI2XOptionsUI.TestUpdated(Sender: TObject);
var
  GridSize : Integer;
  doChange : boolean;
Begin
  if ( FUIEVents ) then begin
    doChange := false;

    GridSize := -1;

    if ( oStrUtil.IsNumeric( txtGridSize.Text )) then begin
      I2XOptions.GridSize := StrToInt( txtGridSize.Text );
      doChange := (( I2XOptions.GridSize >= 1 ) and ( I2XOptions.GridSize <= 50 ));
    end;

    if ( doChange ) then begin
      //reg.RegSet( REG_KEY, 'grid_size', txtGridSize.Text, HKEY_CURRENT_USER );
      I2XOptions.GridSize := StrToInt( txtGridSize.Text );
    end else begin
      //txtGridSize.Text := reg.RegGet( REG_KEY, 'grid_size', '5', HKEY_CURRENT_USER );
    end;
  end;
End;

END.
