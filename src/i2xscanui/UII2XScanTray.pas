unit UII2XScanTray;

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
  ImgList,
  ExtCtrls,
  Menus,
  uUII2XScanTray,
  uStrUtil,
  uFileDir,
  uReg,
  uAppCache,
  StdCtrls,
  Buttons,
  uImage2XML,
  uI2XScan,
  uDWImage,
  uI2XOptions,
  mcmTWAINKernel,
  mcmTWAINIntf,
  mcmTWAIN,
  UITWAINDeviceSelect,
  ShellAPI
  ;

type
  TUII2XScanTray = class(TForm)
    TrayIcon: TTrayIcon;
    IconList: TImageList;
    pmnuTray: TPopupMenu;
    pmnuMinMax: TMenuItem;
    N1: TMenuItem;
    pmnuClose: TMenuItem;
    lblActiveImage: TLabel;
    txtOutputFolder: TEdit;
    bbtnOpenImage: TBitBtn;
    Label1: TLabel;
    txtSubPath: TEdit;
    chkAutoScan: TCheckBox;
    btnScan: TBitBtn;
    pmnuCenter: TMenuItem;
    btnSource: TBitBtn;
    twain: TmcmTWAIN;
    procedure pmnuMinMaxClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pmnuCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure txtOutputFolderChange(Sender: TObject);
    procedure txtSubPathChange(Sender: TObject);
    procedure chkAutoScanClick(Sender: TObject);
    procedure bbtnOpenImageClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pmnuCenterClick(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure txtOutputFolderDblClick(Sender: TObject);
    procedure txtSubPathDblClick(Sender: TObject);
  private
    EnableUIEvents : boolean;
    PopupClose : boolean;
    FReg : TI2XReg;
    FFileDir : CFileDir;
    FStrUtil : CStrUtil;
    FScan : TI2XScan;
    FTWAINSel : TUITWAINDeviceSelect;
    FImageCount : Cardinal;
    procedure MaximizeApp(Sender: TObject);
    procedure MinimizeApp(Sender: TObject);
    procedure ToggleApp(Sender: TObject);
    procedure TrayMsg(const MsgToDisplay: string);
    procedure RestoreUIState;
    procedure SaveUIState;
    procedure OnAcquireImageComplete(Sender: TObject; BitmapHandle: HBITMAP);
    procedure OnAfterScanImageComplete(Sender: TObject; ImageFileName : string);
    function getTWAINSel: TUITWAINDeviceSelect;
    procedure ScanJobStart( Sender: TObject );
    procedure ScanJobEnd( Sender: TObject );
  public
    property TWAINSel : TUITWAINDeviceSelect read getTWAINSel write FTWAINSel;
  end;

var
  fScanTrayUI: TUII2XScanTray;

implementation

{$R *.dfm}

procedure TUII2XScanTray.OnAcquireImageComplete( Sender: TObject; BitmapHandle : HBITMAP );
var
  dwimage : TDWImage;
begin
  try
    dwimage := TDWImage.Create( BitmapHandle );
  finally
    FreeAndNil( dwimage );
  end;
end;


procedure TUII2XScanTray.OnAfterScanImageComplete(Sender: TObject;
  ImageFileName: string);
begin
  Inc( FImageCount );
  self.TrayMsg( 'Image ' + ExtractFileName(ImageFileName) + ' scanned. Image Count: ' + IntToStr(FImageCount) );
end;

procedure TUII2XScanTray.MinimizeApp(Sender: TObject);
var
  k: integer;
begin
  application.Minimize;
  self.pmnuMinMax.Caption := 'Restore';
  self.pmnuMinMax.Tag := APP_MIN;
  for k := 0 to Screen.FormCount - 1 do begin
    Screen.Forms[k].Hide;
  end;
  if ( not AppCache.MinMessageDisplayed ) then begin
    self.TrayMsg('The app is still active here.  Right click to use the menu options (which includes the option to exit the application).');
    AppCache.MinMessageDisplayed := true;
  end;
end;

procedure TUII2XScanTray.bbtnOpenImageClick(Sender: TObject);
var
  sPath : string;
begin
  if ( self.FFileDir.BrowseForFolder( sPath ) ) then begin
    self.txtOutputFolder.Text := sPath;
  end;
end;

procedure TUII2XScanTray.btnScanClick(Sender: TObject);
begin
  FScan.SourceDevice := Options.TWAINDeviceDefault;
  FScan.Scan( self.FFileDir.Slash( self.txtOutputFolder.Text ) + self.txtSubPath.Text );
end;

procedure TUII2XScanTray.btnSourceClick(Sender: TObject);
var
  s : string;
begin
  TWAINSel.SetDeviceList( FScan.SourceList );
  TWAINSel.DefaultDevice := Options.TWAINDeviceDefault;
  TWAINSel.Show;
end;

procedure TUII2XScanTray.chkAutoScanClick(Sender: TObject);
begin
  if ( self.EnableUIEvents ) then
    AppCache.AutoScan := self.chkAutoScan.Checked
end;

procedure TUII2XScanTray.FormActivate(Sender: TObject);
begin
  self.RestoreUIState();
end;

procedure TUII2XScanTray.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := PopupClose;
  if ( not PopupClose ) then
    MinimizeApp( Sender );
end;

procedure TUII2XScanTray.FormCreate(Sender: TObject);
begin
  EnableUIEvents := false;
  self.pmnuMinMax.Tag := APP_NORMAL;
  self.pmnuMinMax.Caption := 'Minimize';
  PopupClose := false;
  self.TrayIcon.IconIndex := Integer( icGreen );
  FReg := TI2XReg.Create();
  FFileDir := CFileDir.Create();
  FStrUtil := CStrUtil.Create();
  AppCache.AutoSave := true;

  self.txtOutputFolder.Text := AppCache.OutputFolder;
  self.txtSubPath.Text := AppCache.SubPath;
  self.chkAutoScan.Checked := AppCache.AutoScan;
  RestoreUIState();
  self.WindowState := wsNormal;
  FScan := TI2XScan.Create( twain );
  FScan.OnImageFileComplete := OnAfterScanImageComplete;
  FScan.OnJobStart := self.ScanJobStart;
  FScan.OnJobComplete := self.ScanJobEnd;

  EnableUIEvents := true;
end;

procedure TUII2XScanTray.FormDestroy(Sender: TObject);
begin
  self.TrayIcon.Visible := false;
  FreeAndNil( FReg );
  FreeAndNil( FFileDir );
  FreeAndNil( FStrUtil );
  FreeAndNil( FScan );
  if ( FTWAINSel <> nil ) then FreeAndNil( FTWAINSel );
end;

procedure TUII2XScanTray.FormHide(Sender: TObject);
begin
  self.SaveUIState();
end;

procedure TUII2XScanTray.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  self.SaveUIState();
end;

procedure TUII2XScanTray.FormResize(Sender: TObject);
begin
  self.SaveUIState();
end;

function TUII2XScanTray.getTWAINSel: TUITWAINDeviceSelect;
begin
  if ( FTWAINSel = nil ) then
    FTWAINSel := TUITWAINDeviceSelect.Create( self );
  Result := FTWAINSel;
end;

procedure TUII2XScanTray.MaximizeApp(Sender: TObject);
var
  k: integer;
begin
    self.RestoreUIState();
    self.pmnuMinMax.Tag := APP_NORMAL;
    self.pmnuMinMax.Caption := 'Minimize';
    for k := Screen.FormCount - 1 downto 0 do begin
      Screen.Forms[k].Show;
    end;
    self.WindowState := wsNormal;
    application.Restore;
    application.BringToFront;
end;

procedure TUII2XScanTray.pmnuCenterClick(Sender: TObject);
begin
  Left:=(Screen.Width-Width)  div 2;
  Top:=(Screen.Height-Height) div 2;
end;

procedure TUII2XScanTray.pmnuCloseClick(Sender: TObject);
begin
  PopupClose := true;
  AppCache.ApplyChanges;
  self.Close;
end;

procedure TUII2XScanTray.pmnuMinMaxClick(Sender: TObject);
Begin
  ToggleApp( Sender );
End;

procedure TUII2XScanTray.RestoreUIState;
begin
  self.Left := FReg.Get( KEY_PREFIX + 'Left', self.Left );
  self.Top := FReg.Get( KEY_PREFIX + 'Top', self.Top );
  self.Height := FReg.Get( KEY_PREFIX + 'Height', self.Height );
  self.Width := FReg.Get( KEY_PREFIX + 'Width', self.Width );
  self.WindowState := TWindowState( FReg.Get( KEY_PREFIX + 'WindowState', integer(wsNormal) ) );
  //if ( self.WindowState = wsMinimized ) then
  //  self.WindowState := wsNormal;
end;

procedure TUII2XScanTray.SaveUIState;
begin
  if ( Self.EnableUIEvents ) then begin
    FReg[ KEY_PREFIX + 'WindowState' ] := integer( self.WindowState );
    FReg[ KEY_PREFIX + 'Left' ] := self.Left;
    FReg[ KEY_PREFIX + 'Top' ] := self.Top;
    FReg[ KEY_PREFIX + 'Height' ] := self.Height;
    FReg[ KEY_PREFIX + 'Width' ] := self.Width;
  end;
end;

procedure TUII2XScanTray.ScanJobEnd( Sender: TObject );
begin
  self.TrayIcon.IconIndex := Integer( icGreen );
  self.TrayMsg( 'Scan Job has completed...  Image Count: ' + IntToStr(FImageCount) );
end;

procedure TUII2XScanTray.ScanJobStart( Sender: TObject );
begin
  self.TrayIcon.IconIndex := Integer( icOrange );
  self.TrayMsg( 'Scan Job has started...' );
  FImageCount := 0;
end;

procedure TUII2XScanTray.TrayIconDblClick(Sender: TObject);
Begin
  ToggleApp( Sender );
End;

procedure TUII2XScanTray.ToggleApp(Sender: TObject);
Begin
  if ( self.pmnuMinMax.Tag = APP_NORMAL ) then
    MinimizeApp( Sender )
  else
    MaximizeApp( Sender );
End;

procedure TUII2XScanTray.TrayMsg( const MsgToDisplay : string );
Begin
  self.TrayIcon.BalloonHint := MsgToDisplay;
  self.TrayIcon.ShowBalloonHint;
End;

procedure TUII2XScanTray.txtOutputFolderChange(Sender: TObject);
begin
  if ( self.EnableUIEvents ) then
    AppCache.OutputFolder := self.txtOutputFolder.Text;
end;

procedure TUII2XScanTray.txtOutputFolderDblClick(Sender: TObject);
begin
  if ( DirectoryExists( txtOutputFolder.Text ) ) then
    ShellExecute(Handle, 'open', pchar( txtOutputFolder.Text ), nil,nil,SW_SHOWNORMAL);
end;

procedure TUII2XScanTray.txtSubPathChange(Sender: TObject);
begin
  if ( self.EnableUIEvents ) then
    AppCache.SubPath := self.txtSubPath.Text;
end;

procedure TUII2XScanTray.txtSubPathDblClick(Sender: TObject);
var
  sPath : string;
begin
  sPath := self.FFileDir.Slash( txtOutputFolder.Text ) + txtSubPath.Text;
  if ( DirectoryExists( sPath ) ) then
    ShellExecute(Handle, 'open', pchar( sPath ), nil,nil,SW_SHOWNORMAL);
end;

END.
