unit UITWAINDeviceSelect;

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
  StdCtrls,
  uI2XOptions,
  uImage2XML,
  uI2XConstants,
  uUII2XScanTray;

type
  TUITWAINDeviceSelect = class(TForm)
    lstTWAINDeviceSelect: TListBox;
    lblInst: TLabel;
    procedure lstTWAINDeviceSelectClick(Sender: TObject);
    procedure SetDefaultDevice(const defaultDevice: string);
    function GetDefaultDevice() : string;
    procedure lstTWAINDeviceSelectDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FReg : TI2XReg;
    EnableUIEvents : boolean;
    procedure RestoreUIState;
    procedure SaveUIState;
  public
    procedure SetDeviceList( const deviceList : TStrings );
    property DefaultDevice : string read GetDefaultDevice write SetDefaultDevice;
  end;

var
  fTWAINDeviceSelectUI: TUITWAINDeviceSelect;

implementation

{$R *.dfm}

procedure TUITWAINDeviceSelect.FormCreate(Sender: TObject);
begin
  EnableUIEvents := false;
  FReg := TI2XReg.Create();
  RestoreUIState();
  EnableUIEvents := true;
end;

procedure TUITWAINDeviceSelect.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FReg );
end;

procedure TUITWAINDeviceSelect.RestoreUIState;
begin
  self.Left := FReg.Get( DLG_KEY_PREFIX + 'Left', self.Left );
  self.Top := FReg.Get( DLG_KEY_PREFIX + 'Top', self.Top );
  self.Height := FReg.Get( DLG_KEY_PREFIX + 'Height', self.Height );
  self.Width := FReg.Get( DLG_KEY_PREFIX + 'Width', self.Width );
  self.WindowState := TWindowState( FReg.Get( DLG_KEY_PREFIX + 'WindowState', integer(wsNormal) ) );
  //if ( self.WindowState = wsMinimized ) then
  //  self.WindowState := wsNormal;
end;

procedure TUITWAINDeviceSelect.SaveUIState;
begin
  if ( Self.EnableUIEvents ) then begin
    FReg[ DLG_KEY_PREFIX + 'WindowState' ] := integer( self.WindowState );
    FReg[ DLG_KEY_PREFIX + 'Left' ] := self.Left;
    FReg[ DLG_KEY_PREFIX + 'Top' ] := self.Top;
    FReg[ DLG_KEY_PREFIX + 'Height' ] := self.Height;
    FReg[ DLG_KEY_PREFIX + 'Width' ] := self.Width;
  end;
end;

function TUITWAINDeviceSelect.GetDefaultDevice: string;
begin
  if ( lstTWAINDeviceSelect.ItemIndex = -1 ) then
    Result := ''
  else
    Result := lstTWAINDeviceSelect.Items[ lstTWAINDeviceSelect.ItemIndex ];
end;

procedure TUITWAINDeviceSelect.lstTWAINDeviceSelectClick(Sender: TObject);
Begin
  //Options.
End;

procedure TUITWAINDeviceSelect.lstTWAINDeviceSelectDblClick(Sender: TObject);
begin
  SaveUIState();
  Options.TWAINDeviceDefault := DefaultDevice;
  Options.ApplyChanges();
  self.Hide;
end;

procedure TUITWAINDeviceSelect.SetDeviceList(const deviceList: TStrings);
begin
  self.lstTWAINDeviceSelect.Items.Clear;
  self.lstTWAINDeviceSelect.Items.AddStrings( deviceList );
end;

procedure TUITWAINDeviceSelect.SetDefaultDevice(const defaultDevice : string );
var
  i : integer;
begin
    for i := 0 to lstTWAINDeviceSelect.Count - 1 do begin
      if ( lstTWAINDeviceSelect.Items[i] = defaultDevice )then begin
        lstTWAINDeviceSelect.ItemIndex := i;
      end;
    end;
end;

END.
