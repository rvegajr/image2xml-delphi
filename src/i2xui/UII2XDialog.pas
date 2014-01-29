unit UII2XDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, ComCtrls, ActnList;

type
  TI2XDialogUI = class(TForm)
    pnlTop: TPanel;
    pnlLeft: TPanel;
    pnlYes: TPanel;
    pnlOK: TPanel;
    pnlCancel: TPanel;
    chkNeverAsk: TCheckBox;
    btnCancel: TSpeedButton;
    btnOK: TSpeedButton;
    btnYes: TSpeedButton;
    pnlNo: TPanel;
    btnNo: TSpeedButton;
    pnlProgressBar: TPanel;
    ProgressBar: TProgressBar;
    lblMainText: TLabel;
    memText: TMemo;
    ActionList: TActionList;
    acSelect: TAction;
    procedure FormCreate(Sender: TObject);
    procedure btnYesClick(Sender: TObject);
    procedure btnNoClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FResult : integer;
    FIsActive : boolean;
    FOnCancelEvent: TNotifyEvent;
    function getMainTitle() : string;
    procedure setMainTitle(const Value: string);
    function getDialogText: string;
    procedure setDialogText(const Value: string);
    function getResult: integer;
    procedure setResult(const Value: integer);
    procedure btnSelect(Sender: TObject);
    function getShowAgainCheck: boolean;
    procedure setShowAgainCheck(const Value: boolean);
    function getProgress: integer;
    procedure setProgress(const Value: integer);
    function getPercentComplete: integer;
    procedure setPercentComplete(const Value: integer);
    function getShowProgressBar: boolean;
    procedure setShowProgressBar(const Value: boolean);
  public
    function ShowDialog(const Msg: string; Buttons: TMsgDlgButtons = [mbOK]; DefaultButton: TMsgDlgBtn = mbOK): integer;
    property MainTitle : string read getMainTitle write setMainTitle;
    property IsActive : boolean read FIsActive write FIsActive;
    property DialogText : string read getDialogText write setDialogText;
    property Result : integer read getResult write setResult;
    property ShowAgainCheckVisible : boolean read getShowAgainCheck write setShowAgainCheck;
    property OnCancel : TNotifyEvent read FOnCancelEvent write FOnCancelEvent;
    property Progress : integer read getProgress write setProgress;
    property ShowProgressBar : boolean read getShowProgressBar write setShowProgressBar;
    property PercentComplete : integer read getPercentComplete write setPercentComplete;
  end;

var
  frmI2XDialogUI: TI2XDialogUI;

implementation

{$R *.dfm}

{ TI2XDialogUI }

procedure TI2XDialogUI.btnYesClick(Sender: TObject);
begin
  btnSelect( Sender );
end;

procedure TI2XDialogUI.btnCancelClick(Sender: TObject);
begin
  btnSelect( Sender );
  if ( Assigned( FOnCancelEvent ) ) then
    FOnCancelEvent( Self );
end;

procedure TI2XDialogUI.btnNoClick(Sender: TObject);
begin
  btnSelect( Sender );
end;

procedure TI2XDialogUI.btnOKClick(Sender: TObject);
begin
  btnSelect( Sender );
end;

procedure TI2XDialogUI.btnSelect(Sender: TObject);
Begin
  self.Result := TControl(Sender).Tag;
  FIsActive := false;
  Self.Hide;
End;

procedure TI2XDialogUI.FormCreate(Sender: TObject);
begin
  self.btnCancel.Tag := mrCancel;
  self.btnOK.Tag := mrOK;
  self.btnYes.Tag := mrYes;
  self.btnNo.Tag := mrNo;
  self.lblMainText.Caption := '';
  self.memText.Text := '';
  FIsActive := false;
  self.ShowProgressBar := false;
end;

function TI2XDialogUI.getDialogText: string;
begin
  Result := self.memText.Text;
end;

function TI2XDialogUI.getMainTitle() : string;
begin
  Result := self.lblMainText.Caption;
end;

function TI2XDialogUI.getPercentComplete: integer;
begin
  Result := self.ProgressBar.Position;
  //BarSpaceWidth := round( self.ProgressBar.Max / 100.0 * (100-self.FPercentComplete));
end;

function TI2XDialogUI.getProgress: integer;
begin
  Result := self.ProgressBar.Position;
end;

function TI2XDialogUI.getResult: integer;
begin
  Result := FResult;
end;

function TI2XDialogUI.getShowAgainCheck: boolean;
begin
  Result := self.pnlLeft.Visible;
end;

function TI2XDialogUI.getShowProgressBar: boolean;
begin
  Result := self.ProgressBar.Visible;
end;

procedure TI2XDialogUI.setDialogText(const Value: string);
begin
  self.memText.Text := Value;
end;

procedure TI2XDialogUI.setMainTitle(const Value: string);
begin
  self.lblMainText.Caption := value;
end;

procedure TI2XDialogUI.setPercentComplete(const Value: integer);
begin
  self.ProgressBar.Position := Value;
end;

procedure TI2XDialogUI.setProgress(const Value: integer);
begin
  self.ProgressBar.Position := Value;
end;

procedure TI2XDialogUI.setResult(const Value: integer);
begin
  FResult := Value;
end;

procedure TI2XDialogUI.setShowAgainCheck(const Value: boolean);
begin
  self.pnlLeft.Visible := Value;
end;

procedure TI2XDialogUI.setShowProgressBar(const Value: boolean);
begin
  self.ProgressBar.Visible := Value;
end;

function TI2XDialogUI.ShowDialog(const Msg: string;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn): integer;
Begin
  DialogText := Msg;
  self.pnlYes.Visible := (mbYes in Buttons);
  self.pnlNo.Visible := (mbNo in Buttons);
  self.pnlCancel.Visible := (mbCancel in Buttons);
  self.pnlOk.Visible := (mbOk in Buttons);
  if ( DefaultButton = mbYes ) then FResult := mrYes;
  if ( DefaultButton = mbNo ) then FResult := mrNo;
  if ( DefaultButton = mbCancel ) then FResult := mrCancel;
  if ( DefaultButton = mbOk ) then FResult := mrOk;
  self.lblMainText.Visible := ( Length( self.lblMainText.Caption ) > 0 );
  FIsActive := true;

  self.Show;

End;

end.
