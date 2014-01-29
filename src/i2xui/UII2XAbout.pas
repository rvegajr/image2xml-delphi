unit UII2XAbout;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  uImage2XML,
  ShellAPI;

type
  TAboutBox = class(TForm)
    pnlAbout: TPanel;
    ProgramIcon: TImage;
    lblProductName: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblURL: TLabel;
    lblComments: TLabel;
    lblSupport: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure lblSupportClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.dfm}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  self.lblProductName.Caption := APP_NAME;
  self.lblVersion.Caption := 'V.' + sVERSION;
  self.lblCopyright.Caption := 'Copyright © 2009 Noctusoft, Inc.';
  self.lblURL.Caption := 'www.image2xml.com';
  self.lblSupport.Caption := 'support.image2xml.com';
  self.lblComments.Caption := '';
end;

procedure TAboutBox.lblSupportClick(Sender: TObject);
var
  sURL : string;
begin
  sURL := 'http://' + lblSupport.Caption;
  ShellExecute(&0, 'open', pchar( sURL ), nil,nil,SW_SHOWNORMAL);
end;

procedure TAboutBox.lblURLClick(Sender: TObject);
var
  sURL : string;
begin
  sURL := 'http://' + lblURL.Caption;
  ShellExecute(&0, 'open', pchar( sURL ), nil,nil,SW_SHOWNORMAL);
end;

end.
 
