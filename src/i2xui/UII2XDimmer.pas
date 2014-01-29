unit UII2XDimmer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TI2XDimmerUI = class(TForm)
    procedure FormCreate(Sender: TObject);
  public
    procedure Display;
  end;

var
  frmDimmer: TI2XDimmerUI;

implementation
{$R *.dfm}

uses UII2XMain;

procedure TI2XDimmerUI.FormCreate(Sender: TObject);
begin
  AlphaBlend := true;
  AlphaBlendValue := 128;
  BorderStyle := bsNone;
end;

procedure TI2XDimmerUI.Display;
begin
  with Self do
  begin
    Left := frmI2XMain.Left;
    Top := frmI2XMain.Top;
    Width := frmI2XMain.Width;
    Height := frmI2XMain.Height;

    Show;
  end;
end;

end.
