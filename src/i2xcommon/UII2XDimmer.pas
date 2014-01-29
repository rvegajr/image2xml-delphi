unit UII2XDimmer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TDimmerForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  public
    procedure Display;
  end;

var
  DimmerForm: TDimmerForm;

implementation
{$R *.dfm}

uses UII2XBase;

procedure TDimmerForm.FormCreate(Sender: TObject);
begin
  AlphaBlend := true;
  AlphaBlendValue := 128;
  BorderStyle := bsNone;
end;

procedure TDimmerForm.Display;
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
