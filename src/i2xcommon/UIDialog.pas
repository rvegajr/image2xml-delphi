unit UIDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmDialog = class(TForm)
    chkIgnore: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDialog: TfrmDialog;

implementation

{$R *.dfm}

end.
