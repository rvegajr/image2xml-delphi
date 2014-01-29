unit uUII2XMainHelper;

interface
uses
  UII2XMain,
  SysUtils,
  uHashTable,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ActnList,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Grids,
  Buttons;

const
  UNIT_NAME = 'uUII2XMainHelper';

type

  TI2XMainUIHelper = class(TObject)
    private
      _Form : TI2XMainUI;
    protected
    public
      constructor Create( var form : TI2XMainUI );
      destructor Destroy; override;
    published
      property Form : TI2XMainUI read _Form write _Form; default;
  end;

implementation

END.
