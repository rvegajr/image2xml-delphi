unit uI2XOCR_GOCR;

interface

uses
  Windows,
  Classes,
  uStrUtil,
  Graphics,
  SysUtils,
  GR32,
  GR32_Image,
  uI2XOCR,
  MapStream,
  uDWImage,
  uI2XConstants,
  Typinfo,
  //uTOCRdll,
  uCmdLine,
  ExtCtrls;

const
  UNIT_NAME = 'ui2xOCR_TOCR';
  X_DPI_DEFAULT = 300;
  Y_DPI_DEFAULT = 300;
  PARM_NAME='i2xocr_GOCR';

type
  TStatusChangeEvent = procedure (Sender: TObject; msgText : string) of object;

  TI2XGOCR = class(TI2XOCR)
    private
    public
      function OCRImage( const ImageMMID, OCRDataMMID : string ) : boolean; virtual;
      constructor Create( const GUID, ParmFileName : string ); override;
      destructor Destroy;  override;
  end;
implementation

{ TI2XGOCR }

constructor TI2XGOCR.Create(const GUID, ParmFileName: string);
Begin
  inherited;

End;

destructor TI2XGOCR.Destroy;
Begin

  inherited;
End;

function TI2XGOCR.OCRImage(const ImageMMID, OCRDataMMID: string): boolean;
Begin
  try

  except
    on E : Exception do
      begin
        self.ErrorUpdate( E.Message, ERROR_I2X_OCR_FAILED );
        Result := false;
      end;
  end;
End;

END.
