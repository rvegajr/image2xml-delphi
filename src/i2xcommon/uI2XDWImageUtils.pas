unit uI2XDWImageUtils;

interface
uses
  SysUtils,
  Graphics,
  uI2XConstants,
  uI2XTemplate,
  uDWImage,
  Windows,
  Classes,
  GIFImg;

const
  UNIT_NAME = 'uI2XDWImageUtils';

function EstimatedImageResolution(img : TDWImage;
  template : CTemplate ): integer; overload;
function EstimatedImageResolution(img : TDWImage;
  modelWidth, modelHeight, modelResolution : integer): integer; overload;
function DPIToPixelsPerMeter( res : integer): Cardinal;
function ImageResolutionCheck(img : TDWImage;
  template : CTemplate ): boolean;

function JPGDPI( JPGFileName : string ) : TPoint;
function GIFDPI( GIFFileName : string ) : TPoint;
function BMPDPI( BMPFileName : string ) : TPoint;
function ImageDPI( const ImageFileName : string ) : TPoint; overload;
function ImageDPI( const ImageFileName : string; const DPIToSetImageTo : cardinal ) : boolean; overload;
//function TIFDPI( TIFFileName : string ) : TPoint;

implementation

function ImageDPI( const ImageFileName : string; const DPIToSetImageTo : cardinal ) : boolean; overload;
var
  dw : TDWImage;
Begin
  try
    dw := TDWImage.Create();
    dw.LoadFromFile( ImageFileName );
    dw.setDPI( DPIToSetImageTo );
    dw.SaveToFile(  ImageFileName );
  finally
    FreeAndNil( dw );
  end;
End;

function ImageDPI( const ImageFileName : string ) : TPoint;
var
  sExt : string;
Begin
  SExt := ExtractFileExt( ImageFileName );
  if (( SExt='.jpg' ) or ( SExt='.jpg' )) then begin
    Result := JPGDPI( ImageFileName );
  end else if (( SExt='.tif' ) or ( SExt='.tiff' )) then begin
    Result := TIFDPI( ImageFileName );
  end else if ( SExt='.gif' ) then begin
    Result := GIFDPI( ImageFileName );
  end else if ( SExt='.bmp' ) then begin
    Result := BMPDPI( ImageFileName );
  end;
End;

function BMPDPI( BMPFileName : string ) : TPoint;
Begin

End;

function JPGDPI( JPGFileName : string ) : TPoint;
const
    BufferSize = 50;
var
    Buffer     :  STRING;
    index      :  INTEGER;
    FileStream :  TFileStream;
    xResolution:  WORD;
    yResolution:  WORD;
Begin
    FileStream := TFileStream.Create(JPGFileName,
                                     fmOpenRead OR fmShareDenyNone);
    TRY
      SetLength(Buffer, BufferSize);
      FileStream.Read(buffer[1], BufferSize);
      index := POS('JFIF'+#$00, buffer);
      IF   index > 0
      THEN BEGIN
        FileStream.Seek(index+7, soFromBeginning);
        FileStream.Read(xResolution, 2);
        FileStream.Read(yResolution, 2);

        xResolution :=  Swap(xResolution);
        yResolution :=  Swap(yResolution);

        Result.X := xResolution;
        Result.Y := yResolution;
        //ShowMessage('xResolution=' +
        //            IntToStr(xResolution) +
        //            ', yResolution=' +
        //            IntToStr(yResolution))
      END
    FINALLY
      FileStream.Free
    END
End;

function GIFDPI( GIFFileName : string ) : TPoint;
var
    gif : TGIFImage;
Begin
    gif := TGIFImage.Create();
    TRY
      gif.LoadFromFile( GIFFileName );
      Result.X := gif.ColorResolution;
      Result.Y := gif.ColorResolution;
    FINALLY
      gif.Free;
    END
End;

function DPIToPixelsPerMeter( res : integer): Cardinal;
Begin
  Result := Round( res / 0.0254 );
End;

function ImageResolutionCheck(img : TDWImage;
  template : CTemplate ): boolean;
var
  estRes : integer;
  bmp : Graphics.TBitmap;
  wbmp : Windows.TBitmap;
Begin
  if (( img.DPIX = 0 ) or ( img.DPIY = 0 )) then begin
    estRes := EstimatedImageResolution( img, template );
    if ( estRes > 0 ) then begin
      try
        bmp := Graphics.TBitmap.Create;
        bmp.Assign( img.BitMap );
//        wbmp := bmp;
        //wbmp.
        //bmp.
        //img.BitMap.Assign();
      finally
        FreeAndNil( bmp );
      end;          
    end;
  end;
End;

function EstimatedImageResolution(img : TDWImage;
  template : CTemplate ): integer; overload;
var
  doc_dpi, model_pixel_width, model_pixel_height : integer;
Begin
  Result := 0;
  if ( template.getData('doc_dpi') = '' ) then
    raise Exception.Create( 'Doc DPI missing the template.' );
  if ( template.getData('model_pixel_width') = '' ) then
    raise Exception.Create( 'Model Pixel Width is missing template.' );
  if ( template.getData('model_pixel_height') = '' ) then
    raise Exception.Create( 'Model Pixel Height is missing from template.' );
  doc_dpi := StrToInt( template.getData('doc_dpi') );
  model_pixel_width := StrToInt( template.getData('model_pixel_width') );
  model_pixel_height := StrToInt( template.getData('model_pixel_height') );
  Result := EstimatedImageResolution( img, model_pixel_width, model_pixel_height, doc_dpi );
End;

function EstimatedImageResolution(img : TDWImage;
  modelWidth, modelHeight, modelResolution : integer): integer;
var
  estResIdx, nLow, nHigh : integer;
  ratios : array[0..9] of Integer;
  ratioResolutions : array[0..9] of Extended;
  ratioWidth : Extended;
  ratioHeight : Extended;
  ratioWork : Extended;
  idxCurrentResolution : Integer;
  //ratioWidth : array[0..9] of Integer;
  //ratioHeight : array[0..9] of Integer;
  i : integer;
Begin
  ratioWork := 99999;

  ratios[0] := 72;
  ratios[1] := 90;
  ratios[2] := 100;
  ratios[3] := 150;
  ratios[4] := 200;
  ratios[5] := 300;
  ratios[6] := 600;
  ratios[7] := 900;
  ratios[8] := 1200;

  for I := 0 to 8 do
    ratioResolutions[i] := ratios[i] / modelResolution;

  ratioWidth := img.Width / modelWidth;
  ratioHeight := img.height / modelHeight;

  for I := 0 to 8 do begin
    if ( abs(ratioResolutions[i] - ratioWidth ) < ratioWork )  then begin
       estResIdx := i;
       ratioWork := abs(ratioResolutions[i] - ratioWidth );
    end;
  end;
  Result := ratios[estResIdx];
End;

END.
