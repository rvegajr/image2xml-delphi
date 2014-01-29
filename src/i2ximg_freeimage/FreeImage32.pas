unit FreeImage32;
// http://www.tech-mash.narod.ru/
//http://superb-east.dl.sourceforge.net/sourceforge/freeimage/FreeImage3100.pdf
interface

uses
  Windows,
  FreeImage,
  GR32;

function FreeImage_CopyToBitmap32(SrcDib: PFIBITMAP; DestBitmap32: TBitmap32): Boolean;
function FreeImage_CopyFromBitmap32(DestBitmap32: TBitmap32): PFIBITMAP;

implementation

function FreeImage_CopyToBitmap32(SrcDib: PFIBITMAP; DestBitmap32: TBitmap32): Boolean;
var
  dib32: PFIBITMAP;
  DeleteDib: Boolean;
  w, h: Integer;
begin
  Result := False;
  DeleteDib := False;

  if (SrcDib = nil) or (DestBitmap32 = nil) then Exit;

  if FreeImage_GetBPP(SrcDib) <> 32 then
  begin
    dib32 := FreeImage_ConvertTo32Bits(SrcDib);
    DeleteDib := True;
  end
  else
    dib32 := SrcDib;

  w := FreeImage_GetWidth(dib32);
  h := FreeImage_GetHeight(dib32);

  DestBitmap32.SetSize(w, h);

  CopyMemory(DestBitmap32.Bits, FreeImage_GetBits(dib32), w * h * 4);

  DestBitmap32.FlipVert;

  if DeleteDib then FreeImage_Unload(dib32);
  Result := True;
  DestBitmap32.Changed;
end;

function FreeImage_CopyFromBitmap32(DestBitmap32: TBitmap32): PFIBITMAP;
begin
  Result := nil;
  if (DestBitmap32 = nil) or DestBitmap32.Empty then Exit;

  Result := FreeImage_Allocate(DestBitmap32.Width, DestBitmap32.Height, 32);
  if Result <> nil then
  begin
    CopyMemory(FreeImage_GetBits(Result), DestBitmap32.Bits, DestBitmap32.Width * DestBitmap32.Height * 4);
    FreeImage_FlipVertical(Result);
  end;
end;


end.
