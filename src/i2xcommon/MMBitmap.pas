unit MMBitmap;

interface

{$DEFINE ODS}


uses
  Windows,      { for HPalette }
  Classes,      { for TStream }
  Graphics      { for TBitmap}
  ;


type
  TMMBitmap = class(TBitmap)
  private

    FCanvas: TCanvas;
    FIgnorePalette: Boolean;
    FTransparentMode: TTransparentMode;
    FFileName: string;
    FPalette : HPalette;
    FData    : pointer;
    FBitmapWidth,
    FBitmapHeight,
    FColours : integer;
    FActive  : boolean;
    FFileHeader : PBitmapFileHeader;
    FInfoHeader : PBitmapInfoHeader;
    FInfo : PBitmapInfo;
    FPixelStart : pointer;
    fPixelFormat : TPixelFormat;

    function GetCanvas: TCanvas;
    function GetHandle: HBITMAP;
    function GetHandleType: TBitmapHandleType;
    function GetMaskHandle: HBITMAP;
    function GetMonochrome: Boolean;
    function GetPixelFormat: TPixelFormat;
    function GetScanline(Row: Integer): Pointer;
    function GetTransparentColor: TColor;
    procedure SetHandle(Value: HBITMAP);
    procedure SetHandleType(Value: TBitmapHandleType); virtual;
    procedure SetMaskHandle(Value: HBITMAP);
    procedure SetMonochrome(Value: Boolean);
    procedure SetPixelFormat(Value: TPixelFormat);
    procedure SetTransparentColor(Value: TColor);
    procedure SetTransparentMode(Value: TTransparentMode);
    function TransparentColorStored: Boolean;

    procedure GetBitmapPalette;
    procedure MapFile;
    procedure UnmapFile;

  protected

    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure Changed(Sender: TObject); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    procedure HandleNeeded;
    procedure MaskHandleNeeded;
    procedure PaletteNeeded;
    procedure ReadData(Stream: TStream); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetWidth(Value: Integer); override;
    procedure WriteData(Stream: TStream); override;

  public

    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure FreeImage;
    function HandleAllocated: Boolean;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    procedure Mask(TransparentColor: TColor);
    function ReleaseHandle: HBITMAP;
    function ReleaseMaskHandle: HBITMAP;
    function ReleasePalette: HPALETTE;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
    procedure SaveToStream(Stream: TStream); override;
    property Canvas: TCanvas read GetCanvas;
    property Handle: HBITMAP read GetHandle write SetHandle;
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property IgnorePalette: Boolean read FIgnorePalette write FIgnorePalette;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property TransparentColor: TColor read GetTransparentColor
      write SetTransparentColor stored TransparentColorStored;
    property TransparentMode: TTransparentMode read FTransparentMode
      write SetTransparentMode default tmAuto;

    procedure LoadFromFile(const FileName : string); override;

  end;


implementation

uses
  sysutils      { for exception }
  ;



const
  BitmapSignature = $4D42;


{ TRSIMMBitmap }


resourcestring

  sNotImplementable = 'Feature cannot be implemented'#13#10'- sorry ;-)';
  sNotImplementedYet = 'Feature not implemented yet '#13#10'- sorry ;-)';
  sFailedOpen = 'Failed to open %s';
  sFailedMap = 'Failed to map file';
  sFailedMapView = 'Failed to map view of file';
  sInvalidBitmap = 'Invalid Bitmap';


procedure TraceMessage(const Msg : string);
begin
{$IFDEF ODS}
  OutputDebugString(Pchar(Msg));
{$ENDIF}
end;


procedure TMMBitmap.Assign(Source: TPersistent);
begin
  if not( Source is TMMBitmap) then
    inherited
  else
  begin
    { TODO : Maybe this could be improved by sharing the file mapping? }
    { however, this works, although multiple assigns could run the application
      out of memory space, if all the mappings get unique ranges }
    LoadFromFile(TMMBitmap(Source).fFileName);
  end;
end;

procedure TMMBitmap.Changed(Sender: TObject);
begin
  { not going to happen}
end;

constructor TMMBitmap.Create;
begin
  TraceMessage('Create');
  inherited;
end;

destructor TMMBitmap.Destroy;
begin
  TraceMessage('Destroy');
  UnMapFile;
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  inherited;
end;


procedure TMMBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  OldMode    : integer;
  OldPalette : HPalette;
begin
  
  with ACanvas do
  begin
      if FPalette <> 0 then
        OldPalette := SelectPalette (Handle, FPalette, false)
      else
        OldPalette := 0;

      try
        RealizePalette (Handle);

        if FColours = 2 then
          OldMode := SetStretchBltMode (Handle, BLACKONWHITE)
        else
          OldMode := SetStretchBltMode (Handle, COLORONCOLOR);
        try

          StretchDIBits (Handle,
                         Rect.Left, Rect.Top,
                         Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
                         0, 0, FBitmapWidth, FBitmapHeight,
                         FPixelStart, FInfo^,
                         DIB_RGB_COLORS,
                         SRCCOPY)
        finally
          SetStretchBltMode (Handle, OldMode)
        end
      finally

        if OldPalette <> 0 then
          SelectPalette (Handle, OldPalette, false)
      end
    end
end;

procedure TMMBitmap.FreeImage;
begin
  { nothing here}
end;

function TMMBitmap.GetCanvas: TCanvas;
begin
  if not Assigned(fCanvas) then
    fCanvas := TCanvas.Create;
  { the beauty of this is that TCanvas.CreateHandle is blank,
    so anyone trying to use the canvas will get a "Canvas does not allow drawing"}
  Result := FCanvas;
end;

function TMMBitmap.GetEmpty: Boolean;
begin
  Result := not fActive;
end;

function TMMBitmap.GetHandle: HBITMAP;
begin
  TraceMessage('Attempt to GetHandle');
  raise Exception.Create(sNotImplementable);
end;

function TMMBitmap.GetHandleType: TBitmapHandleType;
begin
  TraceMessage('Attempt to GetHandleType');
  raise Exception.Create(sNotImplementable);
end;

function TMMBitmap.GetHeight: Integer;
begin
  if FActive then
    Result := FBitmapHeight
  else
    Result := 0;
end;

function TMMBitmap.GetMaskHandle: HBITMAP;
begin
  TraceMessage('Attempt to GetMaskHandle');
  raise Exception.Create(sNotImplementable);
end;

function TMMBitmap.GetMonochrome: Boolean;
begin
  Result := (FColours = 2);
end;

function TMMBitmap.GetPalette: HPALETTE;
begin
  Result := fPalette;
end;

function TMMBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := fPixelFormat;
end;

function TMMBitmap.GetScanline(Row: Integer): Pointer;
begin
  
  TraceMessage('Attempt to GetScanline');
  raise Exception.Create(sNotImplementedYet);
end;

function TMMBitmap.GetTransparentColor: TColor;
begin
  TraceMessage('Attempt to GetTransparentColor');
  raise Exception.Create(sNotImplementedYet);
end;

function TMMBitmap.GetWidth: Integer;
begin
  if FActive then
    Result := FBitmapWidth
  else
    Result := 0;
end;

function TMMBitmap.HandleAllocated: Boolean;
begin
  Result := fActive;
end;

procedure TMMBitmap.HandleNeeded;
begin
  { nothing }
end;

procedure TMMBitmap.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
  TraceMessage('Attempt to LoadFromClipboardFormat');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.LoadFromFile(const FileName: string);
begin
  { override this to allow grabbing of the file to Memory Map }
  if not (FileExists(FileName)) then
  begin
    raise Exception.CreateFmt('File %s does not exist', [FileName]);
  end
  else
  begin
    if fActive then
      UnmapFile;
      
    FFileName := FileName;
    MapFile;
  end;

end;

procedure TMMBitmap.LoadFromResourceID(Instance: THandle;
  ResID: Integer);
begin
  TraceMessage('Attempt to LoadFromResourceID');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.LoadFromResourceName(Instance: THandle;
  const ResName: String);
begin
  TraceMessage('Attempt to LoadFromResourceName');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.LoadFromStream(Stream: TStream);
begin
  TraceMessage('Attempt to LoadFromStream');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.Mask(TransparentColor: TColor);
begin
  TraceMessage('Attempt to Mask');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.MaskHandleNeeded;
begin
  {}
end;


procedure TMMBitmap.PaletteNeeded;
begin
  {}
end;

procedure TMMBitmap.ReadData(Stream: TStream);
begin
  TraceMessage('Attempt to ReadData');
  raise Exception.Create(sNotImplementable);
end;


function TMMBitmap.ReleaseHandle: HBITMAP;
begin
  TraceMessage('Attempt to ReleaseHandle');
  raise Exception.Create(sNotImplementable);
end;

function TMMBitmap.ReleaseMaskHandle: HBITMAP;
begin
  TraceMessage('Attempt to ReleaseMaskHandle');
  raise Exception.Create(sNotImplementable);
end;

function TMMBitmap.ReleasePalette: HPALETTE;
begin
  TraceMessage('Attempt to ReleasePalette');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SaveToClipboardFormat(var Format: Word;
  var Data: THandle; var APalette: HPALETTE);
begin
  TraceMessage('Attempt to SaveToClipboardFormat');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SaveToStream(Stream: TStream);
begin
  TraceMessage('Attempt to SaveToStream');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetHandle(Value: HBITMAP);
begin
  TraceMessage('Attempt to SetHandle');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetHandleType(Value: TBitmapHandleType);
begin
  TraceMessage('Attempt to SetHandleType');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetHeight(Value: Integer);
begin
  TraceMessage('Attempt to SetHeight');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetMaskHandle(Value: HBITMAP);
begin
  TraceMessage('Attempt to SetMaskHandle');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetMonochrome(Value: Boolean);
begin
  TraceMessage('Attempt to SetMonochrome');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetPalette(Value: HPALETTE);
begin
  TraceMessage('Attempt to SetPalette');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetPixelFormat(Value: TPixelFormat);
begin
  TraceMessage('Attempt to SetPixelFormat');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.SetTransparentColor(Value: TColor);
begin
  TraceMessage('Attempt to SetTransparentColor');
  raise Exception.Create(sNotImplementedYet);
end;

procedure TMMBitmap.SetTransparentMode(Value: TTransparentMode);
begin
  TraceMessage('Attempt to SetTransparentMode');
  raise Exception.Create(sNotImplementedYet);
end;

procedure TMMBitmap.SetWidth(Value: Integer);
begin
  TraceMessage('Attempt to SetWidth');
  raise Exception.Create(sNotImplementable);
end;

function TMMBitmap.TransparentColorStored: Boolean;
begin
  TraceMessage('Attempt to SetTransparentColorStored');
  raise Exception.Create(sNotImplementedYet);
end;

procedure TMMBitmap.WriteData(Stream: TStream);
begin
  TraceMessage('Attempt to WriteData');
  raise Exception.Create(sNotImplementable);
end;

procedure TMMBitmap.GetBitmapPalette;
var
  SysPalSize,
  Loop,
  LogSize : integer;
  LogPalette : PLogPalette;
  DC : HDC;
  Focus : HWND;
begin
  FPalette := 0;

  if FColours > 2 then
  begin
    LogSize := SizeOf (TLogPalette) + pred(FColours) * SizeOf(TPaletteEntry);
    LogPalette := AllocMem (LogSize);
    try
      with LogPalette^ do
      begin
        palNumEntries := FColours;
        palVersion := $0300;

(* I prefer to test programs with $R+, but this section of the program
   must be compiled with $R-.  This $IFOPT enables the restoration of
   $R+ condition later on, but only if set now. *)
{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$R-}
{$ENDIF}
        Focus := GetFocus;
        DC := GetDC (Focus);
        try
          SysPalSize := GetDeviceCaps (DC, SIZEPALETTE);
          if (FColours = 16) and (SysPalSize >= 16) then
          begin
            GetSystemPaletteEntries (DC, 0, 8, palPalEntry);
            loop := 8;
            GetSystemPaletteEntries (DC, SysPalSize - loop, loop, palPalEntry[loop])
          end else
            with FInfo^ do
              for loop := 0 to pred (FColours) do
              begin
                palPalEntry[loop].peRed   := bmiColors[loop].rgbRed;
                palPalEntry[loop].peGreen := bmiColors[loop].rgbGreen;
                palPalEntry[loop].peBlue  := bmiColors[loop].rgbBlue
              end
        finally
          ReleaseDC(Focus, DC)
        end
{$IFDEF R_PLUS}
  {$R+}
  {$UNDEF R_PLUS}
{$ENDIF}
      end;
      FPalette := CreatePalette (LogPalette^)
    finally
      FreeMem (LogPalette, LogSize)
    end
  end
end;

procedure TMMBitmap.MapFile;
var
  FileHandle,
  MapHandle : THandle;
begin
  TraceMessage('MapFile');
  if (fActive) then
    exit;

  FileHandle := CreateFile(PChar(FFilename),
                           GENERIC_READ,
                           0, nil,
                           OPEN_EXISTING,
                           FILE_FLAG_SEQUENTIAL_SCAN, 0);

  if FileHandle = INVALID_HANDLE_VALUE then
    raise Exception.CreateFmt(sFailedOpen, [FFilename]);

  try
    MapHandle := CreateFileMapping (FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if MapHandle = 0 then
      raise Exception.Create(sFailedMap)
  finally
    CloseHandle (FileHandle)
  end;

  try
    FData := MapViewOfFile (MapHandle, FILE_MAP_READ, 0, 0, 0);
    if FData = nil then
      raise Exception.Create(sFailedMapView)
  finally
    CloseHandle (MapHandle)
  end;

  FFileHeader := FData;

  if FFileHeader^.bfType <> BitmapSignature then
  begin
    UnmapViewOfFile (FData);
    FData := nil;
    raise Exception.Create(sInvalidBitmap);
  end;

  FInfoHeader := pointer (integer (FData) + sizeof (TBitmapFileHeader));
  FInfo := pointer (FInfoHeader);
  FPixelStart := pointer (Cardinal(FData) + FFileHeader^.bfOffBits);

  with FInfoHeader^ do
    if biClrUsed <> 0 then
      FColours := biClrUsed
    else
      case biBitCount of
        1,
        4,
        8 : FColours := 1 shl biBitCount
      else
        FColours := 0
      end;

  fPixelFormat := pfDevice;
  with FInfoHeader^ do
      case biBitCount of
        1: fPixelFormat := pf1Bit;
        4: fPixelFormat := pf4Bit;
        8: fPixelFormat := pf8Bit;
       16: case biCompression of
             BI_RGB : fPixelFormat := pf15Bit;
           end;
       24: fPixelFormat := pf24Bit;
       32: if biCompression = BI_RGB then fPixelFormat := pf32Bit;
      end;

  FBitmapHeight := FInfoHeader^.biHeight;
  FBitmapWidth := FInfoHeader^.biWidth;

  GetBitmapPalette;

  FActive := true;
  Changed(self);
end;

procedure TMMBitmap.UnMapFile;
begin
  TraceMessage('UnMapFile');
  FActive := false;
  begin
    if FData <> nil then
    begin
      UnmapViewOfFile(FData);
      FData := nil
    end;
    if FPalette <> 0 then
    begin
      DeleteObject(FPalette);
      FPalette := 0;
    end;
  end
end;

END.

