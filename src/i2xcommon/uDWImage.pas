unit uDWImage;

interface
uses
  SysUtils,
  Classes,
  Windows,
  Graphics,
  uStrUtil,
  uFileDir,
  OmniXML,
  GR32,
  GR32_Image,
  GR32_Filters,
  MCMImage,
  mcmImageFile,
  MCMImageTransform,
  MCMImageColor,
  MCMImageTypeDef,
  uHashTable,
  MapStream,
  uI2XConstants,
  INIFiles,
  GraphicEx;

const
  UNIT_NAME = 'uDWImage';

type
  TDWImageInfo = record
    FileName : string;
    Width : Integer;
    Height : Integer;
    DPI : Integer;
  End;
  TTaskEvent = procedure (Sender: TObject; task : integer; action : integer) of object;
  //TDWImage = class(TObject)
  TDWImage = class(TPersistent)
    private
      _BMP : TBitmap;
      FFileName : string;
      BitmapHeader: pBitmapInfo;
      BitmapHeaderIsPtr: pointer;
      //_BMP : TBitmap32;
      FCreatedInternally : boolean;
      FMemoryMap : TMapStream;
      FMemoryMapName : string;
      FLastStreamSize : cardinal;
      function IsBMPHeaderSyned() : boolean;
      function getMemoryMap() : TMapStream;
      function getBitMapHandle() : HBITMAP;
      procedure setBitMapHandle( h : HBITMAP );
      function getMemoryMapHandle: THandle;
      function getColorCount: Cardinal;
      function getBitMapHeight: Cardinal;
      function getBitMapWidth: Cardinal;
      function getDPIX: Cardinal;
      function getDPIY: Cardinal;
      procedure setDPIX(const Value: Cardinal);
      procedure setDPIY(const Value: Cardinal);
      function getDPI() : Cardinal ;
      procedure setInternalBMP(const Value: TBitmap);   
      procedure BMPHeaderSyncCheck;
      procedure LoadExtDataFromFile( const ExtFileName : string );
      procedure SaveExtDataToFile( const ExtFileName : string );
      function getXPelsPerMeter: Cardinal;
      function getYPelsPerMeter: Cardinal;
      function getIsMemoryMapped: boolean;
    public
      procedure BMPHeaderUpdate;
      procedure WriteStreamBmp(Stream : TStream; Bmp: TBitmap);  // Thanks to http://www.delphipages.com/threads/thread.cfm?ID=142063&G=142063
      procedure ReadStreamBmp(Stream : TStream; var Bmp: TBitmap);  // Thanks to http://www.delphipages.com/threads/thread.cfm?ID=142063&G=142063
      property MemoryMap : TMapStream read getMemoryMap;
      property MemoryMapHandle : THandle read getMemoryMapHandle;
      property Bitmap : TBitmap read _BMP write setInternalBMP;
      property LastStreamSize : cardinal read FLastStreamSize write FLastStreamSize;
      property Handle : HBITMAP read getBitMapHandle write setBitMapHandle;
      property Width : Cardinal read getBitMapWidth;
      property Height : Cardinal read getBitMapHeight;
      property DPIX : Cardinal read getDPIX write setDPIX;
      property DPIY : Cardinal read getDPIY write setDPIY;
      property XPelsPerMeter : Cardinal read getXPelsPerMeter;
      property YPelsPerMeter : Cardinal read getYPelsPerMeter;
      property IsMemoryMapped : boolean read getIsMemoryMapped;
      procedure setDPI(const Value: Cardinal);
      function ReleaseHandle() : HBITMAP;
      function SaveToFile( const sFileName : string; const SaveExtData : boolean = false ) : boolean;
      function LoadFromFile( const sFileName : string ) : boolean;
      //function SaveToMemoryMap( var sMapName : string; TrimToSize : boolean = false ) : boolean;
      //function ClearMemoryMap() : boolean;
      //function LoadFromMemoryMap(const sMapName: string; const InitSize : Cardinal = IMAGE_MEMMAP_INIT_SIZE ): boolean;
      function CopyTo( ImageToCopyTo : TDWImage): boolean;
      function CopySlice( const SliceAttrib : TRect; ImageSlice : TDWImage ): boolean;
      property ColorCount : Cardinal read getColorCount;
      property FileName : string read FFileName;
      function ConvertToBW(): boolean;
      function Invert() : boolean;
      constructor Create( ); overload;
      constructor Create( bmp32 : TBitmap32 ); overload;
      constructor Create( bmp : TBitmap ); overload;
      constructor Create( bmp : HBITMAP ); overload;
      destructor Destroy; override;
      procedure AssignTo(Dest: TPersistent); override;
      procedure Assign(Src: TPersistent); override;
  end;

  TDWBitmap32 = class(TBitmap32)
    private
      FMemoryMap : TMapStream;
    public
      //function SaveToMemoryMap( const sMapName : string ) : boolean;
      //function ClearMemoryMap() : boolean;
      //function LoadFromMemoryMap( const sMapName: string; const InitSize : Cardinal = IMAGE_MEMMAP_INIT_SIZE ): boolean;
      constructor Create();
      destructor Destroy; override;
  end;
function TIFDPI( TIFFileName : string ) : TPoint;

implementation
function TIFDPI( TIFFileName : string ) : TPoint;
var
  img : TMCMImage;
Begin
  try
    img := TMCMImage.Create();
    img.FileOpen( TIFFileName );
    Result.X := Round( img.ImageInfo.XResolution / DPM_TO_DPI );
    Result.Y := Round( img.ImageInfo.YResolution / DPM_TO_DPI );
  finally
    FreeAndNil( img );
  end;
End;

// ============================================================================

// Method 2.  hDIB to TBitmap resulting in bmDIB using MemoryStream.
// Anatomy of a DIB written to stream :
// 1.  Bitmap File Header.  Normally 14 bytes, i.e., SizeOf(TBitmapFileHeader).
// 2.  Bitmap Info Header.  Normally 40 bytes, i.e., SizeOf(TBitmapInfoHeader)
// 3.  Color Table.  Bitmaps with > 256 colors do not have a color table.
// 4.  Bitmap Bits.
//
// Based on 12 July 1998 UseNet post "DIB to TBitmap" by Taine Gilliam in
// borland.public.delphi.vcl.components.using
// Taken from http://homepages.borland.com/efg2lab/Library/Delphi/Graphics/LeadToolsConversions.TXT
function hDIBtoTMemoryStream(const hDIB:  THandle; var MemoryStream:  TMemoryStream):  boolean;
VAR
    bmp : TBitmap;
    BitCount        :  INTEGER;
    BitmapFileHeader:  TBitmapFileHeader;
    BitmapInfo      :  pBitmapInfo;
    DIBinMemory     :  Pointer;
    NumberOfColors  :  INTEGER;
BEGIN
  //RESULT := TBitmap.Create;
  Result := false;
  DIBinMemory := GlobalLock(hDIB);
  TRY
    BitmapInfo := DIBInMemory;
    NumberOfColors := BitmapInfo.bmiHeader.biClrUsed;
    BitCount       := BitmapInfo.bmiHeader.biBitCount;
    IF   (NumberOfColors = 0) AND (BitCount <= 8)
    THEN NumberOfColors := 1 SHL BitCount;

    WITH BitmapFileHeader DO
    BEGIN
      bfType := $4D42;  // 'BM'
      bfReserved1 := 0;
      bfReserved2 := 0;
      bfOffBits := SizeOf(TBitmapFileHeader)       +
                   SizeOf(TBitmapInfoHeader)       +
                   NumberOfColors*SizeOf(TRGBQuad);
      bfSize := bfOffBits + BitmapInfo.bmiHeader.biSizeImage;
    END;

    TRY
      MemoryStream.Write(BitmapFileHeader, SizeOf(TBitmapFileHeader));
      MemoryStream.Write(DIBInMemory^,
                         BitmapFileHeader.bfSize - SizeOf(TBitmapFileHeader));
      MemoryStream.Position := 0;
    FINALLY
    END;
    Result := true;

  FINALLY
    GlobalUnlock(hDIB);
    GlobalFree(hDIB)
  END;
END {HBITMAPtoTMemoryStream};

{ TDWImage }

constructor TDWImage.Create;
begin
  FMemoryMap := nil;
  _BMP := TBitMap.Create();
  FCreatedInternally := true;
end;

constructor TDWImage.Create(bmp32: TBitmap32);
begin
  FMemoryMap := nil;
  _BMP := TBitMap.Create();
  Assign( bmp32 );
  //_BMP := bmp32;
  FCreatedInternally := true;
end;

constructor TDWImage.Create( bmp : TBitmap);
begin
  FMemoryMap := nil;
  Assign( bmp );
  //_BMP := bmp32;
  FCreatedInternally := true;
end;
{*
function TDWImage.ClearMemoryMap(): boolean;
begin
  Result := false;
  try
    if ( FMemoryMap <> nil ) then
      FMemoryMap.Free;
    Result := true;
  except
    raise;
  end;
end;
*}
function TDWImage.ConvertToBW: boolean;
var
    imgColor1: TmcmImageColor;
    imgWrk : TmcmImage;
    imgSrc : TmcmImage;
    bmpWork : TBitMap;
Begin
  try
    imgColor1 := TmcmImageColor.Create( nil );
    imgSrc := TmcmImage.Create;
    bmpWork := TBitMap.Create();

    bmpWork.Assign( _BMP );
    imgSrc.DibHandle := bmpWork.ReleaseHandle;

    imgColor1.SourceImage[0] := imgSrc;
    imgColor1.ConvertTo( IF_BW, false );
    imgWrk := imgColor1.ResultImage;

    bmpWork.Handle := imgWrk.ReleaseHandle;
    Assign( bmpWork );
  finally
    FreeAndNil( imgColor1 );
    if ( imgWrk <> nil ) then FreeAndNil( imgWrk );
    if ( imgSrc <> nil ) then FreeAndNil( imgSrc );
    if ( bmpWork <> nil ) then FreeAndNil( bmpWork );
  end;
End;

function TDWImage.CopySlice( const SliceAttrib : TRect; ImageSlice : TDWImage ): boolean;
Var
  oBMPSrc, oBMPDst : TBitMap;
  RectDst : TRect;
begin
  Result := false;
  try
    try
      if ( ImageSlice = nil ) then raise Exception.Create('ImageSlice must be preallocated before calling Copyslice');
      oBMPSrc := TBitMap.Create();
      oBMPDst := TBitMap.Create();
      oBMPSrc.Assign( _BMP );

      RectDst := Rect(0, 0, SliceAttrib.Right - SliceAttrib.Left,
                        SliceAttrib.Bottom - SliceAttrib.Top);
      oBMPDst.Width:=(SliceAttrib.Right - SliceAttrib.Left);
      oBMPDst.Height:=(SliceAttrib.Bottom - SliceAttrib.Top);
      oBMPDst.Canvas.CopyRect( RectDst, oBMPSrc.Canvas, SliceAttrib);

      ImageSlice.Assign( oBMPDst );
      ImageSlice.setDPI( self.DPIX );
      ImageSlice.BMPHeaderUpdate();
      Result := true;
    except
      raise;
    end;
  finally
    FreeAndNil( oBMPSrc );
    FreeAndNil( oBMPDst );
  end;
end;

function TDWImage.CopyTo( ImageToCopyTo  : TDWImage ): boolean;
Var
  oBMPSrc, oBMPDst : TBitMap;
  RectDst : TRect;
begin
  Result := false;
  try
    try
      if ( ImageToCopyTo  = nil ) then raise Exception.Create('ImageSlice must be preallocated before calling Copyslice');
      oBMPSrc := TBitMap.Create();
      oBMPDst := TBitMap.Create();
      oBMPSrc.Assign( _BMP );

      RectDst := Rect(0, 0, self.Width, self.Height);
      oBMPDst.Width:=self.Width;
      oBMPDst.Height:=self.Height;
      oBMPDst.Canvas.CopyRect( RectDst, oBMPSrc.Canvas, RectDst);

      ImageToCopyTo.Assign( oBMPDst );
      Result := true;
    except
      raise;
    end;
  finally
    FreeAndNil( oBMPSrc );
    FreeAndNil( oBMPDst );
  end;
end;

constructor TDWImage.Create(bmp: HBITMAP);
begin
  FMemoryMap := nil;
  //_BMP := TBitMap32.Create();
  _BMP := TBitMap.Create();
  _BMP.Handle := bmp;
  FCreatedInternally := true;
end;

destructor TDWImage.Destroy;
begin
  if (BitmapHeader<>nil) then FreeMem(BitmapHeader);
  //if (BitmapHeader<>nil) then FreeMem(BitmapImage);
  if ( self.FMemoryMap <> nil ) then
    FreeAndNil( FMemoryMap );
  if ( FCreatedInternally ) then
    FreeAndNil( _BMP );
  inherited;
end;

function TDWImage.getBitMapHandle: HBITMAP;
begin
  Result := self._BMP.Handle;
end;

function TDWImage.getBitMapHeight: Cardinal;
begin
  Result := 0;
  if (( _BMP <> nil ) and ( _BMP.Handle <> 0 )) then
    Result := _BMP.Width;
    //Result := Length( _BMP.BitmapInfo.bmiColors ) + 1;
end;

function TDWImage.getBitMapWidth: Cardinal;
begin
  Result := 0;
  if (( _BMP <> nil ) and ( _BMP.Handle <> 0 )) then
    Result := _BMP.Height;
    //Result := Length( _BMP.BitmapInfo.bmiColors ) + 1;
end;

function TDWImage.getDPI() : Cardinal;
var
   Stream: TMemoryStream;
   Data: Word;
   A: Double;
begin
    try
       Result := 0;
       Stream := TMemoryStream.Create;
       _BMP.SaveToStream(Stream);
       Stream.Position := 38;
       if Stream.Read(Data,2) = 2 then
          begin
          A := Data;
          Result := Round(A / 39.370079);
          end;
    finally
           Stream.Free;
    end;
end;

function TDWImage.GetDPIX: Cardinal;
begin
  Result := Round(self.BitmapHeader.bmiHeader.biXPelsPerMeter / 39.370079);
end;

function TDWImage.getDPIY: Cardinal;
begin
  Result := Round(self.BitmapHeader.bmiHeader.biYPelsPerMeter / 39.370079);
end;

function TDWImage.getIsMemoryMapped: boolean;
begin
  Result := (self.FMemoryMap <> nil);
end;

function TDWImage.getColorCount() : Cardinal;
begin
  Result := 0;
  if (( _BMP <> nil ) and ( _BMP.Handle <> 0 )) then
    Result := Length( self.BitmapHeader.bmiColors ) + 1;
    //Result := Length( _BMP.BitmapInfo.bmiColors ) + 1;
end;

function TDWImage.getMemoryMap: TMapStream;
begin
  if ( self.FMemoryMap <> nil )  then
    Result := FMemoryMap
  else if
  (
    ( self.FMemoryMap = nil ) and
    ( not ( (self._BMP = nil) or (self._BMP.Handle = 0) ))
  ) then begin

    Result := FMemoryMap;
  end else begin
    raise Exception.Create( 'Memory map for TDWImage cannot be created unless a bitmap has been assigned to it.' );
  end;
end;

function TDWImage.getMemoryMapHandle: THandle;
begin
  if ( self.MemoryMap = nil ) then
    Result := 0
  else
    Result := MemoryMap.MMHandle;
end;

function TDWImage.getXPelsPerMeter: Cardinal;
begin
  self.BMPHeaderSyncCheck;
  Result := self.BitmapHeader.bmiHeader.biXPelsPerMeter;
end;

function TDWImage.getYPelsPerMeter: Cardinal;
begin
  self.BMPHeaderSyncCheck;
  Result := self.BitmapHeader.bmiHeader.biYPelsPerMeter;
end;

function TDWImage.Invert: boolean;
var
  BMP32 : TBitmap32;
begin
  try
    BMP32 := TBitMap32.Create();
    try
      BMP32.Assign( _BMP );
      GR32_Filters.Invert( BMP32, BMP32 );
      Assign( BMP32 );
      result := true;
    except
      raise;
    end;             finally
    FreeAndNil( BMP32 );
  end;
End;


function TDWImage.IsBMPHeaderSyned: boolean;
begin
  Result := ( Pointer(_BMP) <> self.BitmapHeaderIsPtr );
end;

procedure TDWImage.SaveExtDataToFile(const ExtFileName: string);
var
  ini : TINIFile;
begin
  try
    ini := TIniFile.Create( ExtFileName );
    ini.WriteInteger( 'bitmap_ext', 'dpi_x', self.DPIX );
    ini.WriteInteger( 'bitmap_ext', 'dpi_y', self.DPIY );
  finally
    FreeAndNil( ini );
  end;
end;

function TDWImage.SaveToFile(const sFileName: string; const SaveExtData : boolean = false): boolean;
var
  sExtDataFileName, sExt : string;
begin
  FFileName := sFileName;
  self._BMP.SaveToFile( FFileName );
  if ( SaveExtData ) then begin
    sExt := ExtractFileExt( FFileName );
    sExtDataFileName := Copy(sFileName, 1, Length( FFileName ) - Length( sExt )) + '.INI'; 
    self.SaveExtDataToFile( sExtDataFileName );
  end;
end;
{*
function TDWImage.SaveToMemoryMap( var sMapName: string; TrimToSize : boolean ): boolean;
var
  msBMP : TMemoryStream;
begin
  Result := false;
  try
    msBMP := TMemoryStream.Create;
    //if ( self.FMemoryMap <> nil ) then
    //  self.FMemoryMap.Free;
    if ( Length( sMapName ) = 0 ) then
      sMapName := IMAGE_MEMMAP_HEADER + IntToStr( self._BMP.Handle );
    self.FMemoryMapName := sMapName;

    _BMP.SaveToStream( msBMP );
    FLastStreamSize := msBMP.Size;
    if ( FMemoryMap = nil ) then
      if ( TrimToSize ) then
        FMemoryMap := TMapStream.Create( sMapName, FLastStreamSize + 4 )
      else
        FMemoryMap := TMapStream.Create( sMapName, IMAGE_MEMMAP_INIT_SIZE );

    if ( not FMemoryMap.Write( msBMP ) ) then
      raise Exception.Create('Error while copying BITMAP from Memory Map');
    Result := true;
  finally
    FreeAndNil( msBMP );
  end;
end;
*}
procedure TDWImage.LoadExtDataFromFile(const ExtFileName: string);
var
  ini : TINIFile;
  DPIX, DPIY : cardinal;
begin
  try
    ini := TIniFile.Create( ExtFileName );
    DPIX := ini.ReadInteger( 'bitmap_ext', 'dpi_x', 0);
    DPIY := ini.ReadInteger( 'bitmap_ext', 'dpi_y', 0);
    if ( DPIY = DPIY ) then
      self.setDPI( DPIY )
    else begin
      self.setDPIX( DPIX );
      self.setDPIY( DPIY );
    end;
  finally
    FreeAndNil( ini );
  end;
end;

function TDWImage.LoadFromFile(const sFileName: string): boolean;
var
  FImg : TmcmImage;
  bmp : TBitMap;
  sExtDataFileName, sExt : string;
begin
  try
    Result := false;
    bmp := TBitMap.Create();
    FImg := TmcmImage.Create();
    FImg.FileOpen( sFileName );
    bmp.Handle := FImg.ReleaseHandle;

    Assign( bmp );
    FFileName := sFileName;
    sExt := ExtractFileExt( sFileName );
    sExtDataFileName := Copy(sFileName, 1, Length( sFileName ) - Length( sExt )) + '.INI';
    if ( FileExists(sExtDataFileName) ) then begin
      self.LoadExtDataFromFile( sExtDataFileName );
    end;
    Result := true;
  finally
    FreeAndNil( bmp );
    FreeAndNil( FImg );
  end;
end;

{*
function TDWImage.LoadFromMemoryMap( const sMapName: string;
    const InitSize : Cardinal ): boolean;
var
  msBMP : TMemoryStream;
begin
  FFileName := '';
  Result := false;
  try
    msBMP := TMemoryStream.Create;
    if ( FMemoryMap = nil ) then begin
      FMemoryMap := TMapStream.Create( sMapName, IMAGE_MEMMAP_INIT_SIZE );
    end;

    _BMP.FreeImage;
    //_BMP.Clear;
    if ( not FMemoryMap.Read( msBMP ) ) then
      raise Exception.Create('Error while copying BITMAP to Memory Map');
    self.ClearMemoryMap();
    _BMP.LoadFromStream( msBMP );
    FLastStreamSize := msBMP.Size;
    self.BMPHeaderUpdate();

    Result := true;
  finally
    FreeAndNil( msBMP );
  end;
end;
*}
procedure TDWImage.ReadStreamBmp(Stream: TStream; var Bmp: TBitmap);
var
ms: TMemoryStream;
sz : int64;
begin
  Stream.Read(sz, SizeOf(sz));
  FLastStreamSize := sz;
  if sz > 0 then begin
    ms := TMemoryStream.Create;
    try
      ms.CopyFrom(Stream, sz);
      ms.Position := 0;
      Bmp.LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end;
end;

function TDWImage.ReleaseHandle: HBITMAP;
begin
  try
    Result := _BMP.ReleaseHandle;
  finally
  end;
end;

procedure TDWImage.setBitMapHandle( h : HBITMAP );
begin
  try
    _BMP.Handle := h;
  finally
  end;
end;

procedure TDWImage.setDPI(const Value: Cardinal);
var
   Stream: TMemoryStream;
   Data: Word;
begin
    try
       Stream := TMemoryStream.Create;
       Bitmap.SaveToStream(Stream);
       Data := Round(Value * 39.370079);
       Stream.Position := 38;
       if Stream.Write(Data,2) = 2 then
          begin
          Stream.Position := 42;
          if Stream.Write(Data,2) = 2 then
              begin
                Stream.Position := 0;
                Bitmap.LoadFromStream(Stream);
                self.BMPHeaderUpdate;
              end
          else
            raise Exception.Create('Error writing to Stream. Data not written.');
          end
       else
        raise Exception.Create('Error writing to Stream. Data not written.');
    finally
      Stream.Free;
    end;
end;

procedure TDWImage.setDPIX(const Value: Cardinal);
var
  bmp : TBitmap;
  Stream: TMemoryStream;
  Data: Word;
begin
  try
    bmp := TBitmap.Create;
    //bmp.Assign( self.BitMap );
    Stream := TMemoryStream.Create;
    self._BMP.SaveToStream( Stream );
    //bmp.SaveToStream(Stream);
    Data := Round(Value * 39.370079);
    Stream.Position := 38;
    if Stream.Write(Data,2) = 2 then
    begin
      Stream.Position := 0;
      //bmp.LoadFromStream(Stream);
      //self.BitMap.Assign( bmp );
      self._BMP.LoadFromStream( Stream );
    end
    else
      raise Exception.Create('Error writing to Stream. Data not written.');
    self.BMPHeaderUpdate;
  finally
    Stream.Free;
    FreeAndNil( bmp );
  end;
end;

procedure TDWImage.setDPIY(const Value: Cardinal);
var
  Stream: TMemoryStream;
  Data: Word;
begin
  try
    Stream := TMemoryStream.Create;
    self._BMP.SaveToStream( Stream );
    Data := Round(Value * 39.370079);
    Stream.Position := 42;
    if Stream.Write(Data,2) = 2 then
    begin
      Stream.Position := 0;
      self._BMP.LoadFromStream( Stream );
    end
    else
      raise Exception.Create('Error writing to Stream. Data not written.');
    BMPHeaderUpdate;
  finally
    Stream.Free;
  end;
end;

procedure TDWImage.setInternalBMP(const Value: TBitmap);
begin
  Assign( Value );
end;

procedure TDWImage.Assign(Src: TPersistent);
begin
  //inherited;
  self._BMP.Assign( Src );
  self.BMPHeaderUpdate();
end;

procedure TDWImage.AssignTo(Dest: TPersistent);
begin
  //inherited;
  Dest.Assign( self._BMP );
end;

procedure TDWImage.BMPHeaderSyncCheck;
begin
  if ( not self.IsBMPHeaderSyned() ) then self.BMPHeaderUpdate();
end;

procedure TDWImage.BMPHeaderUpdate();
Var
Bitmap: TBitmap;
BitmapImage : POINTER;
HeaderSize : DWORD;
ImageSize : DWORD;
Begin
  if (BitmapHeader<>nil) then FreeMem(BitmapHeader);

  GetDIBSizes(_BMP.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage, ImageSize);

  GetDIB(_BMP.Handle, _BMP.Palette, BitmapHeader^, BitmapImage^);
  BitmapHeaderIsPtr := pointer( self._BMP );
  BitmapHeader.bmiHeader.biXPelsPerMeter:= Round(getDPI * 39.370079);
  BitmapHeader.bmiHeader.biYPelsPerMeter:= BitmapHeader.bmiHeader.biXPelsPerMeter;

  if (BitmapHeader<>nil) then FreeMem(BitmapImage);
End;

procedure TDWImage.WriteStreamBmp(Stream: TStream; Bmp: TBitmap);
var
ms: TMemoryStream;
sz : int64;
begin
  ms := TMemoryStream.Create;
  try
    Bmp.SaveToStream(ms);
    sz := ms.Size;
    FLastStreamSize := sz;
    Stream.Write(sz, SizeOf(sz));
    if sz > 0 then
      Stream.CopyFrom(ms, 0);
  finally
    ms.Free;
  end;
end;

{ TDWBitmap32 }
{*
function TDWBitmap32.ClearMemoryMap: boolean;
begin
  if ( FMemoryMap <> nil ) then FMemoryMap.Free;
end;
*}
constructor TDWBitmap32.Create;
begin
  inherited Create();
end;

destructor TDWBitmap32.Destroy;
begin
  if ( FMemoryMap <> nil ) then FreeAndNil( FMemoryMap );
  inherited;
end;
{*
//This will read the bitmap from the memory stream.  since it is of TBitmap,
//  it will then get converted into the FreeBitmap
function TDWBitmap32.LoadFromMemoryMap(const sMapName: string;
  const InitSize: Cardinal): boolean;
var
  msBMP : TMemoryStream;
  bmp : TBitmap;
begin
  Result := false;
  try
    if ( FMemoryMap = nil ) then begin
      if ( self.FMemoryMapManager = nil ) then
        raise Exception.Create( 'Cannot create a memory map stream unless you have a memory map manager assigned to this object' );
      FMemoryMap := FMemoryMapManager.Stream( sMapName, InitSize );
      //FMemoryMap := TMapStream.Create( FMemoryMapManager, sMapName, InitSize );
    end;

    msBMP := TMemoryStream.Create;
    bmp := TBitmap.Create;
    if ( not FMemoryMap.Read( msBMP ) ) then
      raise Exception.Create('Error while copying BITMAP from Memory Map');
    bmp.LoadFromStream( msBMP );
    self.Assign( bmp );
    Result := true;
  finally
    FreeAndNil( msBMP );
    FreeAndNil( bmp );
  end;
end;

function TDWBitmap32.SaveToMemoryMap(const sMapName: string): boolean;
var
  msBMP : TMemoryStream;
  bmp : TBitmap;
begin
  Result := false;
  try
    if ( FMemoryMap = nil ) then begin
      if ( self.FMemoryMapManager = nil ) then
        raise Exception.Create( 'Cannot create a memory map stream unless you have a memory map manager assigned to this object' );
      FMemoryMap := FMemoryMapManager.Stream( sMapName, IMAGE_MEMMAP_INIT_SIZE );
      //FMemoryMap := TMapStream.Create( FMemoryMapManager, sMapName, IMAGE_MEMMAP_INIT_SIZE );
    end;
    msBMP := TMemoryStream.Create;
    bmp := TBitmap.Create;
    self.AssignTo( bmp );
    bmp.SaveToStream( msBMP );
    if ( not FMemoryMap.Write( msBMP ) ) then
      raise Exception.Create('Error while copying BITMAP from Memory Map');
    Result := true;
  finally
    FreeAndNil( msBMP );
    FreeAndNil( bmp );
  end;
end;
*}
END.
