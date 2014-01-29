unit ui2ximg_mcm;

interface

uses
  Windows,
  Classes,
  uStrUtil,
  Graphics,
  SysUtils,
  GR32,
  GR32_Image,
  uI2XImg,
  mcmRegion,
  mcmImage,
  mcmImageFilter,
  mcmImageTransform,
  mcmImageTypeDef,
  mcmImageColor,
  mcmImageResStr,
  MapStream,
  uDWImage,
  uI2XConstants,
  Typinfo;

const
  UNIT_NAME = 'ui2ximg_mcm';

type
  TImageInstructionsMCM = (
    iDESKEW = 0,
    iDESPECKLE,
    iCOLOR_REDUCE,
    iAVERAGE,
    iAVERAGEHEAVY,
    iBLUR,
    iBLUR_HEAVY,
    iGBLUR,
    iSMOOTH,
    iSMOOTH_CIRCLE,
    iSMOOTH_CONE,
    iSMOOTH_PYRAMIDAL,
    iSHARPEN,
    iSHARPENHEAVY,
    iUNSHARPMASK,
    iEDGE,
    iEDGEPREWITT,
    iHIGHPASS,
    iGREYEDGE,
    iDILATE,
    iSHARP_DRV,
    iNOISE_REMOVE,
    iDEGRAIN,
    iDEGRAIN_BRIGHT,
    iDEGRAIN_DARK,
    iERODE
  );
  //The class adds the ability to obtain the image from a memory map
  TDWMCMImage = class(TMCMImage)
    private
      FMemoryMap : TMapStream;
    public
      //function SaveToMemoryMap( const sMapName : string ) : boolean;
      //function LoadFromMemoryMap( const sMapName: string; const InitSize : Cardinal = IMAGE_MEMMAP_INIT_SIZE ): boolean;
      function CopyFrom( sourceImage : TMCMImage ): boolean;
      constructor Create();
      destructor Destroy; override;
  end;

  TI2XImgMCM = class(TI2XImg)
    private
      FImg : TDWMCMImage;
      FImgFilter : TmcmImageFilter;
    function DeskewableImage(img: TmcmImage): boolean;
    public
      function Deskew(): boolean;
      Function Despeckle( const level_ABC: string; const iterations : byte ): boolean; overload;
      Function Despeckle( const Parm: string  ) : boolean; overload;
      function ReduceColors( const colors: smallint = 8 ) : boolean; overload;
      function ReduceColors( const Parm : string ) : boolean; overload;
      function Average(): boolean;
      function AverageHeavy(): boolean;
      function Blur(): boolean;
      function BlurHeavy(): boolean;
      function GaussianBlur(): boolean;
      function Smooth(): boolean;
      function SmoothCircle(): boolean;
      function SmoothCone(): boolean;
      function SmoothPyramidal(): boolean;
      function Sharpen(): boolean;
      function SharpenHeavy(): boolean;
      function UnsharpenMask(): boolean;
      function Edge(): boolean;
      function EdgePewitt(): boolean;
      function HighPass(): boolean;
      function GreyEdge(): boolean;
      function Dilate(): boolean;
      function SharpenDerived(): boolean;
      function NoiseRemoval(): boolean;
      function Degrain(): boolean;
      function DegrainBright(): boolean;
      function DegrainDark(): boolean;
      function Erode(): boolean;

      function ExecuteInstruction( const InstructionString : string ) : boolean; override;
      constructor Create( const GUID, ParmFileName : string ); override;
      destructor Destroy;  override;
      function ProcessImage( const MemoryMapName : string; InstructionList : TStrings ) : boolean; override;
  end;

implementation

{ TI2XImgMCM }

function TI2XImgMCM.Average: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_AVERAGE );
    result := true;
  end;
end;

function TI2XImgMCM.AverageHeavy: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_AVERAGEHEAVY );
    result := true;
  end;
end;

function TI2XImgMCM.Blur: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_BLUR );
    result := true;
  end;
end;

function TI2XImgMCM.BlurHeavy: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_BLURHEAVY );
    result := true;
  end;
end;

constructor TI2XImgMCM.Create(const GUID, ParmFileName: string);
begin
  inherited Create( GUID, ParmFileName );
  FImg := TDWMCMImage.Create;
  FImgFilter := TMCMImageFilter.Create( nil );
end;

function TI2XImgMCM.Degrain: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_DEGRAIN );
    result := true;
  end;
end;

function TI2XImgMCM.DegrainBright: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_MEDIANMAXMIN );
    result := true;
  end;
end;

function TI2XImgMCM.DegrainDark: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_MEDIANMINMAX );
    result := true;
  end;
end;

function TI2XImgMCM.DeskewableImage( img : TmcmImage ): boolean;
Begin
{*
  Result := ( img.ImageFormat = IF_BW ) or
            ( img.ImageFormat = IF_GREY4 ) or
            ( img.ImageFormat = IF_PAL4 ) or
            ( img.ImageFormat = IF_GREY8  ) or
            ( img.ImageFormat = IF_BW )
  ;
*}
  Result := img.BitCount <= 8;
End;

function TI2XImgMCM.Deskew: boolean;
var
  ImageTransform : TmcmImageTransform;
  FImgResult : TmcmImage;
  skewAngle : Extended;
  imgColor1: TmcmImageColor;
begin
  try
    Result := false;

    ImageTransform := TmcmImageTransform.Create(nil);
    // Assign source image.
    if ( not DeskewableImage( FImg ) ) then begin
      try
        imgColor1 := TmcmImageColor.Create( nil );
        imgColor1.SourceImage[0] := FImg;
        imgColor1.ConvertTo( IF_BW, false );
        ImageTransform.SourceImage[0] := imgColor1.ResultImage;
        //FImg.Assign( imgColor1.ResultImage );
      finally
        FreeAndNil(imgColor1);
      end;
    end;

    //ImageTransform.SourceImage[0] := self.FImg;
    // Search for deskew angle within +30/-30 degree.
    ImageTransform.DeskewRange := 60;
    // Use 0.5 degree accuracy.
    //ImageTransform.DeskewStep := 0.5;
    ImageTransform.DeskewStep := 0.1;
    // Max character intensity(Luminance) to 63.
    ImageTransform.DeskewMaxInt := 1;
    ImageTransform.Deskew;
    if (ImageTransform.Error = EC_OK) then begin
       // If the angle is greater then |0.5| we'll rotate the image.
      if (abs(ImageTransform.Degree) >= 0.5) then begin
		   // Fill background White.
        ImageTransform.BKColor := $FFFFFF;
        ImageTransform.SourceImage[0] := FImg;
        if (ImageTransform.SourceImage[0].ImageFormat = IF_BW) then
          ImageTransform.Interpolate := ST_NEAREST
        else
          ImageTransform.Interpolate := ST_BILINEAR;
        try
          FImgResult := ImageTransform.Rotate( False, ImageTransform.Degree);
          if (ImageTransform.Error <> EC_OK) then
            raise Exception.Create('Error rotating image: ' + CErrorStrings[word(ImageTransform.Error)]);
          FImg.CopyFrom( FImgResult );
        finally
          if ( FImgResult <> nil ) then FreeAndNil( FImgResult );
        end;
      end;
    end else begin
      if (ImageTransform.Error <> EC_OK) then
      raise Exception.Create('Error finding deskew angle: ' + CErrorStrings[word(ImageTransform.Error)]);
    end;

    result := true;
  finally
    if ( ImageTransform <> nil ) then FreeAndNil( ImageTransform );
  end;
End;

function TI2XImgMCM.Despeckle(const Parm: string): boolean;
begin
  Result := false;
  self.ParseParmString( Parm );
  if ( (ParmMustExist('iterations')) and (ParmMustExist('level'))) then
    Result := Despeckle( Parms.Items['level'], StrToInt( Parms.Items['iterations'] ) );
end;

function TI2XImgMCM.Despeckle( const level_ABC: string; const iterations : byte ): boolean;
begin
  Result := false;
  with FImgFilter do begin
    MaxIterations := iterations;
    SourceImage[0] := FImg;
    ResultImage := FImg;
    if ( level_ABC = 'A' ) then
      Filter( FLT_DESPECKLE_A )
    else if ( level_ABC = 'A' ) then
      Filter( FLT_DESPECKLE_B )
    else
      Filter( FLT_DESPECKLE_C );
    result := true;
  end;
End;

destructor TI2XImgMCM.Destroy;
begin
  FreeAndNil( FImg );
  FreeAndNil( FImgFilter );
  inherited;
end;

function TI2XImgMCM.Dilate: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_MAXIMUM );
    result := true;
  end;
end;

function TI2XImgMCM.Edge: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_EDGE );
    result := true;
  end;
end;

function TI2XImgMCM.EdgePewitt: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_EDGEPREWITT );
    result := true;
  end;
end;

function TI2XImgMCM.Erode: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_MINIMUM );
    result := true;
  end;
end;

function TI2XImgMCM.ExecuteInstruction(
  const InstructionString: string): boolean;
var
  sInst, sParmString, key, val : string;
  ParmCount : byte;
  i : integer;
  bRes : boolean;
Begin
  try

    bRes := false;
    sInst := Split( InstructionString, IMGPROC_PARM_SEP, 0 ); //get the instruction
    sInst := Split( sInst, IMGPROC_PLUGIN_SEP, 1 );                          //strip the PlugIn Numonic
    ParmCount := StringCount(InstructionString, IMGPROC_VALUE_SEP );
    if ( Length( InstructionString ) > 0 ) then
    begin
      if ( Integer( self.DebugLevel ) > 0 ) then
        FImg.FileSave( self.DebugPath + 'MCM-' + sInst + '_IN.bmp' );
      case TImageInstructionsMCM(GetEnumValue(TypeInfo(TImageInstructionsMCM),'i' + sInst)) of
        iDESKEW:
          begin
            bRes := self.Deskew();
          end;
        iDESPECKLE:
          begin
            if ( ParmCount <> 2 ) then raise Exception.Create('Instruction "' + sInst + '" was passed an incorrect amount of parameters.  Parm string=' + InstructionString);
            bRes := self.Despeckle( InstructionString );
          end;
        iCOLOR_REDUCE:
          begin
            if ( ParmCount <> 1 ) then raise Exception.Create('Instruction "' + sInst + '" was passed an incorrect amount of parameters.  Parm string=' + InstructionString);
            bRes := self.ReduceColors( InstructionString );
          end;
        iAVERAGE:
          begin
            bRes := self.Average();
          end;
        iAVERAGEHEAVY:
          begin
            bRes := self.AverageHeavy();
          end;
        iBLUR:
          begin
            bRes := self.Blur();
          end;
        iBLUR_HEAVY:
          begin
            bRes := self.BlurHeavy();
          end;
        iGBLUR:
          begin
            bRes := self.GaussianBlur();
          end;
        iSMOOTH:
          begin
            bRes := self.Smooth();
          end;
        iSMOOTH_CIRCLE:
          begin
            bRes := self.SmoothCircle();
          end;
        iSMOOTH_CONE:
          begin
            bRes := self.SmoothCone();
          end;
        iSMOOTH_PYRAMIDAL:
          begin
            bRes := self.SmoothPyramidal();
          end;
        iSHARPEN:
          begin
            bRes := self.Sharpen();
          end;
        iSHARPENHEAVY:
          begin
            bRes := self.SharpenHeavy();
          end;
        iUNSHARPMASK:
          begin
            bRes := self.UnsharpenMask();
          end;
        iEDGE:
          begin
            bRes := self.Edge();
          end;
        iEDGEPREWITT:
          begin
            bRes := self.EdgePewitt();
          end;
        iHIGHPASS:
          begin
            bRes := self.HighPass();
          end;
        iGREYEDGE:
          begin
            bRes := self.GreyEdge();
          end;
        iDILATE:
          begin
            bRes := self.Dilate();
          end;
        iSHARP_DRV:
          begin
            bRes := self.SharpenDerived();
          end;
        iNOISE_REMOVE:
          begin
            bRes := self.NoiseRemoval();
          end;
        iDEGRAIN:
          begin
            bRes := self.Degrain();
          end;
        iDEGRAIN_BRIGHT:
          begin
            bRes := self.DegrainBright();
          end;
        iDEGRAIN_DARK:
          begin
            bRes := self.DegrainDark();
          end;
        iERODE:
          begin
            bRes := self.Erode();
          end;
      end;
      if (( bRes ) and ( Integer( self.DebugLevel ) > 0 )) then
        FImg.FileSave( self.DebugPath + 'MCM-' + sInst + '_OUT.bmp' );
      Result := bRes;
    end;
  finally
  end;
End;

function TI2XImgMCM.GaussianBlur: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_GAUSSBLUR );
    result := true;
  end;
end;

function TI2XImgMCM.GreyEdge: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_MARRHILDRETH );
    result := true;
  end;
end;

function TI2XImgMCM.HighPass: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_HIGHPASS );
    result := true;
  end;
end;

function TI2XImgMCM.NoiseRemoval: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_MEDIAN );
    result := true;
  end;
End;

function TI2XImgMCM.ProcessImage(const MemoryMapName: string;
  InstructionList: TStrings): boolean;
var
  i : integer;
  bmp : TBitmap;
  sMemoryMapName : string;
Begin
  try
    Result := false;
    sMemoryMapName := MemoryMapName;
    try
      FMemoryMapManager.Flush();

      bmp := TBitmap.Create();
      FMemoryMapManager.DebugMessageText := 'TI2XImgMCM.ProcessImage - Image Read from UI - InstructionList=' + InstructionList.Text;
      self.FMemoryMapManager.Read( sMemoryMapName, bmp );
      self.FImg.DibHandle := bmp.ReleaseHandle;
    finally
      bmp.Free;
    end;
    //self.FImg.LoadFromMemoryMap( MemoryMapName );

    try
      for i := 0 to InstructionList.Count - 1 do begin
        if ( not ExecuteInstruction( InstructionList[i] ) ) then
          Exit;
      end;
      //self.FImg.SaveToMemoryMap( MemoryMapName );
      try
        bmp := TBitmap.Create;
        bmp.Handle := FImg.ReleaseHandle;
        FMemoryMapManager.DebugMessageText := 'TI2XImgMCM.ProcessImage - Image Save for UI - InstructionList=' + InstructionList.Text;
        FMemoryMapManager.Write( sMemoryMapName, bmp );
      finally
        FreeAndNil( bmp );
      end;
      Result := true;
    except
      on E : Exception do
        begin
          self.ErrorUpdate( E.Message, ERROR_IMAGE_PROC_FAILED );
        end;
    end;
  finally
  end;
End;

function TI2XImgMCM.ReduceColors(const Parm: string): boolean;
begin
  Result := false;
  self.ParseParmString( Parm );
  if ( self.ParmMustExist( 'color_bit_count' )) then
    Result := ReduceColors( FStrUtil.ExtractInt( Parms.Items['color_bit_count'] ) );
end;

function TI2XImgMCM.ReduceColors(const colors: smallint) : boolean;
var
    imgColor1: TmcmImageColor;
Begin
  Result := false;
  try
    imgColor1 := TmcmImageColor.Create( nil );

    imgColor1.SourceImage[0] := FImg;
    //imgColor1.ResultImage := FImg;
    if ( colors <= 2 ) then
      imgColor1.ConvertTo( IF_BW, false )
    else if ( colors = 4 ) then
      imgColor1.ConvertTo( IF_PAL4, false )
    else if ( colors = 8 ) then
      imgColor1.ConvertTo( IF_PAL8, false )
    else if ( colors = 15) then
      imgColor1.ConvertTo( IF_RGB15, false )
    else if ( colors = 16) then
      imgColor1.ConvertTo( IF_RGB16, false )
    else if ( colors = 24) then
      imgColor1.ConvertTo( IF_RGB24, false );
    FImg.Assign( imgColor1.ResultImage );
    Result := true;
  finally
    FreeAndNil(imgColor1);
  end;
End;

function TI2XImgMCM.Sharpen: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_SHARPEN );
    result := true;
  end;
end;

function TI2XImgMCM.SharpenDerived: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_MAXMIN );
    result := true;
  end;
end;

function TI2XImgMCM.SharpenHeavy: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_SHARPENHEAVY );
    result := true;
  end;
end;

function TI2XImgMCM.Smooth: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_SMOOTH );
    result := true;
  end;
end;

function TI2XImgMCM.SmoothCircle: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_SMOOTHCIRCLE );
    result := true;
  end;
end;

function TI2XImgMCM.SmoothCone: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_SMOOTHCONE );
    result := true;
  end;
end;

function TI2XImgMCM.SmoothPyramidal: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_SMOOTHPYRAMIDAL );
    result := true;
  end;
end;

function TI2XImgMCM.UnsharpenMask: boolean;
begin
  Result := false;
  with FImgFilter do begin
    SourceImage[0] := FImg;
    ResultImage := FImg;
    Filter( FLT_UNSHARPMASK );
    result := true;
  end;
end;

{ TDWMCMImage }

function TDWMCMImage.CopyFrom(sourceImage: TMCMImage): boolean;
var
  MemoryStream:  TMemoryStream;
Begin
  Result := false;
  try
    MemoryStream := TMemoryStream.Create;
    sourceImage.SaveToStream( MemoryStream );
    MemoryStream.Position := 0;
    self.Clear;
    self.LoadFromStream( MemoryStream );
  finally
    FreeAndNil( MemoryStream );
  end;
End;

constructor TDWMCMImage.Create;
begin
  inherited Create();
end;

destructor TDWMCMImage.Destroy;
begin
  if ( FMemoryMap = nil ) then FreeAndNil( FMemoryMap );
  inherited;
end;

//This will read the bitmap from the memory stream.  since it is of TBitmap,
//  it will then get converted into the FreeBitmap
{*
function TDWMCMImage.LoadFromMemoryMap(const sMapName: string;
  const InitSize: Cardinal): boolean;
var
  msBMP : TMemoryStream;
  bmp : TBitmap;
begin
  Result := false;
  try
    if ( FMemoryMap = nil ) then
      FMemoryMap := ActiveMemoryMaps.Stream( sMapName, InitSize );
      //FMemoryMap := TMapStream.Create( sMapName, InitSize );
    msBMP := TMemoryStream.Create;
    bmp := TBitmap.Create;
    FMemoryMap.Position := 0;
    if ( not FMemoryMap.Read( msBMP ) ) then
      raise Exception.Create('Error while copying BITMAP from Memory Map');
    bmp.LoadFromStream( msBMP );
    self.DibHandle := bmp.ReleaseHandle;

    Result := true;
  finally
    FreeAndNil( msBMP );
    FreeAndNil( bmp );
  end;
end;

function TDWMCMImage.SaveToMemoryMap(const sMapName: string): boolean;
var
  msBMP : TMemoryStream;
  bmp : TBitmap;
begin
  Result := false;
  try
    if ( FMemoryMap = nil ) then
      //FMemoryMap := TMapStream.Create( sMapName, IMAGE_MEMMAP_INIT_SIZE );
      FMemoryMap := ActiveMemoryMaps.Stream( sMapName, IMAGE_MEMMAP_INIT_SIZE );
    msBMP := TMemoryStream.Create;
    bmp := TBitmap.Create;
    bmp.Handle := self.ReleaseHandle;
    bmp.SaveToStream( msBMP );
    FMemoryMap.Position := 0;
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
