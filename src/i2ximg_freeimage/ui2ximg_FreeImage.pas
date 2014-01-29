unit ui2ximg_FreeImage;

interface

uses
  Windows,
  Classes,
  GR32,
  GR32_Image,
  MapStream,
  uStrUtil,
  Graphics,
  SysUtils,
  uI2XImg,
  uI2XPlugin,
  FreeImage,
  FreeImage32,
  FreeBitmap,
  uDWImage,
  uI2XConstants;

const
  UNIT_NAME = 'ui2ximg_FreeImage';
  TEMP_DIR_QUAL = 'DW\';
  TEMP_DIR_SYMBOL = '<TEMP>DW\';
  PARM_NAME='i2ximg_freeimage';

type
  TStatusChangeEvent = procedure (Sender: TObject; msgText : string) of object;

  TDWFreeWinBitmap = class(TFreeWinBitmap)
    private
      FMemoryMap : TMapStream;
    public
      //function SaveToMemoryMap( const sMapName : string ) : boolean;
      //function LoadFromMemoryMap( const sMapName: string; const InitSize : Cardinal = IMAGE_MEMMAP_INIT_SIZE ): boolean;
      constructor Create();
      destructor Destroy; override;
  end;

  TI2XImgFreeImage = class(TI2XImg)
    private
      FFIBMP : TDWFreeWinBitmap;
    public
      function Threshold( const T : byte ) : boolean; overload;
      function Threshold( const parm : string ) : boolean; overload;
      function ReduceColors( const colorCount: smallint = 256 ) : boolean; overload;
      function ReduceColors( const parm : string ) : boolean; overload;
      function Resize( const x, y : cardinal ) : boolean; overload;
      function Resize( const sXParm, sYParm : string ) : boolean; overload;
      function ExecuteInstruction( const InstructionString : string ) : boolean; override;
      constructor Create( const GUID, ParmFileName : string ); override;
      destructor Destroy; override;
      function ProcessImage( const MemoryMapName : string; InstructionList : TStrings ) : boolean; override; 
  end;

implementation

{ TI2XImgFreeImage }

constructor TI2XImgFreeImage.Create( const GUID, ParmFileName : string );
begin
  FFIBMP := TDWFreeWinBitmap.Create();
  inherited Create( GUID, ParmFileName );
end;

destructor TI2XImgFreeImage.Destroy;
begin
  FreeAndNil( FFIBMP );
  inherited;
end;

function TI2XImgFreeImage.ExecuteInstruction(
  const InstructionString: string): boolean;
var
  sInst, key, val : string;
  sl : TStringList;
  i : integer;
  bRes : boolean;
Begin
  try
    try
      sl := TStringList.Create;

      bRes := false;
      if ( FStrUtil.ParseString( InstructionString, IMGPROC_PARM_SEP, sl, true ) > 0 ) then
      begin
        if ( Pos('GREY_THRESH', sl[0] ) > 0 ) then begin
          if ( sl.Count <> 2 ) then raise PluginException.Create('Instruction "GREY_THRESH" was passed an incorrect amount of parameters.  Parm string=' + InstructionString, ERROR_PARM_INVALID, self);
          bRes := self.Threshold( sl[1] );
        end else if ( Pos('COLOR_REDUCE', sl[0] ) > 0 ) then begin
          if ( sl.Count <> 2 ) then raise PluginException.Create('Instruction "COLOR_REDUCE" was passed an incorrect amount of parameters.  Parm string=' + InstructionString, ERROR_PARM_INVALID, self);
          bRes := self.ReduceColors( sl[1] );
        end else if ( Pos('RESIZE', sl[0] ) > 0 ) then begin
          if ( sl.Count <> 3 ) then raise PluginException.Create('Instruction "RESIZE" was passed an incorrect amount of parameters.  Parm string=' + InstructionString, ERROR_PARM_INVALID, self);
          bRes := self.Resize( sl[1], sl[2] );
        end;
      end;
      Result := bRes;

    finally
      FreeAndNil( sl );
    end;
  except
    on E : Exception do
      Result := false;
  end;
End;

function TI2XImgFreeImage.ProcessImage( const MemoryMapName : string; InstructionList : TStrings ) : boolean;
var
  i : integer;
  bmp : TBitmap;
  bmp32 : TBitmap32;
  sMemoryMapName : string;
  hbmp : HBITMAP;
  oFBMP : TFreeWinBitmap;
Begin
  Result := false;
  try
    try
      FMemoryMapManager.Flush();

      sMemoryMapName := MemoryMapName;
      bmp32 := TBitmap32.Create();
      FMemoryMapManager.DebugMessageText := 'TI2XImgFreeImage.ProcessImage - Image Read from UI - InstructionList=' + InstructionList.Text;
      self.FMemoryMapManager.Read( sMemoryMapName, bmp32 );
      if ( Integer( self.DebugLevel ) > 1 ) then
        bmp32.SaveToFile( self.DebugPath + 'FI_' + sMemoryMapName + '_IN.BMP');
      oFBMP := TFreeWinBitmap.Create();
      oFBMP.Dib := FreeImage_CopyFromBitmap32( bmp32 );
      FFIBMP.Assign( oFBMP );

      if ( Integer( self.DebugLevel ) > 1 ) then
        bmp32.SaveToFile( self.DebugPath + 'FI_FFIBMP_' + sMemoryMapName + '_IN.BMP');

      for i := 0 to InstructionList.Count - 1 do begin
        if ( not ExecuteInstruction( InstructionList[i] ) ) then
          Exit;
      end;

      bmp := TBitmap.Create();
      bmp.Handle := FFIBMP.CopyToBitmapH;

      if ( Integer( self.DebugLevel ) > 1 ) then
        bmp.SaveToFile( self.DebugPath + 'FI_' + sMemoryMapName + '_OUT.BMP');
      FMemoryMapManager.DebugMessageText := 'TI2XImgFreeImage.ProcessImage - Image FOR from UI';
      self.FMemoryMapManager.Write( sMemoryMapName, bmp );

      Result := true;
    except
      on E : Exception do begin
        self.ErrorUpdate( E.Message, ERROR_IMAGE_PROC_FAILED );
        //raise PluginException.Create( E.Message, ERROR_IMAGE_PROC_FAILED, self );
      end;
    end;
  finally
    if ( bmp <> nil ) then FreeAndNil( bmp );
    if ( bmp32 <> nil ) then FreeAndNil( bmp32 );
    if ( oFBMP <> nil ) then FreeAndNil( oFBMP );
  end;
End;

function TI2XImgFreeImage.ReduceColors(const parm: string): boolean;
var
  sColorReduce : string;
  lBits : SmallInt;
begin
  try
    if ( Pos('Gray', parm ) > 0 ) then
      lBits := 255
    else
      lBits := FStrUtil.ExtractInt( parm );
    Result := ReduceColors( lBits )
  except
    on E : Exception do begin
      raise PluginException.Create( E.Message, ERROR_IMAGE_PROC_FAILED, self );
    end;
  end;
end;

function TI2XImgFreeImage.Resize(const x, y: cardinal): boolean;
begin
  if ( Integer( self.DebugLevel ) > 0 ) then FFIBMP.Save( self.DebugPath + 'FI-Resize_' + IntToStr(x) + 'X' + IntToStr(y) + '_IN.bmp' );
  try
    Result := FFIBMP.Rescale(x, y, FILTER_BILINEAR);
    if (( Result ) and ( Integer( self.DebugLevel ) > 0 )) then FFIBMP.Save( self.DebugPath + 'FI-Resize_' + IntToStr(x) + 'X' + IntToStr(y) + '_OUT.bmp' );
  except
    on E : Exception do begin
      raise PluginException.Create( E.Message, ERROR_IMAGE_PROC_FAILED, self );
    end;
  end;
end;

function TI2XImgFreeImage.Resize(const sXParm, sYParm: string): boolean;
var
  sXVal, sYVal : string;
begin
  Result := false;
  sXVal := sXParm;
  if ( Pos('=', sXParm ) > 0 ) then
    sXVal := Split( sXParm, '=', 1 );
  if ( not FStrUtil.IsNumeric( sXVal ) ) then
    raise PluginException.Create( 'Invalid Parm Format for sXParm.  Parm Value is=' + sXParm, ERROR_PARM_INVALID, self );

  sYVal := sYParm;
  if ( Pos('=', sYParm ) > 0 ) then
    sYVal := Split( sYParm, '=', 1 );
  if ( not FStrUtil.IsNumeric( sYVal ) ) then
    raise PluginException.Create( 'Invalid Parm Format for sYParm.  Parm Value is=' + sYParm, ERROR_PARM_INVALID, self);

  Result := Resize( StrToInt( sXVal ), StrToInt( sYVal ) );
end;

function TI2XImgFreeImage.ReduceColors(const colorCount: smallint) : boolean;
begin
  if ( Integer( self.DebugLevel ) > 0 ) then FFIBMP.Save( self.DebugPath + 'FI-ReduceColors_' + IntToStr(colorCount) + '_IN.bmp' );

  try
    if ( colorCount = 255 ) then
      Result := FFIBMP.ConvertToGrayscale
    else if ( colorCount = 4 ) then
      Result := FFIBMP.ConvertTo4Bits()
    else if ( colorCount = 8 ) then
      Result := FFIBMP.ConvertTo8Bits()
    else if ( colorCount = 16 ) then
      Result := FFIBMP.ConvertTo16Bits555()
    else if ( colorCount = 24 ) then
      Result := FFIBMP.ConvertTo24Bits()
    else if ( colorCount = 32 ) then
      Result := FFIBMP.ConvertTo32Bits()
    else
      Result := false
    ;
  except
    on E : Exception do begin
      raise PluginException.Create( E.Message, ERROR_IMAGE_PROC_FAILED, self );
    end;
  end;
  if (( Result ) and ( Integer( self.DebugLevel ) > 0 )) then FFIBMP.Save( self.DebugPath + 'FI-ReduceColors_' + IntToStr(colorCount) + '_OUT.bmp' );
end;

function TI2XImgFreeImage.Threshold(const T: byte) : boolean;
begin
  if ( Integer( self.DebugLevel ) > 0 ) then FFIBMP.Save( self.DebugPath + 'FI-Threshold' + IntToStr(T) + '_IN.bmp' );
  Result := FFIBMP.Threshold( T );
  if (( Result ) and ( Integer( self.DebugLevel ) > 0 )) then FFIBMP.Save( self.DebugPath + 'FI-Threshold' + IntToStr(T) + '_OUT.bmp' );
end;

function TI2XImgFreeImage.Threshold(const parm: string): boolean;
var
  sVal : string;
begin
  Result := false;
  sVal := parm;
  if ( Pos('=', sVal ) > 0 ) then
    sVal := Split( parm, '=', 1 );
  if ( FStrUtil.IsNumeric( sVal ) ) then begin
    try
      Result := Threshold( StrToInt( sVal ) )
    except
      on E : Exception do begin
        raise PluginException.Create( E.Message, ERROR_IMAGE_PROC_FAILED, self );
      end;
    end;
  end else
    raise PluginException.Create( 'Invalid Parm Format.  Parm Value is=' + sVal, ERROR_PARM_INVALID, self );
    //raise Exception.Create( 'Invalid Parm Format.  Parm Value is=' + sVal );
end;

{ TDWFreeWinBitmap }

constructor TDWFreeWinBitmap.Create;
begin
  inherited Create();
end;

destructor TDWFreeWinBitmap.Destroy;
begin
  if ( FMemoryMap = nil ) then FreeAndNil( FMemoryMap );
  inherited;
end;

{*
//This will read the bitmap from the memory stream.  since it is of TBitmap,
//  it will then get converted into the FreeBitmap
function TDWFreeWinBitmap.LoadFromMemoryMap(const sMapName: string;
  const InitSize: Cardinal): boolean;
var
  msBMP : TMemoryStream;
  bmp : TBitmap;
begin
  Result := false;
  try
    if ( FMemoryMap = nil ) then
      //FMemoryMap := TMapStream.Create( sMapName, InitSize );
      FMemoryMap := ActiveMemoryMaps.Stream( sMapName, InitSize );
    msBMP := TMemoryStream.Create;
    bmp := TBitmap.Create;
    if ( not FMemoryMap.Read( msBMP ) ) then
      raise Exception.Create('Error while copying BITMAP from Memory Map');
    bmp.LoadFromStream( msBMP );
    //bmp.SaveToFile('C:\Dark\pascal\Image2XML\templates\FIIN.bmp');
    self.CopyFromBitmap( bmp.ReleaseHandle );
    Result := true;
  finally
    FreeAndNil( msBMP );
    FreeAndNil( bmp );
  end;
end;

function TDWFreeWinBitmap.SaveToMemoryMap(const sMapName: string): boolean;
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
    bmp.Handle := self.CopyToBitmapH;
    bmp.SaveToStream( msBMP );
    //bmp.SaveToFile('C:\Dark\pascal\Image2XML\templates\FIOUT.bmp');
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
