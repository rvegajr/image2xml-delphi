unit ui2ximg_G32;

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
  MapStream,
  uDWImage,
  uI2XConstants,
  Typinfo;

const
  UNIT_NAME = 'ui2ximg_G32';


type
  TImageInstructionsG32 = (
    iRBG_COLOR_STRIP = 0
  );

  TI2XImgG32 = class(TI2XImg)
    private
      //FBMP32 : TDWBitmap32;
      FBMP32 : TBitmap32;
      function shouldBeBlackPixel(const color: TColor;
        Threshold: string): boolean;
      function HexToByte(hexStr: string): byte;
      function HexToInt(hexStr: string): integer;
      function HexToTColor(const sColor: string): TColor;
      function TColorToHex(const colColor: TColor): string;
    public
      function StripColors( RGBThresHold : Cardinal ): boolean; overload;
      function StripColors( Parm : string ): boolean; overload;

      function ExecuteInstruction( const InstructionString : string ) : boolean; override;
      constructor Create( const GUID, ParmFileName : string ); override;
      destructor Destroy; override;
      function ProcessImage( const MemoryMapName : string; InstructionList : TStrings ) : boolean; override;
  end;

implementation

{ TI2XImgG32 }

constructor TI2XImgG32.Create(const GUID, ParmFileName: string);
begin
  inherited Create( GUID, ParmFileName );
  FBMP32 := TBitmap32.Create;
end;

destructor TI2XImgG32.Destroy;
begin
  FreeAndNil( FBMP32 );
  inherited;
end;

function TI2XImgG32.ExecuteInstruction(
  const InstructionString: string): boolean;
var
  sInst, sParmString, key, val : string;
  ParmCount : byte;
  sl : TStringList;
  i : integer;
  bRes : boolean;
Begin
  try
    sl := TStringList.Create;

    bRes := false;
    sInst := Split( InstructionString, IMGPROC_PARM_SEP, 0 ); //get the instruction
    sInst := Split( sInst, IMGPROC_PLUGIN_SEP, 1 );                          //strip the PlugIn Numonic
    ParmCount := StringCount(InstructionString, IMGPROC_VALUE_SEP );
    if ( Length( InstructionString ) > 0 ) then
    begin
      if ( Integer( self.DebugLevel ) > 0 ) then
        FBMP32.SaveToFile( self.DebugPath + 'G32-' + sInst + '_IN.bmp' );
      case TImageInstructionsG32(GetEnumValue(TypeInfo(TImageInstructionsG32),'i' + sInst)) of
        iRBG_COLOR_STRIP:
          begin
            if ( ParmCount <> 1 ) then raise Exception.Create('Instruction "' + sInst + '" was passed an incorrect amount of parameters.  Parm string=' + InstructionString);
            Result := self.StripColors( InstructionString );
          end;
      end;
      if (( bRes ) and ( Integer( self.DebugLevel ) > 0 )) then
        FBMP32.SaveToFile( self.DebugPath + 'G32-' + sInst + '_OUT.bmp' );
      Result := bRes;
    end;
  finally
    FreeAndNil( sl );
  end;
End;

function TI2XImgG32.ProcessImage(const MemoryMapName: string;
  InstructionList: TStrings): boolean;
var
  i : integer;
  sMemoryMapName : string;
Begin
  try
    FMemoryMapManager.Flush();

    sMemoryMapName := MemoryMapName;
    FMemoryMapManager.DebugMessageText := 'TI2XImgG32.ProcessImage - Image FROM from UI - ' + InstructionList.Text;
    Result := FMemoryMapManager.Read( sMemoryMapName, FBMP32 );
    if ( not Result ) then
      raise Exception.Create( 'Could not read from memory map.' );
    for i := 0 to InstructionList.Count - 1 do begin
      if ( not ExecuteInstruction( InstructionList[i] ) ) then
        Exit;
    end;
    //self.FBMP32.SaveToMemoryMap( MemoryMapName );
    FMemoryMapManager.DebugMessageText := 'TI2XImgG32.ProcessImage - Image FOR from UI';
    Result := FMemoryMapManager.Write( sMemoryMapName, FBMP32 );
    if ( not Result ) then
      raise Exception.Create( 'Could not save to memory map.' );
  except
    on E : Exception do
      begin
        self.ErrorUpdate( E.Message, ERROR_IMAGE_PROC_FAILED );
      end;
  end;
End;

(* Returns a string representation of the colColor parameter in XXXXXX format,
** X being a hex digit.
Thanks http://www.delphifaq.net/how-to-convert-tcolor-to-a-hex-string-and-vice-versa/
*)
function TI2XImgG32.TColorToHex(const colColor: TColor): string;
begin
  Result := IntToHex(GetRValue(colColor), 2) + { Red value }
    IntToHex(GetGValue(colColor), 2) +         { Green value }
    IntToHex(GetBValue(colColor), 2);          { Blue value }
end;

{ sColor should be in XXXXXX format, X being a hex digit }
function TI2XImgG32.HexToTColor(const sColor: string): TColor;
begin
  Result := RGB(
    StrToInt('$' + Copy(sColor, 1, 2)), { Get red value }
    StrToInt('$' + Copy(sColor, 3, 2)),              { Get green value }
    StrToInt('$' + Copy(sColor, 5, 2))
    );             { Get blue value }
end;

function TI2XImgG32.HexToInt(hexStr : string) : integer;
  CONST HEX : ARRAY['A'..'F'] OF INTEGER = (10,11,12,13,14,15);
  var  i : integer;
begin
  result := 0;
  FOR i := 1 TO Length(hexStr) DO
    if ( hexStr[i] < 'A' ) then
      result := result * 16 + ORD(hexStr[i]) - 48
    else
      result := result * 16 + HEX[hexStr[i]];
End;

function TI2XImgG32.HexToByte(hexStr : string) : byte;
  CONST HEX : ARRAY['A'..'F'] OF INTEGER = (10,11,12,13,14,15);
  var  i : integer;
begin
  result := 0;
  FOR i := 1 TO Length(hexStr) DO
    if ( hexStr[i] < 'A' ) then
      result := result * 16 + ORD(hexStr[i]) - 48
    else
      result := result * 16 + HEX[hexStr[i]];
End;

function TI2XImgG32.StripColors(RGBThresHold: Cardinal): boolean;
const
  uBlack = $00000000;
  uWhite = $00FFFFFF;
  uRed = $000000FF;
  uBlue = $00FF0000;
var
  pixel : TColor;
  x, y : Cardinal;
  i : integer;
  sRGBThreshold : string;
Begin
  sRGBThreshold := IntToHex(RGBThresHold, 6);
  Initialize( i );
  for x := 0 to Fbmp32.Width - 1 do begin
    for y := 0 to Fbmp32.Height - 1 do begin
      //if ((x = 135) and (y = 221)) then
      //  i := i * 1;

      pixel := WinColor(FBmp32.Pixel[x, y]);
      if ( shouldBeBlackPixel( pixel, sRGBThreshold ) ) then begin
        Fbmp32.Pixel[x, y] := clBlack32;
      end else begin
        Fbmp32.Pixel[x, y] := clWhite32;
      end;
    end;

    Inc(i);
  end;
End;

function TI2XImgG32.shouldBeBlackPixel(const color: TColor; Threshold: string): boolean;
var
  r, g, b: Byte;
  lColor: Longint;
begin
  lColor := ColorToRGB(color);
  r     := lColor;
  g     := lColor shr 8;
  b     := lColor shr 16;
  result := (
      (r <= HexToByte(Copy(Threshold, 1, 2)))
  and (g <= HexToByte(Copy(Threshold, 3, 2)))
  and (b <= HexToByte(Copy(Threshold, 5, 2)))
  );
//  result := (( color >= $00000000 ) and ( ColorToRGB(color) <= HexToInt( FRGBThreshold ) ));
End;

function TI2XImgG32.StripColors(Parm: string): boolean;
begin
  Result := false;
  self.ParseParmString( Parm );
  if ( self.ParmMustExist( 'threshold' )) then
    Result := StripColors( StrToInt( Parms.Items['threshold'] ) );
end;

END.
