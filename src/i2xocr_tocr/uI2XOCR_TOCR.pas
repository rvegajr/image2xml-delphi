unit uI2XOCR_TOCR;

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
  uTOCRdll,
  uCmdLine,
  ExtCtrls;

const
  UNIT_NAME = 'ui2xOCR_TOCR';
  X_DPI_DEFAULT = 300;
  Y_DPI_DEFAULT = 300;

type
  BMPINFO = record
    hBmp : HBITMAP; //            As Long         ' handle to bitmap
    Width : Integer; //           As Long         ' pixel width of bitmap
    Height : Integer; //          As Long         ' pixel height of bitmap
    XPelsPerMeter : Integer; //   As Long         ' X pixels per metre
    YPelsPerMeter : Integer; //   As Long         ' Y pixels per metre
  End;

  TMonoBitmapInfo = packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..1] of TRGBQuad;
  end;

  TI2XOCR_TOCR = class(TI2XOCR)
    private
      _DC : HDC;
      FInvert : boolean;
      function TransymOCRCall( const MMHandle : Cardinal;
        var OCRRes: TTOCRResults;
        charsAllowed: string = '';
        rejectFunkyLinesChars : boolean = true;
        invert : boolean = false;
        autoOrient : boolean = true;
        fileNameOverride : string = '' ): integer; overload;
      function TransymOCRCall( const DWImage : TDWImage;
          OCRData : TI2XOCRResult;
          charsAllowed : string = '';
          rejectFunkyLinesChars : boolean = true;
          invert : boolean  = false;
          autoOrient : boolean = true ) : integer; overload;
      function TransymOCRCall( const DWImage : TDWImage;
          OCRData : TI2XOCRResult;
          SliceRect : TRect;
          charsAllowed : string = '';
          rejectFunkyLinesChars : boolean = true;
          invert : boolean  = false;
          autoOrient : boolean = true
           ) : integer; overload;
        function SaveMonoBitmapToMMFile(const DWImage: TDWImage; var MMFileHandle: Cardinal): boolean; overload;
        function SaveMonoBitmapToMMFile(BI : TBitMap; Var hFile : Longint):Boolean; overload;
        function SaveMonoBitmapToMMFile(BI : BMPINFO;
          var MMFileHandle : Cardinal ) : boolean; overload;
      function SaveMonoBitmapToMMFile(const DWImage : TDWImage;
        SliceRect : TRect; var MMFileHandle : Cardinal ) : boolean; overload;
      function GetSelectionAreaBitmap(const DWImage : TDWImage;
      SelectionRect: TRect; var SelectBI: BMPINFO): Boolean;
    procedure OCRResToI2XResults(OCRRes: TTOCRResults; OCRData: TI2XOCRResult);
    function AllowedChars(const OCRData: TI2XOCRResult): string;
    public
      property InvertImage : boolean read FInvert write FInvert;
      function SetOptions(const OptionsString: string): boolean;
      function OCRImage( const ImageMMID, OCRDataMMID : string ) : boolean; virtual;
      constructor Create( const GUID, ParmFileName : string ); override;
      destructor Destroy;  override;
  end;


implementation

{ TI2XOCR_TOCR }

constructor TI2XOCR_TOCR.Create(const GUID, ParmFileName: string);
begin
  inherited;
  _DC := 0;
  FInvert := false;
end;

destructor TI2XOCR_TOCR.Destroy;
begin
  if (_DC <> 0) then DeleteDC( _DC );

  inherited;
end;

function TI2XOCR_TOCR.SetOptions(const OptionsString : string): boolean;
var
  sOptionsString : string;
begin
  try
    sOptionsString := lowerCase( OptionsString );
    self.InvertImage := ( Pos( 'invert', sOptionsString ) > 0 );
  finally
  end;
end;

function TI2XOCR_TOCR.OCRImage(const ImageMMID, OCRDataMMID : string): boolean;
var
  bResult, bRefreshMMID : boolean;
  i, nCharsReturned : integer;
  sMapName, sOCRDataMMID : string;
  OCRResultDefault : TI2XOCRResult;
  FImageSlice : TDWImage;
Begin
  try
    if ( Integer( self.DebugLevel ) >= 1 ) then self.DebugWrite('Calling OCR from image map id "' + ImageMMID + '" and OCRData Map id of "' + OCRDataMMID + '"');
    bResult := false;
    bRefreshMMID := false;
    sMapName := ImageMMID;

    FMemoryMapManager.Flush();
    //bResult := self.FImage.LoadFromMemoryMap( sMapName );
    FMemoryMapManager.DebugMessageText := 'TI2XOCR_TOCR.OCRImage(sMapName=' + sMapName + ') - Initial call ';
    bResult := FMemoryMapManager.Read( sMapName, FImage );
    //bResult := self.FImage.LoadFromMemoryMap( sMapName );
    if ( Integer( self.DebugLevel ) >= 2 ) then self.FImage.SaveToFile(self.DebugPath + 'OCRIMG_BEFORE.BMP');

    if ( Integer( self.DebugLevel ) >= 2 ) then self.DebugWrite( 'Before... XRes=' + IntToStr(FImage.DPIX) + '  YRes=' + IntToStr(FImage.DPIY) );
    if ( self.FImage.ColorCount > 2 ) then begin
      self.FImage.ConvertToBW();
      bRefreshMMID := true;
    end;
    if ( bRefreshMMID ) then begin
      if ( Integer( self.DebugLevel ) >= 2 ) then self.FImage.SaveToFile(self.DebugPath + 'OCRIMG_BEFORE_REFRESH.BMP');
      FMemoryMapManager.DebugMessageText := 'TI2XOCR_TOCR.OCRImage(sMapName=' + sMapName + ') - Debug Write';
      FMemoryMapManager.Write( sMapName, FImage );
      //self.FImage.SaveToMemoryMap( sMapName );
      if ( Integer( self.DebugLevel ) >= 2 ) then self.FImage.SaveToFile(self.DebugPath + 'OCRIMG_AFTER_REFRESH.BMP');
    end;
    if ( Integer( self.DebugLevel ) >= 2 ) then self.DebugWrite( 'After... XRes=' + IntToStr(FImage.DPIX) + '  YRes=' + IntToStr(FImage.DPIY) );
    FOCRData.ImageID := sMapName;

    if ( Integer( self.DebugLevel ) >= 2 ) then self.FImage.SaveToFile(self.DebugPath + 'OCRIMG_BEFOREOCR.BMP');
    if ( not bResult ) then
      raise Exception.Create('Cannot OCR Image because Memory could not be loaded');
    //bResult := FOCRData.LoadFromMemoryMap( OCRDataMMID );
    FMemoryMapManager.DebugMessageText := 'TI2XOCR_TOCR.OCRImage(OCRDataMMID=' + OCRDataMMID + ') - OCR Data Read from main app ';
    bResult := FMemoryMapManager.Read( OCRDataMMID, FOCRData );
    if ( Integer( self.DebugLevel ) >= 2 ) then FOCRData.SaveToFile(self.DebugPath + 'OCRDATA_AFTEROCR.XML');
    if ( not bResult ) then
      raise Exception.Create('Cannot OCR Image because the Image List could not be loaded from memory map "' + ImageMMID + '" or it was not in the expected XML format');
    //bResult := FOCRData.LoadXML( ImageList );

    if ( FOCRData.Count = 0 ) then begin
      OCRResultDefault := TI2XOCRResult.Create;
      OCRResultDefault.ID := 'default';
      FOCRData.Add( OCRResultDefault );
    end;

    //Loop through all the images and slice them,  then OCR them
    for i := 0 to FOCRData.Count - 1 do begin
      with FOCRData.Items[i] do begin
        nCharsReturned := 0;
        //if the slice attributes are zero,  then we can just pass FImage
        if (( X = 0 ) and ( Y = 0 ) and ( Width = 0 ) and ( Height = 0 )) then begin
          nCharsReturned := TransymOCRCall( FImage, FOCRData.Items[i], '', true, self.InvertImage );
          if ( nCharsReturned < 0 ) then
            raise Exception.Create('OCR Call failed.');
          if ( Integer( self.DebugLevel ) >= 2 ) then FOCRData.SaveToFile(  self.DebugPath + 'tocroutput.xml' );
        end else begin
          try
            if ( Integer( self.DebugLevel ) >= 2 ) then FOCRData.SaveToFile(  self.DebugPath + 'tocroutputs_BEFORE.xml' );
            nCharsReturned := TransymOCRCall( FImage, FOCRData.Items[i], AsRect,
                AllowedChars( FOCRData.Items[i] ), true, self.InvertImage );
            if ( nCharsReturned < 0 ) then
              raise Exception.Create('OCR Call failed.');
            if ( Integer( self.DebugLevel ) >= 2 ) then FOCRData.SaveToFile(  self.DebugPath + 'tocroutputs.xml' );
          finally
            FreeAndNil( FImageSlice );
          end;

        end;

      end;
    end;

    sOCRDataMMID := OCRDataMMID;
    //bResult := FOCRData.SaveToMemoryMap( sOCRDataMMID );
    FMemoryMapManager.DebugMessageText := 'TI2XOCR_TOCR.OCRImage(sOCRDataMMID=' + sOCRDataMMID + ') - OCR Data Write for main app';
    bResult := FMemoryMapManager.Write( sOCRDataMMID, FOCRData );
    if ( not bResult ) then
      raise Exception.Create('OCR Image failed because the result XML could not be saved to memory map "' + ImageMMID + '"');
    Result := bResult;
    if ( Integer( self.DebugLevel ) >= 1 ) then self.DebugWrite('  Result=' + BoolToStr(Result));
  except
    on E : Exception do
      begin
        self.ErrorUpdate( E.Message, ERROR_I2X_OCR_FAILED );
        Result := false;
      end;
  end;
End;

procedure TI2XOCR_TOCR.OCRResToI2XResults(OCRRes : TTOCRResults;
          OCRData : TI2XOCRResult);
Var
  FOCRResItem  : TTOCRResultsItem;
  OCRDataItem : TI2XOCRItem;
  i : integer;
Begin
      for i := 0 to OCRRes.Header.NumItems - 1 do begin
        FOCRResItem := OCRRes.Items[ i ];
        if (( FOCRResItem.XDim > 0 ) and ( FOCRResItem.YDim > 0 )) then begin
          OCRDataItem := TI2XOCRItem.Create;
          //OCRData.X[Y] is added to the result because it will offset the image slice
          // in relation to the entire image.
          OCRDataItem.X := OCRData.X + FOCRResItem.XPos;
          OCRDataItem.Y := OCRData.Y + FOCRResItem.YPos;
          OCRDataItem.Width := FOCRResItem.XDim;
          OCRDataItem.Height := FOCRResItem.YDim;
          OCRDataItem.Accuracy := FOCRResItem.Confidence;
          OCRDataItem.Data := Chr( FOCRResItem.OCRCha );
          OCRData.Add( OCRDataItem );
        end;
      end;
      OCRData.Accuracy := OCRRes.Header.MeanConfidence;
End;

function TI2XOCR_TOCR.TransymOCRCall(const DWImage : TDWImage;
          OCRData : TI2XOCRResult;
          charsAllowed : string;
          rejectFunkyLinesChars : boolean;
          invert : boolean ;
          autoOrient : boolean ) : integer;
var
  MMHandle : Cardinal;
  OCRRes : TTOCRResults;
Begin
  try
    Result := 0;
    OCRData.Clear;
    MMHandle := 0;
    if ( invert ) then begin
      DWImage.Invert;
    end;
    DWImage.SaveToFile(self.DebugPath + '_TEST.BMP');
    self.SaveMonoBitmapToMMFile( DWImage, MMHandle );

    Result := TransymOCRCall( MMHandle, OCRRes, charsAllowed,
      rejectFunkyLinesChars, invert, autoOrient );

    if ( Result > 0 ) then begin
      OCRResToI2XResults( OCRRes, OCRData );
      if ( OCRData.Width = 0 ) then OCRData.Width := DWImage.Width;
      if ( OCRData.Height = 0 ) then OCRData.Height := DWImage.Height;
    end;
  finally
  end;
End;

function TI2XOCR_TOCR.AllowedChars( const OCRData: TI2XOCRResult ) : string ;
Begin
  if ( OCRData.DataType = odtNumeric )  then
    Result := DT_n_CHARS_ALLOWED
  else if ( OCRData.DataType = odtDateTime ) then
    Result := DT_dt_CHARS_ALLOWED
  else
    Result := '';
End;

function TI2XOCR_TOCR.TransymOCRCall(const DWImage: TDWImage;
  OCRData: TI2XOCRResult; SliceRect: TRect; charsAllowed: string;
  rejectFunkyLinesChars, invert, autoOrient: boolean): integer;
var
  MMHandle : Cardinal;
  OCRRes : TTOCRResults;
  sTempFile : string;
  dw : TDWImage;
Begin
  try
    Result := 0;
    OCRData.Clear;
    MMHandle := 0;
    dw := TDWImage.Create();
    DWImage.CopySlice( SliceRect, dw );
    if ( invert ) then begin
      dw.Invert;
    end;
    if ( dw.DPIX = 0 ) then
      dw.setDPI( 300 );

    sTempFile := self.DebugPath + '_SLICE_' + IntToStr(dw.Handle) + '.BMP';
    dw.SaveToFile( sTempFile );

    Result := TransymOCRCall( MMHandle, OCRRes, charsAllowed,
      rejectFunkyLinesChars, invert, autoOrient, sTempFile );

    //self.SaveMonoBitmapToMMFile( dw, MMHandle );
    //Result := TransymOCRCall( MMHandle, OCRRes, charsAllowed,
    //  rejectFunkyLinesChars, invert, autoOrient,
    //  'C:\Users\Administrator\AppData\Local\Temp\i2x\_test2CUT.BMP' );

    if ( Result > 0 ) then begin
      OCRResToI2XResults( OCRRes, OCRData );
      if ( OCRData.Width = 0 ) then OCRData.Width := DWImage.Width;
      if ( OCRData.Height = 0 ) then OCRData.Height := DWImage.Height;
      DeleteFile( sTempFile );
    end;
  finally
    FreeAndNil( dw );
  end;
End;

function TI2XOCR_TOCR.TransymOCRCall(const MMHandle : Cardinal;
          var OCRRes : TTOCRResults;
          charsAllowed : string;
          rejectFunkyLinesChars : boolean;
          invert : boolean ;
          autoOrient : boolean;
          fileNameOverride : string ) : integer;
Var
  OCRJobNr, FuncResult: LongInt;
  OCRJobInfo: TTOCRJobInfo;
  OCRJobStatus, OCRJobResultsInf, nWaitInt: Integer;
  chrIdx : byte;
  OCRResultItems : Array Of TTOCRResultsItem;
  S: String;
  RegNo: Array[0..1] Of Cardinal;
  arrFileName: Array[0..255] Of Char;
  ConfidenceAvg, ConfidenceChars: Single;
  OCRResults : PChar;
Begin
  nWaitInt := 0;
  result := 0;
  RegNo[0] := $FC4FB5EB;
  Initialize( OCRResults );
  Initialize( OCRResultItems );
  RegNo[1] := $728B7494;
  Try
    Try
      OCRJobResultsInf:=0;
      OCRJobStatus:=TOCRJobStatus_Busy;
      FillChar(OCRJobInfo, SizeOf(TTOCRJobInfo), 0);
      FillChar(OCRRes.Header, SizeOf(TTOCRResultsHeader), 0);

      FuncResult:=TOCRSetErrorMode(TOCRDefErrorMode, TOCRErrorMode_Silent);
      If FuncResult <> TOCR_OK Then
        raise Exception.Create( 'Error occured while setting error mode: '+IntToStr(FuncResult) )
      Else
      Begin
        // Initialize OCR engine
        FuncResult:=TOCRInitialise(OCRJobNr, RegNo[0]);
        If FuncResult<>TOCR_Ok Then
          raise Exception.Create('Error occured while initializing OCR engine: '+IntToStr(FuncResult) )
        Else
          Try
            if (
                ( Length(fileNameOverride) > 0 ) and
                ( FileExists(fileNameOverride) )
               )  then begin
              FillChar(arrFileName, SizeOf(arrFileName), 0);
              StrPCopy(arrFileName, fileNameOverride+#0);
              If ExtractFileExt(UpperCase(fileNameOverride))='.TIF' Then
                OCRJobInfo.JobType  := TOCRJobType_TiffFile
              Else If ExtractFileExt(UpperCase(fileNameOverride))='.BMP' Then
                OCRJobInfo.JobType  :=TOCRJobType_DibFile;
              OCRJobInfo.InputFile  :=arrFileName;
              OCRJobInfo.PageNo     :=0;
            end else begin
              // Initialize job structure
              OCRJobInfo.JobType    := TOCRJOBTYPE_MMFILEHANDLE;
              OCRJobInfo.InputFile  := '';
              OCRJobInfo.PageNo     := MMHandle;
            end;

          if ( Length( charsAllowed ) > 0 ) then begin
            for chrIdx := 0 to 255 do begin
              OCRJobInfo.ProcessOptions.DisableCharacter[chrIdx] := true;
            end;
            for chrIdx := 1 to Length( charsAllowed ) do begin
              OCRJobInfo.ProcessOptions.DisableCharacter[ Ord( charsAllowed[chrIdx] ) ] := false;
            end;
          end;
          if ( autoOrient ) then
            OCRJobInfo.ProcessOptions.Orientation:=	TOCRJOBORIENT_AUTO
          else
            OCRJobInfo.ProcessOptions.Orientation:=TOCRJobOrient_Off;
          OCRJobInfo.ProcessOptions.StructId := 2;
          //OCRJobInfo.ProcessOptions.StructId := 2;
          OCRJobInfo.ProcessOptions.InvertWholePage   := invert;
          OCRJobInfo.ProcessOptions.DeskewOff         := false;
          OCRJobInfo.ProcessOptions.NoiseRemoveOff    := false;
          OCRJobInfo.ProcessOptions.LineRemoveOff     := false;
          OCRJobInfo.ProcessOptions.DeshadeOff        := false;
          OCRJobInfo.ProcessOptions.InvertOff         := false;
          OCRJobInfo.ProcessOptions.SectioningOn      := false;
          OCRJobInfo.ProcessOptions.MergeBreakOff     := false;
          OCRJobInfo.ProcessOptions.LineRejectOff     := not rejectFunkyLinesChars;
          OCRJobInfo.ProcessOptions.CharacterRejectOff:= not rejectFunkyLinesChars;
          OCRJobInfo.ProcessOptions.LexOff            := false;

          // Start job execution
          FuncResult:=TOCRDoJob(OCRJobNr, OCRJobInfo);
          If FuncResult<>TOCR_Ok Then
            raise Exception.Create('Error occured while starting OCR job: '+IntToStr(FuncResult))
          Else
          Begin
            nWaitInt := 0;
            // Wait for the job to finish
            while ((nWaitInt < 5) AND (OCRJobStatus <> TOCRJobStatus_Done)) do begin
              FuncResult:=TOCRWaitForJob(OCRJobNr, OCRJobStatus);
              if ( OCRJobStatus = -1 ) then begin //Try to redo job if we get -1 error code
                FuncResult:=TOCRShutdown(OCRJobNr); // This will shut down the applied job
                FuncResult:=TOCRInitialise(OCRJobNr, RegNo[0]);
                FuncResult:=TOCRDoJob(OCRJobNr, OCRJobInfo);
                FuncResult:=TOCRWaitForJob(OCRJobNr, OCRJobStatus);
              end;
              Inc(nWaitInt);
            end;

            If FuncResult<>TOCR_Ok Then
              raise Exception.Create('Error occured while waiting for job to be done: '+IntToStr(FuncResult)+' JobStatus: '+IntToStr(OCRJobStatus))
            Else
            If (OCRJobStatus <> TOCRJobStatus_Done) Then
              result := -1
              //raise Exception.Create('Waiting for job OK, but the status was wrong : '+IntToStr(OCRJobStatus))
            Else Begin
              // Get the required size of the job
              FuncResult:=TOCRGetJobResults(OCRJobNr, OCRJobResultsInf, Nil);
              If FuncResult<>TOCR_Ok Then
                raise Exception.Create('Error occured while retrieving the job''s result size: '+IntToStr(FuncResult))
              Else
              If OCRJobResultsInf=TOCRGetResults_NoResults Then
                raise Exception.Create('No result available in the specified job: '+IntToStr(OCRJobNr))
              Else
              // Make sure that at least the header is filled !
              If OCRJobResultsInf<SizeOf(TTOCRResultsHeader) Then
                raise Exception.Create('The size of the job result was less than expected: '+IntToStr(OCRJobResultsInf)+'('+IntToStr(SizeOf(TTOCRResultsHeader))+')')
              Else Begin
                Try
                  try
                    GetMem(OCRResults, OCRJobResultsInf+1);
                  Except
                    //WriteLog('  Warning! pOCRResults pointed to an inaccessable value.  It could be that no data was returned from OCR.  Ignoring this result.');
                    OCRResults := nil;
                  end;
                  if ( OCRResults <> nil ) then begin
                    FuncResult:=TOCRGetJobResults(OCRJobNr, OCRJobResultsInf, OCRResults);
                    If FuncResult<>TOCR_Ok Then
                      raise Exception.Create('Error occured while retrieving the job result: '+IntToStr(FuncResult))
                    Else
                    If OCRJobResultsInf=TOCRGetResults_NoResults Then
                      raise Exception.Create('No result available in the specified job: '+IntToStr(OCRJobNr))
                    Else
                    If (OCRResults=Nil) Or (Integer(OCRResults)=0) Then
                      raise Exception.Create('Error occured while retrieving the job, the pointer was NIL :-( ')
                    Else
                    Begin
                      Move(OCRResults[0], OCRRes.Header, SizeOf(TTOCRResultsHeader));
                      SetLength(OCRRes.Items, OCRRes.Header.NumItems);
                      if ( OCRRes.Header.NumItems = 0 ) then begin
                        //WriteLog('  Warning!  OCRRes.Header.NumItems = 0. Ignoring this OCR result.');
                        result := 0;
                      end else begin
                        Move(OCRResults[SizeOf(TTOCRResultsHeader)], OCRRes.Items[0], SizeOf(TTOCRResultsItem)*OCRRes.Header.NumItems);
                        result := OCRRes.Header.NumItems;
                      end;
                      S:='';
                    end;
                  end; //if ( OCRResults <> nil ) then begin
                Finally
                  FreeMem(OCRResults);
                End;
              End;
            End;
          End;
        Finally
          // Shut down the OCR engine
          FuncResult:=TOCRShutdown(OCRJobNr); // This will shut down the applied job
          If FuncResult<>TOCR_Ok Then
            raise Exception.Create('Error occured while shutting down OCR engine: '+IntToStr(FuncResult))
        End;
      End;
    Except
      raise;
    End;
  Except
    raise;
  End;
end;

function TI2XOCR_TOCR.GetSelectionAreaBitmap(const DWImage : TDWImage;
    SelectionRect : TRect; var SelectBI : BMPINFO ):Boolean;
var
  hbmpTarget, hbmpSource, DWImageHandle, Width, Height : Cardinal;
  hDCWork : HDC;
  DWBMPInfo : BMPINFO;
  Source, Target : TImage;
  dw : TDWImage;
Begin
  try
    try
      Result := false;
      initialize( SelectBI );
      dw := TDWImage.Create();
      DWImage.CopySlice( SelectionRect, dw );

      dw.setDPI( DWImage.DPIX );
      //DWImage.SaveToFile( 'C:\Users\Administrator\AppData\Local\Temp\i2x\_test2SRC.BMP' );
      SelectBI.Width := dw.Width;
      SelectBI.Height := dw.Height;
      SelectBI.XPelsPerMeter := dw.XPelsPerMeter;
      SelectBI.YPelsPerMeter := dw.YPelsPerMeter;
      //dw.SaveToFile( 'C:\Users\Administrator\AppData\Local\Temp\i2x\_test2CUT.BMP' );
      SelectBI.hBmp := dw.ReleaseHandle;

    except
      raise;
    end;
  finally
    //FreeAndNil( Source );
    //FreeAndNil( Target );
    FreeAndNil( dw );
  end;
End;

function TI2XOCR_TOCR.SaveMonoBitmapToMMFile(BI : Tbitmap; Var hFile : LongInt):Boolean;
type
 RGB = array [0..1] of tagRGBQUAD;
 PRGB = ^RGB;
 ByteArray = array [0..maxint-1] of byte;
 parraybyte = ^ByteArray;
Var LineSize:Longint;
    lpMap               : pointer;         // pointer to mapped file
    lp                  : pointer;         // pointer
    hDCMem              : LongInt;         // handle to a memory device context
    hbmpOld             : LongInt;         // handle to a bitmap
    bmi                 : PBitmapInfo;
    Bit                 : BitmapInfo;
    bufer               : parraybyte;
    Colors              : Prgb;
    NumBytes            : LongInt;
    ert                 : TPixelFormat;
    CopyBit             : BitmapInfo;
    cClrBits            : Cardinal;
    Resultat            : LongInt;
    GetDIBResult        : Integer;
begin
//CopyBit:BitmapInfo;
  result := false;
  hFile := 0;

  getmem(bufer,SizeOf(BitmapInfo)+2*SizeOf(integer));
  bmi := PBitmapInfo(pointer(bufer));
  Colors := PRGB(@Bufer[SizeOf(BitmapInfo)]);

  With bmi.bmiHeader do
  begin
    biSize := SizeOf(bmi.bmiHeader);
    biWidth := BI.Width;
    biHeight := BI.Height;
    //biDPIX := 13780;  //BI.DPIX;
    //biYPelsPerMeter := 13780;  //BI.YPelsPerMeter;
    biXPelsPerMeter := 11811;
    biYPelsPerMeter := 11811;
    biPlanes := 1;
    biBitCount := 1;
    biCompression := BI_RGB;
    biClrUsed := 2;
    biClrImportant := 0;
    LineSize := Round((BI.Width + 31) div 32) * 4;
    biSizeImage := LineSize * BI.Height;
    //biSizeImage := biWidth * biHeight * 4;
  End ;
  with Colors[0] do
  begin
    rgbRed := 0;
    rgbGreen := 0;
    rgbBlue := 0;
    rgbReserved := 0;
  end;

  with Colors[1] do
  begin
    rgbRed := 255;
    rgbGreen := 255;
    rgbBlue := 255;
    rgbReserved := 0;
  end;
  BI.Monochrome := true;
  //ert := BI.PixelFormat;
  //NumBytes := SizeOf(bmi.bmiHeader);
  //NumBytes := NumBytes + SizeOf(bmi.bmiColors[0]) * 2;
  //NumBytes := NumBytes + bmi.bmiHeader.biSizeImage;

  NumBytes := SizeOf(bmi.bmiHeader);
  Inc( NumBytes, ( SizeOf(bmi.bmiColors[0]) * 2 ) );
  Inc( NumBytes, ( bmi.bmiHeader.biSizeImage ) );
  hFile := CreateFileMappingA( $FFFFFFFF , nil, PAGE_READWRITE, 0, NumBytes, nil);
  if(hFile <> 0) then
  begin
    lpMap := MapViewOfFile(hFile, FILE_MAP_WRITE, 0, 0, 0);
    if(lpMap <> nil) then
    begin
      lp := lpMap;
      CopyMemory (lp, @bmi.bmiHeader, SizeOf(bmi.bmiHeader));
      lp := @parraybyte(lp)[SizeOf(bmi.bmiHeader)];
      CopyMemory (lp, @bmi.bmiColors[0],  SizeOf(bmi.bmiColors[0]) * 2);
      lp := @parraybyte(lp)[SizeOf(bmi.bmiColors[0])*2 ];
      hDCMem := CreateCompatibleDC(0);
      if(hDCMem <> 0 ) then
      begin
        hbmpOld := SelectObject(hDCMem, BI.Handle);
        if(hbmpOld <> 0) then begin
          copybit:= TBitmapInfo(lpMap^);

          GetDIBResult := GetDIBits( hDCMem, BI.Handle, 0, BI.Height, lp,
            copybit, DIB_RGB_COLORS);
          IF( GetDIBResult <> 0 ) Then
            result:=true
          else
            raise Exception.Create( SysErrorMessage(GetLastError) );
          result:=true;
          BI.Handle := SelectObject(hDCMem, hbmpOld);
        end;
        DeleteDC(hDCMem);
      end;
      UnmapViewOfFile(lpMap)
    end;
  end
  else
  begin
      If not result Then
      begin
        CloseHandle(hFile);
        hFile := 0
       end;
  end;
end;

function TI2XOCR_TOCR.SaveMonoBitmapToMMFile(const DWImage : TDWImage;
          var MMFileHandle : Cardinal ) : boolean;
var
  BI : BMPINFO;
Begin
  //DWImage.BitMap.Handle
    BI.Width := DWImage.Width;
    BI.Height := DWImage.Height;
    BI.XPelsPerMeter := Round( DWImage.DPIX * 39.370079 );
    if ( BI.XPelsPerMeter = 0 ) then
      BI.XPelsPerMeter := Round(X_DPI_DEFAULT * 100 / 2.54 + 0.5);
    BI.YPelsPerMeter := Round( DWImage.DPIY * 39.370079 );
    if ( BI.YPelsPerMeter = 0 ) then
      BI.YPelsPerMeter := Round(Y_DPI_DEFAULT * 100 / 2.54 + 0.5);
    BI.hBmp := DWImage.ReleaseHandle;
    Result := SaveMonoBitmapToMMFile(BI, MMFileHandle);
End;

function TI2XOCR_TOCR.SaveMonoBitmapToMMFile(const DWImage : TDWImage;
  SliceRect : TRect; var MMFileHandle : Cardinal ) : boolean;
var
  BI : BMPINFO;
Begin
  //DWImage.BitMap.Handle
  {*
    BI.Width := DWImage.Width;
    BI.Height := DWImage.Height;
    BI.XPelsPerMeter := Round( DWImage.DPIX * 39.370079 );
    if ( BI.XPelsPerMeter = 0 ) then
      BI.XPelsPerMeter := Round(X_DPI_DEFAULT * 100 / 2.54 + 0.5);
    BI.YPelsPerMeter := Round( DWImage.DPIY * 39.370079 );
    if ( BI.YPelsPerMeter = 0 ) then
      BI.YPelsPerMeter := Round(Y_DPI_DEFAULT * 100 / 2.54 + 0.5);
    BI.hBmp := DWImage.ReleaseHandle;
    *}
    Result := GetSelectionAreaBitmap( DWImage, SliceRect, BI );
    Result := SaveMonoBitmapToMMFile( BI, MMFileHandle );
End;

{---------------------------------------------------------------------------
 Save a bitmap held in memory to a file to memory (only) mapped file.
 The handle to the MM file is returned in hFile. }
function TI2XOCR_TOCR.SaveMonoBitmapToMMFile(BI : BMPINFO;
          var MMFileHandle : Cardinal ) : boolean;
Const
  PAGE_READWRITE = 4;
  FILE_MAP_WRITE = 2;
Var
  tbmp                : TBitmap;
  bmi                 : TMonoBitmapInfo;
  ScanWidth           : Cardinal;//         ' scanline width in bytes
  NumBytes            : Cardinal;//         ' number of bytes required to hold the DIB
  lpMap               : Cardinal;//         ' pointer to mapped file
  pMap                : Pointer;
  lp                  : Cardinal;//         ' pointer
  hDCMem              : Cardinal;//        ' handle to a memory device context
  hbmpOld             : Cardinal;//         ' handle to a bitmap
  hFile               : Cardinal;
Begin
  try

    Result := False;
    hFile := 0;

    // Initialise the header

    With bmi.bmiHeader do begin
      biSize := SizeOf(bmi.bmiHeader);
      biWidth := BI.Width;
      biHeight := BI.Height;
      biXPelsPerMeter := BI.XPelsPerMeter;
      biYPelsPerMeter := BI.YPelsPerMeter;
      biPlanes := 1;
      biBitCount := 1;
      biCompression := BI_RGB;
      biClrUsed := 2;
      biClrImportant := 0;
      ScanWidth := Round((BI.Width + 31) / 32) * 4;
      biSizeImage := ScanWidth * BI.Height;
    end;

    With bmi.bmiColors[0] do begin
      rgbRed := 0;
      rgbGreen := 0;
      rgbBlue := 0;
      rgbReserved := 0;
    end;
    With bmi.bmiColors[1] do begin
      rgbRed := 255;
      rgbGreen := 255;
      rgbBlue := 255;
      rgbReserved := 0;
    end;

    // Calculate the size of the memory mapped file memory

    NumBytes := SizeOf(bmi.bmiHeader) +
      SizeOf(bmi.bmiColors[0]) * 2 + bmi.bmiHeader.biSizeImage;

    //Create a memory only file

    hFile := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, NumBytes, nil);
    if ( hFile <> 0 ) Then begin
      lpMap := Integer(MapViewOfFile(hFile, FILE_MAP_WRITE, 0, 0, 0));
      if ( lpMap <> 0 ) Then begin

          // Copy the bitmap header to the MM file
          lp := lpMap;
          CopyMemory( Pointer(lp), @bmi.bmiHeader, SizeOf(bmi.bmiHeader));
          lp := lp + SizeOf(bmi.bmiHeader);
          CopyMemory( Pointer(lp), @bmi.bmiColors[0], SizeOf(bmi.bmiColors[0]) * 2 );
          lp := lp + SizeOf(bmi.bmiColors[0]) * 2;

          //Retrieve the bitmap bits and copy to the MM file

          hDCMem := CreateCompatibleDC(0);
          If (hDCMem <> 0 ) Then begin
              hbmpOld := SelectObject(hDCMem, BI.hBmp);
              If ( hbmpOld <> 0 ) Then begin
                  pMap := Pointer(lpMap);
                  If (GetDIBits(hDCMem, BI.hBmp, 0, BI.Height, Pointer(lp),
                    PBitmapInfo(lpMap)^, DIB_RGB_COLORS) > 0 ) Then begin

                      Result := True;
                  end;
                  BI.hBmp := SelectObject(hDCMem, hbmpOld);
              end; // hbmpOld
              DeleteDC( hDCMem );

          end; // hDCMem
          UnmapViewOfFile( Pointer( lpMap ));

      end; // lpMap
    end; // hFile

    If (Not Result) Then begin
      If ( hFile <> 0 ) Then begin
          CloseHandle( hFile );
          hFile := 0;
      end;
    end;
    MMFileHandle := hfile;
  finally
    DeleteObject( BI.hBmp );
  end;
End; // SaveMonoBitmapToMMFile

END.
