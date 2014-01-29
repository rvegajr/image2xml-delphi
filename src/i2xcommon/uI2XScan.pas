unit uI2XScan;

interface
uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  StrUtils,
  umcmIntE,
  TWAIN,
  uStrUtil,
  uFileDir,
  Graphics,
  uImage2XML,
  GR32,
  GR32_Image,
  GR32_Transforms,
  GR32_Filters,
  GR32_Resamplers,
  GR32_OrdinalMaps,
  Forms,
  Dialogs,
  Controls,
  Math,
  mcmTWAINKernel,
  mcmTWAINIntf,
  mcmTWAIN,
  mcmRegion,
  mcmImage,
  mcmImageFilter,
  mcmImageTransform,
  mcmImageFile,
  mcmImageColor,
  mcmImageTypeDef,
  mcmImageResStr
  ;

type
  TImageFileCompleteEvent = procedure (Sender: TObject; ImageFileName : string) of object;
  TImageCompleteEvent = procedure (Sender: TObject; BitmapHandle : HBITMAP) of object;

  TI2XScan = class( TObject )
    private
      FIsBusy : boolean;
      FTimeOutTimer : byte;
      twain: TmcmTWAIN;
      FOnStatusChange : TStatusChangeEvent;
      FOnImageFileComplete : TImageFileCompleteEvent;
      FOnImageComplete : TImageCompleteEvent;
      FOnError : TErrorRaisedEvent;
      FOnJobStart : TNotifyEvent;
      FOnJobComplete : TNotifyEvent;
      FImagesProcessed : integer;
      FImagesInPath : integer;
      FMaxImagesToProcess : integer;
      FPath : string;
      slSourceList : TStringList;
      FSourceDevice: string;
      oFileDir : CFileDir;

      procedure twainOnDeviceEvent(Sender : TObject; Event: TTwnDeviceEvent;
      DeviceName  : string; A, B, C : Variant);
      procedure twainImageReady(Sender: TObject; pBmp: Pointer;
        pBmpInfo: PBitmapInfo; hImage: HBITMAP; FilePath: String);
      procedure twainFailure(Sender: TObject; DG: Integer; DAT, CAP,
        MSG: Word; Error, Status: Integer);
      procedure twainNegotiation(Sender: TObject; var CancelScan : boolean);
      procedure twainOnDeviceNotReady(Sender : TObject; DoOpenSouce : boolean);
      procedure twainXferNext(Sender: TObject; var NumImages: Integer;
        var SkipNext: Boolean);
      procedure ErrorRaised(functionName: string; e: TObject);
      procedure StatusChange(const msg: string );
      procedure ImageFileComplete(const ImageFileName : string );
      procedure ImageComplete(const BitmapHandle : HBITMAP );
      procedure resolveDefaultSource();
      procedure JobStart();
      procedure JobComplete();
      function PadL(const StringToPad: string; const Size: Integer): String;
      function getSourceList: TStringList;
      procedure setSourceDevice(const Value: string);
    public
      function SelectSource() : string;
      procedure Scan(const outputPath : string  = ''; const ImagesToScan : integer = MAXINT);
      property ImagesProcessed : integer read FImagesProcessed write FImagesProcessed;
      property ImagesInPath : integer read FImagesInPath write FImagesInPath;
      property Path : string read FPath write FPath;
      property SourceList : TStringList read getSourceList;
      property SourceDevice : string read FSourceDevice write setSourceDevice;
      property OnError : TErrorRaisedEvent read FOnError write FOnError;
      property OnStatusChange: TStatusChangeEvent read FOnStatusChange write FOnStatusChange;
      property OnImageFileComplete : TImageFileCompleteEvent read FOnImageFileComplete write FOnImageFileComplete;
      property OnImageComplete : TImageCompleteEvent read FOnImageComplete write FOnImageComplete;
      property OnJobStart : TNotifyEvent read FOnJobStart write FOnJobStart;
      property OnJobComplete : TNotifyEvent read FOnJobComplete write FOnJobComplete;

      constructor Create( otwain : tMCMTwain );
      destructor Destroy; override;
  end;

implementation

{ TI2XScan }
procedure TI2XScan.twainFailure(Sender: TObject; DG: Integer; DAT, CAP,
  MSG: Word; Error, Status: Integer);
begin
    try
      raise Exception.Create('Scanner ' + FSourceDevice + ' has reported an error. ' +
            'Error Code=' + IntToStr(Error) +            '  Status Code=' + IntToStr(Error) +
            '   DG=' + IntToStr(DG) +
            ' DAT=' + IntToStr(DAT) +
            ' CAP=' + IntToStr(CAP) +
            ' MSG=' + IntToStr(MSG)
            );
    except
        ErrorRaised( 'TI2XScan.twainFailure', ExceptObject );
    end;
end;

function TI2XScan.PadL(const StringToPad: string; const Size: Integer): String;
var
  nStPos : integer;
  sNew, sPadString : string;
begin
    sPadString := '0';
    nStPos := (Size - Length(StringToPad)) + 1;
    if nStPos < 2 then nStPos := 1;
    sNew := StringOfChar(sPadString[1], nStPos - 1);
    Result := sNew + StringToPad;
end;

procedure TI2XScan.twainImageReady(Sender: TObject; pBmp: Pointer;
  pBmpInfo: PBitmapInfo; hImage: HBITMAP; FilePath: String);
var
    sFileName : string;
    image1 :  TmcmImageCtrl;
begin
  try
    if ( hImage = 0 ) then begin
      StatusChange('Handle from TWAIN Device = 0. All Images Acquired (Images acquired:' + IntToStr(FImagesProcessed) + '  in Path:' + IntToStr(FImagesInPath) + ')');
      //raise Exception.Create('Image Handle returned fron TWAIN device is 0.' )
    end else begin
      Inc( FImagesProcessed );
      FIsBusy := true;
      if ( Assigned(FOnImageComplete)) then begin
        ImageComplete( hImage );
      end;

      if ( Assigned(FOnImageFileComplete)) then begin
        try
          image1 := TmcmImageCtrl.Create( nil );
          Inc( FImagesInPath );
          StatusChange('Processing image ' + IntToStr(FImagesProcessed));

          image1.Image.ReleaseHandle;
          image1.Image.DibHandle := hImage;

          if not DirectoryExists( FPath ) then CreateDir( FPath );
          if ( FPath[Length( FPath )] <> '\' ) then FPath := FPath + '\';

          sFileName := FPath + 'img_' + PadL(IntToStr(FImagesInPath), 3) + '.tif';

          image1.Image.Compression := CP_PACKBITS;
          StatusChange('Saving TIF Image of image ' + IntToStr(FImagesProcessed) + ' (Images in path ' + IntToStr(FImagesInPath) + ')');
          image1.Image.FileSave( sFileName );
          ImageFileComplete( sFileName );
          FIsBusy := false;
          StatusChange('Image ' + IntToStr( FImagesProcessed ) + ' has been scanned. ');
        finally
          FreeAndNil( image1 );
        end;
      end;
    end;
  except
    ErrorRaised( 'TI2XScan.twainImageReady', ExceptObject );
  end;
end;

procedure TI2XScan.twainNegotiation(Sender: TObject; var CancelScan : boolean);
var CurrentPixelType : integer;
begin
  { Negotiate PixelType capability. }
  if twain.IsCapSupported(ICAP_PIXELTYPE) then begin
    if (twain.PixelType <> TWPT_RGB) then
        twain.PixelType := TWPT_RGB;
  end;
  if twain.IsCapSupported( ICAP_BITDEPTH ) then begin
    if (twain.BitDepth <> 8) then
        twain.BitDepth := 8;
  end;
  if twain.IsCapSupported( ICAP_XRESOLUTION ) then begin
    if (twain.XResolution <> 300) then
        twain.XResolution := 300;
  end;
  if twain.IsCapSupported( ICAP_YRESOLUTION ) then begin
    if (twain.YResolution <> 300) then
        twain.YResolution := 300;
  end;
  if twain.PaperDetectable then
    if Not(twain.FeederLoaded)
      then CancelScan := True;
  //twain.Rotation := 0;
end;

procedure TI2XScan.ErrorRaised(functionName: string; e: TObject);
Begin
    if ( Assigned(FOnError)) then begin
        FOnError( self, e, functionName );
    end;
End;

procedure TI2XScan.StatusChange( const msg: string );
Begin
    if ( Assigned(FOnStatusChange)) then begin
        FOnStatusChange( self, msg );
    end;
    uImage2XML.WriteLog( msg + #13);
End;

constructor TI2XScan.Create( otwain : tMCMTwain );
begin
  twain := otwain;
  twain.OnNegotiation := twainNegotiation;
  twain.OnImageReady := twainImageReady;
  twain.OnFailure := twainFailure;
  twain.OnXferNext := twainXferNext;
  twain.OnDeviceEvent := twainOnDeviceEvent;

  FImagesProcessed := 0;
  slSourceList := TStringList.Create;
  FSourceDevice := '';
  oFileDir := CFileDir.Create();
end;

destructor TI2XScan.Destroy;
begin
  FreeAndNil( slSourceList );
  FreeAndNil( oFileDir );
  inherited;
end;

procedure TI2XScan.Scan(const outputPath : string; const ImagesToScan : integer  );
var
  slFileList : TStringList;
Begin
    try
      FMaxImagesToProcess := ImagesToScan;
      if ( Length( outputPath ) > 0 ) then
         FPath := outputPath;
      if ( FileExists( FPath ) ) then
        raise Exception.Create(FPath + ' is a file and not a directory.');
      ForceDirectories( FPath );
      //if ( not DirectoryExists( FPath ) ) then begin
      //  if ( MessageDlg('Path "' + FPath + '" does not exist,  would you like me to create it?', mtConfirmation , [mbYes, mbNo], 0 ) = mrYes ) then begin
      //    ForceDirectories( FPath );
      //  end;
      //end;
      if ( not DirectoryExists( FPath ) ) then begin
        raise Exception.Create('Directory ' + FPath + ' does not exist.  Scanning of images cancelled.');
      end else begin
        if ( MessageDlg('Set the document in the scanner.  Once the document has been set,  click "Yes" or click "No" to cancel.' + #13#10 + 'Are you ready?',mtConfirmation, [mbYes, mbNo], 0 ) = mrYes ) then begin
            FImagesProcessed := 0;
            FImagesInPath := 0;
            self.JobStart();
            try
              slFileList := oFileDir.GetFileList( FPath + '*.tif' );
              FImagesInPath := slFileList.Count;
            finally
              FreeAndNil( slFileList );
            end;
            if ( FImagesInPath = 0 ) then
              StatusChange('Beginning Document Scan to path ' + FPath )
            else
              StatusChange('Beginning Document Scan.  There are already ' + IntToStr(FImagesInPath) + ' image(s) in path ' + FPath );
            FTimeOutTimer := 0;
            StatusChange('-Acquiring Image from Scanner...');
            twain.ShowUI := true;
            twain.AutoScan( true );
            twain.FeederEnabled( true );
            if ( not twain.AutoFeed( true ) ) then begin
              StatusChange('-Auto Feed could not be set,  auto feed will be disabled for this run.');
            end;
            twain.ShowUI := false;
            twain.DisableAfterAcquire := true;
            twain.ShowIndicators := false;

            //resolveDefaultSource();
            twain.Acquire( FSourceDevice  );
            FIsBusy := false;
        end else begin
            StatusChange('Acquiring Image canceled by user');
        end
      end;
    except
        ErrorRaised( 'TI2XScan.scan', ExceptObject );
    end;
End;

function TI2XScan.SelectSource: string;
var
  s : string;
begin
  Result := '';
  twain.OpenSourceMgr;
  twain.SelectSource;
  if ( twain.GetDefaultSource( s ) = TWRC_SUCCESS ) then
    Result := s;
  twain.CloseSourceMgr;
end;

procedure TI2XScan.twainXferNext(Sender: TObject; var NumImages: Integer;
      var SkipNext: Boolean);
Begin
    try
      if ( NumImages = 0 ) then begin
        StatusChange( 'Scanning has completed.' );
      end else
        SkipNext := FImagesProcessed >= self.FMaxImagesToProcess;
      NumImages := 1;
      while (( NumImages <> 0 ) and ( not SkipNext ) and ( FIsBusy )) do begin
        if ( FTimeOutTimer >= 120 ) then
          NumImages := 0;
          raise Exception.Create('Timeout while waiting for program to finish processing image so scanner can retieve other messages');
        sleep( 1000 );
        Inc(FTimeOutTimer);
      end;
      if ( NumImages = 0 ) then
        self.JobComplete();
    except
        ErrorRaised( 'TI2XScan.twainXferNext', ExceptObject );
    end;
End;

function TI2XScan.getSourceList: TStringList;
Begin
    try
      twain.GetSourceList( slSourceList );
      result := slSourceList;
    except
        ErrorRaised( 'TI2XScan.getSourceList', ExceptObject );
    end;
End;

procedure TI2XScan.ImageComplete(const BitmapHandle: HBITMAP);
begin
  if ( Assigned( FOnImageComplete )) then begin
    FOnImageComplete( self, BitmapHandle );
  end;
end;

procedure TI2XScan.ImageFileComplete(const ImageFileName: string);
begin
  if ( Assigned( FOnImageFileComplete )) then begin
    FOnImageFileComplete( self, ImageFileName );
  end;
end;

procedure TI2XScan.JobComplete;
begin
  if ( Assigned( FOnJobComplete )) then begin
    FOnJobComplete( Self );
  end;
end;

procedure TI2XScan.JobStart;
begin
  if ( Assigned( FOnJobStart )) then begin
    FOnJobStart( Self );
  end;
end;

procedure TI2XScan.resolveDefaultSource;
var
  i : integer;
Begin
    try
      getSourceList();
      if ( Length( FSourceDevice ) <> 0 ) then begin
        for i := 0 to slSourceList.Count - 1 do begin
          if ( Pos( FSourceDevice, slSourceList[i] ) > 0 ) then begin
            FSourceDevice := slSourceList[i];
          end;
        end;
      end;
    except
        ErrorRaised( 'TI2XScan.resolveDefaultSource', ExceptObject );
    end;
End;

procedure TI2XScan.twainOnDeviceEvent(Sender: TObject;
  Event: TTwnDeviceEvent; DeviceName: string; A, B, C: Variant);
Begin
  if ( Event = TWDE_CHECKAUTOMATICCAPTURE ) then
    StatusChange('OnDeviceEvent:TWDE_CHECKAUTOMATICCAPTURE  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_CHECKBATTERY ) then
    StatusChange('OnDeviceEvent:TWDE_CHECKAUTOMATICCAPTURE  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_CHECKFLASH ) then
    StatusChange('OnDeviceEvent:TWDE_CHECKFLASH  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_CHECKPOWERSUPPLY ) then
    StatusChange('OnDeviceEvent:TWDE_CHECKPOWERSUPPLY  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_CHECKRESOLUTION ) then
    StatusChange('OnDeviceEvent:TWDE_CHECKRESOLUTION  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_DEVICEADDED ) then
    StatusChange('OnDeviceEvent:TWDE_DEVICEADDED  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_DEVICEOFFLINE ) then
    StatusChange('OnDeviceEvent:TWDE_DEVICEOFFLINE  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_DEVICEREADY ) then
    StatusChange('OnDeviceEvent:TWDE_DEVICEREADY  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_DEVICEREMOVED ) then
    StatusChange('OnDeviceEvent:TWDE_DEVICEREMOVED  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_IMAGECAPTURED ) then
    StatusChange('OnDeviceEvent:TWDE_IMAGECAPTURED  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_IMAGEDELETED ) then
    StatusChange('OnDeviceEvent:TWDE_IMAGEDELETED  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_PAPERDOUBLEFEED ) then
    StatusChange('OnDeviceEvent:TWDE_PAPERDOUBLEFEED  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_PAPERJAM ) then
    StatusChange('OnDeviceEvent:TWDE_PAPERJAM  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_LAMPFAILURE ) then
    StatusChange('OnDeviceEvent:TWDE_LAMPFAILURE  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_POWERSAVE ) then
    StatusChange('OnDeviceEvent:TWDE_POWERSAVE  A=' + A  + '  B=' + B + '  C=' + C)
  else if ( Event = TWDE_POWERSAVENOTIFY ) then
    StatusChange('OnDeviceEvent:TWDE_POWERSAVENOTIFY  A=' + A  + '  B=' + B + '  C=' + C)
  ;
End;

procedure TI2XScan.twainOnDeviceNotReady(Sender: TObject;
  DoOpenSouce: boolean);
Begin
  DoOpenSouce := false;
  StatusChange('Scanner ' + FSourceDevice + ' is not ready.  Please verify that it is on and there are not pending jobs.' );
End;

procedure TI2XScan.setSourceDevice(const Value: string);
Begin
  FSourceDevice := Value;
  resolveDefaultSource();
End;

END.
