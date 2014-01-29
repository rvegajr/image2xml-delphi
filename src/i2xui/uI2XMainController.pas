unit uI2XMainController;

interface

uses
  uI2XOCR,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ActnList,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Grids,
  Buttons,
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_RangeBars,
  GR32_Filters,
  GR32_Transforms,
  GR32_Resamplers,
  ImgList,
  uStrUtil,
  uFileDir,
  uHashTable,
  uReg,
  uImage2XML,
  uI2XTemplate,
  mcmImage,
  OmniXML,
  ShellAPI,
  StdActns,
  uDWImage,
  uI2XDLL,
  JclStrings,
  uI2XConstants,
  //uI2XImageProcThread,
  uI2XImageProcJobThread,
  uI2XProductThread,
  uI2XOCRProcThread,
  uI2XProduct,
  UIOCRProductViewDialog,
  uI2XOptions,
  uI2XPluginManager,
  MapStream,
  uI2XThreadBase,
  uI2XMemMap,
  uI2XUndo
  ;

type
  TI2XMainUIController = class(TObject)
  private
    FBaseForm : TForm;
    FOnStatusChange : TOnStatusChangeEvent;
    FOnDebugMessage : TOnStatusChangeEvent;
    FImageProcCache : TDWImage;
    FImageProcCacheInst : string;
    FImageProcCacheMMID : string;
    DWImgView : TDWImage;
    FMemoryMapManager : TI2XMemoryMapManager;
    function getImageProcCacheInst: string;
    procedure setImageProcCacheInst(const Value: string);
    procedure OnImageProcJobEndCache( Sender : TObject; TaskID: string;
      InstructionsCompleted: integer; ImageMMID: string);
    function getTestsForImageAttrib: TDLLOCRTests;
  public
    ImageProcCacheUpdating : boolean;
    LastSelectedTab : string;
    ImgView: TImgView32;

    ImageProcJobThread : TI2XImageProcJobThread;
    ImageProcCacheThread : TI2XImageProcJobThread;
    ProductJobThread : TI2XProductThread;
    OCRJobThread : TI2XOCRProcJobThread;
    FDLLOCRTestsForImageAttrib : TDLLOCRTests;
    function ResolutionCheck( bmp: TBitMap32 ): boolean;
    property DLLOCRTestsForImageAttrib : TDLLOCRTests read getTestsForImageAttrib;
    property MemoryMapManager : TI2XMemoryMapManager read FMemoryMapManager write FMemoryMapManager;
    procedure CacheImageExpire;
    procedure CacheProcessedImage( dwimage : TDWImage ); overload;
    procedure CacheProcessedImage( bmp : TBitMap32 ); overload;
    //procedure TESTCompleteOCR(Sender: TObject);
    procedure TESTImageProcBeforeOCR(Sender: TObject);
    procedure ProcessImageInstructions(const InstructionList: TStringList; PerformOCRAfter : boolean = false; UtilizeCache : boolean = false );
    procedure ProcessImageInstructionsPlusOCR(const InstructionList: TStringList );
    procedure OCRImage( BMP32 : TBitmap32; const Template: CTemplate; const OCRRunType : TOCRRunType = otNormal; const ImageFileNameString : string = ''); overload;
    procedure OCRImage( const ImageMMID : string; const Template : CTemplate; const OCRRunType : TOCRRunType = otNormal; const ImageFileNameString : string = '' ); overload;
    procedure OCRImage( const odwImage : TDWImage; const Template : CTemplate; const OCRRunType : TOCRRunType = otNormal; const ImageFileNameString : string = '' ); overload;
    procedure OCRImageSelection( DWImage: TDWImage; const Template: CTemplate); overload;
    procedure OCRImageSelection( ImageMMID : string; const Template: CTemplate); overload;
    procedure OCRImageSelection( BMP32 : TBitmap32; const Template: CTemplate); overload;
    function OCRImageCheck( DWImage : TDWImage; DisplayPrompt : boolean = true ) : boolean; overload;
    function OCRImageCheck( BMP32: TBitmap32; DisplayPrompt : boolean = true ) : boolean; overload;

    //function GetOCRTests: TDLLOCRTests;
    function getImageProcCache: TDWImage;
    procedure OnImageProcJobEnd( Sender : TObject; TaskId : string; InstructionsCompleted : integer; ImageMMID : string );
    procedure OnImageProcJobStart( Sender : TObject; TaskId : string; InstructionsCount : integer );
    procedure OnJobError( Sender : TObject; TaskId : string; ErrorNo : integer; ErrorString: string  );
    procedure OnJobCacheError( Sender : TObject; TaskId : string; ErrorNo : integer; ErrorString: string  );
    procedure OnThreadTerminate( Sender : TObject );
    procedure OnImageAttributesCalculated( Sender : TObject; const Boundary : TRect; const FirstCharacter : TI2XOCRItem );


    procedure OnImageCacheJobEnd( Sender : TObject; TaskId : string; InstructionsCompleted : integer; ImageMMID : string );
    procedure OnImageCacheJobStart( Sender : TObject; TaskId : string; InstructionsCount : integer );
    procedure OnImageCacheStatusChange( Sender : TObject; TaskId : string; sStatusMessage : string );

    procedure OnProductJobEnd( Sender : TObject; TaskId : string; TestsPerformed : integer; ReturnXML : string );
    procedure OnProductJobStart( Sender : TObject; TaskId : string; TestsCount : integer );
    procedure OnOCRJobEnd( Sender : TObject; TaskId : string; TestsPerformed : integer; ImageMMID : string );
    procedure OnReturnXML( Sender : TObject; TaskId : string; ReturnXML : string );
    procedure OnOCRJobStart( Sender : TObject; TaskId : string; TestsCount : integer );
    procedure OnOCRSelectionJobEnd( Sender : TObject; TaskId : string; TestsPerformed : integer; ImageMMID : string );
    procedure OnOCRSelectionReturnXML( Sender : TObject; TaskId : string; ReturnXML : string );
    procedure OnOCRSelReturnXMLForFirstChar(Sender: TObject; TaskId,
      ReturnXML: string);
    procedure OnOCRSelectionJobStart( Sender : TObject; TaskId : string; TestsCount : integer );
    procedure OnStatusChange( Sender : TObject; TaskId : string; sStatusMessage : string );
    procedure OnDebugMessage( Sender : TObject; TaskId : string; sDebugMessage : string );

    property OnStatusChangeEvent : TOnStatusChangeEvent read FOnStatusChange write FOnStatusChange;
    property OnDebugMessageEvent : TOnStatusChangeEvent read FOnDebugMessage write FOnDebugMessage;
    //property OCRTests : TDLLOCRTests read GetOCRTests;
    property BaseForm : TForm read FBaseForm write FBaseForm;
    property ImageProcCache : TDWImage read getImageProcCache;
    property ImageProcCacheInst : string read getImageProcCacheInst write setImageProcCacheInst;
    property ImageProcCacheMMID : string read FImageProcCacheMMID write FImageProcCacheMMID;
    function TryObtainMMID(PossibleMMIDObject : TObject; var MMID : string ) : boolean;

    procedure StatusMessage( sStatusMessage : string );
    procedure DebugMessage( sDebugMessage : string );
    constructor Create(BaseForm : TForm);
    destructor Destroy; override;
  end;

implementation
uses
  UII2XBase;

{
  I am breaking all sorts of object oriented encapsulation rules here,
  but I can take comfort in knowing that it was because of a single
  'abstract error' I received in the UI when I tried to 'properly' inherit
  a base form that drove me to this abomination.  Basically,  the
  main form can be referenced in this unit using fmain.   

}
var
  fMain : TI2XBaseUI;

{ TI2XMainUIController }

procedure TI2XMainUIController.CacheProcessedImage( dwimage : TDWImage );
var
  sMemoryMapID : string;
  InstructionList : TStringList;
  sImageCacheOnDisk : string;
  sImageCacheOnDiskInst : string;
Begin
  try
    InstructionList := TStringList.Create();
    fMain.ActionListToStringList( fMain.getSelectedActionList(), InstructionList );
    //if ( ImageProcCacheThread <> nil ) then
    //  ImageProcCacheThread.Free;
    if ( InstructionList.Count = 0 ) then begin
      if ( FImageProcCache <> nil ) then
        FImageProcCache.Free;
      FImageProcCache := TDWImage.Create( dwimage.Bitmap );

      FImageProcCacheMMID := '';

      if ( Integer(DebugLevel) >= 2 ) then StatusMessage( 'Image Cached' );
    end else begin
      sImageCacheOnDisk := fMain.Reg.Get('ImageCacheOnDisk', '');
      sImageCacheOnDiskInst := fMain.Reg.Get('ImageCacheOnDiskInst', '');
      if (
          ( sImageCacheOnDiskInst = fMain.ActionListAsString(fMain.getSelectedActionList())) and
          ( FileExists( sImageCacheOnDisk ) )
         )
      then begin
        if ( FImageProcCache <> nil ) then begin
          FImageProcCache.Free;
        end;
        if ( Integer(DebugLevel) >= 2 ) then StatusMessage( 'Reading Cached Image from Disk' );
        FImageProcCache := TDWImage.Create();
        FImageProcCache.LoadFromFile( sImageCacheOnDisk );
        FImageProcCacheMMID := '';
        //FImageProcCache.SaveToMemoryMap( FImageProcCacheMMID );

        if ( Integer(DebugLevel) >= 2 ) then StatusMessage( 'Image Cached' );
      end else begin
        ImageProcCacheThread := TI2XImageProcJobThread.Create();
        ImageProcCacheThread.FreeOnTerminate := true;
        ImageProcCacheThread.TriggerOCRAfter := false;
        ImageProcCacheThread.OnJobStartEvent := self.OnImageCacheJobStart;
        ImageProcCacheThread.OnJobEndEvent := self.OnImageCacheJobEnd;
        ImageProcCacheThread.OnJobErrorEvent := self.OnJobCacheError;
        ImageProcCacheThread.OnStatusChangeEvent := OnImageCacheStatusChange;
        ImageProcCacheThread.OnDebugMessageEvent := OnDebugMessage;
        ImageProcCacheThread.DebugLevel := uImage2XML.DebugLevel;

        if ( Integer(DebugLevel) >= 2 ) then DebugMessage(
          'Image Res: X=' + IntToStr(dwimage.DPIX) + '  Y=' + IntToStr(dwimage.DPIY) );
        sMemoryMapID := IMAGE_MEMMAP_HEADER+ '_CacheProcessedImage_' + RandomString();
        FMemoryMapManager.DebugMessageText := 'At Cache Processed image.';
        self.FMemoryMapManager.Write( sMemoryMapID, dwimage );

        ImageProcCacheThread.ImageMemoryMapID := sMemoryMapID;
        ImageProcCacheThread.InstructionList.Clear;
        ImageProcCacheThread.InstructionList.AddStrings( InstructionList );
        if ( FImageProcCache = nil ) then begin
          FImageProcCache := TDWImage.Create();
        end;
        ImageProcCacheThread.Resume;
      end;
    end;
  finally
    if ( InstructionList <> nil ) then FreeAndNil( InstructionList );
  end;
End;

function TI2XMainUIController.getImageProcCache: TDWImage;
begin
  if ( FImageProcCache = nil ) then begin
    FImageProcCache := TDWImage.Create;
  end;
  Result := FImageProcCache;
end;

function TI2XMainUIController.getImageProcCacheInst: string;
begin
  Result := self.FImageProcCacheInst;
end;


function TI2XMainUIController.getTestsForImageAttrib: TDLLOCRTests;
var
  i : integer;
begin
  self.FDLLOCRTestsForImageAttrib.Clear;
  Options.DLLOCRTestsComplete.CopyTo( FDLLOCRTestsForImageAttrib );
  //self.FDLLOCRTestsForImageAttrib.CopyFrom( Options.DLLOCRTestsComplete );
  for i := 0 to FDLLOCRTestsForImageAttrib.Count - 1 do begin
    //To do image attributes,  our tests need to be whole document OCRs, otherwise,
    //  how can we figure out where the boundaries and the first characters are?
    FDLLOCRTestsForImageAttrib[i].SetOCRTest( ocrtNormalWhole, true );
    FDLLOCRTestsForImageAttrib[i].SetOCRTest( ocrtNormalSliced, false );
    FDLLOCRTestsForImageAttrib[i].SetOCRTest( ocrtInvertedWhole, true );
    FDLLOCRTestsForImageAttrib[i].SetOCRTest( ocrtInvertedSliced, false );
  end;
  Result := FDLLOCRTestsForImageAttrib;
end;

function TI2XMainUIController.ResolutionCheck( bmp : TBitMap32 ) : boolean;
Begin
  dbg( 'Resultion Check=' + IntToStr(bmp.BitmapInfo.bmiHeader.biXPelsPerMeter) );
  Result := true;
End;

procedure TI2XMainUIController.ProcessImageInstructions(const InstructionList: TStringList; PerformOCRAfter : boolean ; UtilizeCache : boolean );
//procedure TI2XMainUIController.ProcessImageInstructions(const InstructionList : TStringList; PerformOCRAfter : boolean  );
var
  sMemoryMapID : string;
Begin
  try
    if (( ResolutionCheck( self.ImgView.Bitmap ) ) and ( InstructionList.Count > 0 )) then begin

      if (( UtilizeCache ) and
          ( FImageProcCacheInst = fMain.ActionListAsString(fMain.getSelectedActionList() ))) then begin
        fMain.txtActiveImage.Text := self.FImageProcCache.FileName;
        fMain.ReLoadActiveImage();
      end else begin
        //if ( not (ImageProcJobThread = nil ) ) then
        //  ImageProcJobThread.Free;
        fMain.pmnuExecuteActionList.Enabled := false;
        fMain.pmnuProcInstAndOCR.Enabled := false;

        ImageProcJobThread := TI2XImageProcJobThread.Create( self.ImgView.Bitmap );
        ImageProcJobThread.FreeOnTerminate := true;
        ImageProcJobThread.TriggerOCRAfter := PerformOCRAfter;
        ImageProcJobThread.OnJobStartEvent := self.OnImageProcJobStart;
        ImageProcJobThread.OnJobErrorEvent := self.OnJobError;
        ImageProcJobThread.OnTerminate := self.OnThreadTerminate;
        //ImageProcJobThread.OnJobEndEvent := self.OnImageProcJobEnd;
        if ( UtilizeCache ) then
          ImageProcJobThread.OnJobEndEvent := OnImageProcJobEndCache
        else
          ImageProcJobThread.OnJobEndEvent := self.OnImageProcJobEnd;
        ImageProcJobThread.OnStatusChangeEvent := OnStatusChange;
        ImageProcJobThread.OnDebugMessageEvent := OnDebugMessage;

        ImageProcJobThread.DebugLevel := uImage2XML.DebugLevel;
        sMemoryMapID := IMAGE_MEMMAP_HEADER+ '_ProcessImageInstructions_' + RandomString();
        MemoryMapManager.Write( sMemoryMapID,  ImageProcJobThread.DWImage );

        ImageProcJobThread.ImageMemoryMapID := sMemoryMapID;
        ImageProcJobThread.InstructionList.Clear;
        ImageProcJobThread.InstructionList.AddStrings( InstructionList );
        ImageProcJobThread.Resume;
      end;
    end;
  finally
  end;
End;

procedure TI2XMainUIController.ProcessImageInstructionsPlusOCR(
  const InstructionList: TStringList);
var
  sMemoryMapID : string;
Begin
  fMain.Repaint;
  try
    if ( InstructionList.Count > 0 ) then begin
      //if ( ImageProcJobThread <> nil ) then
      //  ImageProcJobThread.Free;
      fMain.pmnuExecuteActionList.Enabled := false;
      fMain.pmnuProcInstAndOCR.Enabled := false;

      ProductJobThread := TI2XProductThread.Create( self.ImgView.Bitmap );
      ProductJobThread.FreeOnTerminate := true;
      ProductJobThread.OnJobStartEvent := self.OnOCRJobStart;
      ProductJobThread.OnJobEndEvent := self.OnOCRJobEnd;
      ProductJobThread.OnReturnXMLEvent := self.OnReturnXML;
      ProductJobThread.OnJobErrorEvent := self.OnJobError;

      ProductJobThread.OnStatusChangeEvent := OnStatusChange;
      ProductJobThread.OnDebugMessageEvent := OnDebugMessage;
      ProductJobThread.DebugLevel := DebugLevel;

      sMemoryMapID := IMAGE_MEMMAP_HEADER+ '_ProcessImageInstructionsPlusOCR_' + RandomString();

      MemoryMapManager.Write( sMemoryMapID,  ProductJobThread.DWImage );
      ProductJobThread.ImageMemoryMapID := sMemoryMapID;
      ProductJobThread.InstructionList.Clear;
      ProductJobThread.InstructionList.AddStrings( InstructionList );

      ProductJobThread.OCREngineTests := Options.DLLOCRTestsComplete;

      ProductJobThread.Template := fMain.Template;

      ProductJobThread.Resume;
    end;
  finally
  end;
End;

procedure TI2XMainUIController.setImageProcCacheInst(const Value: string);
begin
  if (( FImageProcCacheInst <> Value ) and ( ImgView.Bitmap.Handle <> 0 )) then begin
    self.CacheProcessedImage( ImgView.Bitmap );
    FImageProcCacheInst := Value;
  end;
end;

procedure TI2XMainUIController.OCRImage( const ImageMMID : string; const Template : CTemplate; const OCRRunType : TOCRRunType; const ImageFileNameString : string  );
Begin
  try
      //if ( OCRJobThread <> nil ) then
      //  OCRJobThread.Free;
      fMain.pmnuExecuteActionList.Enabled := false;
      fMain.pmnuProcInstAndOCR.Enabled := false;

      OCRJobThread := TI2XOCRProcJobThread.Create();
      OCRJobThread.FreeOnTerminate := true;
      OCRJobThread.OnJobStartEvent := self.OnOCRJobStart;
      OCRJobThread.OnJobEndEvent := self.OnOCRJobEnd;
      OCRJobThread.OnJobErrorEvent := self.OnJobError;
      OCRJobThread.ImageFileName := ImageFileNameString;

      OCRJobThread.OnStatusChangeEvent := OnStatusChange;
      OCRJobThread.OnDebugMessageEvent := OnDebugMessage;
      OCRJobThread.DebugLevel := DebugLevel;

      //OCRJobThread.OCREngineTests := GetOCRTests;
      if ( OCRRunType = otAnalysis ) then begin
        OCRJobThread.OnImageAttributesCalculatedEvent := OnImageAttributesCalculated;
        OCRJobThread.OCREngineTests := self.DLLOCRTestsForImageAttrib;
        //OCRJobThread.OnReturnXMLEvent := self.OnReturnXML;
      end else begin
        OCRJobThread.OnReturnXMLEvent := self.OnReturnXML;
        OCRJobThread.OCREngineTests := Options.DLLOCRTestsComplete;
      end;

      OCRJobThread.ImageMemoryMapID := ImageMMID;
      OCRJobThread.Template := Template;
      OCRJobThread.Resume;
  finally
    //if ( InstructionList <> nil ) then FreeAndNil( InstructionList );
  end;
End;

procedure TI2XMainUIController.OCRImageSelection(ImageMMID: string;
  const Template: CTemplate);
var
  img : TDWImage;
begin
  try
      //if ( OCRJobThread <> nil ) then
      //  OCRJobThread.Free;

      if ( integer(DebugLevel) >= 2 ) then begin
        try
          img := TDWImage.Create();
          dbg( 'OCRImageSelection(): FMemoryMapManager.OpenMaps ' + FMemoryMapManager.OpenMapsAsString() );

          FMemoryMapManager.DebugMessageText := 'At OCRImageSelection.';
          FMemoryMapManager.Read( ImageMMID, img, false);
          img.SaveToFile( AppTempDir + 'TEST_OCRSEL_1.BMP' );
        finally
          FreeAndNil( img );
        end;
      end;

      fMain.pmnuExecuteActionList.Enabled := false;
      fMain.pmnuProcInstAndOCR.Enabled := false;

      OCRJobThread := TI2XOCRProcJobThread.Create();
      OCRJobThread.FreeOnTerminate := true;
      OCRJobThread.EnableJobEvents := true;
      OCRJobThread.OnJobStartEvent := self.OnOCRSelectionJobStart;
      OCRJobThread.OnJobEndEvent := self.OnOCRSelectionJobEnd;
      if ( fMain.SelectedElement.isFirstChar )  then
        OCRJobThread.OnReturnXMLEvent := self.OnOCRSelReturnXMLForFirstChar
      else
        OCRJobThread.OnReturnXMLEvent := self.OnOCRSelectionReturnXML;
      OCRJobThread.OnJobErrorEvent := self.OnJobError;

      OCRJobThread.OnStatusChangeEvent := OnStatusChange;
      OCRJobThread.OnDebugMessageEvent := OnDebugMessage;
      OCRJobThread.DebugLevel := DebugLevel;

      //OCRJobThread.OCREngineTests := GetOCRTests;
      OCRJobThread.OCREngineTests := Options.DLLOCRTestsSelect;

      OCRJobThread.ImageMemoryMapID := ImageMMID;
      OCRJobThread.Template := Template;
      OCRJobThread.EnableOffsetCorrection := false;

      OCRJobThread.Resume;
  finally
  end;
end;

procedure TI2XMainUIController.OCRImageSelection( DWImage : TDWImage; const Template : CTemplate );
var
  sMemoryMapID : string;
Begin
  try
    MemoryMapManager.Write( sMemoryMapID,  DWImage );
    OCRImageSelection( sMemoryMapID, Template );
  finally
  end;
End;

procedure TI2XMainUIController.OCRImageSelection(BMP32: TBitmap32;
  const Template: CTemplate);
var
  sMemoryMapID : string;
Begin
  try
    if ( integer(DebugLevel) >= 2 ) then begin
      BMP32.SaveToFile( AppTempDir + 'OCRImageSelection_BMP32.bmp' );
    end;

    MemoryMapManager.Write( sMemoryMapID,  BMP32 );
    OCRImageSelection( sMemoryMapID, Template );
  finally
  end;
End;

function TI2XMainUIController.OCRImageCheck( BMP32: TBitmap32; DisplayPrompt : boolean ) : boolean;
var
  dw : TDWImage;
Begin
  Result := false;
  if ( BMP32 = nil ) then Exit;
  try
    dw := TDWImage.Create( BMP32 );
    try
      Result := OCRImageCheck( dw, DisplayPrompt );
    except
    end;
  finally
    FreeAndNil( dw );
  end;
End;

function TI2XMainUIController.OCRImageCheck( DWImage : TDWImage; DisplayPrompt : boolean ) : boolean;
Begin
  Result := false;
  if ( DWImage = nil ) then Exit;
  try
    try
      if ( DWImage.ColorCount <= 2 ) then
        Result := true
      //else if ( DWImage.ColorCount <= 4 ) then begin
      //  Result := (MessageDlg(FILE_EXISTS_TEXT, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
      else begin
        if ( DisplayPrompt ) then
          Result := (MessageDlg(OCR_COLOR_WARNING_TEXT, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
        else
          self.DebugMessage( OCR_COLOR_WARNING );
      end;
    except

    end;
  finally

  end;
End;

procedure TI2XMainUIController.TESTImageProcBeforeOCR(Sender: TObject);
Begin

End;

procedure TI2XMainUIController.OCRImage( const odwImage : TDWImage; const Template : CTemplate; const OCRRunType : TOCRRunType; const ImageFileNameString : string  );
var
  sMMID : string;
Begin
  self.MemoryMapManager.Write( sMMID, odwImage );
  //odwImage.SaveToMemoryMap( sMMID );
  self.OCRImage( sMMID, Template, OCRRunType, ImageFileNameString );
End;

procedure TI2XMainUIController.OCRImage(BMP32: TBitmap32;
  const Template: CTemplate; const OCRRunType : TOCRRunType;
  const ImageFileNameString : string );
var
  sMMID : string;
begin
  self.MemoryMapManager.Write( sMMID, BMP32 );
  self.OCRImage( sMMID, Template, OCRRunType, ImageFileNameString );
end;

procedure TI2XMainUIController.StatusMessage(sStatusMessage: string);
begin
  self.OnStatusChange( self, '', sStatusMessage );
end;

procedure TI2XMainUIController.OnDebugMessage(Sender : TObject; TaskId, sDebugMessage: string);
begin
  if ( Assigned( self.FOnDebugMessage ) ) then
    self.FOnDebugMessage( Sender, TaskID, sDebugMessage );
end;

procedure TI2XMainUIController.CacheImageExpire();
var
  sImageCacheOnDisk : string;
  sImageCacheOnDiskInst : string;
Begin
  sImageCacheOnDisk := AppTempDir + IMGPROC_CACHE;
  sImageCacheOnDiskInst := fMain.ActionListAsString( fMain.getSelectedActionList() );
  if ( FileExists( sImageCacheOnDisk ) ) then DeleteFile( sImageCacheOnDisk );
  fMain.Reg['ImageCacheOnDisk'] := '';
  fMain.Reg['ImageCacheOnDiskInst'] := '';
  FImageProcCacheInst := '';

  fMain.Boundary.Left:=0;
  fMain.Boundary.Top:=0;
  fMain.Boundary.Right:=0;
  fMain.Boundary.Bottom:=0;
  fmain.FirstChar.Clear;
  //FImageProcCache.Free;
End;

procedure TI2XMainUIController.OnImageAttributesCalculated(Sender: TObject;
  const Boundary: TRect; const FirstCharacter: TI2XOCRItem);
begin
  fMain.Boundary := Boundary;
  fMain.FirstChar.Assign( FirstCharacter );

  fMain.TriggerUIEvents( false );
  fMain.txtFirstCharX.Text := IntToStr( FirstCharacter.X );
  fMain.txtFirstCharY.Text := IntToStr( FirstCharacter.Y );
  fMain.txtFirstCharWidth.Text := IntToStr( FirstCharacter.Width );
  fMain.txtFirstCharHeight.Text := IntToStr( FirstCharacter.Height );

  fMain.txtXBound.Text := IntToStr( Boundary.Left );
  fMain.txtYBound.Text := IntToStr( Boundary.Top );

  if ( not fMain.template.Elements.ContainsKey( STANDARD_REGION_FIRST_CHAR ) ) then begin
    fMain.CreateRegion( STANDARD_REGION_FIRST_CHAR );
  end;
  fMain.FirstCharRegionElement := fMain.template.getElement( STANDARD_REGION_FIRST_CHAR );
  fMain.RegionAttributeChange( fMain.FirstCharRegionElement, FirstCharacter.AsRect );

  if ( not fMain.template.Elements.ContainsKey( STANDARD_REGION_X_AXIS ) ) then begin
    fMain.CreateRegion( STANDARD_REGION_X_AXIS );
  end;
  fMain.XAxisRegionElement := fMain.template.getElement( STANDARD_REGION_X_AXIS );
  fMain.XAxisRegionElement.X := Boundary.Left;

  if ( not fMain.template.Elements.ContainsKey( STANDARD_REGION_Y_AXIS ) ) then begin
    fMain.CreateRegion( STANDARD_REGION_Y_AXIS );
  end;
  fMain.YAxisRegionElement := fMain.template.getElement( STANDARD_REGION_Y_AXIS );
  fMain.YAxisRegionElement.Y := Boundary.Top;
  fMain.DrawRegion( Sender, fMain.XAxisRegionElement, true, clGreen32 );
  fMain.DrawRegion( Sender, fMain.YAxisRegionElement, true, clGreen32 );

  if ( fMain.btnSearchFirstChar.Tag = 1 ) then begin
  //SelectDocumentCorrection will display the region we want to display
    fMain.SelectDocumentCorrection( fMain.rbtnFirstChar.tag );
    fMain.SetZoom( 3 );
    ImgView.ScrollToCenter( fMain.FirstCharRegionElement.x, fMain.FirstCharRegionElement.y );

    fMain.btnSearchFirstChar.Tag := 0;
  end;
  if ( fMain.btnFindXYBoundaries.Tag = 1 ) then begin
  //SelectDocumentCorrection will display the region we want to display
    fMain.SelectDocumentCorrection( fMain.rbtnXYBoundary.tag );
    fMain.SetZoom( 0.25 );
    fMain.btnFindXYBoundaries.Tag := 0;
  end;

  fMain.TriggerUIEvents( true );

end;

procedure TI2XMainUIController.OnImageCacheJobEnd(Sender: TObject; TaskId: string;
  InstructionsCompleted: integer; ImageMMID: string);
var
  sImageCacheOnDisk : string;
  sImageCacheOnDiskInst : string;
  DPIX : integer;
begin
  try
    //if ( FImageProcCache <> nil ) then
    //  FImageProcCache.Free;
    dbg( 'OnImageCacheJobEnd(): FMemoryMapManager.OpenMaps ' + FMemoryMapManager.OpenMapsAsString() );
    FMemoryMapManager.DebugMessageText := 'At OnImageCacheJobEnd.';
    self.MemoryMapManager.Read( ImageMMID, FImageProcCache );

    self.FImageProcCacheMMID := ImageMMID;

    sImageCacheOnDisk := AppTempDir + IMGPROC_CACHE;
    sImageCacheOnDiskInst := fMain.ActionListAsString( fMain.getSelectedActionList() );
    if ( fMain.ActiveImageIsLoaded ) then begin
      DPIX := 0;
      TryStrToInt( fMain.template.getData( 'x_dpi' ), DPIX );
      if ( DPIX <> 0 ) then FImageProcCache.setDPI( DPIX );
    end;
    FImageProcCache.SaveToFile( sImageCacheOnDisk, true );
    fMain.Reg['ImageCacheOnDisk'] := sImageCacheOnDisk;
    fMain.Reg['ImageCacheOnDiskInst'] := sImageCacheOnDiskInst;

    //self.ImageProcCache.ClearMemoryMap;
    if ( Integer(DebugLevel) >= 2 ) then fMain.StatusMessage( 'Image Cached', pgDisable );
    if ( Integer(DebugLevel) <= 1 ) then fMain.StatusMessage( 'Image Processed', pgDisable );
    ImageProcCacheUpdating := false;
    fMain.SetOCRMenuOptions( true );
  finally
  end;
end;

procedure TI2XMainUIController.OnImageCacheJobStart(Sender : TObject; TaskId: string;
  InstructionsCount: integer);
begin
  if ( Integer(DebugLevel) >= 2 ) then StatusMessage( 'Caching Image...' );
  ImageProcCacheUpdating := true;
  fMain.SetOCRMenuOptions( false );
end;

procedure TI2XMainUIController.OnImageCacheStatusChange(Sender : TObject; TaskId, sStatusMessage: string);
begin
  if ( Integer(DebugLevel) >= 2 ) then StatusMessage( 'IMAGE CACHE: ' + sStatusMessage  );
end;

procedure TI2XMainUIController.OnImageProcJobEndCache( Sender : TObject; TaskID : string; InstructionsCompleted: integer; ImageMMID : string );
var
  sImageCacheOnDisk : string;
  sImageCacheOnDiskInst : string;
  DPIX : integer;
  img : TDWImage;
begin
  try
    if ( FImageProcCache <> nil ) then
      FImageProcCache.Free;
    FImageProcCache := TDWImage.Create();
    dbg( 'OnImageProcJobEndCache(): FMemoryMapManager.OpenMaps ' + FMemoryMapManager.OpenMapsAsString() );
    FMemoryMapManager.DebugMessageText := 'At OnImageProcJobEndCache.';
    self.FMemoryMapManager.Read( ImageMMID, FImageProcCache );
    //FImageProcCache.LoadFromMemoryMap( ImageMMID );
    self.FImageProcCacheMMID := ImageMMID;

    sImageCacheOnDisk := AppTempDir + IMGPROC_CACHE;
    sImageCacheOnDiskInst := fMain.ActionListAsString( fMain.getSelectedActionList() );
    if ( fMain.ActiveImageIsLoaded ) then begin
      DPIX := 0;
      TryStrToInt( fMain.template.getData( 'x_dpi' ), DPIX );
      if ( DPIX <> 0 ) then FImageProcCache.setDPI( DPIX );
    end;
    FImageProcCache.SaveToFile( sImageCacheOnDisk, true );
    fMain.Reg['ImageCacheOnDisk'] := sImageCacheOnDisk;
    fMain.Reg['ImageCacheOnDiskInst'] := sImageCacheOnDiskInst;

    if ( Integer(DebugLevel) >= 2 ) then fMain.StatusMessage( 'Image Cached', pgDisable );
    if ( Integer(DebugLevel) <= 1 ) then fMain.StatusMessage( 'Image Processed', pgDisable );
    ImageProcCacheUpdating := false;

    try
      img := TDWImage.Create();
      FImageProcCache.CopyTo( img );
      undo.Add( ImgView );
      self.ImgView.Bitmap.Assign( img.BitMap );
      fMain.txtActiveImage.Text := self.FImageProcCache.FileName;
    finally
      FreeAndNil( img );
    end;

    fMain.SetOCRMenuOptions( true );
  finally
  end;
End;

procedure TI2XMainUIController.OnImageProcJobEnd( Sender : TObject; TaskID : string; InstructionsCompleted: integer; ImageMMID : string );
var
  img : TDWImage;
  bCloseMapAfterRead : boolean;
begin
    try
      img := TDWImage.Create();
      bCloseMapAfterRead := not self.ImageProcJobThread.TriggerOCRAfter;
      dbg( 'OnImageProcJobEnd(): FMemoryMapManager.OpenMaps ' + FMemoryMapManager.OpenMapsAsString() );
      FMemoryMapManager.DebugMessageText := 'At OnImageProcJobEnd. bCloseMapAfterRead=' + BoolToStr(bCloseMapAfterRead);
      Self.FMemoryMapManager.Read( ImageMMID, img, bCloseMapAfterRead );
      undo.Add( ImgView );
      self.ImgView.Bitmap.Assign( img.BitMap );
    finally
      FreeAndNil( img );
    end;
    if ( not self.ImageProcJobThread.TriggerOCRAfter ) then begin
      fMain.Cursor := fMain.FCursorSave;
      fMain.SetOCRMenuOptions( true );

      fMain.StatusMessage( 'Image Processing Completed', pgDisable );
    end else begin
      fMain.StatusMessage( 'Image has been Processed.. beginning OCR', pgIgnore, 'Performing OCR' );
      Self.OCRImage(ImageMMID, fMain.template, otNormal, fMain.txtActiveImage.Text  );
    end;
end;

procedure TI2XMainUIController.OnImageProcJobStart(Sender : TObject; TaskID : string; InstructionsCount: integer);
begin
  fMain.SetOCRMenuOptions( false );
  fMain.FCursorSave := fMain.Cursor;
  fMain.Cursor := crHourGlass;
  fMain.StatusMessage( 'Begin Image Processing...', pgStart, 'Processing Image' );
  ShowCursor(TRUE);
  Application.ProcessMessages;
end;

procedure TI2XMainUIController.OnJobCacheError(Sender : TObject; TaskId: string; ErrorNo: integer;
  ErrorString: string);
begin
  Dbg('The Image could not be cached... ignoring image cache request');
end;

procedure TI2XMainUIController.OnJobError(Sender : TObject; TaskId : string; ErrorNo: integer;
  ErrorString: string);
var
  sMMID : string;
begin
  fMain.ErrorDisplay( '', ErrorString );
  fMain.StatusMessage( ErrorString, pgDisable );
  if ( TryObtainMMID( Sender, sMMID ) ) then begin
    FMemoryMapManager.DebugMessageText := 'At OnJobError.';
    FMemoryMapManager.Delete( sMMID );
  end;
end;

procedure TI2XMainUIController.OnThreadTerminate(Sender: TObject);
begin
  dbg('OnThreadTerminate');
end;

function TI2XMainUIController.TryObtainMMID(PossibleMMIDObject : TObject; var MMID : string ) : boolean;
var
  thd : TI2XThreadedImageJob;
Begin
  Result := false;
  MMID := '';
  try
    thd := TI2XThreadedImageJob( PossibleMMIDObject );
    MMID := thd.ImageMemoryMapID;
    Result := true;
  except
    Result := false;
  end;
End;

procedure TI2XMainUIController.OnOCRJobEnd( Sender : TObject; TaskID : string; TestsPerformed: integer; ImageMMID : string );
var
  sMMID : string;
begin
  fMain.StatusMessage( 'Image Processing Completed', pgDisable );
  if ( TryObtainMMID( Sender, sMMID ) ) then begin
    FMemoryMapManager.DebugMessageText := 'At OnOCRJobEnd.';
    FMemoryMapManager.Delete( sMMID );
  end;
end;

procedure TI2XMainUIController.OnOCRJobStart(Sender : TObject; TaskID : string; TestsCount: integer);
begin
  fMain.SetOCRMenuOptions( false );
  fMain.FCursorSave := fMain.Cursor;
  fMain.Cursor := crHourGlass;
  fMain.StatusMessage( 'Begining OCR', pgStart, 'Performing OCR' );
  ShowCursor(TRUE);
  Application.ProcessMessages;
end;

procedure TI2XMainUIController.OnOCRSelectionReturnXML(Sender : TObject; TaskId: string;
  ReturnXML: string);
Begin
    fMain.Cursor := fMain.FCursorSave;
    fMain.SetOCRMenuOptions( true );

    if ( fMain.dlgOCRProductView <> nil ) then
      fMain.dlgOCRProductView.free;
    fMain.dlgOCRProductView := TOCRProductViewDialog.Create( fMain );
    fMain.dlgOCRProductView.DisplayXML( ReturnXML );
    //fMain.StatusMessage( 'OCR of Selection Completed', pgDisable );
End;

procedure TI2XMainUIController.OnOCRSelReturnXMLForFirstChar(Sender : TObject; TaskId: string;
  ReturnXML: string);
Begin
    fMain.Cursor := fMain.FCursorSave;
    fMain.SetOCRMenuOptions( true );

    if ( fMain.dlgOCRProductView <> nil ) then
      fMain.dlgOCRProductView.free;
    fMain.dlgOCRProductView := TOCRProductViewDialog.Create( fMain );
    fMain.dlgOCRProductView.DisplayXML( ReturnXML );
End;

procedure TI2XMainUIController.OnOCRSelectionJobEnd(Sender : TObject; TaskId: string;
  TestsPerformed: integer; ImageMMID: string);
var
  sMMID : string;
begin
  fMain.StatusMessage( 'Begin OCR of Selected Region', pgDisable );
  if ( TryObtainMMID( Sender, sMMID ) ) then begin
    FMemoryMapManager.DebugMessageText := 'At OnOCRSelectionJobEnd.';
    FMemoryMapManager.Delete( sMMID );
  end;
end;

procedure TI2XMainUIController.OnOCRSelectionJobStart(Sender : TObject; TaskId: string;
  TestsCount: integer);
begin
  fMain.SetOCRMenuOptions( false );
  fMain.FCursorSave := fMain.Cursor;
  fMain.Cursor := crHourGlass;
  StatusMessage( 'Begin OCR of Selected Region' );
  ShowCursor(TRUE);
  Application.ProcessMessages;
  ImageProcCacheInst := fMain.ActionListAsString( fMain.SelectedActionList );
end;

procedure TI2XMainUIController.OnProductJobEnd(Sender : TObject; TaskId: string; TestsPerformed: integer;
  ReturnXML: string);
var
  sMMID : string;
begin
  try
    fMain.Cursor := fMain.FCursorSave;
    fMain.SetOCRMenuOptions( true );

    if ( TryObtainMMID( Sender, sMMID ) ) then begin
      FMemoryMapManager.DebugMessageText := 'At OnProductJobEnd.';
      FMemoryMapManager.Delete( sMMID );
    end;

    if ( Integer(DebugLevel) > 1 ) then WriteLog( ReturnXML, 'C:\Dark\pascal\Image2XML\templates\Result.xml' );
    fMain.StatusMessage( 'Image Processing Completed', pgDisable );
  finally

  end;
end;

Procedure TI2XMainUIController.OnProductJobStart(Sender : TObject; TaskId: string; TestsCount: integer);
Begin
  fMain.SetOCRMenuOptions( false );
  fMain.FCursorSave := fMain.Cursor;
  fMain.Cursor := crHourGlass;
  fMain.StatusMessage( 'Begin Image Processing and OCR of image...', pgStart, 'Image Processing and OCR' );
  ShowCursor(TRUE);
  Application.ProcessMessages;
End;

procedure TI2XMainUIController.OnReturnXML(Sender: TObject; TaskId,
  ReturnXML: string);
begin
    fMain.Cursor := fMain.FCursorSave;
    fMain.SetOCRMenuOptions( true );

    if ( fMain.dlgOCRProductView <> nil ) then
      fMain.dlgOCRProductView.free;
    fMain.dlgOCRProductView := TOCRProductViewDialog.Create( fMain );
    fMain.dlgOCRProductView.DisplayXML( ReturnXML );
end;

Procedure TI2XMainUIController.OnStatusChange(Sender : TObject; TaskId, sStatusMessage: string);
Begin
  if ( Assigned( FOnStatusChange ) ) then
    FOnStatusChange( Sender, TaskId, sStatusMessage );
End;

procedure TI2XMainUIController.CacheProcessedImage(bmp: TBitMap32);
begin
  if ( DWImgView <> nil )  then
    DWImgView.Free;
  DWImgView := TDWImage.Create( bmp );
  self.CacheProcessedImage( DWImgView );
end;

Constructor TI2XMainUIController.Create(BaseForm: TForm);
Begin
  self.FBaseForm := BaseForm;
  fMain := TI2XBaseUI(BaseForm);
  FDLLOCRTestsForImageAttrib := TDLLOCRTests.Create();
End;

procedure TI2XMainUIController.DebugMessage(sDebugMessage: string);
begin
  self.OnDebugMessage( Self, '', sDebugMessage );
end;

destructor TI2XMainUIController.Destroy;
Begin
  if ( FImageProcCache <> nil ) then FreeAndNil( FImageProcCache );
  if ( FDLLOCRTestsForImageAttrib <> nil ) then FreeAndNil( FDLLOCRTestsForImageAttrib );

  //The following should have been freed when it ran
  //if ( ImageProcJobThread <> nil ) then FreeAndNil( ImageProcJobThread );
  //if ( OCRJobThread <> nil ) then FreeAndNil( OCRJobThread );
  //if ( ProductJobThread <> nil ) then FreeAndNil( ProductJobThread );

  //if ( ImageProcCacheThread <> nil ) then FreeAndNil( ImageProcCacheThread );
  //if ( FDLLOCRTests <> nil ) then FreeAndNil( FDLLOCRTests );
  if ( DWImgView <> nil ) then FreeAndNil( DWImgView );

End;

END.
