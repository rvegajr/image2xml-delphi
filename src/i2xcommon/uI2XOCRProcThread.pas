unit uI2XOCRProcThread;

interface
uses
  SysUtils,
  Classes,
  uI2XDLL,
  uDWImage,
  GR32,
  uI2XThreadBase,
  uStrUtil,
  uHashTable,
  uI2XConstants,
  uImage2XML,
  uI2XTemplate,
  uI2XOCR,
  uI2XProduct,
  Typinfo,
  uI2XPluginManager,
  uI2XMemMap;

type
  TOnImageAttributesCalculated = procedure( Sender : TObject; const Boundary : TRect; const FirstCharacter : TI2XOCRItem ) of object;
  TI2XOCRProcJobThread = class(TI2XThreadedImageJob)
  private
    FOnImageAttributesCalculated : TOnImageAttributesCalculated;
    FTemplateAsString : string;
    FTests : TDLLOCRTests;
    FTestsCreatedInternally : boolean;
    FResultXML : string;
    FTestCount : integer;
    FTemplate : CTemplate;
    FBoundary : TRect;
    FFirstCharacter : TI2XOCRItem;
    FFirstCharacterAsXML : string; //thread friend object representation of the first character
    FOffsetCorrection : boolean; //turn off Offset Correcton here
    function getTemplate: CTemplate;
    procedure setTemplate(const Value: CTemplate);
  protected
    procedure Execute; override;
    procedure SetOCREngineTests( OCREngineTests : TDLLOCRTests );
    procedure OnImageAttributesCalculated( const Boundary : TRect; const FirstCharacter : TI2XOCRItem );
    procedure DoImageAttributesCalculated(); virtual;
  public
    function ExecuteOCR() : boolean;
    property Template : CTemplate read getTemplate write setTemplate;
    property OCREngineTests : TDLLOCRTests read FTests write SetOCREngineTests;
    property TestCount : integer read FTestCount write FTestCount;
    property ResultXML : string read FResultXML write FResultXML;
    property EnableOffsetCorrection : boolean read FOffsetCorrection write FOffsetCorrection;
    property OnImageAttributesCalculatedEvent : TOnImageAttributesCalculated
      read FOnImageAttributesCalculated write FOnImageAttributesCalculated;
    constructor Create(); overload;
    constructor Create( bmp32: TBitmap32 ); overload;
    destructor Destroy; override;

  end;

implementation

{ TI2XImageProcJobThread }

constructor TI2XOCRProcJobThread.Create;
begin
  inherited Create();
  FTests := TDLLOCRTests.Create();
  FResultXML := '';
  FFirstCharacter := TI2XOCRItem.Create();
  FOffsetCorrection := true;
end;

constructor TI2XOCRProcJobThread.Create(bmp32: TBitmap32);
begin
  inherited Create( bmp32 );
  FTests := TDLLOCRTests.Create();
  FFirstCharacter := TI2XOCRItem.Create();
  FResultXML := '';
  FOffsetCorrection := true;
end;

destructor TI2XOCRProcJobThread.Destroy;
begin
  if ( FTestsCreatedInternally )  then
    FreeAndNil( FTests );
  if ( FTemplate <> nil ) then
    FreeAndNil( FTemplate );
  if ( FFirstCharacter <> nil ) then
    FreeAndNil( FFirstCharacter );
  inherited;
end;

procedure TI2XOCRProcJobThread.DoImageAttributesCalculated;
begin
  if ( Assigned( self.FOnImageAttributesCalculated )) then begin
    self.FFirstCharacter.FromXML( self.FFirstCharacterAsXML );
    FOnImageAttributesCalculated( self, self.FBoundary, self.FFirstCharacter );
  end;
end;

procedure TI2XOCRProcJobThread.Execute;
Begin
  ExecuteOCR();
End;

function TI2XOCRProcJobThread.ExecuteOCR() : boolean;
var
  DLLOCREngine : TDLLOCREngine;
  iOCRDLLEntry, iOCREngine, iTest,
      iProductUnitCount, TotalTestCount : smallint;
  sOCRDataMMID : string;
  OCREngines : THashTable;
  ocrdata : TI2XOCRResults;
  ocrDataList : array of TI2XOCRResults;
  productsList : array of TI2XProductItems;
  productFinal : TI2XProductItems;
  bInvert : boolean;
  productcompare : TI2XProductItemCompare;
  oDLLTest : TDLLOCRTestEntry;
  oTest : TDLLOCRTest;
  Plugins : TI2XPluginManager;
  ThreadMemoryMapManager : TI2XMemoryMapManager;
  oTemplate : CTemplate;
  sAction : string;
  BoundaryTest: TPoint;
  OCRItemTest: TI2XOCRItem;
  BestAccuracy : single;
  //this flag is important because if we do not have test of a whole page,  it is difficult
  // to determine the offset of the scanned page.  We must perform 1 full page test to determine
  // if there is some offset
  bWholeImageTest, bOffsetTestPerformed : boolean;
  arrTestEntriesSortedByWholeTests: TIndexValueArray;
  Offset : TPoint;
Begin
  try
    //OCRItemTest := TI2XOCRItem.Create();
    Initialize( Offset );
    OCRItemTest := nil;
    bWholeImageTest := false;
    bOffsetTestPerformed := false;
    BestAccuracy := 0;
    ThreadMemoryMapManager := TI2XMemoryMapManager.Create;
    oTemplate := CTemplate.Create();  //life is always much easier if we create this object in the same thread
    oTemplate.LoadFromXML( self.FTemplateAsString );
    FTestCount := 0;
    Result := false;

    if ( self.FCreatePluginMananger ) then begin
      Plugins := TI2XPluginManager.Create( DLLSearchPath );
      if ( Plugins.SearchAndLoadOCRPlugins = 0 ) then
        raise Exception.Create('OCR Processing DLLS were not found in search path ' + DLLSearchPath );
    end else
      Plugins := self.PluginManager;

    //Load Image Plugins
    OCREngines :=  THashTable.Create;
    ocrdata := TI2XOCRResults.Create;

    if self.EnableJobEvents then OnJobStart( FTests.TestCount );
    if ( FTests.TestCount = 0 ) then begin
      OnStatusChange('There were no tests to perform.');
      OnJobEnd( 0, '' );
    end else begin
      try
        if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'Creating TI2XProductItemCompare' );

        productcompare := TI2XProductItemCompare.Create( oTemplate );

        if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'Initializing Variables' );
        //TotalTestCount := FTests.TestCount;
        TotalTestCount := 0;

        if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'Calculating how many test we have to process' );
        // figure out how many tests we have to process
        for iOCRDLLEntry := 0 to self.FTests.Count - 1 do begin
          oDLLTest := FTests.Items[ iOCRDLLEntry ];
          if ( oDLLTest.Enabled ) then begin
            arrTestEntriesSortedByWholeTests := oDLLTest.IndexByWholeTests;
            for iTest := 0 to oDLLTest.Count - 1 do begin
              //oTest := oDLLTest[ iTest ];
              oTest := TDLLOCRTest( arrTestEntriesSortedByWholeTests[ iTest ].obj );
              if ( not bWholeImageTest ) then
                bWholeImageTest := oTest.IsWholePageTest;
              if ( oTest.Enabled ) then begin
                Inc( TotalTestCount );
              end;
            end;
            //if a whole image test was not found and the template has document offset
            // correction set.
            if ( ( not bWholeImageTest) and
                 ( oTemplate.DocumentOffsetCorrectionType <> ocNone ) ) then begin
              oDLLTest.Items[0].Enabled := true;
            end;

          end;
        end;


        if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'Starting Job...' );
        OnJobStart( TotalTestCount );
        SetLength(ocrDataList, TotalTestCount );
        SetLength(productsList,  TotalTestCount);

        for iOCRDLLEntry := 0 to self.FTests.Count - 1 do begin
          oDLLTest := FTests.Items[ iOCRDLLEntry ];
          if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'Accessing DLL ' + oDLLTest.Name );
          if ( not oDLLTest.Enabled ) then begin
            if ( Integer( self.DebugLevel ) >= 1 ) then begin
              OnStatusChange('The DLL (' + oDLLTest.Name + ') is disabled.');
              if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'The DLL (' + oDLLTest.Name + ') is disabled.' );
            end;
          end else begin
            DLLOCREngine := Plugins.OCRDLLByName( oDLLTest.Name );
            if ( DLLOCREngine = nil ) then
              raise Exception.Create( 'DLL Test Entry of "' + oDLLTest.Name + '" does not have a corresponding DLL Engine' );
            //DLLOCREngine := TDLLOCREngine( OCREngines[ oDLLTest.Name ] );

            arrTestEntriesSortedByWholeTests := oDLLTest.IndexByWholeTests;
            for iTest := 0 to oDLLTest.Count - 1 do begin

              oTest := TDLLOCRTest( arrTestEntriesSortedByWholeTests[ iTest ].obj );

              //oTest := oDLLTest[ iTest ];
              self.LastDebugMessage := 'Accessing test ' + oTest.Name + ' of DLL ' + oDLLTest.Name + ' ( Enabled=' + uStrUtil.BoolToStr(oTest.Enabled, 'Yes', 'No') + ', Sliced=' + uStrUtil.BoolToStr(oTest.Slice, 'Yes', 'No') + ', Inverted=' + uStrUtil.BoolToStr(oTest.Invert, 'Yes', 'No') + ' )' ;
              if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );


              if ( oTest.Enabled ) then begin
                ocrDataList[FTestCount] := TI2XOCRResults.Create;

                // if we try to slice and image but no offset test has been performed AND
                //  document offset correction is on,  then we raise and error because
                //  we missed a step somehwere
                // because we are using the template to fill with slices,  it becomes
                // important to set the offset before we call this.
                // this value is calculated using an OCr from a whole image
                if ( oTest.Slice ) then begin
                  self.LastDebugMessage := 'Slicing Images' ;
                  if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                  if ( FOffsetCorrection ) then begin
                    if ( oTemplate.DocumentOffsetCorrectionType <> ocNone ) then begin
                      if ( bOffsetTestPerformed ) then begin
                        if (( Offset.X <> 0 ) or ( Offset.Y <> 0 )) then begin
                          oTemplate.Offset( Offset.X, Offset.Y );
                        end;
                      end else begin
                        raise Exception.Create( 'If the template specifies document offset correction,  then we cannot slice image without first testing to see if the document needs to be offset.' )
                      end;
                    end;
                  end;
                  oTemplate.FillWithRegionSlices( ocrDataList[FTestCount] );
                  if ( oTemplate.IsOffset ) then
                    oTemplate.Offset( 0, 0 );
                end;

                try
                  if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( ' Saving OCR Data to Memory Map' );
                  ThreadMemoryMapManager.Write( sOCRDataMMID, ocrDataList[ FTestCount ] );
                  //ocrDataList[ FTestCount ].SaveToMemoryMap( sOCRDataMMID );
                except
                  on e : Exception do begin
                    self.LastDebugMessage := 'Error while trying to save OCR data to memory map. Exception Error:' + e.Message;
                    if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                    raise Exception.Create( self.LastDebugMessage );
                  end;
                end;
                try
                  if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'Performing OCR using Image MMID=' + self.ImageMemoryMapID + ' and Data MMID=' + sOCRDataMMID + ' Invert=' + uStrUtil.BoolToStr(oTest.Invert, 'Yes', 'No') );
                  DLLOCREngine.OCRImage( self.ImageMemoryMapID, sOCRDataMMID, oTest.Invert );
                except
                  on e : Exception do begin
                    self.LastDebugMessage := 'Error while calling OCR Engine.' + e.Message;
                    if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                    raise Exception.Create( self.LastDebugMessage );
                  end;
                end;
                if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'OCR Complete... Load MemoryMap ' + sOCRDataMMID );

                try
                  ThreadMemoryMapManager.Read( sOCRDataMMID, ocrDataList[ FTestCount ] );
                  //ocrDataList[ FTestCount ].LoadFromMemoryMap( sOCRDataMMID );
                except
                  self.LastDebugMessage := 'Error while Loading from Memory Map.';
                  if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                  raise Exception.Create( self.LastDebugMessage );
                end;
                if ( Integer( self.DebugLevel ) >= 2 ) then
                  ocrDataList[FTestCount].SaveToFile( self.DebugPath + 'OCRDataDump_' + oTest.Name + '.xml' );

                try
                  OCRItemTest := nil;
                  if ( ocrDataList[FTestCount].Count > 0 ) then begin
                    OCRItemTest := ocrDataList[FTestCount].FirstCharacter;
                    if (( OCRItemTest <> nil ) and ( BestAccuracy < OCRItemTest.Accuracy )) then begin
                      FFirstCharacter.Assign( OCRItemTest );
                      FBoundary := ocrDataList[FTestCount].Boundary;
                      BestAccuracy := FFirstCharacter.Accuracy;
                    end;
                  end;
                  if ( FOffsetCorrection ) then begin
                    if ( ( not bOffsetTestPerformed ) and
                         ( oTemplate.DocumentOffsetCorrectionType <> ocNone ) and
                         ( oTest.IsWholePageTest )
                       ) then begin
                      self.LastDebugMessage := 'Performing Offset test...' ;
                      if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                      OffsetCorrection( oTemplate, ocrDataList[FTestCount], Offset );
                      self.LastDebugMessage := '.. Rest returned offset of  X=' + IntToStr(Offset.X) + ' Y=' + IntToStr(Offset.Y) ;
                      bOffsetTestPerformed := true;
                    end;
                  end;

                  //end;

                  self.LastDebugMessage := 'Creating Product Items for test ' + Alphabet[iTest + 1] + '. ' ;
                  if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                  productsList[ FTestCount ] := TI2XProductItems.Create( Alphabet[iTest + 1] );

                  //if this is a whole page and document offset correction is set
                  // and we have an offset test performed,  then we do the page offset
                  // We do not do this for slices because the offset it already calculated when
                  //  we slice up the image
                  if (( FOffsetCorrection ) and
                      ( oTest.IsWholePageTest ) and
                      ( oTemplate.DocumentOffsetCorrectionType <> ocNone ) and
                      ( bOffsetTestPerformed )) then begin
                    self.LastDebugMessage := 'Consuming OCR Results for test ' + Alphabet[(iTest + 1)] + ' with offset of X=' + IntToStr(Offset.X) + ' Y=' + IntToStr(Offset.Y);
                    if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                    oTemplate.Offset( Offset.X, Offset.Y );
                    productsList[ FTestCount ].ConsumeOCRResults( oTemplate, ocrDataList[FTestCount] );
                    if ( oTemplate.IsOffset ) then
                      oTemplate.Offset( 0, 0 );
                  end else begin
                    self.LastDebugMessage := 'Consuming OCR Results for test ' + Alphabet[iTest + 1] + ' with no offset';
                    if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                    productsList[ FTestCount ].ConsumeOCRResults( oTemplate, ocrDataList[FTestCount] );
                  end;

                  self.LastDebugMessage := 'Possible write out... ';
                  if ( Integer( self.DebugLevel ) >= 2 ) then
                    productsList[ FTestCount ].SaveToFile( self.DebugPath + 'OCRDataFormatted_' + oTest.Name + '.xml' );
                except
                  on e : Exception do begin
                    self.LastDebugMessage := self.LastDebugMessage + ' Error while Consuming OCR Results.  Exception Error:' + e.Message;
                    if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( self.LastDebugMessage );
                    raise Exception.Create( self.LastDebugMessage );
                  end;
                end;

                productcompare.Add( productsList[FTestCount] );
                OnStatusChange('OCR is ' + IntToStr( Round((FTestCount / ( TotalTestCount + 1 )) * 100)) + '% Complete.');
                ;
                Inc( FTestCount );
              end;
            end; //for
          end; //if DLLTest enabled
        end;
        if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'Comparing Results' );


        if self.EnableJobEvents then self.OnImageAttributesCalculated( FBoundary, self.FFirstCharacter );
        
        productFinal := TI2XProductItems.Create( 'RESULTS' );
        productcompare.CompareRecusively( productFinal );
        productFinal.SaveToFile( self.DebugPath + 'OCRData_RESULT.xml' );
        productFinal.SourceImageFileName := ImageFileName;
        FResultXML := productFinal.AsXML();

      except
        on E : Exception do begin
          if ( integer( self.DebugLevel ) >= 2 ) then OnDebugMessage( 'ERROR: ' + E.ClassName + ': ' + E.Message + #13 + 'ERROR: Last Debug Message=' + self.LastDebugMessage + #13 + 'ERROR: Last Status Message=' + self.LastStatusMessage );
          self.OnJobError( ERROR_OCR_THREAD_FAILED, E.ClassName + ': ' + E.Message + ' ' + self.LastStatusMessage );
        end;
      end;

      OnStatusChange( 'Image processing completed');
      if self.EnableJobEvents then OnReturnXML( FResultXML );
      if self.EnableJobEvents then OnJobEnd( FTestCount, self.ImageMemoryMapID );
    end;
    Result := true;
  finally
    for iTest := 0 to Length( ocrDataList ) - 1 do
      if ( ocrDataList[iTest] <> nil ) then ocrDataList[iTest].Free;
    for iTest := 0 to Length( productsList ) - 1 do
      if ( productsList[iTest] <> nil ) then productsList[iTest].Free;
      //productsList[iTest].Free;

    if ( productcompare <> nil ) then FreeAndNil( productcompare );
    if ( productFinal <> nil ) then FreeAndNil( productFinal );

    if ( OCREngines <> nil ) then FreeAndNil( OCREngines );
    if ( ocrdata <> nil ) then FreeAndNil( ocrdata );
    if ( oTemplate <> nil ) then FreeAndNil( oTemplate );
    if ( FCreatePluginMananger ) then FreeAndNil( Plugins );
    if ( ThreadMemoryMapManager <> nil ) then FreeAndNil( ThreadMemoryMapManager );
    //if ( OCRItemTest <> nil ) then FreeAndNil( OCRItemTest );
  end;
End;

Function TI2XOCRProcJobThread.getTemplate: CTemplate;
Begin
  if ( FTemplate <> nil ) then
    FreeAndNil( FTemplate );
  FTemplate := CTemplate.Create();
  FTemplate.LoadFromXML( self.FTemplateAsString );
  result := FTemplate;
End;

procedure TI2XOCRProcJobThread.OnImageAttributesCalculated( const Boundary : TRect;
const FirstCharacter : TI2XOCRItem );
begin
  self.FBoundary := Boundary;
  self.FFirstCharacterAsXML := FirstCharacter.AsXML();
  self.Synchronize( self.DoImageAttributesCalculated );
end;

Procedure TI2XOCRProcJobThread.SetOCREngineTests(OCREngineTests: TDLLOCRTests);
Begin
//If we set FTests externally,  since we have already allocated it internally,
//  it will cause a memory leak if we do not destroy the old instance.
  if ( FTests <> nil ) then
    FTests.Free;
  FTestsCreatedInternally := false;
  FTests := OCREngineTests;
End;

procedure TI2XOCRProcJobThread.setTemplate(const Value: CTemplate);
Begin
  self.FTemplateAsString := Value.asXML( xcComplete );
End;

END.
