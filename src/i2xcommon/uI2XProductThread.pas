unit uI2XProductThread;

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
  Typinfo,
  uI2XImageProcJobThread,
  uI2XOCRProcThread,
  uI2XProduct
  ;

type
  TI2XProductThread = class(TI2XImageProcJobThread)
  private                              
      FTemplate : CTemplate;
      FTests : TDLLOCRTests;
      FTestsCreatedInternally : boolean;
      FResultXML : string;
  protected
    procedure Execute; override;
    procedure SetOCREngineTests( OCREngineTests : TDLLOCRTests );
  public
    property Template : CTemplate read FTemplate write FTemplate;
    property OCREngineTests : TDLLOCRTests read FTests write SetOCREngineTests;
    constructor Create(); overload;
    constructor Create( bmp32: TBitmap32 ); overload;
    destructor Destroy; override;

  end;

implementation

{ TI2XProductThread }

procedure TI2XProductThread.Execute;
var
  OCR : TI2XOCRProcJobThread;
Begin
  self.EnableJobEvents := false; //We do not job events enabled on the sub class..
  inherited;
  try
    self.OnJobStart( self.InstructionList.Count );
    OCR := TI2XOCRProcJobThread.Create;
    OCR.DebugLevel := self.DebugLevel;
    OCR.DebugPath := self.DebugPath;
    OCR.ImageMemoryMapID := self.ImageMemoryMapID;
    OCR.Template := self.Template;
    OCR.OCREngineTests := self.OCREngineTests;
    OCR.OnStatusChangeEvent := self.OnStatusChangeEvent;
    OCR.EnableJobEvents := EnableJobEvents;
    OCR.ExecuteOCR();
    self.OnReturnXML( OCR.ResultXML );
    self.OnJobEnd( OCR.TestCount, OCR.ImageMemoryMapID );
  finally
    FreeAndNil( OCR );
  end;
End;

procedure TI2XProductThread.SetOCREngineTests( OCREngineTests : TDLLOCRTests );
begin
//If we set FTests externally,  since we have already allocated it internally,
//  it will cause a memory leak if we do not destroy the old instance.
  if ( FTests <> nil ) then
    FTests.Free;
  FTestsCreatedInternally := false;
  FTests := OCREngineTests;
end;

constructor TI2XProductThread.Create;
begin
  inherited Create();
  FTests := TDLLOCRTests.Create();
  FTestsCreatedInternally := true;
end;

constructor TI2XProductThread.Create(bmp32: TBitmap32);
begin
  inherited Create( bmp32 );
  FTests := TDLLOCRTests.Create();
  FTestsCreatedInternally := true;
end;

destructor TI2XProductThread.Destroy;
begin
  if ( FTestsCreatedInternally ) then FreeAndNil( FTests );
  inherited;
end;


END.
