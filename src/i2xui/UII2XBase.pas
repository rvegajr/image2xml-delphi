unit UII2XBase;

interface

uses
  hh_funcs,
  hh,
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
  uI2XMainController,
  UIOCRProductViewDialog,
  UII2XOptions,
  uI2XOptions,
  uI2XPluginManager,
  uI2XScan,
  mcmTWAINKernel,
  mcmTWAINIntf,
  mcmTWAIN,
  uI2XJob,
  uAppCache,
  uI2XMemMap,
  ProgressDialog,
  AppEvnts,
  UII2XDimmer,
  UII2XDialog,
  uI2XUndo,
  UII2XAbout,
  uI2XDWImageUtils
  ;

type
  TI2XBaseUI = class(TForm)
    pmnuActionList: TPopupMenu;
    pnlActiveImage: TPanel;
    lblActiveImage: TLabel;
    txtActiveImage: TEdit;
    bbtnOpenImage: TBitBtn;
    pnlImageView: TPanel;
    ImgView: TImgView32;  //* .tag property used
    {ImgView.tag property used to flag if Layer recieved mouse event before
      ImgView control.  When ImgView gets the event,  it will always set it to 0.
      If a Layer recieves a mouse down,  it will set it to 1. }
    pgctlTemplateData: TPageControl; // Update TYPE TI2XPageCtl when adding and deleting
    tabInfo: TTabSheet;
    pnlTemplateInfo: TPanel;
    lblTemplateName: TLabel;
    lblImageFileName: TLabel;
    lblDescription: TLabel;
    btnTemplateImageFileSet: TSpeedButton;
    txtImageFileName: TEdit;
    txtTemplateName: TEdit;
    txtTemplateDescription: TMemo;
    tabRegions: TTabSheet;
    pnlRegionsEdit: TPanel;
    lblX: TLabel;
    lblY: TLabel;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblName: TLabel;
    lblDataType: TLabel;
    lblGroupKeyOrder0: TLabel;
    lblElementKeyOrder0: TLabel;
    lblGroupKeyOrder: TLabel;
    lblElementKeyOrder: TLabel;
    txtX: TEdit;
    txtY: TEdit;
    txtWidth: TEdit;
    txtHeight: TEdit;
    txtId: TEdit;
    cboDataType: TComboBox;
    tvRegions: TTreeView;
    tabImage: TTabSheet;
    pnlImageInst: TPanel;
    pnlLabelImgProcInst: TPanel;
    pgctlImageProcessing: TPageControl;
    tabImgProcNormal: TTabSheet;
    tabImgProcUpsize: TTabSheet;
    gridActionList: TStringGrid;
    gridActionListUpsize: TStringGrid;
    tabOffset: TTabSheet;
    btnSearchFirstChar: TSpeedButton;
    btnFindXYBoundaries: TSpeedButton;
    rbtnFirstChar: TRadioButton;
    rbtnXYBoundary: TRadioButton;
    pnlFirstCharData: TPanel;
    lblFirstCharX: TLabel;
    lblFirstCharY: TLabel;
    lblFirstCharWidth: TLabel;
    lblFirstCharHeight: TLabel;
    txtFirstCharX: TEdit;
    txtFirstCharY: TEdit;
    txtFirstCharWidth: TEdit;
    txtFirstCharHeight: TEdit;
    pnlXYBoundary: TPanel;
    lblXBound: TLabel;
    lblYBound: TLabel;
    txtXBound: TEdit;
    txtYBound: TEdit;
    rbtnNoDocCorrection: TRadioButton;
    tabJob: TTabSheet;
    lblImagePath: TLabel;
    lblOutputPath: TLabel;
    btnBrowserForOutputFolder: TSpeedButton;
    btnSetImagePath: TSpeedButton;
    btnOCRJobExecute: TButton;
    txtJobImagePath: TEdit;
    txtJobOutput: TEdit;
    bStatus: TStatusBar;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acOpen: TAction;
    acOpenImage: TAction;
    acSave: TAction;
    acUndo: TAction;
    acSaveAs: TAction;
    acOpenTempPath: TAction;
    acOpenTemplateAsXML: TAction;
    acSelectDocCorrect: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    acOpenImage1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    N3: TMenuItem;
    OpenTempPath1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    mnuEdit: TMenuItem;
    mnuTemplate: TMenuItem;
    OpenAsXML1: TMenuItem;
    help1: TMenuItem;
    Index1: TMenuItem;
    About1: TMenuItem;
    gbZoom: TGaugeBar;
    lblZoom: TLabel;
    acToggleGridSnap: TAction;
    pmnuRegions: TPopupMenu;
    pmnuRegionCreate: TMenuItem;
    acCreateRegion: TAction;
    acRegionLevelUp: TAction;
    pmnuRegionRemoveFromParent: TMenuItem;
    N2: TMenuItem;
    acDeleteRegion: TAction;
    pmnuRegionDelete: TMenuItem;
    acNew: TAction;
    New1: TMenuItem;
    bbtnRefresh: TBitBtn;
    acRefreshImage: TAction;
    acRegionCopyRight: TAction;
    acRegionCopyLeft: TAction;
    acRegionCopyAbove: TAction;
    acRegionCopyBelow: TAction;
    pmnuRegionCopyRight: TMenuItem;
    pmnuRegionCopyLeft: TMenuItem;
    pmnuRegionCopyAbove: TMenuItem;
    pmnuRegionCopyBelow: TMenuItem;
    N4: TMenuItem;
    acEditCopy: TEditCopy;
    acEditPaste: TEditPaste;
    acEditDelete: TEditDelete;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditDelete: TMenuItem;
    memDocOffsetInst: TMemo;
    btnAddImageInst: TSpeedButton;
    btnDoImageInst: TSpeedButton;
    pnlImageInstDescr: TPanel;
    memImageInstDescr: TMemo;
    pnlText1: TPanel;
    lblText1: TLabel;
    Edit1: TEdit;
    pnlText2: TPanel;
    lblText2: TLabel;
    Edit2: TEdit;
    pnlText3: TPanel;
    lblText3: TLabel;
    Edit3: TEdit;
    pnlSlider: TPanel;
    lblSlider1: TLabel;
    lblLevel1: TLabel;
    Slider1: TGaugeBar;
    pnlCombo2: TPanel;
    lblCombo2: TLabel;
    Combo2: TComboBox;
    pnlCombo1: TPanel;
    lblCombo1: TLabel;
    Combo1: TComboBox;
    lstInstAvailable: TListBox;
    lblComboID1: TLabel;
    lblComboID2: TLabel;
    lblSliderID1: TLabel;
    lblTextID3: TLabel;
    lblTextID2: TLabel;
    lblTextID1: TLabel;
    prgbarStatus: TProgressBar;
    TimerStatusBar: TTimer;
    acOCR: TAction;
    mnuTemplateOCR: TMenuItem;
    pmnuExecuteActionList: TMenuItem;
    N5: TMenuItem;
    pmnuDeleteInstruction: TMenuItem;
    pmnuClearAllInstructions: TMenuItem;
    pmnuProcInstAndOCR: TMenuItem;
    N6: TMenuItem;
    acOCRRegion: TAction;
    pmnuOCRRegion: TMenuItem;
    acOptions: TAction;
    mnuTools: TMenuItem;
    mnuOptions: TMenuItem;
    pmnuRegionRename: TMenuItem;
    acRegionRename: TAction;
    mnuScan: TMenuItem;
    mnuTWAINSel: TMenuItem;
    Acquire1: TMenuItem;
    acTwainSelect: TAction;
    acAcquireImages: TAction;
    mcmTWAIN1: TmcmTWAIN;
    lblJobName: TLabel;
    btnJobDelete: TSpeedButton;
    btnJobSave: TSpeedButton;
    cboJobName: TComboBox;
    lblJobTemplate: TLabel;
    btnJobTemplateSelect: TSpeedButton;
    txtJobTemplate: TEdit;
    chkRestartable: TCheckBox;
    acProcessImage: TAction;
    pmnuProcessImage: TMenuItem;
    bbtnImageProcAndOCR: TBitBtn;
    Button1: TButton;
    ApplicationEvents: TApplicationEvents;
    pmnuJobTemplateEdit: TPopupMenu;
    pmnuSetJobTemplate: TMenuItem;
    pmnuEditTemplate: TMenuItem;
    pmnuJobEdit: TPopupMenu;
    pmnuJobSave: TMenuItem;
    pmnuJobDelete: TMenuItem;
    N7: TMenuItem;
    pmnuJobExplore: TMenuItem;
    N8: TMenuItem;
    pmnuJobEditCopy: TMenuItem;
    pmnuJobEditPaste: TMenuItem;
    pmnuJobEditDelete: TMenuItem;
    N9: TMenuItem;
    pmnuJobEditCut: TMenuItem;
    pmnuJobTemplateCut: TMenuItem;
    pmnuJobTemplateCopy: TMenuItem;
    pmnuJobTemplatePaste: TMenuItem;
    pmnuJobTemplateDelete: TMenuItem;
    mnuEditCut: TMenuItem;
    N10: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuEditRedo: TMenuItem;
    mnuHelpActivationActivate: TMenuItem;
    mnuHelpActivationBuy: TMenuItem;
    mnuHelpActivationLicense: TMenuItem;
    lblPaymentDate: TLabel;
    N12: TMenuItem;
    mnuHelpActivation: TMenuItem;
    mnuSupport: TMenuItem;
    pnlImageInfo: TPanel;
    lblResolution: TLabel;
    txtDPI: TMemo;
    procedure acOpenExecute(Sender: TObject);
    procedure gbZoomChange(Sender: TObject);
    //procedure txtGridSizeChange(Sender: TObject);
    //procedure txtGridSizeKeyPress(Sender: TObject; var Key: Char);
    //procedure acToggleGridSnapExecute(Sender: TObject);
    procedure tvRegionsClick(Sender: TObject);
    procedure ImgViewClick(Sender: TObject);
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure RegionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ImgViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure tvRegionsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvRegionsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvRegionsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure acRegionLevelUpExecute(Sender: TObject);
    procedure tvRegionsCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure acCreateRegionExecute(Sender: TObject);
    procedure acDeleteRegionExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure bbtnOpenImageClick(Sender: TObject);
    procedure btnTemplateImageFileSetClick(Sender: TObject);
    procedure txtTemplateNameChange(Sender: TObject);
    procedure txtImageFileNameChange(Sender: TObject);
    procedure txtTemplateDescriptionChange(Sender: TObject);
    procedure txtIdChange(Sender: TObject);
    procedure ElementDisplayOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ElementDisplayOnExit(Sender: TObject);
    procedure KeyPressOnlyAllowNumeric(Sender: TObject; var Key: Char);
    procedure cboDataTypeChange(Sender: TObject);
    procedure txtActiveImageChange(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acOpenTemplateAsXMLExecute(Sender: TObject);
    procedure acRegionCopyRightExecute(Sender: TObject);
    procedure acRegionCopyLeftExecute(Sender: TObject);
    procedure acRegionCopyAboveExecute(Sender: TObject);
    procedure acRegionCopyBelowExecute(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuEditCopyClick(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure mnuEditPasteClick(Sender: TObject);
    procedure pnlImageViewMouseEnter(Sender: TObject);
    procedure ImgViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rbtnNoDocCorrectionClick(Sender: TObject);
    procedure rbtnFirstCharClick(Sender: TObject);
    procedure rbtnXYBoundaryClick(Sender: TObject);
    procedure btnSearchFirstCharClick(Sender: TObject);
    procedure UpdateDisplayRegionSpecialFirstChar(Sender: TObject);
    procedure UpdateDisplayRegionSpecialBoundary(Sender: TObject);
    procedure btnFindXYBoundariesClick(Sender: TObject);
    procedure Slider1Change(Sender: TObject);
    procedure ImgProcSliderChange(Sender: TObject);
    procedure lstInstAvailableClick(Sender: TObject);
    procedure btnAddImageInstClick(Sender: TObject);
    procedure pgctlTemplateDataChange(Sender: TObject);
    procedure ImgProcControlKeyPress(Sender: TObject; var Key: Char);
    procedure gridActionListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SGDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SGDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure btnDoImageInstClick(Sender: TObject);
    procedure bbtnRefreshClick(Sender: TObject);
    procedure TimerStatusBarTimer(Sender: TObject);
    procedure bStatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure OpenTempPath1Click(Sender: TObject);
    procedure mnuTemplateOCRClick(Sender: TObject);
    procedure acOCRExecute(Sender: TObject);
    procedure pmnuExecuteActionListClick(Sender: TObject);
    procedure gridActionListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmnuDeleteInstructionClick(Sender: TObject);
    procedure pmnuClearAllInstructionsClick(Sender: TObject);
    procedure pmnuProcInstAndOCRClick(Sender: TObject);
    procedure tvRegionsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acOCRRegionExecute(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pmnuRegionsPopup(Sender: TObject);
    procedure ImgViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgViewDblClick(Sender: TObject);
    procedure acRegionRenameExecute(Sender: TObject);
    procedure acTwainSelectExecute(Sender: TObject);
    procedure acAcquireImagesExecute(Sender: TObject);
    procedure txtActiveImageDblClick(Sender: TObject);
    procedure txtJobImagePathDblClick(Sender: TObject);
    procedure txtJobOutputDblClick(Sender: TObject);
    procedure txtImageFileNameDblClick(Sender: TObject);
    procedure btnSetImagePathClick(Sender: TObject);
    procedure btnBrowserForOutputFolderClick(Sender: TObject);
    procedure cboJobNameChange(Sender: TObject);
    procedure btnJobTemplateSelectClick(Sender: TObject);
    procedure btnJobSaveClick(Sender: TObject);
    procedure txtJobTemplateChange(Sender: TObject);
    procedure txtJobImagePathChange(Sender: TObject);
    procedure txtJobOutputChange(Sender: TObject);
    procedure chkRestartableClick(Sender: TObject);
    procedure chkRestartableMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOCRJobExecuteClick(Sender: TObject);
    procedure btnJobDeleteClick(Sender: TObject);
    procedure bbtnImageProcAndOCRClick(Sender: TObject);
    procedure acOpenImage1Click(Sender: TObject);
    procedure ProgressDialogCancel(Sender: TObject);
    procedure ApplicationEventsModalBegin(Sender: TObject);
    procedure ApplicationEventsModalEnd(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OnDialogReturn( Sender: TObject );
    procedure OnDialogCancel( Sender: TObject );
    procedure OnProgressCancel( Sender: TObject );
    procedure pmnuJobTemplateEditPopup(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditDeleteClick(Sender: TObject);
    procedure pmnuJobExploreClick(Sender: TObject);
    procedure pmnuSetJobTemplateClick(Sender: TObject);
    procedure pmnuEditTemplateClick(Sender: TObject);
    procedure Index1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure txtFirstCharXContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure OnControlEnter(Sender: TObject);
    procedure mnuEditUndoClick(Sender: TObject);
    procedure mnuEditRedoClick(Sender: TObject);
    procedure mnuHelpActivationBuyClick(Sender: TObject);
    procedure mnuSupportClick(Sender: TObject);
    procedure pnlImageInfoClick(Sender: TObject);
    procedure pnlImageInfoMouseEnter(Sender: TObject);
    procedure pnlImageInfoMouseLeave(Sender: TObject);
    procedure txtDPI_Exit(Sender: TObject);
    procedure txtDPI_Enter(Sender: TObject);
    procedure txtDPIKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure txtDPIChange(Sender: TObject);
  private
    FEventOrigin : string; // Mainly used to determine which object we care to know originated a event
    FImgViewCancelMouseDown : boolean;  
    FMainCtl : TI2XMainUIController;
    IsOverImageInst : boolean;
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    function HasParticularParent( ctl: TWinControl; const ParentName: string) : boolean;
    function RefreshImageCacheIfNeeded: boolean;
    procedure RestoreUIState;
    procedure SaveUIState;
    function IsTemplateLoaded: boolean;
    procedure DbgEle( ele : CEleNodeData );
    procedure RenameRegion( RegionToRename : CEleNodeData; const NewRegionName : string );
    procedure RenameRegionPrompt( RegionToRename : CEleNodeData );
    function getI2XScanObject: TI2XScan;
    procedure ShowOptions( const PanelDisplay : TI2XOptionsEnum = iAPPLICATION);
    procedure JobControlsEnabled(enabled: boolean);
    function GetActiveJobFileName: string;
    function LoadJobList( const DefaultValue : string = '' ): integer;
    function SetActiveJob( const JobName : string ): integer;
    function getLastPathSelected: string;
    procedure setLastPathSelected(const Value: string);
    procedure OffsetRegion(const x, y: integer);
    procedure AnalyzeDocumentForAttributes(Sender: TObject);
    function GetIsBoundarySet: boolean;
    function getFirstChar : TI2XOCRItem;
    procedure setFirstChar(const Value: TI2XOCRItem);
    procedure OnStringGridChange(Sender: TObject);
    procedure MoveChildRegion(eleToMakeChild, eleParent: CEleNodeData);
    function GetNodeByText(ATree: TTreeView; AValue: String;
      const AVisible: Boolean): TTreeNode;
    function getImageInfoMessage: string;
    procedure setImageInfoMessage(const Value: string);
  protected
    FMouseStartDragPoint : TPoint;
    FLastMouseLocation : TPoint;
    FRegionRedrawPending : boolean;
    //frmI2XOptions : TI2XOptionsUI;
    KeyLast : byte;
    fInDrag : Boolean;
    Zoom : Extended;
    isDirty : boolean;
    oFileDir : CFileDir;
    oStrUtil : CStrUtil;
    //sActiveImage : string; //used to cache the name on enter
    ActiveImageInfo : TDWImageInfo;
    FEnableActionListChangeEvent : boolean;
    templateSection : CTemplate;
    FSelection: TPositionedLayer;
    FSelectedImgInst : TI2XImageInstruction;
    FSelectedElement : CEleNodeData;
    FSelectedElementLast : CEleNodeData;
    FModified: boolean;
    sCurrentImageFileName: string;
    MousePosLast : TPoint;
    _FirstChar : TI2XOCRItem;
    FFirstCharRegionElement : CEleNodeData;
    FXAxisRegionElement : CEleNodeData;
    FYAxisRegionElement : CEleNodeData;
    arrImgProcObjectTabOrder : TWinControlArray;
    arrImgProcObjectTabOrderCount : Byte;
    FElementDisplayLevel : ShortInt;
    FXold : integer;
    FYold : integer;
    SourceCol : integer;
    SourceRow : integer;
    FStatusMessageLast : string;
    FPercentComplete : TPercent;
    FDWImageWork : TDWImage;
    RBLayer: TRubberbandLayer;
    FScan : TI2XScan;
    FInFormCreateEvent : boolean;
    FMemoryMapManager : TI2XMemoryMapManager;
    FModalDisplayFaded : boolean;
    procedure OnScanStatusChange(Sender: TObject; msgText : string);
    procedure OnScanErrorChange(Sender: TObject; exc : TObject; procedureName : string);
    procedure OnAcquireImageComplete( Sender: TObject; BitmapHandle : HBITMAP);
    procedure OnRequestRegionAction(Sender: TObject; const Action : TRegionUndoAction; const ElementXMLImage : string; const NamePath : string);
    procedure OnRequestActionGridUpdate(Sender: TObject; const Data : string);
  public
    Reg : TI2XReg;
    dlgOCRProductView : TOCRProductViewDialog;
    ActiveImageIsLoaded : boolean;
    FCursorSave : TCursor;
    template : CTemplate;
    Boundary : TRect;
    procedure SKMenuEnable(const Enable: boolean);
    procedure RegionAttributeChange(ele: CEleNodeData; NewLocation: TRect); overload;
    procedure RegionAttributeChange(ele: CEleNodeData; NewLocation: TFloatRect); overload;
    procedure RegionMove(ele: CEleNodeData; const ChangeOfX, ChangeOfY: integer);
    procedure RegionResize(ele: CEleNodeData; const ChangeOfWidth, ChangeOfHeight: integer);
    function IsCloseToRBLayer(const X, Y: Integer) : boolean;
    procedure DeleteRegion(eleData: CEleNodeData);
    procedure DeleteRegionPrompt(eleData: CEleNodeData);
    procedure RectImageBoundsCheck(ele: CEleNodeData; var loc: TRect); overload;
    procedure RectImageBoundsCheck(ele: CEleNodeData; var loc: TFloatRect); overload;
    property FirstCharRegionElement : CEleNodeData read FFirstCharRegionElement write FFirstCharRegionElement;
    property XAxisRegionElement : CEleNodeData read FXAxisRegionElement write FXAxisRegionElement;
    property YAxisRegionElement : CEleNodeData read FYAxisRegionElement write FYAxisRegionElement;
    property ImageInfoMessage : string read getImageInfoMessage write setImageInfoMessage;

    procedure ShowDialog( const InitMessage : string; ReturnFunction : TNotifyEvent ); overload;
    procedure ShowDialog( const InitMessage : string; ReturnFunction : TNotifyEvent; CancelFunction : TNotifyEvent ); overload;
    property MemoryMapManager : TI2XMemoryMapManager read FMemoryMapManager write FMemoryMapManager;
    property IsBoundarySet : boolean read GetIsBoundarySet;
    property InFormCreateEvent : boolean read FInFormCreateEvent write FInFormCreateEvent;
    property LastPathSelected : string read getLastPathSelected write setLastPathSelected;
    property ModalDisplayFaded : boolean read FModalDisplayFaded write FModalDisplayFaded;
    procedure SetOCRMenuOptions(EnableFlag: boolean);
    procedure OnImageInstExit();
    procedure OnImageInstEnter();
    procedure OnUndoStackChange( Sender : TObject );
    procedure OnTemplateOffsetChange( Sender : TObject );

    function CreatePositionedLayer: TPositionedLayer;
    procedure OnStatusChange( Sender : TObject; TaskId : string; sStatusMessage : string ); virtual;
    procedure OnDebugMessage( Sender : TObject; TaskId : string; sDebugMessage : string ); virtual;
    procedure OnDebug( Sender : TObject; sDebugMessage : string ); virtual;
    procedure DPIWarning(DPI: integer);

    function getElementDisplayActive : boolean;
    procedure UpdateImageProcessingList;
    procedure ImgProcParmClear;
    procedure ImgProcRender(oImgParm: TI2XImgParm);
    function ImgProcCreateParmCombo( const ID, LabelText, SelectedValue : string; OptionList : TStrings ): TPanel;
    function ImgProcCreateParmSlider( const min, max, default, value : integer; const ID, LabelText : string ): TPanel;
    function ImgProcCreateParmText(const ID, lblText, edtText : string): TPanel;
    function ImgProcParmString( var ParmString : string ) : boolean;
    function AppHookFunc(var Message: TMessage): Boolean;
    procedure TabsControlsEnabledCheck;
    procedure FillImgProcTabOrderArray;
    procedure SelectNextImgProc(ctl : TWinControl);
    function SortWinControlsByTopLocation(
      arrCtls: TWinControlArray): TWinControlArray;
    function IsOverBitMapLayer(X, Y: Integer; B: TBitmapLayer): boolean;
    procedure ReLoadActiveImage();
    procedure EmbedProgressBar(Sender: TObject);
    function getPercentComplete: TPercent;
    procedure setPercentComplete(const Value: TPercent);
    procedure ProgressBarStart;
    procedure ProgressBarStop;

    function CreateTemplate( const TemplateName: string = '' ) : boolean;
    property ElementDisplayEventsAreActive : boolean read getElementDisplayActive;
    property PercentComplete : TPercent read getPercentComplete write setPercentComplete;
    property FirstChar : TI2XOCRItem read getFirstChar write setFirstChar;
    function getImagePixelWidth : integer;
    function getImagePixelHeight : integer;
    function getSelectedActionList() : TStringGrid;
    procedure SetSelection(Value: TPositionedLayer);
    procedure SetSelectedElement( eleData : CEleNodeData );
    procedure SetActiveTemplate(TemplateFileName: string);
    procedure TriggerUIEvents(enable: boolean);
    procedure ClearDirty;
    procedure LoadActiveImage(sImageFile: string; setTemplateImageAttrib : boolean = true );
    procedure SetActiveImage(sImageFile: string = '');
    procedure ErrorDisplay(procedureName, msg: string);
    procedure LoadTemplateFromFile(sFileName: string);
    procedure ProcessXMLTemplate(const sFile, vImageFileName: TFileName); overload;
    procedure ProcessXMLTemplate(const sFile : TFileName); overload;
    procedure FillTemplateVars;
    procedure ClearTemplateUIElements;
    procedure SelectDocumentCorrection(tag: integer);
    procedure ClearTree;
    procedure ElementsToTree(tree: TTreeView; ht: THashTable );
    procedure ClearDisplayElement;
    procedure DisplaySelectedElement(eleData: CEleNodeData);
    procedure ControlsEnableSelectedElement(
      enableType: TElementControlsDisplay);
    procedure ControlsEnableImageProc( enable: boolean );
    function ActionListAsString(grd: TStringGrid): string;
    procedure ActionListFromString(grd: TStringGrid; alString: string);
    function ActionListToStringList(grd: TStringGrid; sl : TStringList) : integer;
    procedure AddActionToList(grd: TStringGrid; sActionToAdd: string); overload;
    procedure AddActionToList(sActionToAdd: string); overload;
    function CurrentActionListGrid: TStringGrid;
    function FindValue(cbo: TComboBox; Value: string): integer;
    procedure LayerMouseDown(Sender: TObject; Buttons: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RBResizing(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);
    procedure RBResizeUserChanged(Sender: TObject);
    procedure SetZoom(iNew: Extended);
    procedure ImgViewPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure DrawRegions( ht: THashTable );
    procedure HideRegion( eleData: CEleNodeData );
    procedure ShowRegion( eleData: CEleNodeData );
    function DrawRegion(Sender: TObject) : TPositionedLayer; overload;
    function DrawRegion(Sender: TObject; eleData: CEleNodeData): TPositionedLayer; overload;
    function DrawRegion(Sender: TObject; eleData: CEleNodeData; SpecialRegionFlag : boolean; FillColor: TColor32 = clBlue32): TPositionedLayer; overload;
    procedure RegionOnPaintHandler(Sender: TObject; Buffer: TBitmap32);
    procedure MemoWrite(msg: string);
    function FloatRectToString( Rect : TFloatRect): string;
    procedure GridAlign(var Val : single; const GridSize : single ); overload;
    procedure GridAlign(var Val : Integer; const GridSize : single ); overload;
    procedure RegionOnPaintHandlerMulti(Sender: TObject; Buffer: TBitmap32);
    function HitTestRegions( X, Y: Integer; var B: TBitmapLayer ) : boolean; overload;
    function HitTestRegions( X, Y: Integer; var B: TBitmapLayer; testGroupLevelLayers : boolean ) : boolean; overload;
    procedure SetRegionsPopupMenuOptions(Sender: TObject);
    procedure SetMainMenuOptions(Sender: TObject);
    procedure TemplateUIUpdated;
    procedure UpdateDisplayRegion(Sender: TObject);
    procedure StatusMessage(const msg: string; const ProgressBarState : TProgressBarState = pgIgnore; const MainTitle: string = IGNORE);
    procedure dbg(const msg: string );
    procedure OnTemplateChange( sender : TObject );
    function CopyRegion(RegionToCopy: CEleNodeData; NewLocation : TPoint ): CEleNodeData; overload;
    function CopyRegion( RegionToCopy : CEleNodeData; CopyDirection : TCopyDirection = cpOffset ) : CEleNodeData; overload;
    function Save( sFileName : TFileName ) : boolean ;
    procedure RegionOnPaintOnAxisHandler(Sender: TObject; Buffer: TBitmap32);
    procedure DisplaySpecialElement(eleData: CEleNodeData);
    function CreateRegion(const NewRegionID: string) : CEleNodeData; overload;
    function CreateRegion(const ModelRegion : CEleNodeData) : CEleNodeData; overload;
    function CreateRegion(const ModelRegion, ParentRegion : CEleNodeData) : CEleNodeData; overload;
    procedure SetRegionAtrributesByName(newEleData: CEleNodeData; const DefaultAttributes : boolean = true );
    procedure RBResizeUserChangedSpecial(Sender: TObject);
    procedure CommitDocCorrectToTemplate;
    procedure DrawSpecialRegions;
    procedure ImageInstructionParameterDisplay;
    procedure CleanUpAllTemplateData();
    property Selection: TPositionedLayer read FSelection write SetSelection;
    property SelectedElement: CEleNodeData read FSelectedElement write SetSelectedElement;
    property SelectedActionList: TStringGrid read getSelectedActionList;
    property ImagePixelWidth: integer read getImagePixelWidth;
    property ImagePixelHeight: integer read getImagePixelHeight;
    property Scan : TI2XScan read getI2XScanObject write FScan;
    Procedure CMDialogKey(Var Msg: TWMKey); message CM_DIALOGKEY;
    function GetSelectionTemplate : CTemplate;
    property TemplateSelection : CTemplate read GetSelectionTemplate;
    property ActiveJobFileName : string read GetActiveJobFileName;
  end;
  TI2XPageCtl = (pgInfo, pgRegions, pgImage, pgOffset, pgOptions, pgJob );

var
  frmI2XBase: TI2XBaseUI;

implementation

  uses ClipBrd;
{$R *.dfm}

Procedure TI2XBaseUI.CMDialogKey(Var Msg: TWMKEY);
Begin
//If (ActiveControl = Edit1) Then
  If Msg.Charcode = VK_TAB Then begin
    if (( self.ActiveControl.Parent <> nil ) and ( self.ActiveControl.Parent.Tag = 1 )) then begin
      self.SelectNextImgProc( self.ActiveControl );
      //dbg( 'self.ActiveControl.Name' + self.ActiveControl.Name );
      Msg.Charcode := 0; // to eat the tab key!
    end;
    //ShowMessage('Tab');
  end;
  inherited;
End;

function TI2XBaseUI.AppHookFunc(var Message : TMessage) : Boolean;
Begin
  Result := False;
  if ( Message.Msg = WM_MOUSEMOVE ) then begin
  end;
  if (( Message.Msg = 45078 ) ) then begin
    if (( self.ActiveControl.Parent <> nil ) and ( self.ActiveControl.Parent.Tag = 1 )) then begin
      self.SelectNextImgProc( self.ActiveControl );
      //dbg( 'self.ActiveControl.Name' + self.ActiveControl.Name + '...  Message.Msg=' + IntToStr( Message.Msg )  );
      Result := True;
    end;
  end;
End;

procedure TI2XBaseUI.ApplicationEventsModalBegin(Sender: TObject);
begin
  if ( FModalDisplayFaded )  then
    frmDimmer.Display;
end;

procedure TI2XBaseUI.ApplicationEventsModalEnd(Sender: TObject);
begin
  if ( FModalDisplayFaded )  then
    frmDimmer.Hide;
end;

procedure TI2XBaseUI.OnControlEnter(Sender: TObject);
Begin
  if (( ElementDisplayEventsAreActive ) and
      ( not ( undo.IsUndoing or undo.IsRedoing ))) then begin
    dbg('Sender.ClassName=' + Sender.ClassName );
    if ( Sender.ClassName = 'TEdit' ) then begin
      dbg('Sender.ClassName=' + Sender.ClassName + '  Name=' + TEdit(Sender).Name );
      undo.Prep( Sender, TEdit(Sender).Text );
    end else if ( Sender.ClassName = 'TComboBox' ) then begin
      dbg('Sender.ClassName=' + Sender.ClassName + '  Name=' + TComboBox(Sender).Name );
      undo.Prep( Sender, TComboBox(Sender).Text );
    end else if ( Sender.ClassName = 'TMemo' ) then begin
      dbg('Sender.ClassName=' + Sender.ClassName + '  Name=' + TMemo(Sender).Name );
      undo.Prep( Sender, TMemo(Sender).Lines.Text );
    end else if ( Sender.ClassName = 'CEleNodeData' ) then begin
      dbg('DEBUG PREP: Sender.ClassName=' + Sender.ClassName + '  Name=' + CEleNodeData(Sender).ID + '  data=' + CEleNodeData(Sender).AsString() );
      undo.Prep( Sender, CEleNodeData(Sender).AsXML() );
    end else if ( Sender.ClassName = 'TStringGrid' ) then begin
      dbg('DEBUG PREP: Sender.ClassName=' + Sender.ClassName + '  Name=' + TStringGrid(Sender).Name );
      undo.Prep( Sender, ActionListAsString( self.SelectedActionList ) );
    end;
  end;
End;

procedure TI2XBaseUI.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  pt:TPoint;
  r:TRect;
  ctrl : TWinControl;
begin
  //if (Msg.message = WM_MOUSELEAVE) then begin
  // dbg('Msg.message=WM_MOUSELEAVE - 675' );
  //end else
  if (Msg.message = WM_MOUSEMOVE) then begin
    getCursorPos(pt);
    ctrl := FindVCLWindow( pt ) ;
    if ( ctrl <> nil ) then begin
      if (
           ( ctrl.Name = 'tabImage' ) or
           ( HasParticularParent( ctrl, 'tabImage' ) )
         ) then begin
        if ( not IsOverImageInst ) then begin
          self.OnImageInstEnter;
          IsOverImageInst := true;
        end;
      end else begin
        if ( IsOverImageInst ) then begin
          self.OnImageInstExit;
          IsOverImageInst := false;
        end;

      end;
    end;
  end
  else if ( Msg.message = 256 ) then begin
    //Msg.wParam - virtual-key code,  Msg.lParam - key data
    getCursorPos(pt);
    ctrl := FindVCLWindow( pt );
    if (
        ( Msg.wParam = 46 ) and
        (( ctrl.Name = 'tvRegions' ) or ( ctrl.Name = 'ImgView' )) and
        ( self.SelectedElement <> nil )
       )
    then begin
      acDeleteRegionExecute( ctrl );
    end;
    //dbg( 'Key down?=' + IntToStr(Msg.message) + '   Key=' + IntToStr( Msg.wParam ) + '  ctrl=' + ctrl.Name );
  end else if ( Msg.message = 257 ) then begin
    //dbg('Key up?=' + IntToStr(Msg.message) + '   Key=' + IntToStr( Msg.wParam ) );
  end else if not (( Msg.message = 275 ) or ( Msg.message = 15 ) ) then begin
    //dbg('Msg.message=' + IntToStr(Msg.message) );
  end;
End;

procedure TI2XBaseUI.OnAcquireImageComplete( Sender: TObject; BitmapHandle : HBITMAP );
var
  dwimage : TDWImage;
  sFileName : string;
begin
  try
    dwimage := TDWImage.Create( BitmapHandle );
    //self.ImgView.Bitmap.Assign( dwimage.BitMap );
    sFileName := AppTempDir + '~SCANNED.BMP';
    dwimage.SaveToFile( sFileName );

    //sFileName := ExtractRelativePath( AppPath, sFileName );
    txtActiveImage.Text := sFileName;
    ReLoadActiveImage();

  finally
    FreeAndNil( dwimage );
  end;
end;

procedure TI2XBaseUI.OnImageInstEnter();
begin
end;

procedure TI2XBaseUI.OnImageInstExit();
begin
  {This will force the Image cache to reset if the instructions are different}
//  fMainCtl.ImageProcCacheInst := self.ActionListAsString( self.SelectedActionList );
  //fMainCtl.CacheProcessedImage( self.ImgView.Bitmap  );
end;

procedure TI2XBaseUI.OnProgressCancel(Sender: TObject);
begin
  Dbg('WRITE CANCEL CODE HERE!');
end;

procedure TI2XBaseUI.OnRequestActionGridUpdate(Sender: TObject;
  const Data: string);
begin
  self.ActionListFromString( TStringGrid( Sender ), Data );
end;

procedure TI2XBaseUI.OnRequestRegionAction(Sender: TObject;
  const Action: TRegionUndoAction; const ElementXMLImage: string; const NamePath : string);
var
  ele, eleImage : CEleNodeData;
begin
  try
    case Action of
      ruRegionMove:
        begin
          try
            eleImage := CEleNodeData.Create();
            eleImage.FromXML( ElementXMLImage );
            ele := template.getElement( NamePath, false );
            self.RegionAttributeChange( ele, eleImage.LocationAsRect );
            SetSelection( nil );
            SetSelectedElement( ele );
            SetSelection( TBitmapLayer( ele.GraphicPointer ) );
          finally
            FreeAndNil( eleImage );
          end;
        end;
      ruRegionAdd:
        begin
      //If we added,  then we will have to delete the region back to the elements
          try
            ele := template.getElement( NamePath, false );
            self.DeleteRegionPrompt( ele );
          finally
          end;
        end;
      ruRegionDelete:
      //If we deleted,  then we will have to add the region back to the elements
        begin
          try
            eleImage := CEleNodeData.Create();
            eleImage.FromXML( ElementXMLImage );
            CreateRegion( eleImage );
          finally
            FreeAndNil( eleImage );
          end;
        end;
      ruRegionRename:
        begin
      //If we added,  then we will have to delete the region back to the elements
          try
            eleImage := CEleNodeData.Create();
            eleImage.FromXML( ElementXMLImage );
            self.RenameRegion( self.SelectedElement, eleImage.ID );
            dbg('class name=' + Sender.ClassName);
          finally
            FreeAndNil( eleImage );
          end;
        end;
    end;
  finally
  end;
end;

function TI2XBaseUI.HasParticularParent( ctl: TWinControl; const ParentName: string ) : boolean;
var
  pt:TPoint;
  r:TRect;
  ctrl, parentCtl : TWinControl;
begin
  Result := false;
  parentCtl := ctl.Parent;
  while ( parentCtl <> nil ) do begin
    if ( ParentName = parentCtl.Name ) then begin
      Result := true;
      break;
    end;
    parentCtl := parentCtl.Parent;
  end;
end;

function TI2XBaseUI.CreateTemplate( const TemplateName : string ) : boolean;
var
  sNewTemplateName, sNewTemplateFileName : string;
  bExists : boolean;
  iCount : byte;
Begin
  CleanUpAllTemplateData();
  sNewTemplateName := TemplateName;
  if ( length(sNewTemplateName) = 0 ) then begin
    while ( Length(sNewTemplateName) = 0 ) do begin
      sNewTemplateName :=InputBox(CREATE_TEMPLATE_NAME_CAPTION, CREATE_TEMPLATE_NAME_PROMPT, sNewTemplateName);
      if ( oStrUtil.NonAlphaCharCount(sNewTemplateName) > 0 ) then begin
        MessageDlg( INVALID_NEW_TEMPLATE_NAME, mtError,[mbOk], 0 );
        sNewTemplateName := '';
      end;
    end;
  end;
  ForceDirectories( _FileDir.GetMyDocDir() + I2XFULL_DIR_QUAL );
  sNewTemplateFileName := _FileDir.GetMyDocDir() + I2XFULL_DIR_QUAL + sNewTemplateName + TEMPLATE_FILE_EXT;
  iCount := 0;
  bExists := FileExists(sNewTemplateFileName);
  while bExists do begin
    Inc( iCount );
    sNewTemplateName := TemplateName + ' ' + IntToStr( iCount );
    sNewTemplateFileName := _FileDir.GetMyDocDir() + I2XFULL_DIR_QUAL + sNewTemplateName + TEMPLATE_FILE_EXT;
    bExists := FileExists(sNewTemplateFileName);
  end;
  template.TemplateName := sNewTemplateName;
  self.txtTemplateName.Text := template.TemplateName;
  template.TemplateFileName := sNewTemplateFileName;
  Result := FileExists( template.TemplateFileName );
End;

function TI2XBaseUI.CreateRegion( const NewRegionID : string ) : CEleNodeData;
var
  sNewRegionID : string;
  newEleData : CEleNodeData;
  bExists : boolean;
  iCount : byte;
  tn : TTreeNode;
  P : TPoint;
Begin
  Result := nil;
  sNewRegionID := NewRegionID;
  iCount := 0;
  bExists := template.Elements.ContainsKey( sNewRegionID );
  while bExists do begin
    Inc( iCount );
    sNewRegionID := NewRegionID + ' ' + IntToStr( iCount );
    bExists := template.Elements.ContainsKey( sNewRegionID );
  end;
  if ( template.Elements.ContainsKey( sNewRegionID ) ) then begin

    MessageDlg(Format( NEW_REGION_ERROR_EXISTS_FMT, [sNewRegionID] ), mtError,[mbOk], 0);
  end else begin
    newEleData := CEleNodeData.Create();
    newEleData.id := sNewRegionID;
    SetRegionAtrributesByName( newEleData );

    template.Elements.Add( sNewRegionID, newEleData );
    if ( newEleData.TreeViewNodePointer <> nil ) then begin
    //  if there is a tree node,  then this is a standard region
      self.SetSelectedElement( newEleData );
      Selection := DrawRegion( self, newEleData );
    end else begin
    //  if there is not tree node, then this must be a 'special' region
      Selection := DrawRegion( self, newEleData, true, newEleData.FillColor );
    end;
    self.TemplateUIUpdated();
    Result := newEleData;
    undo.Add( newEleData, ruRegionAdd, template, true );
  end;
End;

function TI2XBaseUI.CreateRegion(const ModelRegion, ParentRegion : CEleNodeData) : CEleNodeData;
var
  sNewRegionID : string;
  bExists : boolean;
  NewRegion : CEleNodeData;
  tn, tnParent : TTreeNode;
begin
  sNewRegionID := ModelRegion.ID;
  if ( ParentRegion = nil ) then
    bExists := template.Elements.ContainsKey( sNewRegionID )
  else
    bExists := ParentRegion.SubNodes.ContainsKey( sNewRegionID );
  if ( bExists ) then begin
    MessageDlg(Format( NEW_REGION_ERROR_EXISTS_FMT, [sNewRegionID] ), mtError,[mbOk], 0);
  end else begin
    NewRegion := CEleNodeData.Create();
    ModelRegion.AssignTo( NewRegion, false );
    NewRegion.id := sNewRegionID;
    if ( ParentRegion = nil ) then begin
      template.Elements.Add( sNewRegionID, NewRegion );

      tn := tvRegions.Items.Add(nil, sNewRegionID );
      tn.Data := NewRegion;
      NewRegion.TreeViewNodePointer := tn;
      tvRegions.AlphaSort();
    end else begin
      ParentRegion.SubNodes.Add( sNewRegionID, NewRegion );
      NewRegion.Parent := ParentRegion;

      tnParent := TTreeNode(ParentRegion.TreeViewNodePointer);
      tn := tvRegions.Items.AddChild( tnParent, sNewRegionID );
      tn.Data := NewRegion;
      NewRegion.TreeViewNodePointer := tn;
      tvRegions.AlphaSort();
    end;
  end;

  result := NewRegion;
end;

function TI2XBaseUI.CreateRegion(const ModelRegion: CEleNodeData) : CEleNodeData;
var
  arrKeyList : TKeyList;
  sParentID : string;
  ele, NewRegion : CEleNodeData;
  i : integer;
Begin
  sParentID := StringReplace( ModelRegion.NamePathShallow, '.' + ModelRegion.ID, '', [rfReplaceAll, rfIgnoreCase]);
  ele := template.getElement( sParentID, false );

  NewRegion := CreateRegion( ModelRegion, ele );
  if ( ModelRegion.SubNodes.Count > 0 ) then begin
    arrKeyList := SortKeyList(ModelRegion.SubNodes.Keys);
    for i := 0 to Length(arrKeyList) - 1 do begin
      ele := CEleNodeData( ModelRegion.SubNodes.get( arrKeyList[i] ));
      CreateRegion( ele, NewRegion );
    end;
  end;

  if ( NewRegion.Parent <> nil ) then
    DrawRegion( NewRegion, CEleNodeData( NewRegion.Parent ) );
  if ( NewRegion.SubNodes.Count > 0 ) then begin
    DrawRegions( NewRegion.SubNodes );
  end;
  Selection := DrawRegion( NewRegion, NewRegion );

  result := NewRegion;
End;

{*
procedure TI2XBaseUI.CreateRegion(const RegionImage: CEleNodeData);
var
  sNewRegionID, sParentID : string;
  bExists : boolean;
  iCount : byte;
  i : integer;
  tn : TTreeNode;
  P : TPoint;
  arrKeyList : TKeyList;
  ele, eleParent : CEleNodeData;
Begin
  sNewRegionID := RegionImage.ID;
  iCount := 0;
  bExists := template.Elements.ContainsKey( sNewRegionID );
  while bExists do begin
    Inc( iCount );
    sNewRegionID := RegionImage.ID + ' ' + IntToStr( iCount );
    bExists := template.Elements.ContainsKey( sNewRegionID );
  end;
  if ( template.Elements.ContainsKey( sNewRegionID ) ) then begin

    MessageDlg(Format( NEW_REGION_ERROR_EXISTS_FMT, [sNewRegionID] ), mtError,[mbOk], 0);
  end else begin
              //since this graphic probably no longer exists,  null out the graphic pointers
    RegionImage.GraphicPointer := nil;
    RegionImage.TreeViewNodePointer := nil;
    SetRegionAtrributesByName( RegionImage, false );

    template.Elements.Add( sNewRegionID, RegionImage );
    if ( RegionImage.TreeViewNodePointer <> nil ) then begin
    //  if there is a tree node,  then this is a standard region
      self.SetSelectedElement( RegionImage );
      Selection := DrawRegion( self, RegionImage );
    end else begin
    //  if there is not tree node, then this must be a 'special' region
      Selection := DrawRegion( self, RegionImage, true, RegionImage.FillColor );
    end;

    if ( RegionImage.SubNodes.count > 0 ) then begin
      arrKeyList := SortKeyList(RegionImage.SubNodes.Keys);
      for i := 0 to Length(arrKeyList) - 1 do begin
        ele := CEleNodeData( RegionImage.SubNodes.get( arrKeyList[i] ));
        CreateRegion( ele );
        RegionImage.SubNodes.Remove( arrKeyList[i] );
        MoveChildRegion( ele, RegionImage );
      end;
    end;

    //if a parent exists with the pointer,  it will
    //sCopiedRegionAddress := StringReplace(Clipboard.AsText, CLIPBOARD_REGION_ID, '', [rfReplaceAll, rfIgnoreCase]);
    sParentID := StringReplace( RegionImage.NamePathShallow, '.' + RegionImage.ID, '', [rfReplaceAll, rfIgnoreCase]);
    eleParent := template.getElement( sParentID );
    if ( eleParent <> nil ) then begin
      MoveChildRegion( RegionImage, eleParent );
    end;
    self.TemplateUIUpdated();
  end;
End;

*}
procedure TI2XBaseUI.SetRegionAtrributesByName( newEleData : CEleNodeData; const DefaultAttributes : boolean );
// DefaultAttributes - true: Used when creating a default/new region
//                     false: When creating a region from CEleNodeData, we do not want to use default attributes,
//                            we will use the attributes passed from the newEleData
var
  sNewRegionID : string;
  tn : TTreeNode;
  P : TPoint;
Begin
  sNewRegionID := newEleData.ID;
    //if ( sNewRegionID = STANDARD_REGION_FIRST_CHAR ) then begin
    if ( newEleData.isFirstChar ) then begin
      with ImgView.GetViewportRect do
        P := ImgView.ControlToBitmap(Point((ImagePixelWidth) div 10, (ImagePixelHeight) div 10));
      newEleData.x := P.X;
      newEleData.y := P.Y;
      newEleData.width := 50;
      newEleData.height := 50;
      newEleData.FillColor := clGreen32;

    //end else if ( sNewRegionID = STANDARD_REGION_X_AXIS ) then begin
    end else if ( newEleData.isXAxis ) then begin
      with ImgView.GetViewportRect do
        P := ImgView.ControlToBitmap(Point((ImagePixelWidth) div 10, 0));
      newEleData.x := (ImagePixelWidth) div 10;
      newEleData.y := 0;
      newEleData.width := 10;
      newEleData.height := self.ImagePixelHeight;
      newEleData.RegionType := rtXAxis;
      newEleData.FillColor := clMaroon32;

    //end else if ( sNewRegionID = STANDARD_REGION_Y_AXIS ) then begin
    end else if ( newEleData.isYAxis ) then begin
      with ImgView.GetViewportRect do
        P := ImgView.ControlToBitmap(Point(0, (ImagePixelHeight) div 10));
      newEleData.x := 0;
      newEleData.y := (ImagePixelHeight) div 10;
      newEleData.width := self.ImagePixelWidth;
      newEleData.height := 10;
      newEleData.RegionType := rtYAxis;
      newEleData.FillColor := clMaroon32;

    end else begin
      with ImgView.GetViewportRect do
        P := ImgView.ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));
      if ( DefaultAttributes ) then begin  //use the default attributes, otherwise use what is passed in the control 
        newEleData.x := P.X - 50;
        newEleData.y := P.Y - 50;
        newEleData.width := 100;
        newEleData.height := 100;
      end;
      newEleData.FillColor := clBlue32;
      tn := tvRegions.Items.Add(nil, newEleData.id );
      tn.Data := newEleData;
      newEleData.TreeViewNodePointer := tn;
      tvRegions.AlphaSort();
    end;
End;

procedure TI2XBaseUI.MoveChildRegion( eleToMakeChild, eleParent : CEleNodeData );
var
  tnParent, tnChild: TTreeNode;
  AttachMode: TNodeAttachMode;
  eleDataSource, eleDataTarget : CEleNodeData;
Begin
  AttachMode := naAddChild;
  //if ( AnItem.Text = eleParent.ID ) then
  //  AttachMode := naAddChild
  //else
  //  AttachMode := naInsert;
  if ( eleParent.TreeViewNodePointer = nil ) then
    raise Exception.Create('Parent Element does not have a parent Tree Node Assoicated with it.');
  //tnParent := TTreeNode( eleParent.TreeViewNodePointer );
  tnParent := GetNodeByText( tvRegions, eleParent.ID, true );
  //if (( AnItem = nil ) or ( AnItem.Text <> eleParent.ID )) then begin
  //  AnItem := GetNodeByText( tvRegions, eleParent.ID, true );
  //end;

  //This means we dragged over something that is not a node,  so get out!
  if ( tnParent = nil ) then
    raise Exception.Create('tnParent = nil');
  eleDataTarget := eleParent;
  eleDataSource := eleToMakeChild;

  eleDataSource := template.MoveElement( eleDataSource, eleDataTarget, true );
  tnChild := GetNodeByText( tvRegions, eleDataSource.ID, true );
  eleDataSource.TreeViewNodePointer := tnChild;
  tnChild.Data := eleDataSource;
  tnChild.MoveTo( tnParent, AttachMode );

  //tvRegions.Select( TTreeNode( eleDataSource.TreeViewNodePointer ) );
  //eleDataSource.TreeViewNodePointer := tvRegions.Selected;
  //tvRegions.Selected.Data := eleDataSource;
  //tvRegions.Selected.MoveTo( tnParent, AttachMode );
  self.FSelectedElement := eleDataSource;
  self.DrawRegion( self, eleDataSource );
  self.DrawRegion( self, eleDataTarget );
  tvRegions.AlphaSort();
End;

procedure TI2XBaseUI.About1Click(Sender: TObject);
var
  sURL : string;
begin
  AboutBox.ShowModal;
end;

procedure TI2XBaseUI.acAcquireImagesExecute(Sender: TObject);
begin
  Scan.SourceDevice := Options.TWAINDeviceDefault;
  Scan.OnStatusChange := self.OnScanStatusChange;
  Scan.OnError := self.OnScanErrorChange;
  Scan.OnImageComplete := self.OnAcquireImageComplete;
  self.Scan.Scan( AppTempDir, 1 );
end;

procedure TI2XBaseUI.acCreateRegionExecute(Sender: TObject);
Begin
  CreateRegion( NEW_REGION_DEFAULT_NAME );
End;

function TI2XBaseUI.CopyRegion( RegionToCopy: CEleNodeData;
  CopyDirection: TCopyDirection): CEleNodeData;
var
  NewLocation : TPoint;
Begin
  Initialize( NewLocation );
  case CopyDirection of
    cpOffset:
      begin
        NewLocation.X := RegionToCopy.X + 20;
        NewLocation.Y := RegionToCopy.Y + 20;
      end;
    cpRight:
      begin
        NewLocation.X := RegionToCopy.X + RegionToCopy.Width;
        NewLocation.Y := RegionToCopy.Y;
      end;
    cpLeft:
      begin
        NewLocation.X := RegionToCopy.X - RegionToCopy.Width;
        NewLocation.Y := RegionToCopy.Y;
      end;
    cpAbove:
      begin
        NewLocation.X := RegionToCopy.X;
        NewLocation.Y := RegionToCopy.Y - RegionToCopy.Height;
      end;
    cpBelow:
      begin
        NewLocation.X := RegionToCopy.X;
        NewLocation.Y := RegionToCopy.Y + ( RegionToCopy.Height );
      end;
  end;
  Result := CopyRegion( RegionToCopy, NewLocation );
End;

function TI2XBaseUI.CopyRegion( RegionToCopy : CEleNodeData; NewLocation : TPoint ) : CEleNodeData;
var
  sNewRegionID : string;
  newEleData : CEleNodeData;
  bExists : boolean;
  iCount : byte;
  tn : TTreeNode;
  P : TPoint;
Begin
  newEleData := nil;
  sNewRegionID := RegionToCopy.ID + ' ' + NEW_REGION_COPY_NAME;
  iCount := 0;
  bExists := template.Elements.ContainsKey( sNewRegionID );
  while bExists do begin
    Inc( iCount );
    sNewRegionID := RegionToCopy.ID + ' ' + NEW_REGION_COPY_NAME + ' ' + IntToStr( iCount );
    bExists := template.Elements.ContainsKey( sNewRegionID );
  end;
  if ( template.Elements.ContainsKey( sNewRegionID ) ) then begin
    MessageDlg(Format( NEW_REGION_ERROR_EXISTS_FMT, [sNewRegionID] ), mtError,[mbOk], 0);
    Result := nil;
  end else begin
    newEleData := CEleNodeData.Create();
    newEleData.id := sNewRegionID;

    newEleData.x := NewLocation.X;
    newEleData.y := NewLocation.Y;
    newEleData.width := RegionToCopy.Width;
    newEleData.height := RegionToCopy.Height;

    //Prevent a new region from appearing outside the ViewPort
    with ImgView.GetViewportRect do begin
      if ( NewLocation.X < 0 ) then newEleData.x := 0;
      if ( NewLocation.Y < 0 ) then newEleData.y := 0;
      if ( ImgView.Bitmap.Width < (newEleData.x + newEleData.width ) ) then
          newEleData.X := ImgView.Bitmap.width - newEleData.width;
      if ( ImgView.Bitmap.Height < (newEleData.Y + newEleData.Height ) ) then
          newEleData.Y := ImgView.Bitmap.Height - newEleData.Height;
    end;

    tn := tvRegions.Items.Add(nil, newEleData.id );
    tn.Data := newEleData;
    newEleData.TreeViewNodePointer := tn;

    template.Elements.Add( sNewRegionID, newEleData );
    self.SetSelectedElement( newEleData );
    Selection := DrawRegion( self, newEleData );
    self.TemplateUIUpdated();
    Result := newEleData;
  end;
End;

procedure TI2XBaseUI.DeleteRegion( eleData : CEleNodeData );
Var
  eleSData : CEleNodeData;
  sNewRegionID : string;
  tn : TTreeNode;
  B : TBitMapLayer;
  arrKeyList : TKeyList;
  i: integer;
Begin
  if ( eleData.SubNodes.count > 0 ) then begin
    arrKeyList := SortKeyList(eleData.SubNodes.Keys);
    for i := 0 to Length(arrKeyList) - 1 do begin
      DeleteRegion( CEleNodeData( eleData.SubNodes.get( arrKeyList[i] )) );
    end;
  end;
  B := TBitmapLayer( eleData.GraphicPointer );
  ImgView.Layers.Delete( B.Index );
  tn := TTreeNode( eleData.TreeViewNodePointer );
  tvRegions.Items.Delete( tn );
  template.DeleteElement( eleData );
End;

procedure TI2XBaseUI.DeleteRegionPrompt(eleData: CEleNodeData);
var
  bExists : boolean;
  iCount : byte;
  P : TPoint;
  res : integer;
Begin
  res := mrNo;
  if ( eleData.nodeType = NODE_TYPE_SINGLE ) then
    res := MessageDlg(DELETE_REGION_TEXT, mtConfirmation,
            [mbYes, mbNo], 0)
  else if ( eleData.nodeType = NODE_TYPE_MULTI ) then
    res := MessageDlg(DELETE_REGION_MULTI_TEXT, mtConfirmation,
            [mbYes, mbNo], 0);
  if ( res = mrYes ) then begin
    undo.Add( eleData, ruRegionDelete, template, true );
    DeleteRegion( eleData );
    SetSelection( nil );
    SetSelectedElement( nil );
    self.TemplateUIUpdated();
  end;
End;

procedure TI2XBaseUI.acDeleteRegionExecute(Sender: TObject);
Begin
  if ( self.FSelectedElement <> nil ) then
    DeleteRegionPrompt( self.FSelectedElement );
End;

procedure TI2XBaseUI.acEditCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := 'I2XREGION:' + self.SelectedElement.AsString();;
end;

procedure TI2XBaseUI.CleanUpAllTemplateData();
Begin
  TriggerUIEvents( false );

  SetSelection( nil );
  self.SetSelectedElement( nil );
  self.ClearTemplateUIElements;

  self.SetActiveImage();

  template.Clear;
  FFirstCharRegionElement := nil;
  FXAxisRegionElement := nil;
  FYAxisRegionElement := nil;

  TriggerUIEvents( true );
End;

procedure TI2XBaseUI.SKMenuEnable( const Enable : boolean );
Begin
  mnuHelpActivation.Enabled := not Enable;
End;


procedure TI2XBaseUI.acNewExecute(Sender: TObject);
var
  buttonSelected : Integer;
  sFileName, sExt : string;
begin
  if ( self.isDirty ) then begin

    buttonSelected := MessageDlg(NOT_SAVED_TEXT,mtWarning,
                              [mbYes,mbNo,mbCancel], 0);
    if buttonSelected = mrYes then begin
      self.acSaveExecute( Sender );
    end else if buttonSelected = mrCancel then begin
      Exit;
    end;
  end;

  TriggerUIEvents( false );

  CleanUpAllTemplateData();

  //template.TemplateName := NEW_TEMPLATE;
  if ( CreateTemplate( NEW_TEMPLATE ) ) then begin
    self.txtTemplateName.Text := template.TemplateName;
    reg.RegSet( REG_KEY, 'active_template', NEW_TEMPLATE, HKEY_CURRENT_USER );
    if ( pgctlTemplateData.TabIndex <> Integer( pgInfo ) ) then begin
      pgctlTemplateData.ActivePageIndex := Integer( pgInfo );
    end;
  end;
  self.ClearDirty;
  TriggerUIEvents( true );
End;

procedure TI2XBaseUI.acOCRExecute(Sender: TObject);
begin
  if ( fMainCtl.OCRImageCheck( ImgView.Bitmap ) ) then begin
    self.StatusMessage( 'Beginning OCR of the Current Active Image', pgStart, 'OCR of Active Image...' );
    fMainCtl.OCRImage( ImgView.Bitmap, self.Template );
  end;
end;

procedure TI2XBaseUI.acOCRRegionExecute(Sender: TObject);
begin
  if ( fMainCtl.OCRImageCheck( ImgView.Bitmap ) ) then begin
    if ( self.FSelectedElement.isFirstChar ) then
      self.StatusMessage( 'Begin OCR for first character', pgStart, 'Finding First Character' )
    else
      self.StatusMessage( 'Beginning OCR of Selected Region', pgStart, 'OCR of Selected Region...' );
    fMainCtl.OCRImageSelection( ImgView.Bitmap, self.TemplateSelection );
  end
end;

function TI2XBaseUI.RefreshImageCacheIfNeeded() : boolean;
Const
  TIMEOUT = 600;
var
  nCounter : smallint;
Begin
  nCounter := 0;
  { This will force the Image cache to reset if the instructions are different}
  fMainCtl.ImageProcCacheInst := self.ActionListAsString( self.SelectedActionList );
  if fMainCtl.ImageProcCacheUpdating then begin
    while fMainCtl.ImageProcCacheUpdating do begin
      Sleep( 100 );
      Inc( nCounter );
      Application.ProcessMessages;
      if ( nCounter >= TIMEOUT ) then begin
        raise Exception.Create('Timeout while waiting for image to be cached.');
      end;
    end;
  end;
End;

procedure TI2XBaseUI.acOpenExecute(Sender: TObject);
var
  sFileName, sExt : string;
begin
  if oFileDir.OpenDlgAPI( sFileName, FILEDLG_TEMPLATE_IMAGE, FILEOPENDLG_TEMPLATE_IMAGE_TEXT, '', 'tif', self.LastPathSelected ) then begin
    sExt := ExtractFileExt(sFileName);
    if ( FileExists( sFileName ) ) then
      self.LastPathSelected := ExtractFilePath( sFileName );
    if ( sExt = TEMPLATE_FILE_EXT ) then begin
      self.SetActiveTemplate( sFileName );
    end else begin    //images....
      txtActiveImage.Text := sFileName;
      ReLoadActiveImage();
      //self.SetActiveImage( sFileName );
    end;
  end;
End;

procedure TI2XBaseUI.acOpenImage1Click(Sender: TObject);
begin
  bbtnOpenImageClick( Sender );
end;

procedure TI2XBaseUI.acOpenTemplateAsXMLExecute(Sender: TObject);
begin
  if ( self.isDirty ) then
    self.acSaveExecute( self );
  ShellExecute(Handle, 'open', pchar( self.template.TemplateName ), nil,nil,SW_SHOWNORMAL);
end;

//SetCurrentPanel(const PanelDisplay : TI2XOptionsEnum )
procedure TI2XBaseUI.ShowOptions(const PanelDisplay : TI2XOptionsEnum );
Begin
  if ( frmI2XOptions = nil ) then
    frmI2XOptions := TI2XOptionsUI.Create( self )
  else
    frmI2XOptions.Show;
  frmI2XOptions.SetCurrentPanel( PanelDisplay );
End;

procedure TI2XBaseUI.acOptionsExecute(Sender: TObject);
begin
  ShowOptions();
  //frmI2XOptions.LoadOptionsAndDisplay( Options );
end;

procedure TI2XBaseUI.FillImgProcTabOrderArray();
Begin
  SetLength( arrImgProcObjectTabOrder, 20 );
  arrImgProcObjectTabOrderCount := 0;
  arrImgProcObjectTabOrder[arrImgProcObjectTabOrderCount] := self.Slider1;
  Inc( arrImgProcObjectTabOrderCount );
  arrImgProcObjectTabOrder[arrImgProcObjectTabOrderCount] := self.Combo1;
  Inc( arrImgProcObjectTabOrderCount );
  arrImgProcObjectTabOrder[arrImgProcObjectTabOrderCount] := self.Combo2;
  Inc( arrImgProcObjectTabOrderCount );
  arrImgProcObjectTabOrder[arrImgProcObjectTabOrderCount] := self.Edit1;
  Inc( arrImgProcObjectTabOrderCount );
  arrImgProcObjectTabOrder[arrImgProcObjectTabOrderCount] := self.Edit2;
  Inc( arrImgProcObjectTabOrderCount );
  arrImgProcObjectTabOrder[arrImgProcObjectTabOrderCount] := self.Edit3;
  Inc( arrImgProcObjectTabOrderCount );
  //arrImgProcObjectTabOrder[arrImgProcObjectTabOrderCount] := self.btnAddImageInst;
  SetLength( arrImgProcObjectTabOrder, arrImgProcObjectTabOrderCount );
End;

procedure TI2XBaseUI.SelectNextImgProc( ctl : TWinControl );
var
  i : byte;
  bFoundCtl : boolean;
  arr : TWinControlArray;
Begin
  bFoundCtl := false;
  arr := SortWinControlsByTopLocation( arrImgProcObjectTabOrder );
  for i := 0 to Length( arr ) - 1 do begin
    //dbg( 'arrImgProcObjectTabOrder[i].Name=' + arr[i].Name );
    //dbg( 'Parent.Name=' + arr[i].Parent.Name );
    if ( arr[i].Name = ctl.Name ) then
      bFoundCtl := true
    else if (( bFoundCtl ) and
             ( arr[i].Parent.Visible ) and
             ( arr[i].Parent.Enabled )) then begin
      arr[i].Parent.SetFocus;
      arr[i].SetFocus;
    end;
  end;

End;

procedure TI2XBaseUI.EmbedProgressBar(Sender: TObject);
//Thank you http://delphi.about.com/library/weekly/aa030805a.htm
var
  ProgressBarStyle: integer;
Begin
  self.bStatus.Panels[ Integer(sbProgressBar) ].Style := psOwnerDraw;
  self.prgbarStatus.Parent := self.bStatus;
  ProgressBarStyle := GetWindowLong(prgbarStatus.Handle,
                                    GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle
                      - WS_EX_STATICEDGE;
  SetWindowLong(prgbarStatus.Handle,
                GWL_EXSTYLE,
                ProgressBarStyle);
End;

procedure TI2XBaseUI.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  buttonSelected : Integer;
begin
  if ( self.isDirty ) then begin
    buttonSelected := MessageDlg(NOT_SAVED_TEXT_CLOSE,mtWarning,
                              [mbYes,mbNo,mbCancel], 0);
    if buttonSelected = mrYes then begin
      self.acSaveExecute( Sender );
    end else if buttonSelected = mrCancel then begin
      CanClose := false;
    end;
  end;
end;

procedure TI2XBaseUI.FormCreate(Sender: TObject);
var
  sActiveTemplateFromReg : string;
  sTemplateName : string;
Begin
  _FirstChar := TI2XOCRItem.Create();
  Initialize( Boundary );

  JobControlsEnabled( false );
  FModalDisplayFaded := true;

  FRegionRedrawPending := false;
  InFormCreateEvent := true;
  IsOverImageInst := false;

  FMemoryMapManager := TI2XMemoryMapManager.Create();
  FMainCtl := TI2XMainUIController.Create( self );
  FMainCtl.ImgView := Self.ImgView;
  FMainCtl.OnDebugMessageEvent := self.OnDebugMessage;
  FMainCtl.OnStatusChangeEvent := self.OnStatusChange;
  FMainCtl.MemoryMapManager := FMemoryMapManager;
  memImageInstDescr.Text := IMAGE_INST_DEFAULT;
  ImageInfoMessage := '';

  self.pgctlImageProcessing.ActivePageIndex := 0;
  //Application.HookMainWindow(AppHookFunc)
  self.gridActionList.RowCount := 1;
  self.gridActionList.cells[0, 0] := '';
  self.gridActionListUpsize.RowCount := 1;
  self.gridActionListUpsize.cells[0, 0] := '';

  FillImgProcTabOrderArray();

  self.FElementDisplayLevel := 0;
  self.TriggerUIEvents( false );
  FFirstCharRegionElement := nil;
  FXAxisRegionElement := nil;
  FYAxisRegionElement := nil;

  oFileDir := CFileDir.Create();
  oStrUtil := CStrUtil.Create();
  reg := TI2XReg.Create();
  template := CTemplate.Create;
  template.OnOffsetChange := OnTemplateOffsetChange;

  ActiveImageIsLoaded := false;

  ClearDirty();
  Self.SetZoom( Reg.Get( 'zoom', 1.0 ) );
  gbZoom.Position := Reg.Get( 'zoom_pos', 100 );

  SetZoom( Zoom );
  SetRegionsPopupMenuOptions( self );

  sTemplateName := Reg.Get( 'active_template', '' );
  SetActiveTemplate( sTemplateName );
  self.TriggerUIEvents( true );
  MousePosLast.X :=0;
  MousePosLast.Y :=0;

  ImageInstructionParameterDisplay();

  if ( Integer(DebugLevel) >= 1 ) then  PluginManager.OnDebug := self.OnDebug;
  PluginManager.SearchAndLoadPlugins( DLLSearchPath );
  Options.SetupDefaultTests( PluginManager );
  Options.TWAINDevices := self.Scan.SourceList;
  UpdateImageProcessingList();
  EmbedProgressBar( Sender );
  SetMainMenuOptions( Sender );

  TabsControlsEnabledCheck();
  Application.OnMessage := self.AppMessage;
  RestoreUIState();
  //RefreshImageCacheIfNeeded();
  InFormCreateEvent := false;
  undo.OnChange := OnUndoStackChange;
  undo.OnRequestRegionAction := OnRequestRegionAction;
  undo.OnRequestActionGridUpdate := OnRequestActionGridUpdate;
  undo.OnDebug := self.OnDebug;
End;

procedure TI2XBaseUI.RestoreUIState();
Begin
  self.Left := Reg.Get( 'Left', self.Left );
  self.Top := Reg.Get( 'Top', self.Top );
  self.Height := Reg.Get( 'Height', self.Height );
  self.Width := Reg.Get( 'Width', self.Width );
  self.WindowState := TWindowState( Reg.Get( 'WindowState', integer(wsNormal) ) );
  if ( self.WindowState = wsMinimized ) then
    self.WindowState := wsNormal;

  AppCache.AutoSave := true;
  LoadJobList();
  self.cboJobName.Text := AppCache.getVar('JobName');
  if ( FileExists( self.ActiveJobFileName ) ) then begin
    self.SetActiveJob( AppCache.getVar('JobName') );
  end else begin
    self.txtJobImagePath.Text := AppCache.getVar('JobImagePath');
    self.txtJobOutput.Text := AppCache.getVar('JobOutput');
    self.txtJobTemplate.Text := AppCache.getVar('JobTemplate');
    self.chkRestartable.Checked := (AppCache.getVar('JobRestartable')='Y');
    //self.cboJobName.Text := AppCache.getVar('JobName');
  end;
End;

procedure TI2XBaseUI.SaveUIState();
Begin
  if ( not InFormCreateEvent ) then begin
    Reg[ 'WindowState' ] := integer( self.WindowState );
    Reg[ 'Left' ] := self.Left;
    Reg[ 'Top' ] := self.Top;
    Reg[ 'Height' ] := self.Height;
    Reg[ 'Width' ] := self.Width;
  end;
End;

procedure TI2XBaseUI.FormDestroy(Sender: TObject);
begin
  FreeAndNil( _FirstChar );

  FreeAndNil( oFileDir );
  FreeAndNil( oStrUtil );
  FreeAndNil( reg );
  FreeAndNil( template );
  if ( templateSection <> nil  ) then FreeAndNil( templateSection  );
  FreeAndNil( FMainCtl );
  FreeAndNil( dlgOCRProductView );
  FreeAndNil( FMemoryMapManager );
  if ( FScan <> nil  ) then FreeAndNil( FScan );
end;

procedure TI2XBaseUI.FormResize(Sender: TObject);
var
  nNewLeft : integer;
begin
  dbg('FormResize - Top:' + IntToStr( self.Top ) + ', Left:' + IntToStr( self.Left ) );
  if ( self.Top < 0 ) then
    self.Top := 0;
  nNewLeft := self.Left;
  if ( (self.Left + self.Width) > Forms.Screen.DesktopWidth ) then
    nNewLeft := Forms.Screen.DesktopWidth - self.Width;
  if ( nNewLeft < 0 ) then
    nNewLeft := 0;
  self.Left := nNewLeft;
  dbg('FormResize (NEW) - Top:' + IntToStr( self.Top ) + ', Left:' + IntToStr( self.Left ) );
  SaveUIState();
End;

procedure TI2XBaseUI.FormShow(Sender: TObject);
begin
  self.ImgView.SetFocus;
end;

procedure TI2XBaseUI.gbZoomChange(Sender: TObject);
begin
  if ( self.ElementDisplayEventsAreActive ) then
    undo.Add( Sender, IntToStr( gbZoom.Position ), true );
  if ( gbZoom.Position <= 100 ) then begin
    SetZoom( gbZoom.Position / 100 );
  end else begin
    SetZoom( (( gbZoom.Position - 100 ) / 10) + 1 );
  end;
  reg.RegSet( REG_KEY, 'zoom_pos', IntToStr(gbZoom.Position), HKEY_CURRENT_USER );
end;

function TI2XBaseUI.GetActiveJobFileName: string;
begin
  if ( Length(self.cboJobName.Text) > 0 ) then
    Result := Options.JobRootPath + self.cboJobName.Text + '\' + self.cboJobName.Text + JOB_FILE_EXT
  else
    Result := '';
end;

function TI2XBaseUI.getElementDisplayActive: boolean;
begin
  Result := (self.FElementDisplayLevel = 0);
end;

function TI2XBaseUI.getFirstChar(): TI2XOCRItem;
var
  i : integer;
begin
  _FirstChar.Clear;
  TryStrToInt( self.txtFirstCharX.Text, i );
  _FirstChar.X := i;
  TryStrToInt( self.txtFirstCharY.Text, i );
  _FirstChar.Y := i;
  TryStrToInt( self.txtFirstCharWidth.Text, i );
  _FirstChar.Width := i;
  TryStrToInt( self.txtFirstCharHeight.Text, i );
  _FirstChar.Height := i;
  Result := _FirstChar;
end;

function TI2XBaseUI.getI2XScanObject: TI2XScan;
begin
  if ( FScan = nil ) then
    FScan := TI2XScan.Create( self.mcmTWAIN1 );
  Result := FScan;
end;

function TI2XBaseUI.getImageInfoMessage: string;
begin
  Result := self.pnlImageInfo.Caption;
end;

function TI2XBaseUI.getImagePixelHeight: integer;
begin
  //Result := StrToInt( template.getData('model_pixel_height'));
  Result := self.ActiveImageInfo.Height;
end;

function TI2XBaseUI.getImagePixelWidth: integer;
begin
  Result := self.ActiveImageInfo.Width;
  //Result := StrToInt( template.getData('model_pixel_width') );
end;

function TI2XBaseUI.GetIsBoundarySet: boolean;
begin
  Result := not (( self.Boundary.Right = 0 ) and ( self.Boundary.Bottom = 0 ));
end;

function TI2XBaseUI.getLastPathSelected: string;
begin
  Result := AppCache.LastFolderSearched;
end;

function TI2XBaseUI.getPercentComplete: TPercent;
Begin
  Result := FPercentComplete;
End;

procedure TI2XBaseUI.setPercentComplete(const Value: TPercent);
Begin
  if (( frmI2XDialogUI <> nil ) and ( frmI2XDialogUI.IsActive )) then
    frmI2XDialogUI.PercentComplete := Value;
  FPercentComplete := value;
  self.bStatus.Invalidate;
  self.bStatus.Repaint;
End;

function TI2XBaseUI.getSelectedActionList: TStringGrid;
begin
  if ( pgctlImageProcessing.ActivePage.Name = 'tabImgProcNormal' ) then
    Result := self.gridActionList
  else if ( pgctlImageProcessing.ActivePage.Name = 'tabImgProcUpsize' ) then
    Result := self.gridActionListUpsize;
end;

procedure TI2XBaseUI.gridActionListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //dbg('Key=' + IntTostr(Key));
end;

procedure TI2XBaseUI.gridActionListMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow: Integer;
  NyHint: string;
begin
  if TObject( Sender ).ClassName = 'TStringGrid' then begin
    with Sender as TStringGrid do
      begin
        MouseToCell(X, Y, ACol, ARow);
        if ShowHint and (ARow >= 0) and (ARow < RowCount) then
          begin
            NyHint := Cells[ACol, ARow];
            if (Hint <> NyHint) or (X <> FXold) or (Y <> FYold) then
              begin
                FXold := X;
                FYold := Y;
                Hint := NyHint;
                Application.ActivateHint(ClientToScreen(Point(X, Y)));
              end;
          end
        else
          begin
            Application.CancelHint;
          end;
      end;
  end;
end;

procedure TI2XBaseUI.OffsetRegion( const x, y : integer );
Begin
  try
    template.Offset( x, y );
  finally
  end;
End;

procedure TI2XBaseUI.ClearDirty();
Begin
  isDirty := false;
  if ( Length( self.template.TemplateName ) = 0 )  then
    self.Caption := APP_NAME + ' V.' + sVERSION + ' - '
  else
    self.Caption := APP_NAME + ' V.' + sVERSION + ' - ' + self.template.TemplateName;
  bStatus.Panels[ Integer(sbSaveInd) ].Text := '';
End;

function TI2XBaseUI.IsTemplateLoaded() : boolean;
Begin
  Result := not (( Length(template.TemplateFileName) = 0 ) or
    ( template.TemplateFileName = NEW_TEMPLATE ));
End;

procedure TI2XBaseUI.SetActiveTemplate(TemplateFileName: string);
var
  sImageFromTemplate, sTemplateFileName : string;
Begin
  sTemplateFileName := TemplateFileName;
  if ( Length(sTemplateFileName) = 0 ) then Exit;
  if ( sTemplateFileName = template.TemplateFileName ) then Exit;
  if ( sTemplateFileName = NEW_TEMPLATE ) then begin
    acNewExecute( self );
    StatusMessage('New Template is ready to be edited' );
    Exit;
  end;
  StatusMessage('Loading Template ' + ExtractFileName( sTemplateFileName ) + '...'  );
  TriggerUIEvents( false );
  sTemplateFileName := AbsolutePath( sTemplateFileName );
  if ( not FileExists( sTemplateFileName ) ) then begin
    MessageDlg('Cannot set a template to a file that does not exist', mtError,[mbOk], 0);
  end else begin
    template.OnChange := nil; //disable cascading event handlers,  since we don't care now
    CleanUpAllTemplateData();
    LoadTemplateFromFile( sTemplateFileName );
    self.ElementsToTree( self.tvRegions, self.template.Elements );
    reg.RegSet( REG_KEY, 'active_template', sTemplateFileName, HKEY_CURRENT_USER );

    with template do begin
      sImageFromTemplate := template.ImageFileName;
      DrawRegions( Elements );
      //DrawSpecialRegions();
    end;
    template.OnChange := OnTemplateChange; //re-enable event handlers

    mnuTemplateOCR.Enabled := ( template.Elements.Count > 0 );
    self.OpenAsXML1.Enabled := ( Length(template.TemplateName) > 0 );
  end;
  TriggerUIEvents( true );
  SetRegionsPopupMenuOptions( self );
  ClearDirty();
  StatusMessage('Template is ready to be edited'  );
End;

procedure TI2XBaseUI.setFirstChar( const Value : TI2XOCRItem );
begin
  _FirstChar.Assign( Value );
  TriggerUIEvents( false );
  self.txtFirstCharX.Text := IntToStr( _FirstChar.X );
  self.txtFirstCharY.Text := IntToStr( _FirstChar.Y );
  self.txtFirstCharWidth.Text := IntToStr( _FirstChar.Width );
  self.txtFirstCharHeight.Text := IntToStr( _FirstChar.Height );
  TriggerUIEvents( true );
end;

procedure TI2XBaseUI.setImageInfoMessage(const Value: string);
begin
  if ( Length( Value ) = 0 ) then begin
    self.pnlImageInfo.Visible := false;
  end else begin
    self.pnlImageInfo.Visible := true;
    self.pnlImageInfo.Caption := Value;
    if ( Pos( 'Warning:', Value ) > 0 ) then
      self.pnlImageInfo.Font.Color := clRed
    else
      self.pnlImageInfo.Font.Color := clGreen;
  end;
end;

procedure TI2XBaseUI.setLastPathSelected(const Value: string);
begin
  AppCache.LastFolderSearched := Value;
end;

procedure TI2XBaseUI.SetMainMenuOptions(Sender: TObject);
begin
  self.mnuEditUndo.Enabled := undo.UndoAvailable;
  self.mnuEditRedo.Enabled := undo.RedoAvailable;
  self.mnuEditCopy.Enabled := ( self.Selection <> nil ) or ( self.ActiveControl is TCustomEdit );
  self.mnuEditPaste.Enabled := (( self.Selection <> nil ) or
                                ( self.ActiveControl is TCustomEdit ) or
                                ( Pos(CLIPBOARD_REGION_ID, Clipboard.AsText ) = 1 ));
  self.mnuEditDelete.Enabled := ( self.Selection <> nil ) or ( self.ActiveControl is TCustomEdit );


  mnuTemplateOCR.Enabled := ( template.Elements.Count > 0 );
  self.OpenAsXML1.Enabled := ( Length(template.TemplateName) > 0 );

end;

procedure TI2XBaseUI.SetSelectedElement( eleData: CEleNodeData );
var
  tn : TTreeNode;
Begin
  if ( eleData = nil ) then begin
    //dbg( 'SetSelectedElement( nil )' );
    self.DisplaySelectedElement( nil );
    self.FSelectedElement := nil;
  end else begin
    FSelectedElementLast := FSelectedElement;
    //dbg( 'SetSelectedElement() Changed? ' + eleData.AsString() );
    if ( eleData.TreeViewNodePointer <> nil ) then begin
      tn := TTreeNode( eleData.TreeViewNodePointer );
      if ( pgctlTemplateData.TabIndex <> Integer( pgRegions ) ) then begin
        pgctlTemplateData.ActivePageIndex := Integer( pgRegions );
      end;
      if ( not InFormCreateEvent ) then self.tvRegions.SetFocus;
      tn.Selected := true;
      self.DisplaySelectedElement( eleData );
    end else if ( eleData.TreeViewNodePointer = nil ) then begin
      if ( pgctlTemplateData.TabIndex <> Integer( pgOffset ) ) then begin
        pgctlTemplateData.ActivePageIndex := Integer( pgOffset );
      end;
    end;

    self.FSelectedElement := eleData;
    OnControlEnter( FSelectedElement );
  end;
End;

procedure TI2XBaseUI.SetSelection(Value: TPositionedLayer);
var
  eleData: CEleNodeData;
  elePtr : Pointer;
begin
  if ( Value = nil ) then
  begin
    if RBLayer <> nil then
    begin
      RBLayer.ChildLayer := nil;
      RBLayer.LayerOptions := LOB_NO_UPDATE;
      ImgView.Invalidate;
      if (not InFormCreateEvent ) then
        RBLayer.Visible := false;
    end;
    FSelection := nil;

    //if (not InFormCreateEvent ) then
    //  RBLayer.Visible := false;
    SetSelectedElement( nil );
  end
  else if Value <> FSelection then
  begin
    if RBLayer <> nil then
    begin
      RBLayer.ChildLayer := nil;
      RBLayer.LayerOptions := LOB_NO_UPDATE;
      ImgView.Invalidate;
    end;

    FSelection := Value;

    if ( FSelection.Tag <> 0 ) then
    begin
      elePtr := Pointer( FSelection.Tag );
      eleData :=  CEleNodeData( elePtr );
      SetSelectedElement( eleData );
    end;

    if Value <> nil then
    begin
      if RBLayer = nil then
      begin
        RBLayer := TRubberBandLayer.Create(ImgView.Layers);
        //RBLayer.MinHeight := 5;
        //RBLayer.MinWidth := 5;
      end
      else begin
        RBLayer.BringToFront;
        RBLayer.Visible := true;
      end;
      RBLayer.ChildLayer := Value;
      RBLayer.LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS or LOB_NO_UPDATE;
      //RBLayer.OnResizing := RBResizing;
      //if the rubberband layer contains and element that does not have a treeView node pointer,
      //  Then this element must be a special elements,  handle this change differently
      if (( eleData <> nil) and ( eleData.TreeViewNodePointer = nil )) then
        RBLayer.OnUserChange := RBResizeUserChangedSpecial
      else
        RBLayer.OnUserChange := RBResizeUserChanged
      ;

      if Value is TBitmapLayer then
        with TBitmapLayer(Value) do
        begin
          //pnlBitmapLayer.Visible := True;
          //LayerOpacity.Position := Bitmap.MasterAlpha;
          //LayerInterpolate.Checked := Bitmap.Resampler.ClassType = TDraftResampler;
        end
      else if Value.Tag = 2 then
      begin
        // tag = 2 for magnifiers
        //pnlMagn.Visible := True;
      end;
    end;
  end;
end;

procedure TI2XBaseUI.TriggerUIEvents( enable : boolean );
Begin
{sub functions will want to disable UI events also,  in theory,  this will
 increment the display level for every sub function,  assuming the sub function
 will re-allow the display,  theis will decrement when it comes out of the function.  When the Level is 0, then no functions will prevent the display for triggering its events}
 
  if ( enable ) then
    DEC( self.FElementDisplayLevel )
  else
    INC( self.FElementDisplayLevel );
End;

procedure TI2XBaseUI.tvRegionsClick(Sender: TObject);
var
  eleData : CEleNodeData;
begin
  if (( tvRegions.Selected = nil ) or ( tvRegions.Selected.Data = nil )) then
    self.ClearDisplayElement()
  else begin
    eleData := CEleNodeData( tvRegions.Selected.Data );
    if ( eleData = nil ) then
      dbg('eleData = nil');
    self.DisplaySelectedElement( eleData );
    self.Selection := ( TBitmapLayer( eleData.GraphicPointer ) );
    ImgView.ScrollToCenter( eleData.x, eleData.y );
  end;
end;

procedure TI2XBaseUI.tvRegionsCompare(Sender: TObject; Node1, Node2: TTreeNode;
  Data: Integer; var Compare: Integer);
Begin
  if Node1.Text < Node2.Text then Compare:= -1
  else if Node1.Text = Node2.Text then Compare:= 0
  else Compare:= 1;
End;


procedure TI2XBaseUI.tvRegionsDblClick(Sender: TObject);
var
  eleData : CEleNodeData;
  sRegionName, sOriginalName : string;
  tn : TTreeNode;
Begin
  //dbg('tvRegionsDblClick() SelectedElement=' + self.SelectedElement.AsString() );
  if ( not (( tvRegions.Selected = nil ) or ( tvRegions.Selected.Data = nil )) ) then begin
    //dbg( ' if ( not (( tvRegions.Selected = nil ) or ( tvRegions.Selected.Data = nil )) ) then begin ' );
   self.ImgView.Layers.MouseEvents := false;
    tn := tvRegions.Selected;
    self.RenameRegionPrompt( self.SelectedElement );
    tvRegions.SetFocus;
{*
    sOriginalName := self.SelectedElement.ID;
    sRegionName :=InputBox(REGION_RENAME_CAPTION, REGION_RENAME_PROMPT,
      self.SelectedElement.ID);
    if (( length( sRegionName ) > 0 ) and ( sRegionName <> sOriginalName )) then begin

        if ( template.RenameIsValid( self.SelectedElement, sRegionName ) ) then begin

         self.SelectedElement.ID := sRegionName;
          tn := TTreeNode( SelectedElement.TreeViewNodePointer );
          tn.Text := sRegionName;
          SetSelection( nil );
          self.SetSelectedElement( nil );

          SetCursorPos(MousePosLast.X, MousePosLast.Y);
          Mouse_Event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
          Mouse_Event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);


        end else begin
          MessageDlg(Format( NEW_REGION_ERROR_EXISTS_FMT, [sRegionName] ),
                      mtError, [mbOk], 0);
        end;
    end;
    *}
    self.ImgView.Layers.MouseEvents := true;
  end;
End;

procedure TI2XBaseUI.tvRegionsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  AnItem: TTreeNode;
  AttachMode: TNodeAttachMode;
  HT: THitTests;
  eleDataTarget, eleDataSource : CEleNodeData;
  doDropOnTarget : boolean;
begin
  //template.saveAsXML('C:\Dark\pascal\Image2XML\templates\TEST_BEFORE.XML');
  doDropOnTarget := false;
  if tvRegions.Selected = nil then Exit;
  HT := tvRegions.GetHitTestInfoAt(X, Y) ;
  AnItem := tvRegions.GetNodeAt(X, Y);
  //This means we dragged over something that is not a node,  so get out!
  if ( AnItem = nil ) then
    Exit;

  eleDataTarget := CEleNodeData( AnItem.Data );
  if ( eleDataTarget.nodeType = NODE_TYPE_SINGLE ) then begin
    if MessageDlg(NODE_TYPE_CHANGE_TEXT, mtConfirmation,
            [mbYes, mbNo], 0) = mrYes then
    begin
      doDropOnTarget := true;
    end;
  end else begin
    doDropOnTarget := true;
  end;

  if (( doDropOnTarget ) and (HT - [htOnItem, htOnIcon, htNowhere, htOnIndent] <> HT)) then
  begin
    if (htOnItem in HT) or
       (htOnIcon in HT) then
        AttachMode := naAddChild
    else if htNowhere in HT then
       AttachMode := naAdd
    else if htOnIndent in HT then
       AttachMode := naInsert;
    eleDataSource := CEleNodeData( tvRegions.Selected.Data );
    eleDataSource := template.MoveElement( eleDataSource, eleDataTarget );
    //AnItem.Data := eleDataSource;
    eleDataSource.TreeViewNodePointer := tvRegions.Selected;
    tvRegions.Selected.Data := eleDataSource;
    tvRegions.Selected.MoveTo(AnItem, AttachMode);
    self.FSelectedElement := eleDataSource;
    self.DrawRegion( Sender, eleDataSource );
    self.DrawRegion( Sender, eleDataTarget );
    tvRegions.AlphaSort();
  //template.saveAsXML('C:\Dark\pascal\Image2XML\templates\TEST_AFTER.XML');
  end;
end;

procedure TI2XBaseUI.tvRegionsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  AnItem : TTreeNode;
begin
  if Source is TTreeView then begin
        Accept := true;
  end;
end;

procedure TI2XBaseUI.SetRegionsPopupMenuOptions( Sender: TObject );
Begin
  acCreateRegion.Enabled := ActiveImageIsLoaded;
  self.pmnuRegionCreate.Enabled :=
      ( ( ActiveImageIsLoaded ) );
  self.pmnuRegionRemoveFromParent.Enabled := (( self.FSelectedElement <> nil ) and
                                   (self.FSelectedElement.Parent <> nil ));

  self.pmnuOCRRegion.Enabled := ( self.FSelectedElement <> nil );
  if ( self.pmnuOCRRegion.Enabled ) then begin
    if ( FSelectedElement.isFirstChar ) then
      self.pmnuOCRRegion.Caption := 'Find First Character'
    end else begin
      self.pmnuOCRRegion.Caption := 'OCR Region';
  end;

  self.pmnuRegionDelete.Enabled := ( self.FSelectedElement <> nil );
  self.pmnuRegionRename.Enabled := ( self.FSelectedElement <> nil );
  if ( self.pmnuRegionDelete.Enabled ) then begin
    if ( FSelectedElement.isFirstChar ) then
      self.pmnuRegionDelete.Caption := 'Delete First Char Definition'
    else if ( FSelectedElement.isXAxis ) then
      self.pmnuRegionDelete.Caption := 'Delete Left Boundary Mark'
    else if ( FSelectedElement.isYAxis ) then
      self.pmnuRegionDelete.Caption := 'Delete Top Boundary Mark'
    else
      self.pmnuRegionDelete.Caption := 'Delete ''' + self.FSelectedElement.id + '''';
  end else
    self.pmnuRegionDelete.Caption := 'Delete <Select Node>';
  self.pmnuRegionCopyRight.Enabled := ( self.FSelectedElement <> nil );
  self.pmnuRegionCopyLeft.Enabled := ( self.FSelectedElement <> nil );
  self.pmnuRegionCopyAbove.Enabled := ( self.FSelectedElement <> nil );
  self.pmnuRegionCopyBelow.Enabled := ( self.FSelectedElement <> nil );


End;

procedure TI2XBaseUI.tvRegionsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   p : TPoint;
Begin
  case Button of
    mbLeft:
      begin
      end;
   mbRight:
      begin
        SetRegionsPopupMenuOptions( Sender );
      end;
  end;
End;

procedure TI2XBaseUI.txtActiveImageChange(Sender: TObject);
begin
  if ( self.ElementDisplayEventsAreActive ) then begin
    if ( FileExists(self.txtActiveImage.Text) OR ( template.TemplateName = NEW_TEMPLATE )) then begin
      reg.RegSet( REG_KEY, 'active_image', self.txtActiveImage.Text, HKEY_CURRENT_USER );
    end;
    undo.Add( Sender, TEdit(Sender).Text );
  end;
end;
procedure TI2XBaseUI.txtActiveImageDblClick(Sender: TObject);
begin
  if ( DirectoryExists( ExtractFilePath( txtActiveImage.Text ) ) ) then
    ShellExecute(Handle, 'open', pchar( ExtractFilePath( txtActiveImage.Text ) ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.txtDPIChange(Sender: TObject);
begin
  if ( not ( oStrUtil.IsNumeric( txtDPI.Lines.Text ) ) ) then begin
    txtDPI.Lines.Text := IntToStr( txtDPI.Tag );
  end;
end;

procedure TI2XBaseUI.txtDPIKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( Key = 13 ) then
    ImgView.SetFocus();
end;

procedure TI2XBaseUI.txtDPI_Enter(Sender: TObject);
begin
  with TMemo(Sender) do begin
    if ( Length( Lines.Text ) > 0 ) then
      Tag := StrToInt( Lines.Text );
  end;
  OnControlEnter(Sender);
end;

procedure TI2XBaseUI.txtDPI_Exit(Sender: TObject);
const
  DPI_CHANGE_WARNING_0 =
  'WARNING: Changing the Resolution/Dots Per Inch may have unexpected results.' + #10#13 +

  'This value is usually set by the application or scanner generating the image.' + #10#13 + #10#13 +

  'With a DPI of 0, it is most likey that the scanning/generating application did not properly ' + #10#13 +
  'write this resolution info.  If you are sure of the DPI you scanned/generated the image in, ' + #10#13 +
  'then allowing this change is acceptable as the image is probably the resolution you chose to scan at.' + #10#13 + #10#13 +

  'Are you sure you wish to change the resolution from "%u" to "%u"?';

  DPI_CHANGE_WARNING =
  'WARNING: Changing the Resolution/Dots Per Inch may have unexpected results.' + #10#13 +

  'This value is usually set by the application or scanner generating the image.' + #10#13 + #10#13 +

  'A DPI greater than 0 is most likey the images true DPI (unless it was changed by you with this this applicaiton).' + #10#13 +
  'An OCR with a low resolution (300 DPI or less) will lose accuracy.  Changing this value will not' + #10#13 +
  'improve this accuracy.  The best thing to do is to rescan the image with a higher resolution.' + #10#13 + #10#13 +

  'Are you sure you wish to change the resolution from "%u" to "%u"?';

var
  nNewDPI, nOldDPI : Cardinal;
  dwimg : TDWImage;
  sDPIWarning : string;
begin
  with TMemo(Sender) do begin
    if ( Length( Lines.Text ) > 0 ) then begin
      nNewDPI  := StrToInt( Lines.Text );
      nOldDPI := Tag;
      if ( nOldDPI <> nNewDPI ) then begin
          if ( nOldDPI = 0 ) then
            sDPIWarning := DPI_CHANGE_WARNING_0
          else
            sDPIWarning :=DPI_CHANGE_WARNING;
          if ( MessageDlg(Format( sDPIWarning, [ nOldDPI, nNewDPI ] ), mtConfirmation, [mbYes, mbNo], 0) = mrYes ) then begin
            try
              Application.ProcessMessages;
              self.StatusMessage( 'Setting Active Images Resolution' );
              dwimg := TDWImage.Create(ImgView.Bitmap);
              dwimg.setDPI( nNewDPI );
              ImgView.Bitmap.Assign( dwimg.Bitmap );
              ImageDPI( self.ActiveImageInfo.FileName, nNewDPI );
              ActiveImageInfo.DPI := nNewDPI;
              self.DPIWarning( ActiveImageInfo.DPI );
              self.StatusMessage( '' );
            finally
              FreeAndNil( dwimg );
            end;
          end else begin
            self.txtDPI.Lines.Text := IntToStr( nOldDPI );
          end; //MessageDlg
      end;
    end;
  end;
//
end;

procedure TI2XBaseUI.txtFirstCharXContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

{*
procedure TI2XBaseUI.txtGridSizeChange(Sender: TObject);
var
  GridSize : Integer;
  doChange : boolean;
Begin
  doChange := false;
  GridSize := -1;

  if ( oStrUtil.IsNumeric( txtGridSize.Text )) then begin
    GridSize := StrToInt( txtGridSize.Text );
    doChange := (( GridSize >= 1 ) and ( GridSize <= 50 ));
  end;

  if ( doChange ) then begin
    reg.RegSet( REG_KEY, 'grid_size', txtGridSize.Text, HKEY_CURRENT_USER );
    FGridSize := StrToInt( txtGridSize.Text );
  end else begin
    txtGridSize.Text := reg.RegGet( REG_KEY, 'grid_size', '5', HKEY_CURRENT_USER );
  end;
End;

procedure TI2XBaseUI.txtGridSizeKeyPress(Sender: TObject; var Key: Char);
begin
  if not( Key in ['0'..'9'] ) then Key := #0;
end;
*}
procedure TI2XBaseUI.KeyPressOnlyAllowNumeric(Sender: TObject; var Key: Char);
begin
  if not( (Key in ['0'..'9']) or ( Key = #8 ) ) then Key := #0;
end;

procedure TI2XBaseUI.txtIdChange(Sender: TObject);
begin
  if ( ElementDisplayEventsAreActive ) then begin
    self.tvRegions.Selected.Text := self.txtId.Text;
    self.SelectedElement.id := self.txtId.Text;
    self.TemplateUIUpdated;
  end;
end;

procedure TI2XBaseUI.txtImageFileNameChange(Sender: TObject);
begin
  if ( self.ElementDisplayEventsAreActive ) then begin
    template.ImageFileName := txtImageFileName.Text;
    undo.Add( Sender, TEdit(Sender).Text );
    self.TemplateUIUpdated;
  end;
end;

procedure TI2XBaseUI.txtImageFileNameDblClick(Sender: TObject);
Var
  sPath : string;
begin
  sPath := ExtractFilePath( txtImageFileName.Text );
  if (( Length( sPath ) = 0 ) and ( IsTemplateLoaded())) then begin
    sPath := ExtractFilePath( template.TemplateFileName );
  end;
  if ( DirectoryExists( sPath ) ) then
    ShellExecute(Handle, 'open', pchar( sPath ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.JobControlsEnabled( enabled : boolean );
Begin
  // Job Name
  self.btnJobDelete.Enabled := FileExists(ActiveJobFileName);
  self.btnJobSave.Enabled := (Enabled and
              ( DirectoryExists(self.txtJobImagePath.Text )) and
              ( DirectoryExists(self.txtJobOutput.Text )));

  // Template
  lblJobTemplate.Enabled := Enabled;
  btnJobTemplateSelect.Enabled := Enabled;
  txtJobTemplate.Enabled := Enabled;

  // Image Path
  btnSetImagePath.Enabled := Enabled;
  self.txtJobImagePath.Enabled := Enabled;
  lblImagePath.Enabled := Enabled;

  // Output Path
  lblOutputPath.Enabled := Enabled;
  self.btnBrowserForOutputFolder.Enabled := Enabled;
  txtJobOutput.Enabled := Enabled;

  //Restartable
  chkRestartable.Enabled := Enabled;

  self.btnJobSave.Enabled := (Enabled and
              ( Length( cboJobName.Text ) > 0 ) and
              ( FileExists( txtJobTemplate.Text )) and
              ( DirectoryExists( txtJobImagePath.Text ) ));
End;

procedure TI2XBaseUI.txtJobOutputChange(Sender: TObject);
begin
  if ( not InFormCreateEvent ) then begin
    JobControlsEnabled( true );
    undo.Add( Sender, TEdit(Sender).Text );
    AppCache.setVar( 'JobOutput', txtJobOutput.Text );
  end;
end;

procedure TI2XBaseUI.txtJobOutputDblClick(Sender: TObject);
begin
  if ( DirectoryExists( txtJobOutput.Text ) ) then
    ShellExecute(Handle, 'open', pchar( txtJobOutput.Text ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.txtJobTemplateChange(Sender: TObject);
begin
  if ( not InFormCreateEvent ) then begin
    JobControlsEnabled( true );
    undo.Add( Sender, TEdit(Sender).Text );
    AppCache.setVar( 'JobTemplate', txtJobTemplate.Text );
  end;
end;

procedure TI2XBaseUI.txtJobImagePathChange(Sender: TObject);
begin
  if ( not InFormCreateEvent ) then begin
    JobControlsEnabled( true );
    undo.Add( Sender, TEdit(Sender).Text );
    AppCache.setVar( 'JobImagePath', txtJobImagePath.Text );
  end;
end;

procedure TI2XBaseUI.txtJobImagePathDblClick(Sender: TObject);
begin
  //if ( FileExists( txtActiveImage.Text ) ) then
  if ( DirectoryExists( txtJobImagePath.Text ) ) then
    ShellExecute(Handle, 'open', pchar( txtJobImagePath.Text ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.txtTemplateDescriptionChange(Sender: TObject);
begin
  if ( self.ElementDisplayEventsAreActive ) then begin
    template.setData('description', txtTemplateDescription.Lines.Text);
    undo.Add( Sender, TMemo(Sender).Lines.Text );
    self.TemplateUIUpdated;
  end;
end;

procedure TI2XBaseUI.txtTemplateNameChange(Sender: TObject);
begin
  if ( self.ElementDisplayEventsAreActive ) then begin
    template.TemplateName := txtTemplateName.Text;
    template.setData('name', txtTemplateName.Text);
    undo.Add( Sender, TEdit(Sender).Text );
    //UpdateUndoItemFocus(txtTemplateName, txtTemplateName.Text );
    self.TemplateUIUpdated;
  end;
end;

procedure TI2XBaseUI.UpdateDisplayRegion(Sender: TObject);
var
  tn : TTreeNode;
  eleData : CEleNodeData;
  txt : TEdit;
Begin
  if ( ElementDisplayEventsAreActive ) then begin
    txt := TEdit( Sender );
    //self.SelectedElement.X :=  StrToInt( self.txtX.Text );
    //self.SelectedElement.Y :=  StrToInt( self.txtY.Text );
    //self.SelectedElement.Width :=  StrToInt( self.txtWidth.Text );
    //self.SelectedElement.Height :=  StrToInt( self.txtHeight.Text );
    RegionAttributeChange( self.SelectedElement,
            Rect(StrToInt(self.txtX.Text),
                 StrToInt(self.txtY.Text),
                 StrToInt(self.txtX.Text) + StrToInt( self.txtWidth.Text ),
                 StrToInt(self.txtY.Text) + StrToInt( self.txtHeight.Text )
                 ));
    self.TemplateUIUpdated;
    self.DrawRegion( Sender, SelectedElement);
  end;
End;

procedure TI2XBaseUI.UpdateDisplayRegionSpecialFirstChar(Sender: TObject);
var
  eleData : CEleNodeData;
  txt : TEdit;
Begin
  if (( ElementDisplayEventsAreActive ) and (FFirstCharRegionElement <> nil )) then begin
    txt := TEdit( Sender );
    txt.Undo;
    self.FFirstCharRegionElement.X :=  StrToInt( self.txtFirstCharX.Text );
    self.FFirstCharRegionElement.Y :=  StrToInt( self.txtFirstCharY.Text );
    self.FFirstCharRegionElement.Width :=  StrToInt( self.txtFirstCharWidth.Text );
    self.FFirstCharRegionElement.Height :=  StrToInt( self.txtFirstCharHeight.Text );

    self._FirstChar.X := self.FFirstCharRegionElement.X;
    self._FirstChar.Y := self.FFirstCharRegionElement.Y;
    self._FirstChar.Width := self.FFirstCharRegionElement.Width;
    self._FirstChar.Height := self.FFirstCharRegionElement.Height;

    self.DrawRegion( Sender, FFirstCharRegionElement, true, FFirstCharRegionElement.FillColor );
    undo.Add( Sender, TEdit(Sender).Text );
    self.TemplateUIUpdated;
  end;
End;

procedure TI2XBaseUI.UpdateDisplayRegionSpecialBoundary(Sender: TObject);
var
  eleData : CEleNodeData;
  txt : TEdit;
Begin
  if ( ElementDisplayEventsAreActive ) then begin
    txt := TEdit( Sender );
    if (( txt.Name = 'txtXBound' ) and ( FXAxisRegionElement <> nil )) then begin
      FYAxisRegionElement.X := StrToInt( txt.Text );
      self.DrawRegion( Sender, FXAxisRegionElement, true, FYAxisRegionElement.FillColor );
      self.TemplateUIUpdated;
    end;
    if (( txt.Name = 'txtYBound' ) and ( FYAxisRegionElement <> nil )) then begin
      FYAxisRegionElement.Y := StrToInt( txt.Text );
      self.DrawRegion( Sender, FYAxisRegionElement, true, FYAxisRegionElement.FillColor );
      self.TemplateUIUpdated;
    end;
  end;
End;

procedure TI2XBaseUI.ElementDisplayOnExit(Sender: TObject);
begin
  UpdateDisplayRegion( Sender );
end;

procedure TI2XBaseUI.ElementDisplayOnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( Key = 13 ) then
    UpdateDisplayRegion( Sender );
end;

procedure TI2XBaseUI.TemplateUIUpdated();
begin
  isDirty := true;
  self.Caption := APP_NAME + ' ' + sVERSION + ' - *' + self.template.TemplateName;
  self.acSave.Enabled := true;
  self.acSaveAs.Enabled := true;
  bStatus.Panels[ Integer(sbSaveInd) ].Text := uImage2XML.UNSAVED_INDICATOR;
end;

procedure TI2XBaseUI.TimerStatusBarTimer(Sender: TObject);
Begin
  if ( self.PercentComplete >= 90 ) then
    self.PercentComplete := 10
  else
    self.PercentComplete := self.PercentComplete + 10;
End;

procedure TI2XBaseUI.SetActiveImage( sImageFile: string );
Begin
  //self.txtActiveImage.Text := sImageFile;
  //self.txtActiveImage.Text := ExtractRelativePath( AppPath, sImageFile );
  self.txtActiveImage.Text := sImageFile;

  if ( Length(sImageFile) = 0 ) then begin
    FreeAndNil( RBLayer );
    ImgView.Bitmap.Clear();
    ActiveImageIsLoaded := false;
    self.ImgView.Layers.Clear;
    ControlsEnableImageProc( false );
  end else begin
    //if ( self.IsTemplateLoaded() ) then begin
      //self.ReLoadActiveImage();
    //end else begin
      bbtnRefresh.Visible := false;
      LoadActiveImage( sImageFile );
      SetRegionsPopupMenuOptions( self );
    //end;
  end;
End;

procedure TI2XBaseUI.DPIWarning( DPI : integer);
Begin
  if ( DPI < 300 ) then
    self.ImageInfoMessage := 'Warning: Image2XML Recommends a DPI of 300, this image has a DPI of ' + IntToStr(DPI) + '.  Click here for more info'
  else
    self.ImageInfoMessage := '';
End;

procedure TI2XBaseUI.ReLoadActiveImage();
var
  img : TMCMImage;
  bmpWork : TBitmap;
  sImageFile : string;
  dpi : TPoint;
Begin
  sImageFile := AbsolutePath( txtActiveImage.Text );
  if (IsValidI2XImage( sImageFile )) then begin
    try
      bmpWork := TBitmap.Create();
      img := TMCMImage.Create;
      try
        img.FileOpen( sImageFile );

        ActiveImageInfo.FileName := sImageFile;
        ActiveImageInfo.Width := img.Width;
        ActiveImageInfo.Height := img.Height;
        ActiveImageInfo.DPI := Round( img.XResolution / DPM_TO_DPI );
        if ( ActiveImageInfo.DPI = 0 ) then begin
          dpi := ImageDPI( sImageFile );
          ActiveImageInfo.DPI := dpi.X;
        end;
        dbg('ReLoadActiveImage(): ActiveImageInfo - FileName="' + ActiveImageInfo.FileName + '" Width=' + IntToStr( ActiveImageInfo.Width ) + ' Height=' + IntToStr(ActiveImageInfo.Height) + ' DPI= ' + IntToStr(ActiveImageInfo.DPI) );
        self.DPIWarning( ActiveImageInfo.DPI );
        txtDPI.Text := IntToStr( ActiveImageInfo.DPI );

        bmpWork.Handle := img.ReleaseHandle;
        with ImgView do
        try
          Bitmap.Assign( bmpWork );
          FMainCtl.CacheImageExpire();
          ControlsEnableImageProc( true );
          //self.bbtnRefresh.Visible := false;
          ActiveImageIsLoaded := true;
        finally
        end;
      finally
        FreeAndNil(img);
      end;
    finally
      FreeAndNil( bmpWork );
    end;
  end;
End;

procedure TI2XBaseUI.LoadActiveImage(sImageFile: string;
    setTemplateImageAttrib : boolean );
var
  img : TMCMImage;
  bmpWork : TBitmap;
  dpi : TPoint;
begin
  try
    bmpWork := TBitmap.Create();
    if (IsValidI2XImage( sImageFile )) then begin
      img := TMCMImage.Create;
      try
        img.FileOpen( sImageFile );
        //dpi := ImageDPI( sImageFile );

        ActiveImageInfo.FileName := sImageFile;
        ActiveImageInfo.Width := img.Width;
        ActiveImageInfo.Height := img.Height;
        ActiveImageInfo.DPI := Round( img.XResolution / DPM_TO_DPI );
        if ( ActiveImageInfo.DPI = 0 ) then begin
          dpi := ImageDPI( sImageFile );
          ActiveImageInfo.DPI := dpi.X;
        end;
        dbg('ReLoadActiveImage(): ActiveImageInfo - FileName="' + ActiveImageInfo.FileName + '" Width=' + IntToStr( ActiveImageInfo.Width ) + ' Height=' + IntToStr(ActiveImageInfo.Height) + ' DPI= ' + IntToStr(ActiveImageInfo.DPI) );
        self.DPIWarning( ActiveImageInfo.DPI );
        txtDPI.Text := IntToStr( ActiveImageInfo.DPI );

        if ( setTemplateImageAttrib ) then begin
          template.setData('model_pixel_width', img.Width );
          template.setData('model_pixel_height', img.Height );
          if ( img.XResolution = 0 ) then
            template.setData('x_dpi', 0 )
          else
            template.setData('x_dpi', Round( img.XResolution / DPM_TO_DPI ) );
          if ( img.YResolution = 0 ) then
            template.setData('y_dpi', 0 )
          else
            template.setData('y_dpi', Round( img.YResolution / DPM_TO_DPI ) );
          if ( img.XResolution = img.YResolution ) then
            template.setData('doc_dpi', Round( img.XResolution / DPM_TO_DPI ) )
          else
            template.setData('doc_dpi', 0 );
        end;
        bmpWork.Handle := img.ReleaseHandle;
      finally
        FreeAndNil(img);
      end;
      with ImgView do
      try
        Selection := nil;
        RBLayer := nil;
        Layers.Clear;
        ///Scale := 1;
        SetZoom( Zoom );
        Bitmap.Assign( bmpWork );
        ControlsEnableImageProc( true );

        ActiveImageIsLoaded := true;
      finally
      end;
    end;
  finally
    FreeAndNil( bmpWork );
  end;
end;

procedure TI2XBaseUI.ErrorDisplay(procedureName, msg : string);
Begin
  raise Exception.Create( msg + ' ' + procedureName );
  //fMain.StatusMessage( ErrorString + ' (Error No:' + IntToStr(ErrorNo) + ')' , pgDisable );
End;

procedure TI2XBaseUI.Exit1Click(Sender: TObject);
begin
  self.Close;
end;

procedure TI2XBaseUI.LoadTemplateFromFile(sFileName : string);
begin
  if not FileExists(sFileName) then begin
    ErrorDisplay( 'TRegionEditorUI.LoadTemplateFromFile(' + sFileName + ')', 'Template does not exist');
  end else begin
    self.processXMLTemplate( sFileName );
    self.SetActiveImage( template.ImageFileName );
    FillTemplateVars();
  end;
end;

procedure TI2XBaseUI.ImgProcParmClear();
var
  i : integer;
Begin
  for i:=tabImage.ControlCount-1 downto 0 do
    if (tabImage.Controls[i].Tag = 1) then
      tabImage.Controls[i].Visible := false;
End;

function TI2XBaseUI.ImgProcParmString( var ParmString : string ) : boolean;
var
  iParmPanels, iControls : integer;
  pnl : TPanel;
  txt : TEdit;
  lbl, lblId : TLabel;
  edt : TEdit;
  sld : TGaugeBar;
  cbo : TComboBox;
  sKey, sVal, sErr : string;
  sRes : TStringBuilder;
Begin
  try
    Result := false;
    sErr := '';
    sRes := TStringBuilder.Create();
    for iParmPanels := tabImage.ControlCount-1 downto 0 do begin

      if ((tabImage.Controls[iParmPanels].Tag = 1) and (tabImage.Controls[iParmPanels].Visible)) then begin
        //tabImage.Controls[iParmPanels].Visible := false;
        if ( TObject(tabImage.Controls[iParmPanels]).ClassName = 'TPanel') then begin
          sKey := '';
          sVal := '';
          pnl := TPanel(tabImage.Controls[iParmPanels]);

          for iControls := pnl.ControlCount-1 downto 0 do begin
            if ( TObject(pnl.Controls[iControls]).ClassName = 'TLabel' ) then begin
              lbl := TLabel(TObject(pnl.Controls[iControls]));
              if ( lbl.Tag = 2 ) then begin
                  sKey := lbl.Caption;
                end;
            end;
            if ( TObject(pnl.Controls[iControls]).ClassName = 'TComboBox' ) then begin
              cbo := TComboBox(TObject(pnl.Controls[iControls]));
              sVal := cbo.Text;
            end;
            if ( TObject(pnl.Controls[iControls]).ClassName = 'TEdit' ) then begin
              edt := TEdit(TObject(pnl.Controls[iControls]));
              sVal := edt.Text;
            end;
            if ( TObject(pnl.Controls[iControls]).ClassName = 'TGaugeBar' ) then begin
              sld := TGaugeBar(TObject(pnl.Controls[iControls]));
              sVal := IntToStr( sld.Position );
            end;
          end;
          if ( Length( sVal ) = 0 ) then begin
            sErr := sErr + #13#10 + '"' + sKey + '" requires a value.';
          end;

        end;
        if (( Length( sKey ) > 0 ) and ( Length( sVal ) > 0 )) then begin
          if ( sRes.Length > 0 )  then
            sRes.Append( IMGPROC_PARM_SEP );
          sRes.Append( sKey );
          sRes.Append( '=' );
          sRes.Append( sVal );
        end;

      end;

    end;
    if ( Length( sErr ) > 0 )  then begin
      sErr := 'Please fix the following issues before adding this image instruction:'
          + #13#10 + #13#10  + sErr;
    end else begin
      Result := true;
      ParmString := sRes.ToString();
    end;
  finally
    FreeAndNil( sRes );
  end;
End;

function TI2XBaseUI.ImgProcCreateParmText( const ID, lblText, edtText : string ) : TPanel;
var
  pnl : TPanel;
  txt : TEdit;
  lbl, lblId : TLabel;
Begin
  if ( pnlText1.Visible = false ) then begin
    pnl := pnlText1;
    txt := Edit1;
    lbl := lblText1;
    lblId := lblTextID1;
  end else if ( pnlText2.Visible = false ) then begin
    pnl := pnlText2;
    txt := Edit2;
    lbl := lblText2;
    lblId := lblTextID2;
  end else if ( pnlText3.Visible = false ) then begin
    pnl := pnlText3;
    txt := Edit3;
    lbl := lblText3;
    lblId := lblTextID3;
  end;
  pnl.Visible := true;
  pnl.Enabled := true;
  txt.Text := edtText;
  lbl.Caption := lblText;
  lblID.Caption := ID;
End;

function TI2XBaseUI.ImgProcCreateParmSlider( const min, max, default, value : integer; const ID, LabelText : string ) : TPanel;
var
  pnl : TPanel;
  lbl, lblLevel, lblID : TLabel;
  Slider : TGaugeBar;
Begin
  if ( pnlSlider.Visible = false ) then begin
    pnl := self.pnlSlider;
    Slider := Slider1;
    lblLevel := lblLevel1;
    lbl := lblSlider1;
    lblID := lblSliderID1;
  end;
  pnl.Visible := true;
  pnl.Enabled := true;
  lbl.Caption := LabelText;
  lblLevel.Caption := '';
  lblID.Caption := ID;
  Slider.Min := min;
  Slider.Max := max;
  Slider.Position := default;
End;

function TI2XBaseUI.ImgProcCreateParmCombo( const ID, LabelText, SelectedValue : string; OptionList : TStrings ) : TPanel;
var
  pnl : TPanel;
  lbl, lblID : TLabel;
  cbo : TComboBox;
Begin
  if ( pnlCombo1.Visible = false ) then begin
    pnl := pnlCombo1;
    cbo := Combo1;
    lbl := lblCombo1;
    lblId := lblComboID1;
  end else if ( pnlCombo2.Visible = false ) then begin
    pnl := pnlCombo2;
    cbo := Combo2;
    lbl := lblCombo2;
    lblId := lblComboID2;
  end;
  pnl.Visible := true;
  pnl.Enabled := true;
  lbl.Caption := LabelText;
  lblID.Caption := ID;
  cbo.Items.Clear;
  cbo.Items.AddStrings( OptionList );
End;

procedure TI2XBaseUI.ImgProcRender( oImgParm : TI2XImgParm );
var
  ParmText : TI2XImgParmText;
  ParmCbo  : TI2XImgParmCombo;
  ParmSlider : TI2XImgParmSlider;
  sl : TStringList;
  i, iParms, iOpt : integer;
  pnl : TPanel;
  sClassName : string;

Begin
  sClassName := TObject( oImgParm ).ClassName;
  if ( sClassName = 'TI2XImgParmText' ) then begin
      ParmText := TI2XImgParmText(oImgParm);
      ImgProcCreateParmText( ParmText.ID, ParmText.LabelText, '' );

  end else if ( sClassName = 'TI2XImgParmSlider' ) then begin
      ParmSlider := TI2XImgParmSlider(oImgParm);
      ImgProcCreateParmSlider( ParmSlider.Min, ParmSlider.Max,
          ParmSlider.DefaultValue, 128, ParmSlider.ID, ParmSlider.LabelText );

  end else if ( sClassName = 'TI2XImgParmCombo' ) then begin
      ParmCbo := TI2XImgParmCombo(oImgParm);
      try
        sl := TStringList.Create;
        KeysAsStringList( ParmCbo.Options, sl );
        ImgProcCreateParmCombo( ParmCbo.ID, ParmCbo.LabelText, '' , sl );
      finally
        FreeAndNil( sl );
      end;
  end;
End;

procedure TI2XBaseUI.ImgProcSliderChange(Sender: TObject);
var
  lbl : TLabel;
  Slider : TGaugeBar;
begin
  Slider := TGaugeBar( Sender );
  if ( Slider.Tag <> 0 ) then begin
    lbl := TLabel( Pointer( Slider.Tag ) );
    lbl.Caption := IntToStr( Slider.Position );
  end;
end;

procedure TI2XBaseUI.lstInstAvailableClick(Sender: TObject);
var
  arrParmKeyList, arrOptKeyList : TKeyList;
  sParmKey : string;
  oImgParm : TI2XImgParm;
  ParmText : TI2XImgParmText;
  ParmCbo  : TI2XImgParmCombo;
  ParmSlider : TI2XImgParmSlider;
  i, iParms, iOpt : integer;
Begin
  if ( lstInstAvailable.ItemIndex > -1 ) then begin
    FSelectedImgInst := TI2XImageInstruction(lstInstAvailable.Items.Objects[lstInstAvailable.ItemIndex]);
    memImageInstDescr.Text := FSelectedImgInst.Description;
    ImgProcParmClear();

    arrParmKeyList := FSelectedImgInst.ImgParms.Keys;

    for iParms := Length(arrParmKeyList) - 1 downto 0 do begin
      sParmKey := arrParmKeyList[iParms];
      //dbg( TObject(oImgInst.ImgParms.Items[ sParmKey ]).ClassName );
      oImgParm := TI2XImgParm( FSelectedImgInst.ImgParms.Items[ sParmKey ] );
      ImgProcRender( oImgParm );
    end;
    self.Repaint;
  end;
End;

function TI2XBaseUI.CreatePositionedLayer: TPositionedLayer;
var
  P: TPoint;
begin
  // get coordinates of the center of viewport
  with ImgView.GetViewportRect do
    P := ImgView.ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));

  Result := TPositionedLayer.Create(ImgView.Layers);
  Result.Location := FloatRect(P.X - 32, P.Y - 32, P.X + 32, P.Y + 32);
  Result.Scaled := True;
  Result.MouseEvents := True;
  //Result.OnMouseDown := LayerMouseDown;
end;

procedure TI2XBaseUI.ProcessXMLTemplate(const sFile : TFileName );
Begin
  self.ProcessXMLTemplate( sFile, '' );
End;

procedure TI2XBaseUI.ProcessXMLTemplate(const sFile, vImageFileName: TFileName );
var
  sDialogResponse, sImageFileName, sTargetImageFileName : string;
Begin
      if ( FileExists(sFile) ) then begin
        template.loadFromFile( sFile );
        if Length(vImageFileName) > 0 then begin
          sImageFileName := vImageFileName;
        end else begin
          sImageFileName := template.ImageFileName;
          if Length(ExtractFilePath( sImageFileName )) = 0 then
            sImageFileName := ExtractFilePath( sFile ) + sImageFileName;
        end;
        sCurrentImageFileName := sImageFileName;
        if ( Length( sImageFileName ) = 0  ) then begin
          if MessageDlg('There is not a image_file_name specified on the template.  Would you like to assign one to this template now?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

            if oFileDir.OpenDlgAPI( sDialogResponse, FILEDLG_IMAGE, FILEOPENDLG_IMAGE_TEXT, '', 'tif' ) then begin
              sTargetImageFileName := sDialogResponse;

              sImageFileName := ExtractFileDir( sFile ) + '\' + ExtractFileName( sTargetImageFileName );
              //sImageFileName := ExtractRelativePath( GetCurrentDir(), sImageFileName );
              sImageFileName := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sImageFileName );
              CopyFile( PChar( sTargetImageFileName), PChar( sImageFileName ), false);

              template.ImageFileName := sImageFileName;
              FModified := true;
            end;
          end;
        end;
        if ( FileExists( sImageFileName ) ) then begin
          sCurrentImageFileName := sImageFileName;
        end else begin
          //raise Exception.Create('Image file ' + sImageFileName + ' does not exist.');
          MessageDlg(Format( IMAGE_DNE_FMT, [sImageFileName] ), mtError,[mbOk], 0);
        end;
      end;
End;

procedure TI2XBaseUI.ImgProcControlKeyPress(Sender: TObject; var Key: Char);
begin
   If Key = #13 Then Begin
    If HiWord(GetKeyState(VK_SHIFT)) <> 0 then
      SelectNextImgProc( Sender as TWinControl )
    else
      SelectNextImgProc( Sender as TWinControl );
    Key := #0
   end;
end;

procedure TI2XBaseUI.CommitDocCorrectToTemplate();
var
  docFirstChar : TDOCFirstCharacter;
  docOffset : TDOCOffset;
  docBoundary : TDOCBoundary;
Begin
  if ( self.rbtnNoDocCorrection.Checked ) then begin
    template.DocumentOffsetCorrectionType := ocNone;
    //template.DocumentOffsetCorrection := nil;
  end else if ( self.rbtnFirstChar.Checked ) then begin
    template.DocumentOffsetCorrectionType := ocFirstChar;
  end else if ( self.rbtnXYBoundary.Checked ) then begin
    template.DocumentOffsetCorrectionType := ocBoundary;
  end;
End;

procedure TI2XBaseUI.FillTemplateVars();
var
  i,j  : integer;
  sFieldName, str : string;
  docFirstChar : TDOCFirstCharacter;
  docOffset : TDOCOffset;
  docBoundary : TDOCBoundary;
  eleData : CEleNodeData;
Begin
  try
    FirstChar.Assign( self.template.FirstChar );
    Boundary := self.template.Boundary;
    self.TriggerUIEvents( false );
        //eleData := CEleNodeData( template.getElement( 'date_time' ) );
        //DebugString( eleData.id );
    ClearTemplateUIElements();

    self.txtTemplateDescription.Lines.Text := template.getData('description');
    self.txtTemplateName.Text := template.getData('name');
    //self.txtImageFileName.Text := ExtractRelativePath( GetCurrentDir(), template.getData('image_file_name'));
    self.txtImageFileName.Text := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), template.ImageFileName );

    self.ActionListFromString(self.gridActionList, template.ImageProcessingInstructionString );
    self.ActionListFromString(self.gridActionListUpsize,
      template.ImageProcessingInstructionUpsizeString );

    txtFirstCharX.Text := IntToStr( self.template.FirstChar.X );
    txtFirstCharY.Text := IntToStr( self.template.FirstChar.Y );
    txtFirstCharWidth.Text := IntToStr( self.template.FirstChar.Width );
    txtFirstCharHeight.Text := IntToStr( self.template.FirstChar.Height );

    txtXBound.Text := IntToStr( self.template.Boundary.Left );
    txtYBound.Text := IntToStr( self.template.Boundary.Top );

    //self.FMainCtl.OnImageAttributesCalculated( self, self.template.Boundary,
    //  self.template.FirstChar );
    if ( self.template.DocumentOffsetCorrectionType = ocFirstChar ) then begin
      self.rbtnFirstChar.Checked := true;
      self.SelectDocumentCorrection( rbtnFirstChar.tag );
      //self.txtFirstCharX.Text := IntToStr( FirstChar.X );
      //self.txtFirstCharY.Text := IntToStr( FirstChar.Y );
      //self.txtFirstCharWidth.Text := IntToStr( FirstChar.Width );
      //self.txtFirstCharHeight.Text := IntToStr( FirstChar.Height );
    end else if ( self.template.DocumentOffsetCorrectionType = ocBoundary ) then begin
      self.rbtnXYBoundary.Checked := true;
      self.SelectDocumentCorrection( rbtnXYBoundary.tag );
      //self.txtXBound.Text := IntToStr( Boundary.Top );
      //self.txtYBound.Text := IntToStr( Boundary.Left );
    end;
  finally
    self.TriggerUIEvents( true );
  end;
End;

procedure TI2XBaseUI.acRegionCopyAboveExecute(Sender: TObject);
begin
  self.SetSelectedElement( CopyRegion( self.SelectedElement, cpAbove ));
end;

procedure TI2XBaseUI.acRegionCopyBelowExecute(Sender: TObject);
begin
  self.SetSelectedElement( CopyRegion( self.SelectedElement, cpBelow ));
end;

procedure TI2XBaseUI.acRegionCopyLeftExecute(Sender: TObject);
begin
  self.SetSelectedElement( CopyRegion( self.SelectedElement, cpLeft ));
end;

procedure TI2XBaseUI.acRegionCopyRightExecute(Sender: TObject);
begin
  self.SetSelectedElement( CopyRegion( self.SelectedElement, cpRight ));
end;

procedure TI2XBaseUI.acRegionLevelUpExecute(Sender: TObject);
var
  tn, CurrentItem, DeleteItem, NextItem, DropItem : TTreeNode;
  eleDataSource, ParentEle : CEleNodeData;
begin
  ParentEle := nil;
  eleDataSource := CEleNodeData( self.tvRegions.Selected.Data );
  if ( eleDataSource.Parent <> nil ) then
    ParentEle := CEleNodeData( eleDataSource.Parent );

  template.MoveElement( eleDataSource );
  CurrentItem := tvRegions.Selected;
  DeleteItem := CurrentItem;
  NextItem := tvRegions.Items.Insert( tvRegions.Items[0], CurrentItem.Text );

  tvRegions.Items.Delete(CurrentItem);
  NextItem.Data := eleDataSource;  //give the node visibility to the Template Element object
  eleDataSource.TreeViewNodePointer := NextItem;  //give Template element visibility to the node

  tvRegions.AlphaSort(false);
end;

procedure TI2XBaseUI.acRegionRenameExecute(Sender: TObject);
begin
  //self.RenameRegion( Sender );
  if ( self.FSelectedElement <> nil ) then
    self.RenameRegionPrompt( self.FSelectedElement );
end;

procedure TI2XBaseUI.acSaveAsExecute(Sender: TObject);
var
  bDoSave : boolean;
  sFileName : string;
Begin
  bDoSave := true;
  if oFileDir.SaveDlgAPI( sFileName, FILEDLG_TEMPLATE, FILESAVEDLG_TEMPLATE_TEXT,
      '', TEMPLATE_FILE_EXT, AppCache.LastUserFolderSearched ) then
  begin
    if ( FileExists(sFileName) ) then
      bDoSave := (MessageDlg(FILE_EXISTS_TEXT, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    if ( bDoSave ) then begin
      StatusMessage( 'Saving Template to file ' + sFileName );
      AppCache.LastUserFolderSearched := ExtractFilePath( sFileName );
      Save( sFileName );
      StatusMessage( 'Template has been saved to ' + sFileName);
    end else begin
      StatusMessage( 'Save Cancelled by user.');
    end;
  end;
End;

procedure TI2XBaseUI.StatusMessage(const msg: string; const ProgressBarState : TProgressBarState; const MainTitle : string );
Begin
  bStatus.Panels[ Integer(sbStatusText) ].Text := msg;
  if (ProgressBarState = pgDisable) then begin
    self.PercentComplete := 0;
    self.ProgressBarStop();
    if (( frmI2XDialogUI <> nil ) and ( frmI2XDialogUI.IsActive )) then frmI2XDialogUI.Hide;
  end else if (ProgressBarState = pgStart) then begin
    self.PercentComplete := 10;
    self.ProgressBarStart();
    if (( FModalDisplayFaded ) and ( frmI2XDialogUI <> nil )) then begin
      frmDimmer.Display;
      if ( MainTitle <> IGNORE ) then
        frmI2XDialogUI.Caption := MainTitle;
      frmI2XDialogUI.MainTitle := '   ';
      frmI2XDialogUI.ShowDialog( msg, [mbCancel], mbCancel );
      frmI2XDialogUI.ShowProgressBar := true;
      frmI2XDialogUI.PercentComplete := 10;
      frmI2XDialogUI.ShowAgainCheckVisible := false;
      frmI2XDialogUI.OnHide := OnDialogReturn;
      frmI2XDialogUI.OnCancel := OnProgressCancel;
      frmI2XDialogUI.BringToFront;
    end;
  end else if (ProgressBarState = pgStop) then begin
    self.ProgressBarStop();
    if (( frmI2XDialogUI <> nil ) and ( frmI2XDialogUI.IsActive )) then frmI2XDialogUI.Hide;
  end;
  if (( frmI2XDialogUI <> nil ) and ( frmI2XDialogUI.IsActive )) then begin
    frmI2XDialogUI.DialogText := msg;
    if ( MainTitle <> IGNORE ) then
      frmI2XDialogUI.Caption := MainTitle;
    frmI2XDialogUI.Refresh;
    frmI2XDialogUI.BringToFront;
    Application.ProcessMessages;
  end;
End;

function TI2XBaseUI.Save( sFileName : TFileName ) : boolean;
var
  sNewImageFileName, sSourceImageFileName, sTemplateFileShortName, sTemplateImageFileName : string;
  bCopyImage : boolean;
  res : integer;
Begin
  Result := false;
  if (( Length(template.TemplateFileName) = 0 ) or
      ( template.TemplateFileName = NEW_TEMPLATE ))  then
    template.TemplateFileName := sFileName;
  if (( self.txtTemplateName.Text = NEW_TEMPLATE ) or
      ( Length( self.txtTemplateName.Text ) = 0 ))   then begin
    self.txtTemplateName.Text := oFileDir.ucExtractFileName( sFileName , true );
  end;
  sTemplateFileShortName := _FileDir.ucExtractFileName( sFileName, false );

  bCopyImage := ( Length(txtImageFileName.Text) = 0 );
  if ( Length(txtImageFileName.Text) > 0 ) then begin
    sSourceImageFileName := AbsolutePath( self.txtActiveImage.Text );
    sTemplateImageFileName := AbsolutePath( ExtractFilePath(sFileName) + self.txtImageFileName.Text );
    if ( sSourceImageFileName <> sTemplateImageFileName ) then begin
      res := mrNo;

      res := MessageDlg( TEMPLATE_MESSAGES_DIFF, mtConfirmation, [ mbYes, mbNo, mbCancel ], 0);
      if ( res = mrCancel ) then Exit;
      bCopyImage := (res = mrYes);
    end;
  end;

  if ( bCopyImage ) then begin
    sNewImageFileName := _FileDir.ChangeExt( sFileName, 'BMP' );
    sSourceImageFileName := AbsolutePath( self.txtActiveImage.Text );
    if ( FileExists( sNewImageFileName )) then DeleteFile( sNewImageFileName );
    CopyFile( PChar(sSourceImageFileName), PChar(sNewImageFileName), false );
    template.ImageFileName := ExtractRelativePath( ExtractFilePath( sFileName ), sNewImageFileName );
    txtImageFileName.Text := ExtractRelativePath( ExtractFilePath( sFileName ), sNewImageFileName );
  end;

  template.ImageProcessingInstructionString := self.ActionListAsString( self.gridActionList );
  template.ImageProcessingInstructionUpsizeString := self.ActionListAsString( self.gridActionListUpsize );
  template.Boundary := self.Boundary;
  template.FirstChar.Assign( self.FirstChar );

  CommitDocCorrectToTemplate();
  template.saveAsXML( sFileName );
  self.acSave.Enabled := false;
  ClearDirty();
  Result := true;
End;

procedure TI2XBaseUI.acSaveExecute(Sender: TObject);
Begin
  if ( not FileExists(template.TemplateFileName ) or
     ( template.TemplateName = NEW_TEMPLATE ) ) then begin
    acSaveAs.Execute;
  end else begin
    with template do begin
      StatusMessage( 'Saving Template....' );
      if ( Save( template.TemplateFileName ) ) then
        StatusMessage( 'Template has been saved.' )
      else
        StatusMessage( 'Template HAS NOT been saved.' );
    end;
  end;
End;

function TI2XBaseUI.ActionListAsString(grd: TStringGrid): string;
var
  str : string;
  I : integer;
begin
  str := '';
  for I := 0 to grd.RowCount - 1 do begin
    if (I = 0 ) then
      str:=grd.Cells[0, I]
    else
      str := str + IMGPROC_INST_SEP + grd.Cells[0, I];
  end;
  Result := str;
end;

procedure TI2XBaseUI.ActionListFromString(grd: TStringGrid; alString: string);
var
  sl : TStringList;
  I : integer;
begin
  if (Length(alString) > 0) then begin
    try
      sl := TStringList.Create;
      grd.RowCount :=0;
      grd.Cells[0, grd.RowCount - 1] := '';

      oStrUtil.ParseString( alString, IMGPROC_INST_SEP, sl, true);
      FEnableActionListChangeEvent := false;
      for I := 0 to sl.Count - 1 do begin
        if ( Length( sl[I] ) > 0 ) then
          AddActionToList( grd, sl[I] );
      end;
      FEnableActionListChangeEvent := true;
    finally
      FreeAndNil( sl );
    end;
  end;
end;

function TI2XBaseUI.ActionListToStringList(grd: TStringGrid; sl : TStringList) : integer;
//function TI2XBaseUI.ActionListToStringList(grd: TStringGrid): TStringList;
var
  I : integer;
begin
  //sl := TStringList.Create;
  sl.Clear;
  for I := 0 to grd.RowCount - 1 do begin
    if ( Length( grd.Cells[0, I] ) > 0 ) then
      sl.Add(grd.Cells[0, I]);
  end;
  Result := sl.Count;
end;

procedure TI2XBaseUI.acTwainSelectExecute(Sender: TObject);
begin
  self.ShowOptions( iTWAIN );
end;

{*
procedure TI2XBaseUI.acToggleGridSnapExecute(Sender: TObject);
Begin
  if (( ElementDisplayEventsAreActive ) and ( chkGridSnap.Tag = 0 )) then begin
    reg.RegSet( REG_KEY, 'grid_align', uStrUtil.BoolToStr( self.chkGridSnap.Checked ), HKEY_CURRENT_USER );
    self.FEnableGridSnap := self.chkGridSnap.Checked;
    self.txtGridSize.Enabled := self.FEnableGridSnap;
  end;
End;
*}
procedure TI2XBaseUI.AddActionToList(sActionToAdd: string);
begin
  if ( self.ElementDisplayEventsAreActive ) then
    undo.Prep( self.SelectedActionList, ActionListAsString( self.SelectedActionList ), true);
  self.AddActionToList( self.SelectedActionList, sActionToAdd );
  if ( self.ElementDisplayEventsAreActive ) then
    undo.Add( self.SelectedActionList );
  self.TemplateUIUpdated;
end;

procedure TI2XBaseUI.AddActionToList(grd: TStringGrid; sActionToAdd: string);
begin
  if ( self.ElementDisplayEventsAreActive ) then
    undo.Prep( grd, ActionListAsString( grd ), true);
  if (( grd.RowCount >  1 ) or
      (( grd.RowCount = 1 ) and
      (grd.Cells[0, grd.RowCount - 1] <> ''))) then
    grd.RowCount := grd.RowCount + 1;
  grd.Cells[0, grd.RowCount - 1] :=  sActionToAdd;
  if (FEnableActionListChangeEvent) then begin
    reg.RegSet( REG_KEY, UNIT_NAME+grd.Name, ActionListAsString( grd ), HKEY_CURRENT_USER );
  end;
  if ( self.ElementDisplayEventsAreActive ) then
    undo.Add( grd );
end;

procedure TI2XBaseUI.MemoWrite(msg : string );
begin
  //Memo1.Lines.Add( msg );
end;

procedure TI2XBaseUI.mnuEditClick(Sender: TObject);
begin
  self.SetMainMenuOptions( self );
end;

procedure TI2XBaseUI.mnuEditCopyClick(Sender: TObject);
begin
  if ( self.Selection <> nil ) then begin
    Clipboard.AsText := CLIPBOARD_REGION_ID + IntToStr(Integer(pCEleNodeData( SelectedElement )));
  end else if ( self.ActiveControl is TCustomEdit ) then begin
    TCustomEdit(self.ActiveControl).CopyToClipboard;
  end else if ( self.ActiveControl is TComboBox ) then begin
    Clipboard.AsText := TComboBox(self.ActiveControl).SelText;
  end;
end;

procedure TI2XBaseUI.mnuEditCutClick(Sender: TObject);
begin
  //if ( self.Selection <> nil ) then begin
  //  Clipboard.AsText := CLIPBOARD_REGION_ID + IntToStr(Integer(pCEleNodeData( SelectedElement )));
  //end else
  if ( self.ActiveControl is TCustomEdit ) then begin
    TCustomEdit(self.ActiveControl).CutToClipboard;
  end;
  if ( self.ActiveControl is TComboBox ) then begin
    Clipboard.AsText := TComboBox(self.ActiveControl).SelText;
  end;
end;

procedure TI2XBaseUI.mnuEditDeleteClick(Sender: TObject);
begin
  if ( self.ActiveControl is TCustomEdit ) then begin
    TCustomEdit(self.ActiveControl).ClearSelection;
  end;
  if ( self.ActiveControl is TComboBox ) then begin
    TComboBox(self.ActiveControl).ItemIndex := -1;
  end;
end;

procedure TI2XBaseUI.mnuEditPasteClick(Sender: TObject);
var
  CopiedElement : CEleNodeData;
  sCopiedRegionAddress : string;
begin
  if ( Pos(CLIPBOARD_REGION_ID, Clipboard.AsText ) = 1 ) then begin
    sCopiedRegionAddress := StringReplace(Clipboard.AsText, CLIPBOARD_REGION_ID, '', [rfReplaceAll, rfIgnoreCase]);
    CopiedElement := CEleNodeData(pCEleNodeData(StrToInt( sCopiedRegionAddress )));
    self.CopyRegion( CopiedElement );
  end else if ( self.ActiveControl is TCustomEdit ) then begin
    TCustomEdit(self.ActiveControl).PasteFromClipboard;
  end else if ( self.ActiveControl is TComboBox ) then begin
    TComboBox(self.ActiveControl).SelText := Clipboard.AsText;
  end;
end;

procedure TI2XBaseUI.mnuEditRedoClick(Sender: TObject);
begin
  undo.Redo();
end;

procedure TI2XBaseUI.mnuEditUndoClick(Sender: TObject);
begin
  undo.Undo();
end;

procedure TI2XBaseUI.mnuHelpActivationBuyClick(Sender: TObject);
var
  sURL : string;
begin
  sURL := 'http://secure.softwarekey.com/solo/products/Cart.aspx?action=add&actiondata0=10155';
  ShellExecute(0, 'open', pchar( sURL ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.mnuSupportClick(Sender: TObject);
var
  sURL : string;
begin
  sURL := 'http://support.image2xml.com';
  ShellExecute(0, 'open', pchar( sURL ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.mnuTemplateOCRClick(Sender: TObject);
begin
  self.acOCR.Execute();
end;

procedure TI2XBaseUI.OnScanErrorChange(Sender, exc: TObject;
  procedureName: string);
begin
  self.StatusMessage( 'ERROR! ' + Exception(exc).Message );
end;

procedure TI2XBaseUI.OnScanStatusChange(Sender: TObject; msgText: string);
begin
  self.StatusMessage( msgText );
end;

procedure TI2XBaseUI.OnStatusChange( Sender : TObject; TaskID : string; sStatusMessage: string);
begin
  StatusMessage( sStatusMessage  );
end;

procedure TI2XBaseUI.OnDebug(Sender: TObject; sDebugMessage: string);
begin
  dbg( sDebugMessage );
end;

procedure TI2XBaseUI.OnDebugMessage( Sender : TObject; TaskID : string; sDebugMessage : string );
begin
  dbg( sDebugMessage );
end;

procedure TI2XBaseUI.OnDialogCancel(Sender: TObject);
begin
  Dbg('Return Value was=' + IntToStr( frmI2XDialogUI.Result ) );
end;

procedure TI2XBaseUI.OnDialogReturn(Sender: TObject);
begin
  if ( FModalDisplayFaded ) then begin
    frmDimmer.Hide;
    Dbg('Return Value was=' + IntToStr( frmI2XDialogUI.Result ) );
  end;
end;

procedure TI2XBaseUI.OnTemplateChange(sender: TObject);
var
  ele, eleSub : CEleNodeData;
  i : integer;
begin
  if ( sender.ClassName = 'CEleNodeData' ) then begin
    //dbg( 'className=' + sender.ClassName );
    ele := CEleNodeData(sender);
    if ( ele.SubNodes.Count > 0 ) then
      FRegionRedrawPending := true;
    //DrawRegion( self, ele );
    //if ( ele.SubNodes.Count > 0 ) then begin
    //  for i := 0 to ele.SubNodes.Count - 1 do begin
    //    DrawRegion( self, CEleNodeData( ele.SubNodes.Items[i] ) );
    //  end;
    //end;
  end;
  self.TemplateUIUpdated;
end;

procedure TI2XBaseUI.OnTemplateOffsetChange(Sender: TObject);
var
  tmpl : CTemplate;
begin
  tmpl := CTemplate( Sender );
  if ( tmpl.IsOffset ) then begin
    dbg( 'OnTemplateOffsetChange - isoffset' );
    ImgView.Enabled := false;
    DrawRegions( tmpl.Elements );
  end else begin
    dbg( 'OnTemplateOffsetChange not offset' );
    ImgView.Enabled := true;
    DrawRegions( tmpl.Elements );
  end;
end;

procedure TI2XBaseUI.OnUndoStackChange(Sender : TObject);
begin
  self.mnuEditUndo.Enabled := undo.UndoAvailable;
  self.mnuEditRedo.Enabled := undo.RedoAvailable;
end;

procedure TI2XBaseUI.OpenTempPath1Click(Sender: TObject);
Begin
  //ShellExecute(Handle, 'open', pchar( 'Explorer.exe ' +  AppTempDir ), nil,nil,SW_SHOWNORMAL);
  ShellExecute(Handle, 'open', pchar( AppTempDir ), nil,nil,SW_SHOWNORMAL);
End;

procedure TI2XBaseUI.pgctlTemplateDataChange(Sender: TObject);
Begin
  self.TabsControlsEnabledCheck;
  fMainCtl.LastSelectedTab := pgctlTemplateData.ActivePage.Name;
End;

procedure TI2XBaseUI.pmnuClearAllInstructionsClick(Sender: TObject);
Var
  nRow,i  : integer;
  sItem : string;
  grd : TStringGrid;
Begin
    if MessageDlg(DELETE_ALL_INST, mtConfirmation,
            [mbYes, mbNo], 0) = mrYes then
    begin
      grd := self.CurrentActionListGrid;
      grd.RowCount := 1;
      grd.Cells[0, i] := '';
    end;
End;

procedure TI2XBaseUI.pmnuDeleteInstructionClick(Sender: TObject);
var
  nRow,i  : integer;
  sItem : string;
  grd : TStringGrid;
begin
  grd := self.CurrentActionListGrid;
  undo.Prep( grd, ActionListAsString( grd ), true );

  if ( grd.RowCount = 1 ) then
    grd.Cells[0, i] := ''
  else begin
    nRow := grd.Row;
    for i := nRow to grd.RowCount - 2 do begin
      grd.Cells[0, i] := grd.Cells[0, i + 1];
    end;
    grd.RowCount := grd.RowCount - 1;
  end;
  undo.Add( grd );
end;

procedure TI2XBaseUI.pmnuEditTemplateClick(Sender: TObject);
begin
  if ( self.txtJobTemplate.Text <> self.template.TemplateFileName ) then begin
    self.SetActiveTemplate( self.txtJobTemplate.Text );
  end;
end;

procedure TI2XBaseUI.pmnuExecuteActionListClick(Sender: TObject);
var
  sl : TStringList;
Begin
  try
    sl := TStringList.Create();
    if (( ActionListToStringList( getSelectedActionList(), sl ) ) > 0 ) then
      fMainCtl.ProcessImageInstructions( sl );
  finally
    FreeAndNil( sl );
  end;
End;

procedure TI2XBaseUI.pmnuJobExploreClick(Sender: TObject);
begin
  if ( FileExists( self.ActiveJobFileName ) ) then
    ShellExecute(Handle, 'open', pchar( ExtractFilePath(ActiveJobFileName) ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.pmnuJobTemplateEditPopup(Sender: TObject);
begin
  dbg('pmnuJobEditPopup');
end;

procedure TI2XBaseUI.pmnuProcInstAndOCRClick(Sender: TObject);
var
  sl : TStringList;
Begin
  try
    sl := TStringList.Create();
    if (( ActionListToStringList( getSelectedActionList(), sl ) ) > 0 ) then
      fMainCtl.ProcessImageInstructionsPlusOCR( sl );
  finally
    FreeAndNil( sl );
  end;
End;

procedure TI2XBaseUI.pmnuRegionsPopup(Sender: TObject);
begin
  //FImgViewCancelMouseDown := true;
end;

procedure TI2XBaseUI.pmnuSetJobTemplateClick(Sender: TObject);
begin
  if ( FileExists(self.template.TemplateFileName) ) then begin
    self.txtJobTemplate.Text := self.template.TemplateFileName;
  end;
end;

procedure TI2XBaseUI.pnlImageInfoClick(Sender: TObject);
var
  sURL : string;
begin
  sURL := 'http://faq.image2xml.com';
  ShellExecute(0, 'open', pchar( sURL ), nil,nil,SW_SHOWNORMAL);
end;

procedure TI2XBaseUI.pnlImageInfoMouseEnter(Sender: TObject);
begin
  TPanel( Sender ).Height := 20;
end;

procedure TI2XBaseUI.pnlImageInfoMouseLeave(Sender: TObject);
begin
  with TPanel( Sender ) do begin
    if ( Tag = 0 ) then begin
      Height := 5;
    end;
  end;
end;

procedure TI2XBaseUI.pnlImageViewMouseEnter(Sender: TObject);
begin
  SetMainMenuOptions( Sender );
end;

procedure TI2XBaseUI.bbtnImageProcAndOCRClick(Sender: TObject);
var
  sl : TStringList;
Begin
  try
    sl := TStringList.Create();
    if (( ActionListToStringList( getSelectedActionList(), sl ) ) > 0 ) then begin
      self.StatusMessage( 'Beginning Image Processing and OCR', pgStart, 'Processing Image...' );
      fMainCtl.ProcessImageInstructions( sl, true, false );
    end;
  finally
    FreeAndNil( sl );
  end;
End;

procedure TI2XBaseUI.bbtnOpenImageClick(Sender: TObject);
var
  sFileName, sExt : string;
Begin
  if oFileDir.OpenDlgAPI( sFileName, FILEDLG_IMAGE, FILEOPENDLG_IMAGE_TEXT, '', 'tif', self.LastPathSelected ) then begin
    self.LastPathSelected := ExtractFilePath( sFileName );
    txtActiveImage.Text := sFileName;
    ReLoadActiveImage();
  end;
End;

procedure TI2XBaseUI.ProgressBarStart();
Begin
  self.TimerStatusBar.Enabled := true;
End;

procedure TI2XBaseUI.ProgressBarStop();
Begin
  self.TimerStatusBar.Enabled := false;
End;

procedure TI2XBaseUI.ProgressDialogCancel(Sender: TObject);
begin
  Dbg('CANCEL REQUESTED');
end;

procedure TI2XBaseUI.bbtnRefreshClick(Sender: TObject);
var
  ActiveImageFileName, TemplateImageFileName : string;
Begin
  StatusMessage('Reloading Image...', pgStart );
  ActiveImageFileName := AbsolutePath(self.txtActiveImage.Text);
  TemplateImageFileName := ExtractFilePath( template.TemplateFileName ) + self.txtImageFileName.Text;
  if ( FileExists( TemplateImageFileName ) ) then begin
    if ( ActiveImageFileName <> TemplateImageFileName ) then
      self.txtActiveImage.Text := ExtractRelativePath( AppPath, TemplateImageFileName );
    self.ReLoadActiveImage();
    StatusMessage('Image has been reloaded', pgDisable );
  end else begin
    if ( FileExists( ActiveImageFileName )  ) then begin
      self.ReLoadActiveImage();
      StatusMessage('Image has been reloaded', pgDisable );
    end else
      StatusMessage('Image could not be reloaded.. does it exist?', pgDisable );
  end;
End;

procedure TI2XBaseUI.bStatusDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  MaxWidth, BarSpaceWidth : integer;
  myRect : TRect;
Begin
  if Panel = bStatus.Panels[ Integer(sbProgressBar) ] then begin
    MaxWidth := rect.Right - rect.Left;
    BarSpaceWidth := round( MaxWidth / 100.0 * (100-self.FPercentComplete));
    myRect := Rect;
    myRect.Right := myRect.Right - BarSpaceWidth;
    bStatus.Canvas.Brush.Color := clGreen;
    bStatus.Canvas.FillRect( myRect );
  end;
End;

procedure TI2XBaseUI.btnAddImageInstClick(Sender: TObject);
var
  sParmStr : string;
  oDLL : TDLLImageProc;
begin
  if not ( PluginManager.ImageDLLExists( self.FSelectedImgInst.DLLName ) ) then begin
    raise Exception.Create('DLL of ' + self.FSelectedImgInst.DLLName + ' for selected Image Processing Instruction does not exist.' );
  end;

  oDLL := PluginManager.ImageDLL[self.FSelectedImgInst.DLLName];
  if ( self.ImgProcParmString( sParmStr ) ) then begin
    if ( Length(sParmStr) > 0 ) then
      sParmStr := oDLL.ShortName + ':' + self.FSelectedImgInst.Neumonic + IMGPROC_PARM_SEP +
        sParmStr
    else
      sParmStr := oDLL.ShortName + ':' + self.FSelectedImgInst.Neumonic;
  end;
  AddActionToList( sParmStr );
end;

procedure TI2XBaseUI.btnBrowserForOutputFolderClick(Sender: TObject);
var
  sFolder : string;
Begin
  if oFileDir.BrowseForFolder( self.txtJobOutput.Text, sFolder ) then
    self.txtJobOutput.Text := sFolder;
End;

procedure TI2XBaseUI.btnDoImageInstClick(Sender: TObject);
var
  sParmStr : string;
  oDLL : TDLLImageProc;
  sl : TStringList;
begin
  if not ( PluginManager.ImageDLLExists( self.FSelectedImgInst.DLLName ) ) then begin
    raise Exception.Create('DLL of ' + self.FSelectedImgInst.DLLName + ' for selected Image Processing Instruction does not exist.' );
  end;
  oDLL := PluginManager.ImageDLL[ self.FSelectedImgInst.DLLName ];
  if ( self.ImgProcParmString( sParmStr ) ) then begin
    if ( Length(sParmStr) > 0 ) then
      sParmStr := oDLL.ShortName + ':' + self.FSelectedImgInst.Neumonic + IMGPROC_PARM_SEP +
        sParmStr
    else
      sParmStr := oDLL.ShortName + ':' + self.FSelectedImgInst.Neumonic;
  end;
  try
    sl := TStringList.Create();
    sl.Add( sParmStr );
    fMainCtl.ProcessImageInstructions( sl, false, false );
  finally
    FreeAndNil( sl );
  end;
end;

procedure TI2XBaseUI.SGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  SG : TStringGrid;
  p : TPoint;
  sInst : string;
begin
  if TObject( Sender ).ClassName = 'TStringGrid' then begin
    SG := TStringGrid( Sender );
    { Convert mouse coordinates X, Y to
      to StringGrid related col and row numbers }
    SG.MouseToCell(X, Y, SourceCol, SourceRow);
    case Button of
      mbLeft:
        begin
          { Allow dragging only if an acceptable cell was clicked
            (cell beyond the fixed column and row) }
          if (SourceCol >= 0) and (SourceRow >= 0) then
          { Begin dragging after mouse has moved 4 pixels }
            SG.BeginDrag(False, 4);
        end;
      mbRight:
        begin
          if (SourceCol >= 0) and (SourceRow >= 0) then begin
            sInst := Split( SG.Cells[ SourceCol, SourceRow ], IMGPROC_PARM_SEP, 0 );
            self.pmnuClearAllInstructions.Enabled := ( not ((SG.RowCount <= 1) and (SG.Cells[0,0]='')));
            self.pmnuExecuteActionList.Enabled := pmnuClearAllInstructions.Enabled;
            if ( Length(sInst) > 0 ) then begin
              self.pmnuDeleteInstruction.Caption := 'Delete "' + sInst + '"';
              self.pmnuDeleteInstruction.Enabled := true;
            end else begin
              self.pmnuDeleteInstruction.Caption := 'Delete Instruction';
              self.pmnuDeleteInstruction.Enabled := false;
            end;

            GetCursorPos(p);
            pmnuActionList.Popup( p.X, p.Y );
          end;
        end;
    end;
  end;
end;

procedure TI2XBaseUI.SGDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  CurrentCol, CurrentRow: integer;
  SG : TStringGrid;
begin
  if TObject( Sender ).ClassName = 'TStringGrid' then begin
    SG := TStringGrid( Sender );
    SG.MouseToCell(X, Y, CurrentCol, CurrentRow); // convert mouse coord.
    { Accept dragged stuff only if it came from StringGrid
      and the mouse is now over an acceptable region }
    Accept := (Sender = Source) and
            (CurrentCol >= 0) and (CurrentRow >= 0);
  end else begin
    //dbg('Over the outside');
  end;
end;

procedure TI2XBaseUI.OnStringGridChange(Sender : TObject);
Begin
  //undo.Add( Sender, ActionListAsString( TStringGrid(Sender) ) );
  undo.Add( TStringGrid( Sender ) );
End;

procedure TI2XBaseUI.SGDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DestCol, DestRow: Integer;
  SG : TStringGrid;
  sTargetContents : string;
begin
  if TObject( Sender ).ClassName = 'TStringGrid' then begin
    SG := TStringGrid( Sender );
    undo.Prep( SG, ActionListAsString( TStringGrid(SG) ), true );
    SG.MouseToCell(X, Y, DestCol, DestRow); // convert mouse coord.
    sTargetContents := SG.Cells[DestCol, DestRow];
    { Move contents from source to destination }
    SG.Cells[DestCol, DestRow] := SG.Cells[SourceCol, SourceRow];
    if (SourceCol <> DestCol) or (SourceRow <> DestRow) then
      SG.Cells[SourceCol, SourceRow] := sTargetContents;
    OnStringGridChange( SG );
  end;
end;

procedure TI2XBaseUI.btnFindXYBoundariesClick(Sender: TObject);
Begin
  if ( self.IsBoundarySet ) then begin

    if not template.Elements.ContainsKey( STANDARD_REGION_X_AXIS ) then begin
     CreateRegion( STANDARD_REGION_X_AXIS );
     FXAxisRegionElement := template.getElement( STANDARD_REGION_X_AXIS );
     FXAxisRegionElement.X := template.Boundary.Left;
    end;
    if not template.Elements.ContainsKey( STANDARD_REGION_Y_AXIS ) then begin
     CreateRegion( STANDARD_REGION_Y_AXIS );
     FYAxisRegionElement := template.getElement( STANDARD_REGION_Y_AXIS );
     FYAxisRegionElement.Y := template.Boundary.Top;
    end;

    DrawRegion( Sender, self.FXAxisRegionElement, true, clGreen32 );
    DrawRegion( Sender, self.FYAxisRegionElement, true, clGreen32 );
    self.SetZoom( 0.25 );
    btnFindXYBoundaries.Tag := 1;
  end else begin
    AnalyzeDocumentForAttributes( sender );
  end;
{*
  if ( FXAxisRegionElement = nil ) then begin
    CreateRegion( STANDARD_REGION_X_AXIS );
    FXAxisRegionElement := template.getElement( STANDARD_REGION_X_AXIS );
    self.DisplaySpecialElement( FXAxisRegionElement );
  end else begin
    DrawRegion( Sender, self.FXAxisRegionElement, true, clGreen32 );
  end;
  if ( FYAxisRegionElement = nil ) then begin
    CreateRegion( STANDARD_REGION_Y_AXIS );
    FYAxisRegionElement := template.getElement( STANDARD_REGION_Y_AXIS );
    self.DisplaySpecialElement( FYAxisRegionElement );
  end else begin
    DrawRegion( Sender, self.FYAxisRegionElement, true, clGreen32 );
  end;
  *}
End;

function TI2XBaseUI.SetActiveJob( const JobName: string ): integer;
var
  job : TI2XJob;
begin
  job := TI2XJob.Create();
  try
    if ( not FileExists( ActiveJobFileName ) ) then begin
      self.cboJobName.Text := '';
      self.txtJobImagePath.Text := '';
      self.txtJobOutput.Text := '';
      self.txtJobTemplate.Text := '';
      self.chkRestartable.Checked := false;
    end else if ( job.LoadFromFile( ActiveJobFileName ) ) then begin
      self.cboJobName.Text := JobName;
      self.txtJobImagePath.Text := job.ImagePath;
      self.txtJobOutput.Text := job.OutputPath;
      self.txtJobTemplate.Text := job.TemplateFileName;
      self.chkRestartable.Checked := job.Restartable;
    end;
  finally
    FreeAndNil( job );
  end;
end;

function TI2XBaseUI.LoadJobList( const DefaultValue : string ) : integer;
var
  DirList, JobFileList, FileList : TStringList;
  iDirList, iFileList, iJobList, i : integer;
Begin
  try
    if ( not DirectoryExists( Options.JobRootPath ) ) then begin
      ForceDirectories( Options.JobRootPath );
      Result := 0;
      Exit;
    end;
      //raise Exception.Create( 'Job Root directory "' + Options.JobRootPath + '" does not exist.' );

    JobFileList := TStringList.Create;

    DirList := _FileDir.GetSubDirList( Options.JobRootPath  );
    for iDirList := 0 to DirList.Count - 1 do begin
      try
        FileList := _FileDir.GetFileList( DirList[ iDirList ] + '\*' + JOB_FILE_EXT );
        if ( FileList.Count > 0 ) then begin
          for iFileList := 0 to FileList.Count - 1 do begin
            JobFileList.Add( _FileDir.ucExtractFileName( FileList[ iFileList ], true ) );
          end;
        end;
      finally
        FileList.Free;
      end;
    end;
    JobFileList.Sort();


    self.cboJobName.Clear;
    for iJobList := 0 to JobFileList.Count - 1 do begin
      self.cboJobName.Items.Add( JobFileList[ iJobList ] );
    end;

    for i := 0 to cboJobName.Items.Count - 1 do begin
      if ( cboJobName.Items[i] = DefaultValue )then begin
        cboJobName.ItemIndex := i;
      end;
    end;

    Result := JobFileList.Count;
  finally
    FreeAndNil( JobFileList );
    FreeAndNil( DirList );
  end;
End;

procedure TI2XBaseUI.btnJobDeleteClick(Sender: TObject);
var
  res : integer;
  sJobName : string;
  iIdx : integer;
Begin
  sJobName := self.cboJobName.Text;
  if ( FileExists( self.ActiveJobFileName ) ) then begin
    res := MessageDlg(DELETE_JOB, mtConfirmation,
            [mbYes, mbNo], 0);
    if ( res = mrYes ) then begin
      self.StatusMessage( 'Deleting Job ' + sJobName, pgStart );
      self.oFileDir.DestroyPath( ExtractFilePath( self.ActiveJobFileName ) );
      self.StatusMessage( 'Job ' + sJobName + ' has been deleted to the recycle bin.', pgDisable );
    end;
    iIdx := cboJobName.ItemIndex;
    if ( iIdx > cboJobName.Items.Count ) then iIdx := cboJobName.Items.Count;
    Self.LoadJobList();
    cboJobNameChange( self );
    //Self.SetActiveJob( self.cboJobName.Items[ iIdx ] )
  end else begin
    res := MessageDlg(CLEAR_JOB, mtConfirmation,
            [mbYes, mbNo], 0);
    if ( res = mrYes ) then begin
      self.StatusMessage( 'Clearing Job ' + sJobName, pgStart );
      self.cboJobName.Text := '';
      self.txtJobImagePath.Text := '';
      self.txtJobOutput.Text := '';
      self.txtJobTemplate.Text := '';
      self.chkRestartable.Checked := false;
      AppCache.setVar( 'JobRestartable', 'N');
      self.StatusMessage( 'Job ' + sJobName + ' has been cleared.', pgDisable );
    end;
  end;
End;

procedure TI2XBaseUI.btnJobSaveClick(Sender: TObject);
var
  job : TI2XJob;
begin
  job := TI2XJob.Create();
  try
    if ( FileExists(self.ActiveJobFileName) )  then
      job.LoadFromFile( self.ActiveJobFileName );
    job.Name := self.cboJobName.Text;
    job.RootPath := Options.JobRootPath;
    job.ImagePath := self.txtJobImagePath.Text;
    job.OutputPath := self.txtJobOutput.Text;
    job.TemplateFileName := self.txtJobTemplate.Text;
    job.Restartable := self.chkRestartable.Checked;
    job.Save();
  finally
    FreeAndNil( job );
  end;
  JobControlsEnabled( (Length( cboJobName.Text ) > 0) );
  Self.LoadJobList( self.cboJobName.Text );
  self.StatusMessage( 'Job has been saved to ' + self.ActiveJobFileName );
end;

procedure TI2XBaseUI.btnJobTemplateSelectClick(Sender: TObject);
var
  sFileName, sExt : string;
begin
  if oFileDir.OpenDlgAPI( sFileName, FILEDLG_TEMPLATE, FILEOPENDLG_TEMPLATE_TEXT, '', TEMPLATE_FILE_EXT ) then begin
    sExt := ExtractFileExt(sFileName);
    self.txtJobTemplate.Text := sFileName;
  end;
end;

procedure TI2XBaseUI.btnOCRJobExecuteClick(Sender: TObject);
var
  JobExec : TI2XJobExecute;
begin
  btnJobSaveClick( Sender );
  if ( FileExists( ActiveJobFileName )) then begin
    try
      JobExec := TI2XJobExecute.Create( ActiveJobFileName );
      JobExec.Run;
    finally
      FreeAndNil( JobExec );
    end;
  end;
end;

procedure TI2XBaseUI.AnalyzeDocumentForAttributes(Sender: TObject);
var
  res : integer;
Begin
  if ( fMainCtl.OCRImageCheck( ImgView.Bitmap ) ) then begin
    if ( not self.IsBoundarySet ) then begin
      res := MessageDlg(ANALYZE_DOCUMENT, mtConfirmation,
            [mbYes, mbNo], 0);
      if ( res = mrYes ) then begin
        self.StatusMessage( 'Beginning analysis of current document', pgStart, 'Document Analysis' );
        fMainCtl.OCRImage( ImgView.Bitmap, self.Template, otAnalysis );
      end;
    end;
  end;
End;

procedure TI2XBaseUI.btnSearchFirstCharClick(Sender: TObject);
Begin
  if ( self.IsBoundarySet ) then begin
    if not template.Elements.ContainsKey( STANDARD_REGION_FIRST_CHAR ) then begin
     CreateRegion( STANDARD_REGION_FIRST_CHAR );
     FFirstCharRegionElement := template.getElement( STANDARD_REGION_FIRST_CHAR );
     self.RegionAttributeChange( FFirstCharRegionElement, template.FirstChar.AsRect );
    end;
    DrawRegion( Sender, self.FFirstCharRegionElement, true, clGreen32 );
    self.Selection := ( TBitmapLayer( FFirstCharRegionElement.GraphicPointer ) );
    self.SetZoom( 3 );
    ImgView.ScrollToCenter( FFirstCharRegionElement.x, FFirstCharRegionElement.y );
  end else begin
    btnSearchFirstChar.Tag := 1; //note that we want to do something with the results
    AnalyzeDocumentForAttributes( sender );
  end;
//  if ( FFirstCharRegionElement = nil ) then begin
//    CreateRegion( STANDARD_REGION_FIRST_CHAR );
//   FFirstCharRegionElement := template.getElement( STANDARD_REGION_FIRST_CHAR );
//    self.DisplaySpecialElement( FFirstCharRegionElement );
//  end else begin
//    DrawRegion( Sender, self.FFirstCharRegionElement, true, clGreen32 );
//  end;
End;

procedure TI2XBaseUI.btnSetImagePathClick(Sender: TObject);
var
  sFolder : string;
Begin
  if oFileDir.BrowseForFolder( self.txtJobImagePath.Text, sFolder ) then
    self.txtJobImagePath.Text := sFolder;
End;

procedure TI2XBaseUI.btnTemplateImageFileSetClick(Sender: TObject);
var
  sFileName, sExt, sNewImageFileName : string;
  bDoOverwrite : boolean;
Begin
  //dbg( 'Exists?  ' + AbsolutePath(txtActiveImage.Text) );
  sFileName := AbsolutePath( txtActiveImage.Text );
  sNewImageFileName := _FileDir.ChangeExt( template.TemplateFileName, 'BMP' );
  If not FileExists( sFileName ) then begin
    if oFileDir.OpenDlgAPI( sFileName, FILEDLG_IMAGE, FILEOPENDLG_IMAGE_TEXT, '', 'tif' ) then begin
      sExt := ExtractFileExt(sFileName);
      //sFileName := ExtractRelativePath( AppPath, sFileName);
      sFileName := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sFileName );
      self.SetActiveImage( sFileName );
    end;
    //self.txtImageFileName.Text := sFileName;
    self.txtImageFileName.Text := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sFileName );
  end else begin
    //sFileName := self.txtActiveImage.Text;
    //sFileName := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sFileName );
    if (( ExtractFileName(sFileName) = IMGPROC_CACHE ) or
        ( ExtractFileName(sFileName) = ACQ_IMAGE )) then begin
      //sNewImageFileName := _FileDir.ChangePath( sFileName, ExtractFilePath( template.TemplateFileName ) );
      if ( FileExists( sNewImageFileName ) ) then begin
        bDoOverwrite := (MessageDlg(IMGFILE_EXISTS_TEXT, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
        if ( bDoOverwrite ) then begin
          if ( FileExists( sNewImageFileName ) ) then DeleteFile( sNewImageFileName );
          CopyFile( PChar(sFileName), PChar(sNewImageFileName), false );
          sFileName := sNewImageFileName;
          //self.txtImageFileName.Text := sFileName;
          //self.txtImageFileName.Text := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sFileName );
          self.txtImageFileName.Text := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sFileName );
        end else begin
          self.StatusMessage('Did not set template image from active image from cache because a target file for this template already exists.');
        end;
      end else begin
        CopyFile( PChar(sFileName), PChar(sNewImageFileName), false );
        sFileName := sNewImageFileName;
        self.txtImageFileName.Text := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sFileName );
      end;
    end else if ( FileExists(sFileName) ) then begin
      ImgView.Bitmap.SaveToFile( sNewImageFileName );
      self.txtImageFileName.Text := ExtractRelativePath( ExtractFilePath( template.TemplateFileName ), sFileName );
    end;
  end;
End;


procedure TI2XBaseUI.Button1Click(Sender: TObject);
Begin
  try
    if ( template.IsOffset ) then
      self.OffsetRegion( 0, 0 )
    else
      self.OffsetRegion( 10, 10 );
  finally
  end;
  //ShowDialog( self.OnDialogReturn, self.OnDialogCancel );

end;

procedure TI2XBaseUI.ShowDialog( const InitMessage : string; ReturnFunction : TNotifyEvent );
Begin
  frmDimmer.Display;
  frmI2XDialogUI.Caption := 'Main Caption?';
  frmI2XDialogUI.ShowDialog( InitMessage );
  frmI2XDialogUI.ShowAgainCheckVisible := false;
  frmI2XDialogUI.OnHide := ReturnFunction;
End;

procedure TI2XBaseUI.ShowDialog(const InitMessage : string; ReturnFunction, CancelFunction: TNotifyEvent);
begin
  frmDimmer.Display;
  frmI2XDialogUI.Caption := 'Main Caption?';
  frmI2XDialogUI.ShowDialog( InitMessage );
  frmI2XDialogUI.ShowAgainCheckVisible := false;
  frmI2XDialogUI.OnHide := ReturnFunction;
  frmI2XDialogUI.OnCancel := CancelFunction;
end;

procedure TI2XBaseUI.cboDataTypeChange(Sender: TObject);
begin
  if ( ElementDisplayEventsAreActive ) then begin
    self.SelectedElement.DataTypeString := cboDataType.Text;
    self.TemplateUIUpdated;
  end;
end;

procedure TI2XBaseUI.cboJobNameChange(Sender: TObject);
begin
  if ( not InFormCreateEvent ) then begin
    if ( FileExists( ActiveJobFileName )) then
      SetActiveJob( cboJobName.Text );
    AppCache.setVar( 'JobName', cboJobName.Text);
    JobControlsEnabled( (Length( cboJobName.Text ) > 0) );
    undo.Add( Sender, TEdit(Sender).Text );
  end;
end;

procedure TI2XBaseUI.chkRestartableClick(Sender: TObject);
begin
  JobControlsEnabled( true );
end;

procedure TI2XBaseUI.chkRestartableMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AppCache.setVar( 'JobRestartable', uStrUtil.BoolToStr( chkRestartable.Checked, 'Y', 'N' ));
end;

function TI2XBaseUI.CurrentActionListGrid() : TStringGrid ;
begin
  if ( self.pgctlImageProcessing.ActivePageIndex = 0 ) then
    Result := self.gridActionList
  else if ( self.pgctlImageProcessing.ActivePageIndex = 1 ) then
    Result := self.gridActionListUpsize;
end;

procedure TI2XBaseUI.dbg(const msg: string);
begin
  uImage2XML.dbg( msg );
end;

procedure TI2XBaseUI.SelectDocumentCorrection( tag : integer );
begin
  //This prevents element display recusion from occuring
  self.TriggerUIEvents( false );

  //case Tag of
  //  0:
  //    begin
  //      if ( rbtnNoDocCorrection.Checked ) then Exit;
  //    end;
  //  1:
  //    begin
  //      if ( rbtnFirstChar.Checked ) then Exit;
  //    end;
  //  2:
  //    begin
  //      if ( rbtnXYBoundary.Checked ) then Exit;
  //    end;
  //end;

  self.rbtnNoDocCorrection.Checked := false;
  self.rbtnFirstChar.Checked := false;
  self.rbtnXYBoundary.Checked := false;
  //pnlFirstCharData.Enabled := false;
  //pnlXYBoundary.Enabled := false;

  self.txtFirstCharX.Enabled := false;
  self.txtFirstCharY.Enabled := false;
  self.txtFirstCharHeight.Enabled := false;
  self.txtFirstCharWidth.Enabled := false;
  self.lblFirstCharX.Enabled := false;
  self.lblFirstCharY.Enabled := false;
  self.lblFirstCharHeight.Enabled := false;
  self.lblFirstCharWidth.Enabled := false;
  self.lblXBound.Enabled := false;
  self.lblYBound.Enabled := false;
  self.txtXBound.Enabled := false;
  self.txtYBound.Enabled := false;
  self.btnSearchFirstChar.Visible := false;
  self.btnFindXYBoundaries.Visible := false;
  if (( template.Elements.ContainsKey(STANDARD_REGION_FIRST_CHAR)) and ( FFirstCharRegionElement <> nil )) then HideRegion( FFirstCharRegionElement );
  if (( template.Elements.ContainsKey(STANDARD_REGION_X_AXIS)) and ( FXAxisRegionElement <> nil )) then HideRegion( FXAxisRegionElement );
  if (( template.Elements.ContainsKey(STANDARD_REGION_Y_AXIS)) and ( FYAxisRegionElement <> nil )) then HideRegion( FYAxisRegionElement );

    case Tag of
      0:
        begin
          self.rbtnNoDocCorrection.Checked := true;
          self.memDocOffsetInst.Text := DOC_CORRECTION_NONE;

        end;
      1:
        begin
          self.rbtnFirstChar.Checked := true;
          //pnlFirstCharData.Enabled := true;

          self.txtFirstCharX.Enabled := true;
          self.txtFirstCharY.Enabled := true;
          self.txtFirstCharHeight.Enabled := true;
          self.txtFirstCharWidth.Enabled := true;
          self.btnSearchFirstChar.Visible := true;
          self.lblFirstCharX.Enabled := true;
          self.lblFirstCharY.Enabled := true;
          self.lblFirstCharHeight.Enabled := true;
          self.lblFirstCharWidth.Enabled := true;

          if ( FFirstCharRegionElement <> nil ) then begin
            ShowRegion( FFirstCharRegionElement );
          end;


          self.memDocOffsetInst.Text := DOC_CORRECTION_FIRSTCHAR;
        end;
      2:
        begin
          self.rbtnXYBoundary.Checked := true;
          //pnlXYBoundary.Enabled := true;

          self.lblXBound.Enabled := true;
          self.lblYBound.Enabled := true;
          self.btnFindXYBoundaries.Visible := true;

          if ( FXAxisRegionElement <> nil ) then ShowRegion( FXAxisRegionElement );
          if ( FYAxisRegionElement <> nil ) then ShowRegion( FYAxisRegionElement );

          self.memDocOffsetInst.Text := DOC_CORRECTION_XY;
        end;
    end;

    self.TriggerUIEvents( true );
    TemplateUIUpdated();
end;

procedure TI2XBaseUI.ClearTemplateUIElements();
Begin
  self.TriggerUIEvents( false );

  self.txtImageFileName.Text := '';
  self.txtTemplateDescription.Lines.Clear;
  self.txtTemplateName.Text := '';

  SelectDocumentCorrection( 0 );
  self.txtFirstCharX.Text := '';
  self.txtFirstCharY.Text := '';
  self.txtFirstCharWidth.Text := '';
  self.txtFirstCharHeight.Text := '';
  self.txtXBound.Text := '';
  self.txtYBound.Text := '';

  self.gridActionList.RowCount:=0;
  self.gridActionList.Cells[0,0] := '';
  self.gridActionListUpsize.RowCount:=0;
  self.gridActionListUpsize.Cells[0,0] := '';

  //self.lblLevel.Caption := IntToStr(gbThreshold.Position);
  //self.txtScaleY.Text := '';
  //self.txtScaleX.Text := '';
  //self.cboColorReduction.ItemIndex := -1;
  //self.cboImageChanges.ItemIndex := -1;

  self._FirstChar.Clear;
  Initialize( Self.Boundary );

  ClearTree();
  ClearDisplayElement();

  self.TriggerUIEvents( true );
End;


procedure TI2XBaseUI.ElementsToTree( tree : TTreeView; ht : THashTable );
var
  i : cardinal;
  arrKeyList : TKeyList;
  eleData : CEleNodeData;
  procedure ProcessNode( Node : CEleNodeData; tn : TTreeNode);
  var
    i : cardinal;
    arrKeyList : TKeyList;
    cNode : CEleNodeData;
  Begin
    if Node = nil then Exit;
    with Node do
    begin
      tn := tree.Items.AddChild(tn, Node.id );
      tn.Data := Node;
      Node.TreeViewNodePointer := tn;
    end;
    if ( Node.SubNodes.count > 0 ) then begin
      arrKeyList := SortKeyList(Node.SubNodes.Keys);
      for i := 0 to Length(arrKeyList) - 1 do begin
        cNode := CEleNodeData( Node.SubNodes.get( arrKeyList[i] ));
        ProcessNode(cNode, tn);
      end;
    end;
  End;
Begin
  if ( ht.count > 0 ) then begin
    arrKeyList := SortKeyList(ht.Keys);
    for i := 0 to Length(arrKeyList) - 1 do begin
      eleData := CEleNodeData( ht.get( arrKeyList[i] ));
      ProcessNode(eleData, nil);
    end;
  end;
End;

procedure TI2XBaseUI.ClearTree;
var
  cnt : integer;
begin
  //for cnt := 0 to tvRegions.Items.Count - 1 do
  //  Dispose(tvRegions.Items[cnt].Data) ;
  tvRegions.Items.Clear;
end;

procedure TI2XBaseUI.ClearDisplayElement;
begin
  self.DisplaySelectedElement( nil );
end;

//TElementControlsDisplay = (ecDisable, ecSingle, ecMulti);
procedure TI2XBaseUI.ControlsEnableSelectedElement( enableType : TElementControlsDisplay );
begin
    self.txtX.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblX.Enabled := txtX.Enabled;
    self.txtY.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblY.Enabled := txtY.Enabled;
    self.txtWidth.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblWidth.Enabled := txtWidth.Enabled;
    self.txtHeight.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblHeight.Enabled := txtHeight.Enabled;
    self.txtId.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblName.Enabled := txtId.Enabled;
    self.cboDataType.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblDataType.Enabled := cboDataType.Enabled;
    self.lblGroupKeyOrder.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblGroupKeyOrder0.Enabled := self.lblGroupKeyOrder.Enabled;
    self.lblElementKeyOrder.Enabled := ((enableType = ecSingle ) or ( enableType = ecMulti ));
    lblElementKeyOrder0.Enabled := lblElementKeyOrder.Enabled;
end;

procedure TI2XBaseUI.DisplaySelectedElement( eleData : CEleNodeData );
var OldEventValue : boolean;
begin
  TriggerUIEvents( false );

  if ( eleData = nil ) then begin
    self.txtX.Text := '';
    self.txtY.Text := '';
    self.txtWidth.Text := '';
    self.txtHeight.Text := '';
    self.txtId.Text := '';
    self.cboDataType.ItemIndex := -1;
    self.lblGroupKeyOrder.Caption := 'N\A';
    self.lblElementKeyOrder.Caption := 'N\A';
    cboDataType.ItemIndex := FindValue( cboDataType, 'Strings' );
    ControlsEnableSelectedElement( ecDisable );

  end else begin
    self.txtX.Text := IntToStr( eleData.x );
    self.txtY.Text := IntToStr( eleData.y );
    self.txtWidth.Text := IntToStr( eleData.width );
    self.txtHeight.Text := IntToStr( eleData.height );
    self.txtId.Text := eleData.id;
    self.cboDataType.ItemIndex := -1;
    if ( eleData.groupkey ) then
      self.lblGroupKeyOrder.Caption := IntToStr( eleData.groupOrder )
    else
      self.lblGroupKeyOrder.Caption := 'N\A';
    if ( eleData.eekey ) then
      self.lblElementKeyOrder.Caption := IntToStr( eleData.eeKeyOrder )
    else
      self.lblElementKeyOrder.Caption := 'N\A';

    with self.cboDataType.Items do begin
      if (eleData.DataType = dtNumeric) then
        cboDataType.ItemIndex := FindValue( cboDataType, 'Numeric' )
      else if (eleData.DataType = dtDateTime) then
        cboDataType.ItemIndex := FindValue( cboDataType, 'DateTime' )
      else
        cboDataType.ItemIndex := FindValue( cboDataType, 'Strings' );
    end;
    if ( eleData.nodeType = NODE_TYPE_SINGLE ) then
      ControlsEnableSelectedElement( ecSingle )
    else
      ControlsEnableSelectedElement( ecMulti );
  end;

  TriggerUIEvents( true );
end;

procedure TI2XBaseUI.DisplaySpecialElement( eleData : CEleNodeData );
var
  OldEventValue : boolean;
  rt : TRegionType;
begin
  TriggerUIEvents( false );
  if ( eleData = nil ) then begin
      self.txtFirstCharX.Text := '';
      self.txtFirstCharY.Text := '';
      self.txtFirstCharWidth.Text := '';
      self.txtFirstCharHeight.Text := '';

      self.txtYBound.Text := '';
      self.txtXBound.Text := '';
  end else begin
    rt := eleData.RegionType;
    if ( rt = rtStandard ) then begin
      self.txtFirstCharX.Text := IntToStr( eleData.X );
      self.txtFirstCharY.Text := IntToStr( eleData.Y );
      self.txtFirstCharWidth.Text := IntToStr( eleData.Width );
      self.txtFirstCharHeight.Text := IntToStr( eleData.Height );
      self._FirstChar.X := eleData.X;
      self._FirstChar.Y := eleData.Y;
      self._FirstChar.Width := eleData.Width;
      self._FirstChar.Height := eleData.Height;
    end else if ( rt = rtXAxis ) then begin
      self.txtXBound.Text := IntToStr( eleData.X );
      self.Boundary.Left := eleData.X;
    end else if ( rt = rtYAxis ) then begin
      self.txtYBound.Text := IntToStr( eleData.Y );
      self.Boundary.Top := eleData.Y;
    end;
  end;
  TriggerUIEvents( true );
end;


function TI2XBaseUI.FindValue( cbo : TComboBox; Value: string ) : integer;
var
  I, idx : Integer;
Begin
  idx := -1;
  for I := 0 to cbo.Items.Count - 1 do begin
    if ( cbo.Items[I] = Value ) then
      idx := I;
  end;
  Result := idx;
End;

procedure TI2XBaseUI.LayerMouseDown(Sender: TObject; Buttons: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  B : TBitmapLayer;
  ele : CEleNodeData;
  LastParent : TObject;
  elePtr : pointer;
Begin

  ImgView.Tag := 1; // inform down streamevents that this layer recieved this event first
  if Sender <> nil then begin
    if ( self.SelectedElement <> nil ) then
      LastParent := self.SelectedElement.Parent;

    ele := nil;
    if ( TPositionedLayer(Sender).Tag <> 0 ) then begin
      elePtr := Pointer( TPositionedLayer(Sender).Tag );
      ele :=  CEleNodeData( elePtr );
      {*
      if ( ele <> nil ) then begin
        //We want to select the parent of a selected element first...
        if (( ele.Parent <> nil ) and ( ele.Parent <> LastParent )) then begin
          //ele := CEleNodeData( ele.Parent );
          Selection := TPositionedLayer( CEleNodeData( ele.Parent ).GraphicPointer );
        end else begin
          Selection := TPositionedLayer(Sender);
        end;
      end;
      *}
    end;
  end;
End;

procedure TI2XBaseUI.SetZoom( iNew : Extended );
Begin
  Zoom := iNew;
  ImgView.Scale := Zoom;
  reg.RegSet( REG_KEY, 'zoom', FloatToStrF(Zoom, ffGeneral, 2, 2), HKEY_CURRENT_USER );
  bStatus.Panels[ Integer(sbZoom) ].Text := 'Zoom: ' +  FloatToStrF(Zoom, ffGeneral, 2, 5);
End;

procedure TI2XBaseUI.ShowRegion(eleData: CEleNodeData);
var
  B : TBitmapLayer;
begin
  if ( eleData.GraphicPointer = nil ) then
    raise Exception.Create( 'Graphic pointer for eleData with id of ' + eleData.ID + ' is nil' );

  B := ( TBitmapLayer( eleData.GraphicPointer ) );
  B.Visible := true;
end;

procedure TI2XBaseUI.Slider1Change(Sender: TObject);
begin
  self.lblLevel1.Caption := IntToStr(Slider1.Position);
end;

procedure TI2XBaseUI.ImageInstructionParameterDisplay();
var
  i : integer;
Begin
  for i := 0 to tabImage.ControlCount - 1 do
    tabImage.Controls[i].Enabled := false;
End;

function TI2XBaseUI.FloatRectToString( Rect : TFloatRect ) : string;
Begin
  Result := 'L=' + FloatToStrF(Rect.Left, ffGeneral, 6, 2) + ' ' +
            'R=' + FloatToStrF(Rect.Right, ffGeneral, 6, 2) + ' ' +
            'T=' + FloatToStrF(Rect.Top, ffGeneral, 6, 2) + ' ' +
            'B=' + FloatToStrF(Rect.Bottom, ffGeneral, 6, 2);
End;

procedure TI2XBaseUI.GridAlign(var Val : single; const GridSize : single );
Begin
  If ((Trunc(Val) Mod Trunc(GridSize) ) >= Trunc( GridSize ) / 2 ) Then
    Val := ((Trunc(Val / GridSize) + 1) * GridSize)
  Else
    Val := ( Trunc(Val / GridSize) * GridSize );
End;

procedure TI2XBaseUI.GridAlign(var Val: Integer; const GridSize: single);
Begin
  If ((Trunc(Val) Mod Trunc(GridSize) ) >= Trunc( GridSize ) / 2 ) Then
    Val := Trunc((Trunc(Val / GridSize) + 1) * GridSize)
  Else
    Val := Trunc( Trunc(Val / GridSize) * GridSize );
End;

procedure TI2XBaseUI.DbgEle( ele : CEleNodeData );
var
  i : cardinal;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
Begin
  //Dbg('DbgEle: Ele=' + ele.AsString() );
  if ( ele.SubNodes.Count > 0 ) then begin
    arrKeyList := ele.SubNodes.Keys;
    EleKeyListXSort( ele.SubNodes, arrKeyList );
    for i := 0 to Length(arrKeyList) - 1 do begin
      cNode := CEleNodeData( ele.SubNodes.get( arrKeyList[i] ));
      //Dbg('DbgEle:   Child Ele=' + cNode.AsString() );
      Dbg('    ' + cNode.ID + '.. w=' + IntToStr(cNode.Width) );
    end;
  end;
End;

procedure TI2XBaseUI.RegionAttributeChange(ele: CEleNodeData;
  NewLocation: TFloatRect);
begin
  RegionAttributeChange( ele, Rect( Trunc(NewLocation.Left), Trunc(NewLocation.Top), Trunc(NewLocation.Right), Trunc(NewLocation.Bottom) ) );
end;

procedure TI2XBaseUI.RegionMove(ele : CEleNodeData; const ChangeOfX, ChangeOfY : integer );
Begin
  RegionAttributeChange( ele, Rect( ele.X + ChangeOfX, ele.Y + ChangeOfY,
        ele.X + ChangeOfX + ele.Width, ele.Y + ChangeOfY + ele.Height ) );
End;

procedure TI2XBaseUI.RegionResize(ele : CEleNodeData; const ChangeOfWidth, ChangeOfHeight : integer );
Begin
  RegionAttributeChange( ele, Rect( ele.X, ele.Y,
        ele.X + ele.Width + ChangeOfWidth, ele.Y + ele.Height + ChangeOfHeight ) );
End;

procedure TI2XBaseUI.RectImageBoundsCheck(ele : CEleNodeData; var loc : TRect );
Begin
  if ( ele <> nil ) then begin
    if ( loc.Left <= 0 ) then begin
      loc.Left := 0;
      loc.Right := ele.Width;
    end;
    if ( loc.Top <= 0 ) then begin
      loc.Top := 0;
      loc.Bottom := ele.Height;
    end;
    if ( loc.Right > ImagePixelWidth ) then begin
      loc.left := ImagePixelWidth - ele.Width;
      loc.Right := ImagePixelWidth;
    end;
    if ( loc.Bottom > ImagePixelHeight ) then begin
      loc.Top := ImagePixelHeight - ele.Height;
      loc.Bottom := ImagePixelHeight;
    end;
  end;
End;

procedure TI2XBaseUI.RectImageBoundsCheck(ele: CEleNodeData;
  var loc: TFloatRect);
begin
  if ( ele <> nil ) then begin
    if ( loc.Left <= 0 ) then begin
      loc.Left := 0;
      loc.Right := ele.Width;
    end;
    if ( loc.Top <= 0 ) then begin
      loc.Top := 0;
      loc.Bottom := ele.Height;
    end;
    if ( loc.Right > ImagePixelWidth ) then begin
      loc.left := ImagePixelWidth - ele.Width;
      loc.Right := ImagePixelWidth;
    end;
    if ( loc.Bottom > ImagePixelHeight ) then begin
      loc.Top := ImagePixelHeight - ele.Height;
      loc.Bottom := ImagePixelHeight;
    end;
  end;
end;

procedure TI2XBaseUI.RegionAttributeChange(ele : CEleNodeData; NewLocation : TRect );
var
  loc : TRect;
Begin
  if ( Options.EnableGridSnap ) then begin
    GridAlign( NewLocation.Left, Options.GridSize );
    GridAlign( NewLocation.Right, Options.GridSize );
    GridAlign( NewLocation.Top, Options.GridSize );
    GridAlign( NewLocation.Bottom, Options.GridSize );
  end;

  loc := NewLocation;
  RectImageBoundsCheck( ele, loc );

  ele.x := loc.Left;
  ele.y := loc.Top;
  ele.width := Trunc(loc.Right - loc.Left);
  ele.height := Trunc(loc.Bottom - loc.Top);

  self.DisplaySelectedElement( ele );
  if ( ele.Parent <> nil ) then
    DrawRegion( ele, CEleNodeData( ele.Parent ) );
  if ( ele.SubNodes.Count > 0 ) then begin
    DrawRegions( ele.SubNodes );
  end;
  DrawRegion( ele, ele );
  undo.Add( ele, ruRegionMove, template );
End;

procedure TI2XBaseUI.RBResizeUserChanged(Sender: TObject);
var
  B: TBitmapLayer;
  elePtr : Pointer;
  ele : CEleNodeData;
  PosLayer : TRubberbandLayer;
begin
  if Sender is TPositionedLayer then begin
    if  ( ele = nil ) then
    else if ( self.FEventOrigin = 'ImgViewMouseDownSelection' ) then
      self.FEventOrigin := ''
    else begin
      //B := TBitmapLayer(self.FSelection);
      if ( RBLayer.ChildLayer <> nil ) then begin
        B := TBitmapLayer( RBLayer.ChildLayer );
        elePtr := Pointer( B.Tag );
        self.FSelectedElement :=  CEleNodeData( elePtr );
        RegionAttributeChange( self.FSelectedElement, B.Location );
      end;
    end;
  end;
end;

procedure TI2XBaseUI.RBResizeUserChangedSpecial(Sender: TObject);
var
  B: TBitmapLayer;
  elePtr : Pointer;
  eleData : CEleNodeData;
begin
    //dbg( 'RBResizeUserChangedSpecial(): Sender.ClassName=' + Sender.ClassName + '  Sel=' + SelectedElement.AsString );
  if Sender is TPositionedLayer then begin
    if ( self.FSelection = nil ) then
      //dbg( 'self.FSelection = nil' )
    else begin
      B := TBitmapLayer(self.FSelection);
      elePtr := Pointer( B.Tag );
      eleData :=  CEleNodeData( elePtr );
      eleData.x := Trunc(B.Location.Left);
      eleData.y := Trunc(B.Location.Top);
      eleData.width := Trunc(B.Location.Right - B.Location.Left);
      eleData.height := Trunc(B.Location.Bottom - B.Location.Top);
      self.DisplaySpecialElement( eleData );
      DrawRegion( Sender, eleData, true, eleData.FillColor );
    end;
  end;
end;

procedure TI2XBaseUI.RBResizing(Sender: TObject;
  const OldLocation: TFloatRect; var NewLocation: TFloatRect;
  DragState: TDragState; Shift: TShiftState);
var
  w, h, cx, cy: Single;
  nw, nh: Single;
  GridSize : Single;
  B: TBitmapLayer;
  eleData : CEleNodeData;
  maxW, maxH, maxX, maxY, minW, minH: Cardinal;
begin
  //The following line fixes a strange bug in which the region shifts suddenly after the
  //  user selections double click
  //if ( self.FEventOrigin = 'ImgViewDblClick' ) then Exit;
  //dbg( 'RBResizeUserChangedSpecial(): Sender.ClassName=' + Sender.ClassName );
  //dbg(  '  Sel=' + SelectedElement.AsString );
  if ( self.FEventOrigin = 'ImgViewDblClick' ) then NewLocation := OldLocation;

  maxW := self.ImagePixelWidth;
  maxH := self.ImagePixelHeight;
  maxX := self.ImagePixelWidth;
  maxY := self.ImagePixelHeight;
  minW := 2;
  minH := 2;

  if Sender is TRubberbandLayer then begin
    B := TBitmapLayer(RBLayer.ChildLayer);
    if ( B.Tag = 0 ) then
      raise Exception.Create('RubberBand Layer does not have a corresponding pointer to a CEleNodeData object!');
    eleData := CEleNodeData( Pointer( B.Tag ) );
    if ( eleData.RegionType = rtXAxis ) then begin
      maxW := 10;
    end else if ( eleData.RegionType = rtYAxis ) then begin
      maxH := 10;
    end;
  end;
  with NewLocation do begin
    if (( Right - Left ) < minW ) then
      Right := (Right + minW);
    if (( Bottom - Top ) < minH ) then
      Bottom := (Bottom + minH);

    if ( eleData.RegionType = rtXAxis ) then begin
      Right := Left + maxW;
      Top := 0;
      Bottom := maxH;
    end else if ( eleData.RegionType = rtYAxis ) then begin
      Bottom := Top + maxH;
      Left := 0;
      Right := maxW;
    end;

    if ( Options.EnableGridSnap ) then begin
      GridSize := Options.GridSize;
      GridAlign( Left, GridSize );
      GridAlign( Right, GridSize );
      GridAlign( Top, GridSize );
      GridAlign( Bottom, GridSize );
    end;
    RectImageBoundsCheck( eleData, NewLocation );

    //dbg('Left=' + FloatToStrF(Left, ffGeneral, 6, 2) + ', Right=' + FloatToStrF(Right, ffGeneral, 6, 2) + ', Top=' + FloatToStrF(Top, ffGeneral, 6, 2) + ', Bottom=' + FloatToStrF(Bottom, ffGeneral, 6, 2));
  end;

  if DragState = dsMove then Exit; // we are interested only in scale operations
  if Shift = [] then Exit; // special processing is not required

  if ssCtrl in Shift then
  begin
    { make changes symmetrical }

    with OldLocation do
    begin
      cx := (Left + Right) / 2;
      cy := (Top + Bottom) / 2;
      w := Right - Left;
      h := Bottom - Top;
    end;

    with NewLocation do
    begin
      nw := w / 2;
      nh := h / 2;
      case DragState of
        dsSizeL: nw := cx - Left;
        dsSizeT: nh := cy - Top;
        dsSizeR: nw := Right - cx;
        dsSizeB: nh := Bottom - cy;
        dsSizeTL: begin nw := cx - Left; nh := cy - Top; end;
        dsSizeTR: begin nw := Right - cx; nh := cy - Top; end;
        dsSizeBL: begin nw := cx - Left; nh := Bottom - cy; end;
        dsSizeBR: begin nw := Right - cx; nh := Bottom - cy; end;
      end;
      if nw < 2 then nw := 2;
      if nh < 2 then nh := 2;

      Left := cx - nw;
      Right := cx + nw;
      Top := cy - nh;
      Bottom := cy + nh;
    end;
  end;
end;

procedure TI2XBaseUI.rbtnFirstCharClick(Sender: TObject);
begin
  if ElementDisplayEventsAreActive then begin
    self.SelectDocumentCorrection( rbtnFirstChar.tag );
    if (( FFirstCharRegionElement <> nil ) and ( self.IsBoundarySet )) then begin
      SetZoom( 3.0 );
      self.Selection := ( TBitmapLayer( self.FFirstCharRegionElement.GraphicPointer ) );
      ImgView.ScrollToCenter( self.FFirstCharRegionElement.x, self.FFirstCharRegionElement.y );
    end;
  end;
end;

procedure TI2XBaseUI.rbtnNoDocCorrectionClick(Sender: TObject);
begin
  if ElementDisplayEventsAreActive then begin
    self.SelectDocumentCorrection( rbtnNoDocCorrection.tag );
    SetZoom( 0.25 );
  end;
end;

procedure TI2XBaseUI.rbtnXYBoundaryClick(Sender: TObject);
begin
  if ElementDisplayEventsAreActive then begin
    self.SelectDocumentCorrection( rbtnXYBoundary.tag );
    if ( self.IsBoundarySet ) then
      SetZoom( 0.25 );
  end;
end;

function TI2XBaseUI.DrawRegion(Sender: TObject): TPositionedLayer;
var
  B: TBitmapLayer;
  P: TPoint;
  W, H: Single;
  FillColor: TColor32;
Begin
      FillColor := clBlue32;
      B := TBitmapLayer.Create(ImgView.Layers);
      B.Tag := 0;
      with B do
      try
        Bitmap.SetSize(100, 100);
        Bitmap.Clear(FillColor);
        Bitmap.DrawMode := dmBlend;

        with ImgView.GetViewportRect do
          P := ImgView.ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));

        W := Bitmap.Width / 2;
        H := Bitmap.Height / 2;

        with ImgView.Bitmap do begin
          Location := FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);
        end;
        B.Bitmap.MasterAlpha := 50;

        Scaled := True;
        //OnMouseDown := LayerMouseDown;
        B.OnPaint := RegionOnPaintHandler;
      except
        Free;
        raise;
      end;
      Selection := B;
      Result := B;
End;

function TI2XBaseUI.DrawRegion(Sender: TObject; eleData: CEleNodeData): TPositionedLayer;
var
  B: TBitmapLayer;
  P: TPoint;
  FillColor: TColor32;
  i, pos : smallint;
  WriteVertical : boolean;
  elePtr : Pointer;
  tmpl : ctemplate;
Begin
  //We will draw a different region single nodes vs multi nodes
  if ( eleData.nodeType = NODE_TYPE_SINGLE ) then begin

      FillColor := eleData.FillColor;
      if ( ( template.IsOffset ) and ( FillColor = clBlue32 ) ) then
        FillColor := clYellow32;

      if ( eleData.GraphicPointer = nil ) then begin
        B := TBitmapLayer.Create(ImgView.Layers);
        eleData.GraphicPointer := B;
      end else
        B := ( TBitmapLayer( eleData.GraphicPointer ) );

      B.Tag := 0;
      with B do
      try
        Bitmap.SetSize( eleData.width, eleData.height );
        Bitmap.Clear(FillColor);
        Bitmap.DrawMode := dmBlend;

        with ImgView.GetViewportRect do
          P := ImgView.ControlToBitmap( Point(eleData.x, eleData.y ) );

        with ImgView.Bitmap do begin
          Location := FloatRect(eleData.x, eleData.y,
                  eleData.x + eleData.width, eleData.y + eleData.height);
        end;
        B.Bitmap.MasterAlpha := 50;

        Scaled := True;
        //OnMouseDown := LayerMouseDown;
        B.OnPaint := RegionOnPaintHandler;
        elePtr := eleData;
        B.Tag := Integer( elePtr );
      except
        Free;
        raise;
      end;
      Result := B;
  end else if ( eleData.nodeType = NODE_TYPE_MULTI ) then begin

      if ( not template.IsOffset ) then
        FillColor := clGreen32
      else
        FillColor := clOlive32;
      //FillColor := clBlue32;
      if ( eleData.GraphicPointer = nil ) then begin
        B := TBitmapLayer.Create(ImgView.Layers);
        eleData.GraphicPointer := B;
      end else
        B := ( TBitmapLayer( eleData.GraphicPointer ) );
      B.Tag := 0;
      with B do
      try
        Bitmap.SetSize( eleData.width, eleData.height );
        Bitmap.Clear();
        Bitmap.DrawMode := dmBlend;

        with ImgView.GetViewportRect do
          P := ImgView.ControlToBitmap( Point(eleData.x, eleData.y ) );

        with ImgView.Bitmap do begin
          Location := FloatRect( eleData.x, eleData.y,
                  eleData.x + eleData.width, eleData.y + eleData.height);
        end;
        B.Bitmap.MasterAlpha := 50;

        Scaled := True;
        //OnMouseDown := LayerMouseDown;
        B.OnPaint := RegionOnPaintHandlerMulti;
        //eleData.GraphicPointer := B;
        elePtr := eleData;
        B.Tag := Integer( elePtr );
      except
        Free;
        raise;
      end;
      Result := B;
  end;
End;

procedure TI2XBaseUI.DrawSpecialRegions();
Begin
  if (template.DocumentOffsetCorrectionType = ocFirstChar ) then begin
    CreateRegion( STANDARD_REGION_FIRST_CHAR );
    FFirstCharRegionElement := template.getElement( STANDARD_REGION_FIRST_CHAR );
    FFirstCharRegionElement.X := StrToInt( self.txtFirstCharX.Text );
    FFirstCharRegionElement.Y := StrToInt( self.txtFirstCharY.Text );
    FFirstCharRegionElement.Width := StrToInt( self.txtFirstCharWidth.Text );
    FFirstCharRegionElement.Height := StrToInt( self.txtFirstCharHeight.Text );
    DrawRegion( self, FFirstCharRegionElement, true, FFirstCharRegionElement.FillColor );

  end else if (template.DocumentOffsetCorrectionType = ocBoundary ) then begin
    if ( FXAxisRegionElement = nil ) then begin
      CreateRegion( STANDARD_REGION_X_AXIS );
      FXAxisRegionElement := template.getElement( STANDARD_REGION_X_AXIS );
      FXAxisRegionElement.Y := StrToInt( self.txtXBound.Text );
      DrawRegion( self, FXAxisRegionElement, true, FXAxisRegionElement.FillColor );
    end;
    if ( FYAxisRegionElement = nil ) then begin
      CreateRegion( STANDARD_REGION_Y_AXIS );
      FYAxisRegionElement := template.getElement( STANDARD_REGION_Y_AXIS );
      FYAxisRegionElement.X := StrToInt( self.txtYBound.Text );
      DrawRegion( self, FYAxisRegionElement, true, FYAxisRegionElement.FillColor );
    end;
  end;
  self.SetSelection( nil );
End;

procedure TI2XBaseUI.DrawRegions(ht: THashTable);
var
  i : cardinal;
  arrKeyList : TKeyList;
  eleData : CEleNodeData;
  procedure ProcessElement( ele : CEleNodeData);
  var
    i : cardinal;
    arrKeyList : TKeyList;
    cNode : CEleNodeData;
  Begin
    if ele = nil then Exit;
    //if ( ele.ID = 'player_detail_for') then
    //  ele.ID := 'player_detail_for';
    if (Copy(ele.ID, 0, 2) <> STANDARD_REGION_PREFIX ) then 
      DrawRegion( self, ele );
    if ( ele.SubNodes.count > 0 ) then begin
      arrKeyList := ele.SubNodes.Keys;
      for i := 0 to Length(arrKeyList) - 1 do begin
        cNode := CEleNodeData( ele.SubNodes.get( arrKeyList[i] ));
        ProcessElement( cNode );
      end;
    end;
  End;
Begin
  if ( ht.count > 0 ) then begin
    arrKeyList := ht.Keys;
    for i := 0 to Length(arrKeyList) - 1 do begin
      eleData := CEleNodeData( ht.get( arrKeyList[i] ));
      ProcessElement( eleData );
    end;
  end;
End;

function TI2XBaseUI.DrawRegion(Sender: TObject; eleData: CEleNodeData; SpecialRegionFlag : boolean; FillColor: TColor32 ) : TPositionedLayer;
var
  B : TBitmapLayer;
  P: TPoint;
  i, pos : smallint;
  elePtr : Pointer;
  RegionType : TRegionType;
Begin
      RegionType := eleData.RegionType;
      if ( not eleData.FillColor = 0 ) then
        FillColor := eleData.FillColor;
      if ( eleData.GraphicPointer = nil ) then begin
        B := TBitmapLayer.Create(ImgView.Layers);
        eleData.GraphicPointer := B;
      end else
        B := ( TBitmapLayer( eleData.GraphicPointer ) );

      B.Tag := 0;
      with B do
      try
        if ( RegionType = rtStandard ) then
          Bitmap.SetSize( eleData.width, eleData.height  )
        else if ( RegionType = rtXAxis ) then
          Bitmap.SetSize( 10, ImagePixelHeight )
        else if ( RegionType = rtYAxis ) then
          Bitmap.SetSize( ImagePixelWidth, 10 )
        ;

        Bitmap.Clear(FillColor);
        Bitmap.DrawMode := dmBlend;

        with ImgView.GetViewportRect do
          P := ImgView.ControlToBitmap( Point(eleData.x, eleData.y ) );


        with ImgView.Bitmap do begin
          Location := FloatRect(eleData.x, eleData.y,
            eleData.x + eleData.width, eleData.y + eleData.height);
          if ( RegionType = rtStandard ) then begin
            B.Bitmap.MasterAlpha := 50;
          end else if ( RegionType = rtXAxis ) then begin
            B.Bitmap.MasterAlpha := 255;
          end else if ( RegionType = rtYAxis ) then begin
            B.Bitmap.MasterAlpha := 255;
          end else
            B.Bitmap.MasterAlpha := 50;
        end;

        Scaled := True;
        //if not SpecialRegionFlag then begin
        //OnMouseDown := LayerMouseDown;
        //end;

        B.OnPaint := RegionOnPaintHandler;
        eleData.FillColor := FillColor;
        eleData.RegionType := RegionType;
        elePtr := eleData;
        B.Tag := Integer( elePtr );
      except
        Free;
        raise;
      end;
      Selection := B;
      Result := B;
End;

procedure TI2XBaseUI.RegionOnPaintHandler(Sender: TObject; Buffer: TBitmap32);
var
  R: TRect;
  B: TBitmapLayer;
  elePtr : Pointer;
  eleData : CEleNodeData;
  FillColor : TColor32;
begin
  FillColor := clBlue32;
  if Sender is TPositionedLayer then
    with TPositionedLayer(Sender).GetAdjustedLocation do
    begin
      B := TBitmapLayer(Sender);
      if ( B.Tag <> 0 ) then begin
        elePtr := Pointer( B.Tag );
        eleData :=  CEleNodeData( elePtr );
        FillColor := eleData.FillColor;
        //if ( not template.IsOffset ) then
        //  FillColor := clBlue32
        //else
        //  FillColor := clYellow32;

      end;

      R := MakeRect(TPositionedLayer(Sender).GetAdjustedLocation);
      Buffer.FrameRectS(R.Left, R.Top, R.Right, R.Bottom, FillColor);
    end;
end;

procedure TI2XBaseUI.RegionOnPaintHandlerMulti(Sender: TObject; Buffer: TBitmap32);
var
  R, ROuter, RInner: TRect;
  B: TBitmapLayer;
  elePtr : Pointer;
  eleData : CEleNodeData;
  FillColor : TColor32;
begin
  if Sender is TPositionedLayer then
    with TPositionedLayer(Sender).GetAdjustedLocation do
    begin
      B := TBitmapLayer(Sender);
      if ( B.Tag <> 0 ) then begin
        elePtr := Pointer( B.Tag );
        eleData :=  CEleNodeData( elePtr );
        FillColor := eleData.FillColor;
        if ( not template.IsOffset ) then
          FillColor := clGreen32
        else
          FillColor := clOlive32;

      end;

      R := MakeRect(TPositionedLayer(Sender).GetAdjustedLocation);
      ROuter := R;
      Dec( ROuter.Left , 2);
      Dec( ROuter.Top , 2);
      Inc( ROuter.Right, 2);
      Inc( ROuter.Bottom, 2);

      RInner := R;
      Inc( RInner.Left , 2);
      Inc( RInner.Top , 2);
      Dec( RInner.Right, 2);
      Dec( RInner.Bottom, 2);

      Buffer.FrameRectS(ROuter, clGreen32);
      Buffer.FrameRectS(R, FillColor);
      Buffer.FrameRectS(RInner, clGreen32);
    end;
end;

procedure TI2XBaseUI.RegionOnPaintOnAxisHandler(Sender: TObject; Buffer: TBitmap32);
var
  R: TRect;
  B: TBitmapLayer;
begin
  if Sender is TPositionedLayer then
    with TPositionedLayer(Sender).GetAdjustedLocation do
    begin
      R := MakeRect(TPositionedLayer(Sender).GetAdjustedLocation);
      Buffer.FrameRectS(R.Left, R.Top, R.Right, R.Bottom, clBlue32);
    end;
end;

procedure TI2XBaseUI.TabsControlsEnabledCheck();
Begin
  if (( self.pgctlTemplateData.ActivePage.Name = 'tabImage' ) and
      ( ActiveImageIsLoaded )) then
      ControlsEnableImageProc(true);
End;


procedure TI2XBaseUI.ControlsEnableImageProc(enable: boolean);
var
  I : integer;
begin
  self.gbZoom.Enabled := enable;
  self.lblZoom.Enabled := enable;
  self.gridActionList.Enabled := enable;
  self.gridActionListUpsize.Enabled := enable;
  self.bbtnRefresh.Visible := enable;
  self.bbtnImageProcAndOCR.Visible := enable and FileExists( self.txtActiveImage.Text );
  pgctlImageProcessing.Enabled := enable;
  pnlImageInst.Enabled := enable;
  tabImage.Enabled := enable;
  for I := 0 to pnlImageInst.ControlCount - 1 do
    pnlImageInst.Controls[I].Enabled := enable;
  for I := 0 to self.tabOffset.ControlCount - 1 do
    self.tabOffset.Controls[I].Enabled := enable;
  for I := 0 to self.tabImage.ControlCount - 1 do
    self.tabImage.Controls[I].Enabled := enable;
end;

procedure TI2XBaseUI.ImgViewClick(Sender: TObject);
begin
  dbg('ImgViewClick() Sender=' + Sender.ClassName );
  //SetSelection( nil );
end;

procedure TI2XBaseUI.RenameRegion( RegionToRename : CEleNodeData; const NewRegionName : string );
var
  sRegionName, sOriginalName : string;
  tn : TTreeNode;
  RegionToRenameCopy : CEleNodeData;
  elePtr : pointer;
  B : TBitmapLayer;
begin
    sOriginalName := RegionToRename.ID;
    sRegionName := NewRegionName;
      if (( Length( sRegionName ) > 0 ) and ( sRegionName <> sOriginalName )) then begin
        if ( template.RenameIsValid( RegionToRename, sRegionName ) ) then begin
          undo.Add( RegionToRename, ruRegionRename, template, true  );
          RegionToRenameCopy := CEleNodeData.Create();
          RegionToRenameCopy.Assign( RegionToRename );
          RegionToRenameCopy.ID := sRegionName;
          template.Elements.Add( sRegionName, RegionToRenameCopy );
          template.Elements.Delete( sOriginalName );
          //template.DeleteElement( RegionToRename );
          FSelectedElement := RegionToRenameCopy;
          B := TBitmapLayer( RegionToRenameCopy.GraphicPointer );
          tn := TTreeNode( SelectedElement.TreeViewNodePointer );
          tn.Text := sRegionName;
          tn.Data := RegionToRenameCopy;
          elePtr := RegionToRenameCopy;
          B.Tag := Integer( elePtr );

        end else begin
          MessageDlg(Format( NEW_REGION_ERROR_EXISTS_FMT, [sRegionName] ),
                      mtError, [mbOk], 0);
        end;
      end else begin
          //SetCursorPos(MousePosLast.X, MousePosLast.Y);
          //Mouse_Event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
          //Mouse_Event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
      end;
end;

procedure TI2XBaseUI.RenameRegionPrompt(RegionToRename: CEleNodeData);
var
  sRegionName, sOriginalName : string;
begin
    sOriginalName := RegionToRename.ID;
    sRegionName :=InputBox(REGION_RENAME_CAPTION, REGION_RENAME_PROMPT,
      RegionToRename.ID);
      if (( Length( sRegionName ) > 0 ) and ( sRegionName <> sOriginalName )) then begin
        if ( template.RenameIsValid( RegionToRename, sRegionName ) ) then begin
          RenameRegion( RegionToRename, sRegionName );
        end else begin
          MessageDlg(Format( NEW_REGION_ERROR_EXISTS_FMT, [sRegionName] ),
                      mtError, [mbOk], 0);
        end;
      end else begin
      end;
end;

procedure TI2XBaseUI.ImgViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //dbg('ImgViewKeyDown  key=' +  IntToStr(key ));
  //if ( ( key=VK_DELETE ) and ( self.SelectedElement <> nil )) then
  //  acDeleteRegionExecute( Sender );
end;

procedure TI2XBaseUI.RegionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  B : TBitmapLayer;
  lUnit : smallint;
begin

  dbg('RegionsKeyDown  key=' +  IntToStr(Key ));
  if (ssCtrl in Shift) and (ssShift in Shift) then
    lUnit := 20
  else if (ssCtrl in Shift) then
    lUnit := 5
  else
    lUnit := 1;
  if (( FSelection <> nil ) and ( FSelection is TPositionedLayer )) then begin
    B := TBitmapLayer(FSelection);
    case Key of
      37 :
        begin
          self.FSelectedElement.x := self.FSelectedElement.x - lUnit;
        end;
      38 :
        begin
          self.FSelectedElement.y := self.FSelectedElement.y - lUnit;
        end;
      39 :
        begin
          self.FSelectedElement.x := self.FSelectedElement.x + lUnit;
        end;
      40 :
        begin
          self.FSelectedElement.y := self.FSelectedElement.y + lUnit;
        end;
    end;
    self.DisplaySelectedElement( self.FSelectedElement );
    DrawRegion( Sender, CEleNodeData( self.FSelectedElement ) );
    if ( self.FSelectedElement.Parent <> nil ) then
      DrawRegion( Sender, CEleNodeData( self.FSelectedElement.Parent ) );
  end;
end;

procedure TI2XBaseUI.HideRegion(eleData: CEleNodeData);
var
  B : TBitmapLayer;
begin
  if ( eleData.GraphicPointer = nil ) then
    raise Exception.Create( 'Graphic pointer for eleData with id of ' + eleData.ID + ' is nil' );

  if ( RBLayer <> nil ) then
    if ( self.Selection = TPositionedLayer( eleData.GraphicPointer ) )  then
//    if () ( RBLayer.ChildLayer = TPositionedLayer( eleData.GraphicPointer ) )  then
      self.SetSelection( nil );

  B := ( TBitmapLayer( eleData.GraphicPointer ) );
  B.Visible := false;
end;

function TI2XBaseUI.HitTestRegions( X, Y: Integer; var B: TBitmapLayer) : boolean;
//This function will preform a HitTest for every drawn Region.  This is great to check
// to see which region the mouse event has occured above
var
  i : cardinal;
  arrKeyList : TKeyList;
  cNode : CEleNodeData;
  oB : TBitmapLayer;
Begin
  arrKeyList := template.Elements.Keys;
  for i := 0 to Length(arrKeyList) - 1 do begin
    cNode := CEleNodeData( template.Elements.get( arrKeyList[i] ));
    if ( cNode.GraphicPointer = nil ) then
      dbg('Region Element with id of ' + cNode.ID + ' had a null graphic pointer.')
    else begin
      //bTestThisLayer := ( testGroupLevelLayers or ( (not testGroupLevelLayers) and (cNode.NodeType = NODE_TYPE_SINGLE) ));
      //if ( bTestThisLayer )  then begin
        oB := ( TBitmapLayer( cNode.GraphicPointer ) );
        if ( oB.HitTest( X, Y ) ) then begin
          B := oB;
          Result := true;
          Exit;
        end;
      //end;

    end;
  end;
  B := nil;
  Result := false;
End;

function TI2XBaseUI.IsOverBitMapLayer( X, Y: Integer; B: TBitmapLayer ) : boolean;
var
  loc : TFloatRect;
Begin
  loc := B.Location;
  Result := (((X >= loc.Top) and (X <= loc.Bottom)) AND
             ((Y >= loc.Left) and (Y <= loc.Right)));
End;

function TI2XBaseUI.HitTestRegions( X, Y: Integer; var B: TBitmapLayer; testGroupLevelLayers : boolean ) : boolean;
var
  i : cardinal;
  arrKeyList : TKeyList;
  eleData : CEleNodeData;
  oB : TBitmapLayer;
  ht :THashTable;
  function fHitTestRegion( Node : CEleNodeData; X, Y: Integer; var B: TBitmapLayer; testGroupLevelLayers : boolean ) : boolean;
  var
    i : cardinal;
    arrKeyList : TKeyList;
    cNode : CEleNodeData;
    oB : TBitmapLayer;
  Begin
    if Node = nil then Exit;
    with Node do
    begin
      if (( testGroupLevelLayers ) or
          (( not testGroupLevelLayers ) and ( Node.NodeType = NODE_TYPE_SINGLE ))) then
      begin
        if ( Node.GraphicPointer = nil ) then
          dbg('Region Element with id of ' + Node.ID + ' had a null graphic pointer.')
        else begin
          oB := ( TBitmapLayer( Node.GraphicPointer ) );
          if ( IsOverBitMapLayer( X, Y, oB ) ) then begin
          //if ( oB.HitTest( X, Y ) ) then begin
            B := oB;
            Result := true;
            Exit;
          end;
        end;
      end;
    end;

    if ( Node.SubNodes.count > 0 ) then begin
      arrKeyList := SortKeyList(Node.SubNodes.Keys);
      for i := 0 to Length(arrKeyList) - 1 do begin
        cNode := CEleNodeData( Node.Subnodes.get( arrKeyList[i] ));
        Result := fHitTestRegion( cNode, X, Y, B, testGroupLevelLayers );
        if Result then Exit;
      end;
    end;
  End;
Begin
  Result := false;
  ht := template.Elements;
  if ( ht.count > 0 ) then begin
    arrKeyList := SortKeyList(ht.Keys);
    for i := 0 to Length(arrKeyList) - 1 do begin
      eleData := CEleNodeData( ht.get( arrKeyList[i] ));
      Result := fHitTestRegion( eleData, X, Y, B, testGroupLevelLayers );
      if Result then Exit;
    end;
  end;
End;

procedure TI2XBaseUI.ImgViewDblClick(Sender: TObject);
begin
  //dbg('ImgViewClick() Sender=' + Sender.ClassName );
  if (( self.SelectedElement <> nil ) and ( self.SelectedElement.TreeViewNodePointer <> nil )) then begin
    //dbg( 'ImgViewDblClick(): Sender.ClassName=' + Sender.ClassName + '  Sel=' + SelectedElement.AsString );
    self.FEventOrigin := 'ImgViewDblClick'
  end;
end;

function TI2XBaseUI.IsCloseToRBLayer(const X, Y: Integer) : boolean;
const
  PADD = 5;
Begin
  Result := false;
  if (
      ( RBLayer <> nil ) and
      ( self.RBLayer.Visible )
     ) then begin
    if (
        (( X >= RBLayer.Location.Left - PADD ) and (X <= RBLayer.Location.Right + PADD)) and
        (( Y >= RBLayer.Location.Top - PADD ) and (Y <= RBLayer.Location.Bottom + PADD))
       )
    then
      Result := true;
  end;
End;

function TI2XBaseUI.GetNodeByText(ATree : TTreeView; AValue:String; const AVisible: Boolean): TTreeNode;
var
    Node: TTreeNode;
begin
  Result := nil;
  if ATree.Items.Count = 0 then Exit;
  Node := ATree.Items[0];
  while Node <> nil do
  begin
    if UpperCase(Node.Text) = UpperCase(AValue) then
    begin
      Result := Node;
      if AVisible then
        Result.MakeVisible;
      Break;
    end;
    Node := Node.GetNext;
  end;
end;

procedure TI2XBaseUI.ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
type
  TRegionSelectType = ( rsNoChange, rsNothing, rsCurrent, rsParent, rsSub );
var
  oB : TBitmapLayer;
  pt : TPoint;
  elePtr : Pointer;
  ElementMouseDownIsOver, SubEle : CEleNodeData;
  P: TPoint;
  RegionSel : TRegionSelectType;
begin
  dbg('ImgViewMouseDown() Sender=' + Sender.ClassName );
  P := ImgView.ControlToBitmap(Point(X,Y));
  if ( FImgViewCancelMouseDown ) then begin
    FImgViewCancelMouseDown := false;
    Exit;
  end;
  MousePosLast := ClientToScreen(point(x, y)) ;
  pt := ImgView.ControlToBitmap(Point(X,Y));
  case Button of
    mbLeft:
      begin
        SubEle :=  nil;
        if ( ImgView.Tag = 0 ) then begin
          RegionSel := rsNothing;
          //Dbg('Is Close to RBLayer=' + uStrUtil.BoolToStr(self.IsCloseToRBLayer(P.X, P.Y), 'Y', 'N') );
          ElementMouseDownIsOver := template.getElement( self.template.Elements, pt.X, pt.Y, 1, 1, 4 );
          if ( (self.IsCloseToRBLayer(P.X, P.Y)) and
               (self.FEventOrigin <> 'ImgViewDblClick') ) then
            RegionSel := rsNoChange
          else if ( ElementMouseDownIsOver = nil ) then
            RegionSel := rsNothing
          else begin
            if ( ElementMouseDownIsOver.SubNodes.Count = 0 ) then
              RegionSel := rsCurrent
            else if ( ElementMouseDownIsOver.SubNodes.Count > 0 ) then begin
              SubEle := template.getElement( ElementMouseDownIsOver.SubNodes, pt.X, pt.Y, 1, 1, 2 );
              if (( self.SelectedElement = nil ) or
                  ( self.SelectedElement <> SubEle )) then
                RegionSel := rsCurrent;
              if ( self.FEventOrigin = 'ImgViewDblClick' ) then
                if ( SubEle <> nil ) then
                  RegionSel := rsSub;
            end;
          end;
        end;

        if ( RegionSel = rsNothing ) then begin
          SetSelection( nil );
          self.SetSelectedElement( nil );
        end else if ( RegionSel = rsCurrent ) then
          Selection := TPositionedLayer( ElementMouseDownIsOver.GraphicPointer )
        else if ( RegionSel = rsParent ) then
          Selection := TPositionedLayer( CEleNodeData(SubEle.Parent).GraphicPointer )
        else if ( RegionSel = rsSub ) then
          Selection := TPositionedLayer( SubEle.GraphicPointer )
        ;
        //This code will control what happens when we move an element
        FMouseStartDragPoint.X := P.X;  //use the real mouse points because using the
        FMouseStartDragPoint.Y := P.Y;  // calculated ones will product strange results
        Exit;
      end;
   mbRight:
      begin
          SetRegionsPopupMenuOptions( Sender );
      end;
  end;

  ImgView.Tag := 0;  //clear out the layer recieved flag
end;

procedure TI2XBaseUI.ImgViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
  oB : TBitmapLayer;
  elePtr : Pointer;
  eleData : CEleNodeData;
  NyHint: string;
  XDiff, YDiff : Integer;
begin
  P := ImgView.ControlToBitmap(Point(X,Y));
  bStatus.Panels[ Integer(sbXPos) ].Text := Format('X: %d', [P.X]);
  bStatus.Panels[ Integer(sbYPos) ].Text := Format('Y: %d', [P.Y]);
  if ( ssLeft in Shift ) and ( self.SelectedElement <> nil )then
    RegionMove( self.SelectedElement,
        P.X - self.FLastMouseLocation.X,
        P.Y - self.FLastMouseLocation.Y);

  FLastMouseLocation.X := P.X;
  FLastMouseLocation.Y := P.Y;
end;

procedure TI2XBaseUI.ImgViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
  ele : CEleNodeData;
begin
  P := ImgView.ControlToBitmap(Point(X,Y));
  case Button of
    mbLeft:
      begin
        self.FMouseStartDragPoint.X := 0;
        self.FMouseStartDragPoint.Y := 0;

        if ( self.SelectedElement <> nil ) then begin
          ele := self.SelectedElement;
          SetSelection( nil );
          self.SetSelectedElement( nil );
          self.SetSelectedElement( ele );
          Selection := DrawRegion( self, ele );
        end;

      end;
    mbRight:
      begin
      end;
  end;
  self.FEventOrigin := '';
end;

procedure TI2XBaseUI.ImgViewPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  TileHeight := 13;
  TileWidth := 13;

  TilesHorz := Buffer.Width div TileWidth;
  TilesVert := Buffer.Height div TileHeight;
  TileY := 0;

  for J := 0 to TilesVert do
  begin
    TileX := 0;
    OddY := J and $1;
    for I := 0 to TilesHorz do
    begin
      R.Left := TileX;
      R.Top := TileY;
      R.Right := TileX + TileWidth;
      R.Bottom := TileY + TileHeight;
      Buffer.FillRectS(R, Colors[I and $1 = OddY]);
      Inc(TileX, TileWidth);
    end;
    Inc(TileY, TileHeight);
  end;
end;

procedure TI2XBaseUI.Index1Click(Sender: TObject);
var
  sHelpFile : string;
begin
  //THANK YOU http://www.helpware.net/delphi/
  HtmlHelp(0, pchar(Application.HelpFile), HH_DISPLAY_TOPIC, 0);
end;

procedure TI2XBaseUI.UpdateImageProcessingList();
Var
  arrKeyList, arrInstKeyList : TKeyList;
  oImgInst : TI2XImageInstruction;
  sDLLKeyName, sInstKeyName : string;
  oImageDLL : TDLLImageProc;
  iDLLs, iInst : integer;
  listitem : TListItem;
Begin
  arrKeyList := PluginManager.ImageDLLs.Keys;
  for iDLLs := 0 to Length(arrKeyList) - 1 do begin
    sDLLKeyName := arrKeyList[iDLLs];
    oImageDLL := PluginManager.ImageDLL[ sDLLKeyName ];
    arrInstKeyList := oImageDLL.ImageInstructions.Instructions.Keys;
    for iInst := 0 to Length(arrInstKeyList) - 1 do begin
      sInstKeyName := arrInstKeyList[iInst];
      oImgInst := TI2XImageInstruction( oImageDLL.ImageInstructions.Instructions.Items[ sInstKeyName ] );

      lstInstAvailable.AddItem(oImgInst.DescrName + ' (' + oImageDLL.ShortName + ')', oImgInst );
    end;
  end;
End;

function TI2XBaseUI.SortWinControlsByTopLocation( arrCtls : TWinControlArray ) : TWinControlArray;
var
  i, j, cnt: Integer;
  temp: TWinControl;
  arr : TWinControlArray;
begin
  SetLength( arr, Length( arrCtls ));
  cnt := 0;
  for i := 0 to length( arrCtls ) - 1 do begin
    if (( arrCtls[i].Parent <> nil ) and ( arrCtls[i].Parent.Visible ))  then begin
      arr[ cnt ] := arrCtls[ i ];
      Inc( cnt );
    end;
  end;
  SetLength( arr, cnt );
  // bubble sort
  for i := 0 to length( arr ) - 1 do begin
    for j := 0 to ( length( arr ) - 1 ) - i do begin
      // Condition to handle i=0 & j = 9. j+1 tries to access x[10] which
      // is not there in zero based array
      if ( j + 1 = length(arr) ) then
        continue;
      if ( arr[j].Parent.Top > arr[j+1].Parent.Top ) then begin
        temp := arr[j];
        arr[j]   := arr[j+1];
        arr[j+1] := temp;
      end; // endif
    end; // endwhile
  end; // endwhile
  Result := arr;
end;

function TI2XBaseUI.GetSelectionTemplate: CTemplate;
begin
  dbg( 'A. self.template.TemplateFileName=' + self.template.TemplateFileName );
  if ( self.templateSection <> nil ) then
    FreeAndNil( templateSection );
  self.template.Clone( self.templateSection, self.SelectedElement.ID );
  Result := self.templateSection;
  dbg( 'B. self.template.TemplateFileName=' + self.template.TemplateFileName );
end;

procedure TI2XBaseUI.SetOCRMenuOptions( EnableFlag : boolean );
Begin
  btnDoImageInst.Enabled := EnableFlag;
  pmnuExecuteActionList.Enabled := EnableFlag;
  pmnuProcInstAndOCR.Enabled := EnableFlag;
End;

END.
