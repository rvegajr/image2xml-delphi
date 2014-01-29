unit uImage2XML;
interface
uses
  uFileDir,
  uStrUtil,
  uHashTable,
  Classes,
  SysUtils,
  Dialogs,
  Controls,
  Math,
  uDebug,
  uI2XDLL,
  uI2XConstants,
  uReg,
  Windows,
  GR32_Image,
  uAppCache
  ;
const
  APP_NAME = 'Image2XML';
  NEW_TEMPLATE = 'Untitled Template';
  REG_KEY = 'Software\NoctuSoft\' + APP_NAME;
  TEMP_DIR_QUAL = 'i2x\';
  TEMP_DIR_SYMBOL = '<TEMP>i2x\';
  TEMPLATE_FILE_EXT = '.i2x';
  JOB_FILE_EXT = '.i2xjob';
  BAT_FILE_EXT = '.bat';
  LOG_FILE_EXT = '.log';
  UNSAVED_INDICATOR='{Unsaved}';
  APP_PATH_SYMBOL = '<APPPATH>';

  FILEDLG_IMAGE='Supported files (*.bmp *.jpeg *.jpg *.tiff *.tif )|*.bmp;*.tiff;*.tif;*.jpg;*.jpeg;';
  FILEOPENDLG_IMAGE_TEXT='Open Image';
  FILESAVEDLG_IMAGE_TEXT='Save Image';

  FILEDLG_TEMPLATE='Template (*' + TEMPLATE_FILE_EXT + ')|*' + TEMPLATE_FILE_EXT + ';';
  FILEOPENDLG_TEMPLATE_TEXT='Open Template';
  FILESAVEDLG_TEMPLATE_TEXT='Save Template';

  FILEDLG_TEMPLATE_IMAGE='Supported files (*.bmp *.jpeg *.jpg *.tiff *.tif *' + TEMPLATE_FILE_EXT + ')|*.bmp;*.tiff;*.tif;*.jpg;*.jpeg;*' + TEMPLATE_FILE_EXT + ';';
  FILEOPENDLG_TEMPLATE_IMAGE_TEXT='Open Template or Image';
  FILESAVEDLG_TEMPLATE_IMAGE_TEXT='Save Template or Image';

  NODE_TYPE_CHANGE_TEXT='Would you like to make this field a parent field and the source field the child?';

  NEW_REGION_TITLE = 'New Region Name';
  NEW_REGION_TEXT = 'What would you like the new region to be called?';
  NEW_REGION_ERROR_EXISTS = 'An Element with this ID already exists.';
  NEW_REGION_ERROR_EXISTS_FMT = 'An Element with this ID [%s] already exists.';
  IMAGE_DNE_FMT = 'Image "%s" does not exist.';
  INVALID_NEW_TEMPLATE_NAME = 'That template name cannot be used, (do you have any ./?\<> characters in it?),  please correct it and click ok to continue.';

  NOT_SAVED_TEXT='This template has not been saved yet.  Would you like to save it first before creating a new one?';
  NOT_SAVED_TEXT_CLOSE='This template has not been saved yet.  Would you like to save it first before exiting this application?';
  IMAGE_PROC_CACHE_UPDATING = 'Image Proc cache is updating..  try again in a few moments.';

  FILE_EXISTS_TEXT='This file already exists,  are you sure you want to write over it?';
  IMGFILE_EXISTS_TEXT='An image with this name already exists,  are you sure you want to write over it?';

  DOC_CORRECTION_NONE='If this is selected,  the application will attempt to process every image without any attempt to figure out if the printed image was shifted from the model.';
  DOC_CORRECTION_FIRSTCHAR='If this is selected,  the specified region will contain the first character.  When the application processes every image with this template,  it will the offset the image relative to this first character.  ' + #10#13 + #10#13 + #10#13 + 'Very useful for left justified forms data on forms.';
  DOC_CORRECTION_XY='If this is selected,  the application take the offset the form by the furthest left pixel and/or the first top character pixel.' + #10#13 + #10#13 + 'A "0" in X or Y will cause the application to ignore that axis when offsetting the image before processing.';

  OCR_COLOR_WARNING_TEXT='WARNING:  The image you wish to process has more than 2 colors.'+ #10#13 + 'It is a good odea to reduce the colors using the image processing instructions before attempting to OCR as colors may interfere with the OCR Process.' + #10#13 + 'Instructions such as Grey Thresholds, Sharpen and Color Reduce to 2 bits can help with this.' + #10#13 + #10#13 + 'Are you sure you wish to continue the OCR?';
  OCR_COLOR_WARNING='WARNING:  Image has more than 2 colors,  so the OCR result may be unpredictable.';

  STANDARD_REGION_FIRST_CHAR = STANDARD_REGION_PREFIX + 'FIRST_CHAR';
  STANDARD_REGION_X_AXIS = STANDARD_REGION_PREFIX + 'X_AXIS';
  STANDARD_REGION_Y_AXIS = STANDARD_REGION_PREFIX + 'Y_AXIS';

type
  TStatusBarPanels = (
    sbXPos = 0,
    sbYPos,
    sbZoom,
    sbSaveInd,
    sbProgressBar,
    sbStatusText
  );
  TPercent = 0..100;
  TProgressBarState = (
    pgIgnore = 0,
    pgDisable,
    pgStart,
    pgStop
  );
  TWinControlArray = array of TWinControl;
  TStatusChangeEvent = procedure (Sender: TObject; msgText : string) of object;
  TErrorRaisedEvent = procedure (Sender: TObject; exc : TObject; procedureName : string) of object;
  TImageProcEvent = procedure (Sender: TObject; fileName : string) of object;
  TWriteLogEvent = procedure (Sender: TObject; msgText : string) of object;
  TElementControlsDisplay = (ecDisable, ecSingle, ecMulti);
  TRegionType = (rtStandard, rtXAxis, rtYAxis);

  TObjectChangeNotify = class(TObject)
    private
      FOnChange : TNotifyEvent;
    public
      property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;
  TRegDataType = (rgString, rgInteger, rgExtended, rgBoolean);
  TRegValue = record
    DataType: TRegDataType;

    IntValue: Integer;
    StringValue: AnsiString;
    FloatValue: Extended;
    BooleanValue: Boolean;

    class operator Implicit(aValue: Integer): TRegValue;
    class operator Implicit(const aValue: AnsiString): TRegValue;
    class operator Implicit(aValue: Extended): TRegValue;
    class operator Implicit(aValue: Boolean): TRegValue;
  end;

  TI2XReg = class(CReg)
    private
      FStrUtil : CStrUtil;
      function GetValue(key: string): TRegValue;
      procedure SetValue(key: string; const Value: TRegValue);
    public
      constructor Create();
      destructor Destroy; override;
      property Value[key: string]: TRegValue read GetValue write SetValue; default;
      function Get( const key : string; defaultValue : string ) : string; overload;
      function Get( const key : string; defaultValue : integer ) : integer; overload;
      function Get( const key : string; defaultValue : Extended ) : Extended; overload;
      function Get( const key : string; defaultValue : boolean ) : boolean; overload;
  end;

var
  sVERSION : string;
  LogFileName : string;
  OCRDumpFileName : string;
  AppTempDir : string;
  AppPath : string;
  DLLSearchPath : string;
  _FileDir : CFileDir;
  ignoreException : boolean = false;
  DebugLevel : TDebugLevel;
  //Undo : TI2XUndo;
  //UndoItemFocus : TUndoItem;

  ResourceStrings : THashStringTable;

Procedure WriteLog(str : string); overload;
Procedure WriteLog(str, sFileName : string);  overload;
Procedure ClearLog(); overload;
Procedure ClearLog(sFileName : string); overload;
Function Confirm(sMessage : string): boolean;
Function Replace(Expression,Find,Replace:String):string;
procedure FreeAndNilSafe(var Obj);
procedure DebugString(aText: string); overload;
procedure DebugString(aCaption, aText: string); overload;
procedure dbg(aText: string);
function IsValidI2XImage( fileName : TFileName ) : boolean;
function AbsolutePath( const path : string; const RelativeOffsetPath : string = APP_PATH_SYMBOL ) : string;

implementation
function AbsolutePath( const path : string; const RelativeOffsetPath : string ) : string;
var
  sFileName : string;
Begin
  sFileName := ExtractFileName( path );
  if ( ( length(path) > 0 ) and ( path[1] = '.' ) )   then
    if ( RelativeOffsetPath = APP_PATH_SYMBOL ) then
      Result := ExpandUNCFileName( AppPath + path )
    else
      Result := ExpandUNCFileName( RelativeOffsetPath + path )
  else
    Result := ExpandUNCFileName( path );
  if (( Length( sFileName ) = 0 ) and  //This means original was a path.. so add a slash at the end if missing
      ( Length( Result ) > 0 ) and
      ( Result[Length(Result)] <> '\' )) then
    Result := Result + '\';

End;

function IsValidI2XImage( fileName : TFileName ) : boolean;
var
  sExt : string;
Begin
  sExt := LowerCase(ExtractFileExt( fileName ));
  Result := ((sExt = '.tiff') or
             (sExt = '.tif') or
             (sExt = '.jpg') or
             (sExt = '.jpeg') or
             (sExt = '.bmp')
            );
End;

Procedure dbg(aText: string);
Begin
  uDebug.dbg('Image2XML', aText);
End;

Procedure DebugString(aText: string);
Begin
  uDebug.DebugString('Image2XML', aText);
End;

Procedure DebugString(aCaption, aText: string);
Begin
  uDebug.DebugString( aCaption , aText);
End;

Procedure FreeAndNilSafe(var Obj);
Begin
  try
    ignoreException := true;
    try
      FreeAndNil( Obj );
    except
    end;
  finally
    ignoreException := false;
  end;
End;

function Replace(Expression,Find,Replace:String):string;
Var
  c: integer;
  sTemp, sTemp2: string;
Begin
      sTemp := Expression;
      c := pos(Find,sTemp);
      while c <> 0 do
       Begin
         sTemp2 := copy(sTemp, 1, c - 1) + Replace + copy(sTemp,c+1,C+Length(sTemp));
         sTemp := sTemp2;
         c := pos(Find, sTemp);
       End;
      Result := sTemp;
End;

Function Confirm(sMessage : string) : boolean;
Var
  buttonSelected : Integer;
Begin
  // Show a confirmation dialog
  buttonSelected := MessageDlg( sMessage, mtConfirmation, mbOKCancel, 0);

  // Show the button type selected
  if ( buttonSelected = mrOK ) then
    result := true
  else if ( buttonSelected = mrCancel ) then
    result := false;
End;

Procedure WriteLog(str : string);
Begin
  _FileDir.WriteString( str, LogFileName, true );
End;

Procedure WriteLog(str, sFileName : string);
Begin
  _FileDir.WriteString( str, sFileName, true );
End;

Procedure ClearLog();
Begin
  _FileDir.WriteString( '', LogFileName, false );
End;

Procedure ClearLog(sFileName : string);
Begin
  _FileDir.WriteString( '', sFileName, false );
End;

procedure ConsoleStatusChange(sender: TObject; msgText: string);
begin
  WriteLn( msgText );
End;

procedure ConsoleOnError(Sender: TObject; exc : TObject; procedureName : string);
begin
  if ( exc <> nil )  then begin
    WriteLn( procedureName + ': ' + (exc as Exception).Message);
  end else begin
    WriteLn( procedureName );
  end;
End;

procedure ConsoleOnTemplateOCRThreadTerminate( Sender: TObject );
Begin
  WriteLn( 'OCR Thread Terminated...' );
End;

procedure ConsoleWriteLog( Sender: TObject; msgText : string );
Begin
  WriteLn( msgText );
End;

procedure LoadResourceStrings();
Begin
  ResourceStrings.Add('HASHTABLE', '');
End;

{ TRegValue }

class operator TRegValue.Implicit(const aValue: AnsiString): TRegValue;
begin
  Result.DataType := rgString;
  Result.StringValue := aValue;
end;

class operator TRegValue.Implicit(aValue: Integer): TRegValue;
begin
  Result.DataType := rgInteger;
  Result.IntValue := aValue;
end;

class operator TRegValue.Implicit(aValue: Boolean): TRegValue;
begin
  Result.DataType := rgBoolean;
  Result.BooleanValue := aValue;
end;

class operator TRegValue.Implicit(aValue: Extended): TRegValue;
begin
  Result.DataType := rgExtended;
  Result.FloatValue := aValue;
end;

{ TI2XReg }

constructor TI2XReg.Create;
begin
  FStrUtil := CStrUtil.Create();
end;

destructor TI2XReg.Destroy;
begin
  FreeAndNil( FStrUtil );
  inherited;
end;

function TI2XReg.Get(const key: string; defaultValue: integer): integer;
begin
  try
  If not ( TryStrToInt( Self.Get( key, IntToStr(defaultValue) ), Result )) then
    Result := defaultValue;
  except
    Result := defaultValue;
  end;
end;

function TI2XReg.Get(const key: string; defaultValue: string): string;
begin
  Result := self.RegGet( REG_KEY, key, defaultValue, HKEY_CURRENT_USER );
end;

function TI2XReg.Get(const key: string; defaultValue: boolean): boolean;
begin
  try
    Result :=
        Self.Get( key,
          uStrUtil.BoolToStr(defaultValue, 'true', 'false')
        ) = 'true';
  except
    Result := defaultValue;
  end;
end;

function TI2XReg.Get(const key: string; defaultValue: Extended): Extended;
begin
  try
    if not TryStrToFloat( Self.Get( key, FloatToStr(defaultValue) ), Result ) then
      Result := defaultValue;
  except
    Result := defaultValue;
  end;
end;

function TI2XReg.GetValue(key: string): TRegValue;
var
  sVal : string;
begin
  sVal := self.RegGet( REG_KEY, key );
  if (( Length(sVal) > 0 ) and ( FStrUtil.IsNumeric( sVal ) )) then begin
    if ( Pos('.', sVal ) > 0 ) then begin
      Result.DataType := rgExtended;
      Result.FloatValue := StrToFloat( sVal );
    end else begin
      Result.DataType := rgInteger;
      Result.IntValue := StrToInt( sVal );
    end;
  end else if (( sVal = 'true' ) or ( sVal = 'false' )) then begin
    Result.DataType := rgInteger;
    Result.BooleanValue := ( sVal = 'true' );
  end else begin
    Result.DataType := rgString;
    Result.StringValue := sVal;
  end;
end;

procedure TI2XReg.SetValue(key: string; const Value: TRegValue);
begin
  if ( value.DataType = rgString ) then
    self.RegSet( REG_KEY, key, Value.StringValue, HKEY_CURRENT_USER )
  else if ( value.DataType = rgInteger ) then
    self.RegSet( REG_KEY, key, IntToStr(Value.IntValue), HKEY_CURRENT_USER )
  else if ( value.DataType = rgExtended ) then
    self.RegSet( REG_KEY, key, FloatToStr( Value.FloatValue ), HKEY_CURRENT_USER )
  else if ( value.DataType = rgBoolean ) then
    self.RegSet( REG_KEY, key, uStrUtil.BoolToStr( Value.BooleanValue, 'true', 'false'), HKEY_CURRENT_USER );
end;

initialization
  _FileDir := CFileDir.Create;
  ResourceStrings := THashStringTable.Create;
  LogFileName := _FileDir.GetSystemTempDir + TEMP_DIR_QUAL + 'Image2XML' + LOG_FILE_EXT;
  OCRDumpFileName := _FileDir.GetSystemTempDir + TEMP_DIR_QUAL + 'OCRDump' + LOG_FILE_EXT;
  AppTempDir := _FileDir.GetSystemTempDir + TEMP_DIR_QUAL;
  if ( not DirectoryExists( AppTempDir ) ) then ForceDirectories( AppTempDir );

finalization
  FreeAndNil( _FileDir );
  FreeAndNil( ResourceStrings );
end.
