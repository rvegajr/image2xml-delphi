library i2xocr_gocr;

uses
  FastMM4 in '..\i2xcommon\FastMM4.pas',
  FastMM4Messages in '..\i2xcommon\FastMM4Messages.pas',
  SysUtils,
  Classes,
  Windows,
  uI2XOCR_GOCR in 'uI2XOCR_GOCR.pas',
  uDWImage in '..\i2xcommon\uDWImage.pas',
  uI2XConstants in '..\i2xcommon\uI2XConstants.pas',
  uI2XPlugin in '..\i2xcommon\uI2XPlugin.pas',
  uStrUtil in '..\i2xcommon\uStrUtil.pas',
  uHashTable in '..\i2xcommon\uHashTable.pas',
  MapStream in '..\i2xcommon\MapStream.pas',
  uFileDir in '..\i2xcommon\uFileDir.pas',
  uOmniXML in '..\i2xcommon\uOmniXML.pas',
  uCmdLine in '..\i2xcommon\uCmdLine.pas',
  uI2XMemMap in '..\i2xcommon\uI2XMemMap.pas',
  uI2XOCR in '..\i2xcommon\uI2XOCR.pas';

const
  NAME='Tesseract OCR Engine Plugin for I2X';
  SHORT_NAME='GOCR';
  VERSION = '00.00.02';
type
  TOnEvent = procedure();

{$R *.res}
var
  oI2XGOCR : TI2XGOCR;
  ParmFileName : string;

// $$  STANDARD i2X OCR Lib Code below ---->  (search for next $$ )
var
  GUID : string;
  Descr : string;
  OnInitialize : TOnEvent;
  OnTerminate : TOnEvent;

function InitCheck(): boolean;
Begin
  if ( Length(GUID) = 0 ) then
    raise Exception.Create('You must call Initialize before calling any of the procs in this DLL.');
End;

function Initialize(): integer; stdcall;
var
  oguid : TGUID;
begin
  ParmFileName := StringReplace( ThisDLLPath, 'dll', 'i2xcfg', [rfReplaceAll]);
  if ( not FileExists(ParmFileName) ) then
    raise Exception.Create( 'Parm File ' + ParmFileName + ' is missing!' );

  Result := -1;

  CreateGuid(oguid);
  GUID := GuidToString(oguid);
  GUID := Copy(GUID, 2, Length(GUID) - 2);

  if ( Assigned(OnInitialize) ) then OnInitialize();
  Result := 0;
end;

function Terminate(): integer; stdcall;
begin
  if ( Assigned(OnTerminate) ) then OnTerminate();
  Result := -1;
  Result := 0;
end;

function GetVersion( lpVersionInfo : PChar; lMaxLen : integer ) : integer; stdcall;
begin
  InitCheck();
  Result := -1;
  StrLCopy( lpVersionInfo, PCHar( VERSION ), lMaxLen );
  Result := StrLen(lpVersionInfo);
end;

function GetName( lpName : PChar; lMaxLen : integer ) : integer; stdcall;
begin
  InitCheck();
  Result := -1;
  StrLCopy( lpName, PCHar( NAME ), lMaxLen );
  Result := StrLen(lpName);
end;

function GetShortName( lpShortName : PChar; lMaxLen : integer ) : integer; stdcall;
begin
  InitCheck();
  Result := -1;
  StrLCopy( lpShortName, PCHar( SHORT_NAME ), lMaxLen );
  Result := StrLen(lpShortName);
end;

function GetGUID( lpGUID : PChar; lMaxLen : integer ) : integer; stdcall;
begin
  InitCheck();
  Result := -1;
  StrLCopy( lpGUID, PCHar( GUID ), lMaxLen );
  Result := StrLen( lpGUID );
end;

function GetDescription( lpDescription : PChar; lMaxLen : integer ) : integer; stdcall;
begin
  InitCheck();
  Result := -1;
  StrLCopy( lpDescription, PCHar( DESCR ), lMaxLen );
  Result := StrLen( lpDescription );
end;

function GetLastErrorMessage( lpErrorMessage : PChar; lMaxLen : integer ) : integer; stdcall;
begin
  InitCheck();
  Result := -1;
  StrLCopy( lpErrorMessage, PCHar( oI2XGOCR.LastErrorString ), lMaxLen );
  Result := StrLen( PCHar( oI2XGOCR.LastErrorString ) );
end;

function GetLastErrorCode() : integer; stdcall;
begin
  InitCheck();
  Result := -1;
  Result := oI2XGOCR.LastErrorCode;
end;

// <--- END OF STANDARD i2X OCR Lib Code $$

function OCRImage( lpImageMMID : PChar; lpOCRDataMMID : PChar ) : integer; stdcall;
var
  sImageMMID, sOCRDataMMID : string;
begin
  InitCheck();
  sImageMMID:= string( lpImageMMID );
  sOCRDataMMID := string( lpOCRDataMMID );
  try
   if ( oI2XGOCR.OCRImage(sImageMMID, sOCRDataMMID )) then
      Result := 0
    else
      Result := -1;
  finally
  end;
end;

function CleanUp() : integer; stdcall;
begin
  InitCheck();
  oI2XGOCR.CleanUp();
  try
  finally
  end;
end;

function SetOptions( lpOptions : PChar ) : integer; stdcall;
var
  sOptions : string;
begin
  InitCheck();
  sOptions:= string( lpOptions );
  try
    //oI2XGOCR.SetOptions( sOptions );
  finally
  end;
end;

procedure _Initialize();
begin
  oI2XGOCR := TI2XGOCR.Create( GUID, ParmFileName );
  Descr := NAME;
end;

procedure _Terminate();
begin
  FreeAndNil( oI2XGOCR );
end;

procedure OnDLLLoad;
begin
//Do not want to do too many heavy duty stuff here according to the CodeGear docs..
//  Use the _Initialize and _Terminate methods instead
  OnInitialize := _Initialize;
  OnTerminate := _Terminate;
end;

procedure OnDLLUnLoad;
begin
  //Do not want to do too many heavy duty stuff here according to the CodeGear docs..
  //  Use the _Initialize and _Terminate methods instead
end;

procedure MyDLLProc(dwReason: Integer);
begin
  case dwReason of
    DLL_PROCESS_ATTACH:   OnDLLLoad;
    DLL_PROCESS_DETACH:   OnDLLUnLoad;
  end;
end;

exports
  Initialize,  //Standard I2X Export
  Terminate,   //Standard I2X Export
  GetVersion,  //Standard I2X Export
  GetName,     //Standard I2X Export
  GetGUID,     //Standard I2X Export
  GetShortName,//Standard I2X Export
  GetDescription,//Standard I2X Export
  GetLastErrorMessage,//Standard I2X Export
  GetLastErrorCode,//Standard I2X Export
  OCRImage,
  CleanUp,
  SetOptions;

begin
  DllProc := MyDLLProc;
  DllProc(DLL_PROCESS_ATTACH);
end.
