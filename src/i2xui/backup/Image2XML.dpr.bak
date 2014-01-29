program Image2XML;

uses
  FastMM4 in '..\i2xcommon\FastMM4.pas',
  FastMM4Messages in '..\i2xcommon\FastMM4Messages.pas',
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  SysUtils,
  uCmdLine in '..\i2xcommon\uCmdLine.pas',
  uHashTable in '..\i2xcommon\uHashTable.pas',
  uFileDir in '..\i2xcommon\uFileDir.pas',
  uFileIO in '..\i2xcommon\uFileIO.pas',
  uReg in '..\i2xcommon\uReg.pas',
  uStrUtil in '..\i2xcommon\uStrUtil.pas',
  uDebug in '..\i2xcommon\uDebug.pas',
  uI2XTemplate in '..\i2xcommon\uI2XTemplate.pas',
  RegExpr in '..\i2xcommon\RegExpr.pas',
  uDWImage in '..\i2xcommon\uDWImage.pas',
  uI2XDLL in '..\i2xcommon\uI2XDLL.pas',
  MapStream in '..\i2xcommon\MapStream.pas',
  uI2XConstants in '..\i2xcommon\uI2XConstants.pas',
  uI2XOCR in '..\i2xcommon\uI2XOCR.pas',
  uI2XPlugin in '..\i2xcommon\uI2XPlugin.pas',
  uI2XProduct in '..\i2xcommon\uI2XProduct.pas',
  UIOCRProductViewDialog in 'UIOCRProductViewDialog.pas' {OCRProductViewDialog},
  UII2XBase in 'UII2XBase.pas' {I2XBaseUI},
  UII2XMain in 'UII2XMain.pas' {I2XBaseUI1},
  UII2XOptions in 'UII2XOptions.pas' {OptionsBaseUI1},
  UIOptionsBase in 'UIOptionsBase.pas' {OptionsBaseUI},
  uHashTableXML in '..\i2xcommon\uHashTableXML.pas',
  uI2XPluginManager in '..\i2xcommon\uI2XPluginManager.pas',
  uOmniXML in '..\i2xcommon\uOmniXML.pas',
  uI2XDWImageUtils in '..\i2xcommon\uI2XDWImageUtils.pas',
  uI2XUndo in '..\i2xcommon\uI2XUndo.pas',
  uI2XScan in '..\i2xcommon\uI2XScan.pas',
  uI2XMainController in 'uI2XMainController.pas',
  uImage2XML in '..\i2xcommon\uImage2XML.pas',
  uI2XThreadBase in '..\i2xcommon\uI2XThreadBase.pas',
  uI2XProductThread in '..\i2xcommon\uI2XProductThread.pas',
  uI2XOptions in '..\i2xcommon\uI2XOptions.pas',
  uI2XOCRProcThread in '..\i2xcommon\uI2XOCRProcThread.pas',
  uI2XImageProcJobThread in '..\i2xcommon\uI2XImageProcJobThread.pas',
  uI2XJob in '..\i2xcommon\uI2XJob.pas',
  uAppCache in '..\i2xcommon\uAppCache.pas',
  uI2XMemMap in '..\i2xcommon\uI2XMemMap.pas',
  UIDialog in '..\i2xcommon\UIDialog.pas' {frmDialog},
  UII2XDimmer in 'UII2XDimmer.pas' {I2XDimmerUI},
  UII2XDialog in 'UII2XDialog.pas' {frmI2XDialogUI},
  Help in 'Help.pas',
  hh in '..\i2xcommon\hh.pas',
  hh_funcs in '..\i2xcommon\hh_funcs.pas',
  IP_API in '..\i2xcommon\IP_API.PAS',
  PPP4 in '..\i2xcommon\PPP4.pas',
  _TEST in '..\i2xcommon\_TEST.pas',
  Unlock in '..\i2xcommon\Unlock.pas' {UnlockScreen},
  PPP_FUNC in '..\i2xcommon\PPP_FUNC.PAS',
  ONLINE in '..\i2xcommon\ONLINE.pas' {frmOnline},
  SKCA32 in '..\i2xcommon\SKCA32.PAS',
  UII2XAbout in 'UII2XAbout.pas' {AboutBox};

{$R *.res}
var
  cmd : TCommandLine;
  JobFile, DebugFileName : string;
begin
  try
    cmd := TCommandLine.Create( CmdLine ); // the parameter is provided by the system

    AppPath := ExtractFilePath( Application.ExeName );
    _FileDir.GetVersion( Application.ExeName, sVERSION );

    if ( cmd.Exists( 'o' ) )  then begin
      uDebug.DebugOutputFileName := AbsolutePath(_FileDir.Slash( cmd.GetParameter( 'o', true ) ));
      DebugFileName := uDebug.DebugOutputFileName; //Job Debug output override
    end else begin
      DebugFileName := '';
      uDebug.DebugOutputFileName := AppPath + DebugOutputFileName;
    end;
    ForceDirectories( ExtractFilePath( DebugOutputFileName ));
    if (( Length(DebugOutputFileName) > 0 ) and FileExists(DebugOutputFileName)) then DeleteFile(DebugOutputFileName);

    DLLSearchPath := AppPath;
    DebugLevel := dbNone;
    if ( cmd.Exists( 'x' ) )  then
      DLLSearchPath := AbsolutePath(_FileDir.Slash( cmd.GetParameter( 'x', true ) ));
    if ( cmd.Exists( 'd' ) )  then begin
      if ( cmd.GetParameter( 'd', true ) = '1' ) then
        DebugLevel := dbDetailed
      else if ( cmd.GetParameter( 'd', true ) = '2' ) then
        DebugLevel := dbVerbose;
    end;

    JobFile := '';
    if ( cmd.Exists( 'j' ) and ( FileExists(cmd.GetParameter( 'j', true ) ))) then
      JobFile := cmd.GetParameter( 'j', true );
  finally
    FreeAndNil( cmd );
  end;
  if ( Integer( DebugLevel ) >= 1 ) then begin
    dbg( 'Image2XML Started on ' + uStrUtil.sNow );
    dbg( '      AppPath =' + AppPath );
    dbg( 'DLLSearchPath =' + DLLSearchPath );
    dbg( '      JobFile =' + JobFile );
  end;
  if ( FileExists( JobFile )) then begin
    try
      if ( Integer( DebugLevel ) >= 1 ) then dbg( 'This execution of Image2XML will be in "Job Console" mode.' );
      JobExec := TI2XJobExecute.Create( JobFile );
      JobExec.DebugOutputOverride := DebugFileName;
      JobExec.Run;
    finally
      FreeAndNil( JobExec );
    end;
  end else begin
    if ( Integer( DebugLevel ) >= 1 ) then dbg( 'This execution of Image2XML will bring up the User Interface' );
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'Image2XML';
    Application.HelpFile := AppPath + 'Image2XML.chm';
    //Application.CreateForm(TI2XMainUI, frmI2XMain);
    Application.CreateForm(TI2XMainUI, frmI2XMain);
  Application.CreateForm(TUnlockScreen, UnlockScreen);
  Application.CreateForm(TfrmOnline, frmOnline);
  Application.CreateForm(TAboutBox, AboutBox);
  //Application.CreateForm(TI2XDialogUI, frmI2XDialogUI);
    Application.CreateForm(TI2XDialogUI, frmI2XDialogUI);
    Application.CreateForm(TI2XDimmerUI, frmDimmer);
    Application.Run;
  end;
end.
