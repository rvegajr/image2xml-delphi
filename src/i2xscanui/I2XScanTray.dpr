program I2XScanTray;

uses
  FastMM4 in '..\i2xcommon\FastMM4.pas',
  FastMM4Messages in '..\i2xcommon\FastMM4Messages.pas',
  Forms,
  UII2XScanTray in 'UII2XScanTray.pas' {Form1},
  uUII2XScanTray in 'uUII2XScanTray.pas',
  uCmdLine in '..\i2xcommon\uCmdLine.pas',
  uFileDir in '..\i2xcommon\uFileDir.pas',
  uFileIO in '..\i2xcommon\uFileIO.pas',
  uHashTable in '..\i2xcommon\uHashTable.pas',
  uOmniXML in '..\i2xcommon\uOmniXML.pas',
  uReg in '..\i2xcommon\uReg.pas',
  uStrUtil in '..\i2xcommon\uStrUtil.pas',
  uDebug in '..\i2xcommon\uDebug.pas',
  uI2XDLL in '..\i2xcommon\uI2XDLL.pas',
  uI2XOCR in '..\i2xcommon\uI2XOCR.pas',
  MapStream in '..\i2xcommon\MapStream.pas',
  uDWImage in '..\i2xcommon\uDWImage.pas',
  uI2XConstants in '..\i2xcommon\uI2XConstants.pas',
  uI2XPlugin in '..\i2xcommon\uI2XPlugin.pas',
  uAppCache in '..\i2xcommon\uAppCache.pas',
  uHashTableXML in '..\i2xcommon\uHashTableXML.pas',
  uImage2XML in '..\i2xcommon\uImage2XML.pas',
  uI2XScan in '..\i2xcommon\uI2XScan.pas',
  uI2XOptions in '..\i2xcommon\uI2XOptions.pas',
  uI2XPluginManager in '..\i2xcommon\uI2XPluginManager.pas',
  UITWAINDeviceSelect in 'UITWAINDeviceSelect.pas' {UITWAINDeviceSelect},
  uI2XMemMap in '..\i2xcommon\uI2XMemMap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Image2XML Scanner Tray Utility';
  //Application.CreateForm(TUII2XScanTray, fScanTrayUI);
  Application.HelpFile := 'C:\Dark\pascal\Image2XML\docs\help\Image2XML.chm';
  Application.CreateForm(TUII2XScanTray, fScanTrayUI);
  Application.Run;
end.
