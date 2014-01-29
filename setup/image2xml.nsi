;  NOTE THAT YOU MAY NEED TO REGENERATE THE EXECUTABLE USING TRIALWARE
; ***********************  MAIN FLAG CONTROLS - BEGIN  *************************
!define debug   ;Commend this out if you wish to not include the debug files
!define LQ   ;Commend this out if you do not wish to include the image files
; *********************** MAIN FLAG CONTROLS - END ****************************

!define appversion "v00_09_09"
!ifdef LQ
	!define setup "image2xml_lq_${appversion}_x86setup.exe"
!else
	!define setup "image2xml_${appversion}_x86setup.exe"
!endif 
!define projdir "C:\Dark\pascal\Image2XML"
!define srcdir "${projdir}\src\i2xui"
!define depdir "${projdir}\setup\dep"
!define docdir "${projdir}\docs"
!define graphicsdir "${projdir}\graphics"
!define company "NoctuSoft"
!define prodname "Image2XML"
!define exec "image2xml.exe"
 
; optional stuff
!define website "www.image2xml.com"
!define notefile "README.txt"
!define licensefile "LICENSE.txt"
!define helpfile "Image2XML.chm"
!define icon "I2XPurple.ico"
!define iconUninstall "TrashcanEmpty.ico"
!define screenimage "dwregionedit.bmp"
!define lffile "i2x.lf"
 
!define regkey "Software\${company}\${prodname}"
!define uninstkey "Software\Microsoft\Windows\CurrentVersion\Uninstall\${prodname}"
 
!define startmenu "$SMPROGRAMS\${prodname}"
!define uninstaller "uninstall.exe"
 
;--------------------------------
 
XPStyle on
ShowInstDetails hide
ShowUninstDetails hide
 
Name "${prodname}"
Caption "${prodname}"
 
!ifdef icon
Icon "${graphicsdir}\${icon}"
!endif
 
OutFile "${projdir}\setup\${setup}"
 
SetDateSave on
SetDatablockOptimize on
CRCCheck on
SilentInstall normal
 
InstallDir "$PROGRAMFILES\${prodname}"
InstallDirRegKey HKLM "${regkey}" ""
 
!ifdef licensefile
LicenseText "License"
LicenseData "${docdir}\${licensefile}"
!endif
 
; pages
; we keep it simple - leave out selectable installation types
 
!ifdef licensefile
Page license
!endif
 
; Page components
Page directory
Page instfiles
 
UninstPage uninstConfirm
UninstPage instfiles
 
;--------------------------------
 
AutoCloseWindow false
ShowInstDetails show
 
 
!ifdef screenimage
 
; set up background image
; uses BgImage plugin
Function .onGUIInit
	; extract background BMP into temp plugin directory
	InitPluginsDir
	File /oname=$PLUGINSDIR\1.bmp "${graphicsdir}\${screenimage}"
 
	BgImage::SetBg /NOUNLOAD /FILLSCREEN $PLUGINSDIR\1.bmp
	BgImage::Redraw /NOUNLOAD
FunctionEnd
 
Function .onGUIEnd
	; Destroy must not have /NOUNLOAD so NSIS will be able to unload and delete BgImage before it exits
	BgImage::Destroy
FunctionEnd
 
!endif

;Function .onInit
  ;call InitVersion
  
;FunctionEnd

;Function InitVersion
  ;!execute '"${_RESHACKER}" -extract ${_SRCDIR}\File.exe, ${projdir}\FileVersionInfo.rc, versioninfo,1,'
; 
  ;Push 'VALUE "FileVersion", "'
  ;Push '"'
  ;Push 'C:\WinApps\NSIS2\_Production\FileVersionInfo.rc'
  ;Call GetBetween
  ;Pop "$_VERSION"
  
;  !execute '"${_RESHACKER}" -extract ${srcdir}\${exec}, FileVersionInfo.res, versioninfo,1,'
  ;!packhdr tmp.dat '"${_RESHACKER}" -addoverwrite tmp.dat, tmp.dat, FileVersionInfo.res, versioninfo,1,'
  ;!echo "VersionInfo added"
;
;FunctionEnd

; beginning (invisible) section
Section
 
	WriteRegStr HKLM "${regkey}" "Install_Dir" "$INSTDIR"
; write uninstall strings
	WriteRegStr HKLM "${uninstkey}" "DisplayName" "${prodname} (remove only)"
	WriteRegStr HKLM "${uninstkey}" "UninstallString" '"$INSTDIR\${uninstaller}"'
 
	!ifdef filetype
		WriteRegStr HKCR "${filetype}" "" "${prodname}"
	!endif
 
	WriteRegStr HKCR "${prodname}\Shell\open\command\" "" '"$INSTDIR\${exec} ""%1"""'
 
	!ifdef icon
		WriteRegStr HKCR "${prodname}\DefaultIcon" "" "$INSTDIR\${icon}"
	!endif

	SetOutPath $INSTDIR
 
	File /a "${srcdir}\${exec}"

	!ifdef licensefile
	File /a "${docdir}\${licensefile}"
	!endif
 
	!ifdef notefile
	File /a "${docdir}\${notefile}"
	!endif
 
	!ifdef icon
		File /a "${srcdir}\${icon}"
	!endif
 
	!ifdef iconUninstall
		File /a "${graphicsdir}\${iconUninstall}"
	!endif
 
	!ifdef helpfile
		File /a "${docdir}\help\${helpfile}"
	!endif

	!ifdef lffile
		File /a "${projdir}\${lffile}"
	!endif
 
	; BEGIN application-specific files
	File /a "${srcdir}\TOCRR.ini"
	File /a "${srcdir}\FastMM_FullDebugMode.dll"
	File /a "${srcdir}\FreeImage.dll"
	File /a "${srcdir}\tocrlib.dll"
	File /a "${srcdir}\TOCRRdll.dll"

	File /a "${projdir}\src\i2xscanui\I2XScanTray.exe"
	!ifdef debug
		File /a "${projdir}\src\i2xscanui\I2XScanTray.map"
	!endif

	; BEGIN install plugins    ${projdir}
	File /a "${projdir}\src\i2ximg_freeimage\i2ximg_freeimage.dll"
	File /a "${projdir}\src\i2ximg_freeimage\i2ximg_freeimage.i2xcfg"
	!ifdef debug
		File /a "${projdir}\src\i2ximg_freeimage\i2ximg_freeimage.map"
	!endif

	File /a "${projdir}\src\i2ximg_g32\i2ximg_g32.dll"
	File /a "${projdir}\src\i2ximg_g32\i2ximg_g32.i2xcfg"
	!ifdef debug
		File /a "${projdir}\src\i2ximg_g32\i2ximg_g32.map"
	!endif

	File /a "${projdir}\src\i2ximg_mcm\i2ximg_mcm.dll"
	File /a "${projdir}\src\i2ximg_mcm\i2ximg_mcm.i2xcfg"
	!ifdef debug
		File /a "${projdir}\src\i2ximg_mcm\i2ximg_mcm.map"
	!endif
	
	File /a "${projdir}\src\i2xocr_tocr\i2xocr_tocr.dll"
	File /a "${projdir}\src\i2xocr_tocr\i2xocr_tocr.i2xcfg"
	!ifdef debug
		File /a "${projdir}\src\i2xocr_tocr\i2xocr_tocr.map"
	!endif

	; END install plugins
	
	SetOutPath "$INSTDIR\ocr\"
	File /a "${srcdir}\ocr\TOCRRService.exe"
	File /a "${srcdir}\ocr\TOCR1.teh"
	File /a "${srcdir}\ocr\TOCR1.qnp"
	File /a "${srcdir}\ocr\TOCR1.n3s"
	File /a "${srcdir}\ocr\TOCR1.gar"

	File /a "${srcdir}\ocr\tesseract.exe"
	File /a "${srcdir}\ocr\tessdll.lib"
	File /a "${srcdir}\ocr\tessdll.dll"
	
	SetOutPath "$INSTDIR\ocr\training\"
	File /a "${srcdir}\ocr\training\cnTraining.exe"
	File /a "${srcdir}\ocr\training\mfTraining.exe"
	File /a "${srcdir}\ocr\training\unicharset_extractor.exe"
	File /a "${srcdir}\ocr\training\wordlist2dawg.exe"

	SetOutPath "$INSTDIR\ocr\tessdata\"
	File /a "${srcdir}\ocr\tessdata\eng.DangAmbigs"
	File /a "${srcdir}\ocr\tessdata\eng.freq-dawg"
	File /a "${srcdir}\ocr\tessdata\eng.inttemp"
	File /a "${srcdir}\ocr\tessdata\eng.normproto"
	File /a "${srcdir}\ocr\tessdata\eng.pffmtable"
	File /a "${srcdir}\ocr\tessdata\eng.unicharset"
	File /a "${srcdir}\ocr\tessdata\eng.user-words"
	File /a "${srcdir}\ocr\tessdata\eng.word-dawg"

	SetOutPath "$INSTDIR\samples\"
	File /a "${projdir}\docs\samples\I2XFORMSAMPLE.tif"
	
	!ifdef LQ
		SetOutPath "$INSTDIR\samples\LQ\"
		File /a "${projdir}\templates\lq.i2x"
		File /a "${projdir}\templates\image001.tif"
		
		SetOutPath "$INSTDIR\samples\LQ\Images\Game 01\"
		File /a "${projdir}\templates\Images\Game 01\img_001.tif"
		File /a "${projdir}\templates\Images\Game 01\img_002.tif"

		SetOutPath "$INSTDIR\samples\LQ\Images\Game 02\"
		File /a "${projdir}\templates\Images\Game 02\img_003.tif"
	!endif

	SetOutPath $SYSDIR
	SetOverwrite ifnewer
	File /a "C:\Windows\System32\KEYLIB32.dll"
	File /a "C:\Windows\System32\skca32.dll"
	
	;SetOutPath "$INSTDIR\samples\templates\"
	;File /a "${srcdir}\samples\templates\lq.xml"

	; END application-specific files
 
  WriteUninstaller "${uninstaller}"
 
SectionEnd
 
; create shortcuts
Section

	CreateDirectory "${startmenu}"
	SetOutPath $INSTDIR ; for working directory

	CreateShortCut "${startmenu}\Install Directory.lnk" "$INSTDIR\"
	CreateShortCut "${startmenu}\I2X ScanTray.lnk" "$INSTDIR\I2XScanTray.exe"

	!ifdef icon
		CreateShortCut "${startmenu}\${prodname}.lnk" "$INSTDIR\${exec}" "" "$INSTDIR\${icon}"
	!else
		CreateShortCut "${startmenu}\${prodname}.lnk" "$INSTDIR\${exec}"
	!endif
 
	;!ifdef notefile
		;  CreateShortCut "${startmenu}\Release Notes.lnk" "$INSTDIR\${notefile}"
	;!endif
 
	!ifdef helpfile
		CreateShortCut "${startmenu}\Image2XML Help.lnk" "$INSTDIR\${helpfile}"
	!endif
 
	!ifdef website
		WriteINIStr "${startmenu}\Image2XML On the Web.url" "InternetShortcut" "URL" ${website}
		; CreateShortCut "${startmenu}\Web Site.lnk "${website}" "URL"
	!endif

	!ifdef notefile
		ExecShell "open" "$INSTDIR\${notefile}"
	!endif
	
	!ifdef licensefile
		;ExecShell "open" "$INSTDIR\${licensefile}"
	!endif

	CreateShortCut "${startmenu}\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\${iconUninstall}"
 
SectionEnd
 
; Uninstaller
; All section names prefixed by "Un" will be in the uninstaller
 
UninstallText "This will uninstall ${prodname}."
 
!ifdef icon
UninstallIcon "${graphicsdir}\${iconUninstall}"
!endif
 
Section "Uninstall"
 
  DeleteRegKey HKLM "${uninstkey}"
  DeleteRegKey HKLM "${regkey}"
  DeleteRegKey HKCU "${regkey}"
 
  Delete "${startmenu}\*.*"
  RMDir "${startmenu}"
 
	!ifdef licensefile
		Delete "$INSTDIR\${licensefile}"
	!endif
 
	!ifdef notefile
		Delete "$INSTDIR\${notefile}"
	!endif
 
	!ifdef icon
		Delete "$INSTDIR\${icon}"
	!endif

	!ifdef iconUninstall
		Delete "$INSTDIR\${iconUninstall}"
	!endif

	!ifdef lffile
		Delete "$INSTDIR\${lffile}"
	!endif
 
	Delete "$INSTDIR\${exec}"
 
	; BEGIN application-specific files
	
	Delete "$INSTDIR\${notefile}"
	Delete "$INSTDIR\ocr\*.*"
	RMDir /r /REBOOTOK "$INSTDIR\ocr"

	!ifdef LQ
		RMDir /r /REBOOTOK "$INSTDIR\samples\LQ"
	!endif
	RMDir /r /REBOOTOK "$INSTDIR\samples"

	Delete "$INSTDIR\*.*"
	RMDir "$INSTDIR\"
	; END application-specific files
 
SectionEnd