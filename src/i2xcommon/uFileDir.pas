unit uFileDir;

interface
uses
  SysUtils,
  Classes,
  ShellAPI,
  ShlObj,
  Forms,
  Dialogs,
  Windows;

const
  UNIT_NAME = 'FileDir';
  kernel = 'kernel32.dll';
  user = 'user32.dll';
  COMPARED_EQUAL = 0;
  ucCRLF = #13#10;
  BUFFER_MAX_LENGTH   = 400;
  apiTEMP = 'temp'; {Temp system variable}
  apiUSERDIR = 'USERPROFILE'; {User variable}
  OFN_DONTADDTORECENT = $02000000;
  OFN_FILEMUSTEXIST = $00001000;
  OFN_HIDEREADONLY = $00000004;
  OFN_PATHMUSTEXIST = $00000800;

var
    lg_StartFolder: String;

function OpenSaveFileDialog(ParentHandle: THandle; const DefExt, Filter,
InitialDir, Title: string; var FileName: string; IsOpenDialog: Boolean):
Boolean;

type

  TErrorRaisedEvent = procedure (Sender: TObject; exc : TObject; procedureName : string) of object;
  CFileDir = class(TObject)
  private
    FShowError : Boolean;
    //p : TObject;
    app : TApplication;
    FOnError : TErrorRaisedEvent;
    procedure Emsg( s : string );
    function Parms(v: array of Variant; Paired: Boolean): String;
    Function CountStrOcc(const StringWithStringOccurances, StringToCount : String; StartPos : Cardinal = 1; CaseSensitive : Boolean = True) : Cardinal;
    Function ucPos( StringToSearchIn, StringToFind : String; StartPos : Cardinal = 1; CaseSensitive : boolean = True) : Cardinal;
    function CompareString( const S1, S2 : String; CaseSensitive : boolean = False) : integer;
    Function PosRev( const StringToSearch, StringToFind : String; StartPos : Cardinal = 0; CaseSensitive : boolean = false) : Cardinal;
    Procedure ParseString(const vS : string; ParseChar : Char; ParsedString : TStringList; TrimOnInsert : boolean = false);
    function ForceStr( StringtoForceCharIn : String; StringToForceToFront : String = ''; StringToForceAtEnd : String = '' ) : String;
    function ucLeft( s : String; NumOfLeftChar : integer ) : String;
    function ucRight( s : String; NumOfRightChar : integer ) : String;
    function GetStringParm( v: array of Variant; Paired : Boolean = false; MajorDelimiter : String = ', '; MinorDelimiter : String = '') : String;
    Function GetPairStrParm( sa: array of String; QuoteStrings : boolean = True; MajorDelimiter : String = ';'; MinorDelimiter : String = ',') : String;
    function GetEnvironmentVariable(sVariable: string): String;
    procedure ErrorRaised(functionName: string; e: TObject);
  public
    function GetVersion(const sFileName: string): string; overload;
    procedure GetVersion(const sFileName: string; var ReturnVersion : string ); overload;
    property ShowError: boolean read FShowError write FShowError;
    property OnError: TErrorRaisedEvent read FOnError write FOnError;
    function Slash( StringToForceSlashAtEnd : String) : String;
    function GetSystemTempDir: String;
    Function AppDir: String;
    Function AddPath(const FileName : String) : String;
    Function AppExe: String;
    Function RelativePath(const OriginalPath, OffsetPath : String) : String;
    function AppendToFileName(FullPathName,
      StringToAppend: String): String;
    function ChangeExt(FullPathName, NewExtention: String): String;
    function ChangePath(FullPathName, NewPath: String): String;
    function CopyFile(ExistingFile, NewFile: String) : boolean;
    Function ucExtractFileName( FullPathName : String; ExcludeExtention : Boolean = False) : String;
    function GetFileList( PathName : String; ShowDirectory : boolean = false) : TStringList;
    function DeleteFilesInPath( const PathName : String ) : integer;
    function TextFileToString(FileToReadFrom: String;
      StripCrlf: Boolean = false): String;
    procedure WriteString(StringToWrite, FileToWriteTo: String;
      AppendToFile: boolean = false);
    function BrowseForFolder( var FolderName: String ): Boolean; overload;
    function BrowseForFolder( const InitialFolder: string; var FolderName: String ): Boolean; overload;
    function DestroyPath( const pathToDestroy : string ) : boolean;
    Function OpenDlg(var SelectedFile : String; DefaultFileTypes : String = 'All Files (*.*)|*.*'; Title : String = '{AppName}'; DefaultFile : String = '') : Boolean;
    Function OpenDlgAPI(var SelectedFile : String; DefaultFileTypes : String = 'All Files (*.*)|*.*'; Title : String = '{AppName}'; DefaultFile : String = ''; DefaultExt : String = 'txt'; InitialPath : String = '{CURDIR}' ) : Boolean;
    Function SaveDlgAPI(var SelectedFile : String; DefaultFileTypes : String = 'All Files (*.*)|*.*'; Title : String = '{AppName}'; DefaultFile : String = ''; DefaultExt : String = 'txt'; InitialPath : String = '{CURDIR}' ) : Boolean;
    function GetChildDirs( PathName : String; DirectoryList : TStringList; CurrentDepth : longint;
                       MaxDepth : longint = 0 ) : longint;
    function _GetSubDirList( PathName : String; DirectoryList : TStringList;
                         CurrentDepth : longint; MaxDepth : longint = 0 ) : longint;
    function GetSubDirList( PathName : String; MaxDepth : longint = 0 ) : TStringList;
    constructor Create(); overload;
    constructor Create(callingWindowedApp : TObject); overload;
    destructor Destroy; override;
    function GetFileDateTime(FullFileName : String): TDateTime;
    function GetShortFileName(const FileName : TFileName ) : TFileName;
    function GetUserDir() : string;
    function GetAppDataDir: string;
    function GetCookiesDir: string;
    function GetDesktop: string;
    function GetDesktopDir: string;
    function GetFavDir: string;
    function GetFontsDir: string;
    function GetHistoryDir: string;
    function GetMyDocDir: string;
    function GetNetHoodDir: string;
    function GetPrintHoodDir: string;
    function GetProgDir: string;
    function GetRecentDir: string;
    function GetSendToDir: string;
    function GetSpecialFolder(FolderID: Integer): string;
    function GetStartMenuDir: string;
    function GetStartUpDir: string;
    function GetTemplateDir: string;
    function GetTmpInternetDir: string;
  end;

  POpenFilenameA = ^TOpenFilenameA;
  POpenFilename = POpenFilenameA;
  tagOFNA = packed record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: PAnsiChar;
    lpstrCustomFilter: PAnsiChar;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: PAnsiChar;
    nMaxFile: DWORD;
    lpstrFileTitle: PAnsiChar;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: PAnsiChar;
    lpstrTitle: PAnsiChar;
    Flags: DWORD;
    nFileOffset: Word;
    nFileExtension: Word;
    lpstrDefExt: PAnsiChar;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
    lpTemplateName: PAnsiChar;
  end;
  TOpenFilenameA = tagOFNA;
  TOpenFilename = TOpenFilenameA;

    function APICopyFile(ExistingFile, NewFile: PChar; nFailIfExists: LongInt): LongInt; stdcall;
      external kernel name 'CopyFileA';
    function APIGetSystemDir(pcDir : PChar; lMaxBufferLength: LongInt): LongInt; stdcall;
      external kernel name 'GetSystemDirectoryA';
    //function APIGetTempDir(pcDir : PChar; lMaxBufferLength: LongInt): LongInt; stdcall;
    //  external kernel name 'GetTempPathA';
    function APIGetTempDir(lMaxBufferLength: Cardinal; pcDir : PChar): Cardinal; stdcall;
      external kernel name 'GetTempPathA';
    function APIGetWinDir(pcDir : PChar; lMaxBufferLength: LongInt): LongInt; stdcall;
      external kernel name 'GetWindowsDirectoryA';
    function APIGetEnvironmentVariable(pcVarName, pcVarValue : PChar; lMaxVarValueLength: LongInt): LongInt; stdcall;
      external kernel name 'GetEnvironmentVariableA';
function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT; lParam,
lpData: LPARAM): Integer stdcall;
function BrowseForFolderAPI(const browseTitle: String; var SelectedFolder : string;
        const initialFolder: String =''): boolean;
Procedure WriteString( StringToWrite, FileToWriteTo : String; AppendToFile : boolean);
function GetModuleName: string;

implementation
function GetOpenFileName(var OpenFile: TOpenFilename): Bool; stdcall; external 'comdlg32.dll'  name 'GetOpenFileNameA';
function GetSaveFileName(var OpenFile: TOpenFilename): Bool; stdcall; external 'comdlg32.dll'  name 'GetSaveFileNameA';

function CharReplace(const Source: string; oldChar, newChar: Char): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to Length(Result) do
    if Result[i] = oldChar then
      Result[i] := newChar
end;

function OpenSaveFileDialog(ParentHandle: THandle; const DefExt, Filter, InitialDir, Title: string; var FileName: string; IsOpenDialog: Boolean): Boolean;
var
  ofn: TOpenFileName;
  szFile: array[0..MAX_PATH] of Char;
begin
  Result := False;
  FillChar(ofn, SizeOf(TOpenFileName), 0);
  with ofn do
  begin
    lStructSize := SizeOf(TOpenFileName);
    hwndOwner := ParentHandle;
    lpstrFile := szFile;
    nMaxFile := SizeOf(szFile);
    if (Title <> '') then
      lpstrTitle := PChar(Title);
    if (InitialDir <> '') then
      lpstrInitialDir := PChar(InitialDir);
    StrPCopy(lpstrFile, FileName);
    lpstrFilter := PChar(CharReplace(Filter, '|', #0)+#0#0);
    if DefExt <> '' then
      lpstrDefExt := PChar(DefExt);
  end;
  if IsOpenDialog then
  begin
    if GetOpenFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end
  else
  begin
    if GetSaveFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end
end;

procedure CFileDir.Emsg( s : string );
begin
  if ( FShowError ) then
    MessageDlg(s, mtError,[mbOk], 0);
End;

Function CFileDir.AddPath(const FileName : String) : String;
var
  s : String;
Begin
  try
    result := '';
    if ( FileName <> '' ) then begin
      if (FileName[1] = '.') then begin //Current Directory
        s := FileName;
        Result := AppDir;
      end else if ((FileName[1] = '.') and (FileName[2] = '\')) then begin //Current Directory
        s := FileName;
        Delete(s, 1, 2);
        Result := AppDir + s;
      end else if ( ExtractFilePath(FileName) = '' ) then        //No Directory
        Result := AppDir + FileName
      else if ((FileName[1] = '.') and (FileName[2] = '.')) then begin  //Realtive Directory
        Result := RelativePath(AppDir, FileName)
      end else                                              //Leave alone of path doesn't meet above criteria
        Result := FileName;
    end;
  except
    ErrorRaised(UNIT_NAME + ':CFileDir.AddPath(' + Parms( [ 'FileName', FileName ], True) + ')', ExceptObject);
    raise;
  end; {try}
End;

Function CFileDir.AppDir: String;
Begin

  Result := ExtractFilePath(app.EXEName)
End;

Function CFileDir.AppExe: String;
Begin
  Result := ExtractFileName(app.EXEName)
End;

function CFileDir.BrowseForFolder(const InitialFolder: string;
  var FolderName: String): Boolean;
begin
  lg_StartFolder := InitialFolder;
  Result := self.BrowseForFolder( FolderName );
end;

procedure CFileDir.ErrorRaised(functionName: string; e: TObject);
begin
    if ( Assigned( FOnError )) then begin
      FOnError( self, e, functionName );
    end;
End;

Function CFileDir.RelativePath(const OriginalPath, OffsetPath : String) : String;
var i, nPos, nAntiOffset : SmallInt;
    ParsedDirectory : TStringList;
    s : String;
Begin
  result := OriginalPath;
  ParsedDirectory := TStringList.Create;
  try
    nAntiOffset := CountStrOcc(OffsetPath, '..');
    if ( nAntiOffset > 0 ) then begin

      nPos := PosRev(OffsetPath, '..');
      s := Copy(OffsetPath, nPos + 2, 9999);
      ParseString(OriginalPath, '\', ParsedDirectory);
      with ParsedDirectory do begin

        if Count > 2 then begin
          if ( ParsedDirectory[0] = '\' ) then begin //UNC Path
            ParsedDirectory[1] := '\\' + ParsedDirectory[1];
            ParsedDirectory.Delete(0);
          end;
          for i := (Count - (1 + nAntiOffset)) downto 1 do begin  //We stop at one because we should always have at least the root directory
            s := '\' + ParsedDirectory[i] + s;
          end; {i}
        end; {if}
      end; {with}

    end; {if}
    s := ParsedDirectory[0] + s;
    result := s;
  except
    ErrorRaised(UNIT_NAME + ':CFileDir.RelativePath(' + Parms( [ 'OriginalPath', OriginalPath, 'OffsetPath', OffsetPath ], True) + ')', ExceptObject);
    raise;
  end; {try}
  ParsedDirectory.Free;
End;

Function CFileDir.ChangePath( FullPathName, NewPath : String) : String;
Begin
  result := Slash(NewPath) + ExtractFileName(FullPathName);
End;

Function CFileDir.AppendToFileName( FullPathName, StringToAppend : String) : String;
Begin
  result := ExtractFilePath(FullPathName) + ucExtractFileName(FullPathName, True) + StringToAppend + ExtractFileExt(FullPathName);
End;

Function CFileDir.ChangeExt( FullPathName, NewExtention : String) : String;
Begin
  result := FullPathName;
  try
    if ( NewExtention[1] = '.' ) then
      result := ExtractFilePath(FullPathName) + ucExtractFileName(FullPathName, True) + NewExtention
    else
      result := ExtractFilePath(FullPathName) + ucExtractFileName(FullPathName, True) + '.' + NewExtention;
  except
    ErrorRaised(UNIT_NAME + ':CFileDir.ChangeExt(' + Parms( [ 'FullPathName', FullPathName, 'NewExtention', NewExtention ], True) + ')', ExceptObject);
    raise;
  end; {try}
End;

function CFileDir.Parms( v: array of Variant; Paired : Boolean ) : String;
Begin
  try
    Parms := GetStringParm(v, Paired, ', ', ' = ');
  except
    ErrorRaised(UNIT_NAME + ':CFileDir.Parms' + '- Error', ExceptObject);
    raise;
  end; {try.. except}
End;

function CFileDir.CopyFile(ExistingFile, NewFile : String) : boolean;
{Wrapper around the semi cryptic Windows API CopyFile Rountine}
var
  lResult : LongInt;
begin
  lResult := -1;
  try
    if not DirectoryExists(ExtractFilePath(NewFile)) then ForceDirectories(ExtractFilePath(NewFile));
    lResult := APICopyFile(PChar(ExistingFile), PChar(NewFile), 0 ); {copy even if NewFileExists}
    if ( lResult = -1 ) then
      raise Exception.Create('APICopyFile returned an error value.  lResult=' + IntToStr(lResult) );
    result := true;
  except
    ErrorRaised('Kernel Call via procedure "DuplicateFile" for function CopyFile Failed! lResult=' + IntToStr(lResult), ExceptObject);
    result := false;
  end {try};
End;


constructor CFileDir.Create;
begin
  lg_StartFolder := '';
end;

constructor CFileDir.Create(callingWindowedApp: TObject);
begin
    app := callingWindowedApp as TApplication;
end;

destructor CFileDir.Destroy;
begin
    app := nil;
    inherited;
end;

function CFileDir.DestroyPath(const pathToDestroy : string): boolean;
var
  SHFileOpStruct : TSHFileOpStruct;
  DirBuf : array [0..255] of char;
begin
  try
    if ( Length( pathToDestroy ) = 0 ) then begin
      Result := false;
      Exit;
    end;

    Fillchar(SHFileOpStruct,Sizeof(SHFileOpStruct),0) ;
    FillChar(DirBuf, Sizeof(DirBuf), 0 ) ;
    if ( pathToDestroy[Length(pathToDestroy)] = '\' ) then
      StrPCopy( DirBuf, Copy(pathToDestroy, 0, Length(pathToDestroy)-1 ) )
    else
      StrPCopy( DirBuf, pathToDestroy );
    with SHFileOpStruct do begin
      Wnd := 0;
      pFrom := @DirBuf;
      wFunc := FO_DELETE;
      fFlags := FOF_ALLOWUNDO;
      fFlags := fFlags or FOF_NOCONFIRMATION;
      fFlags := fFlags or FOF_SILENT;
    end;
    Result := (SHFileOperation(SHFileOpStruct) = 0) ;
  except
    Result := false;
  end;
end;

function CFileDir.CountStrOcc(const StringWithStringOccurances,
  StringToCount: String; StartPos: Cardinal;
  CaseSensitive: Boolean): Cardinal;
var cCounter, cStPos : Cardinal;
Begin
  cCounter := 0;
  cStPos := StartPos;
  try
    While (cStPos > 0) do begin
      cStPos := ucPos( StringWithStringOccurances, StringToCount, cStPos, CaseSensitive);
      if ( cStPos > 0 ) then begin
        Inc(cCounter);
        Inc(cStPos);
      end;
    end;
  except
    ErrorRaised(UNIT_NAME + ':CountWord(' + Parms( [ 'StringWithStringOccurances', StringWithStringOccurances, 'StringToCount', StringToCount, 'StartPos', StartPos, 'CaseSensitive', CaseSensitive ], True) + ')', ExceptObject);
    raise;
  end; {try}
  result := cCounter;
end;

function CFileDir.CompareString(const S1, S2: String;
  CaseSensitive: boolean): integer;
Begin
 if CaseSensitive then
   CompareString := CompareStr(S1, S2)
 else
   CompareString := CompareText(S1, S2);
End;

function CFileDir.PosRev(const StringToSearch, StringToFind: String;
  StartPos: Cardinal; CaseSensitive: boolean): Cardinal;
var i, lStringToFindLength, lStringToSearchLength : Cardinal;
Begin
  try
    result := 0;
    lStringToFindLength := Length(StringToFind);
    lStringToSearchLength := Length(StringToSearch);
    if (( StartPos > 0 ) and ( StartPos < lStringToSearchLength + 1))  then
      i := StartPos
    else
      i := lStringToSearchLength;
    while i > 0 do begin
      if (CompareString(Copy(StringToSearch, i, lStringToFindLength), StringToFind, CaseSensitive) = 0 ) then begin
        result := i;
        break;
      end; {if}
      Dec(i);
    end; {while}
  except
    ErrorRaised(UNIT_NAME + ':PosRev - Error finding substring. Parms=' + Parms([ 'StringToSearch', StringToFind, 'StringToFind', StringToFind, 'StartPos', StartPos, 'CaseSensitive', CaseSensitive], True), ExceptObject);
    raise;
  end; {try}
End;

Procedure CFileDir.ParseString(const vS : string; ParseChar : Char; ParsedString : TStringList; TrimOnInsert : boolean );
Var
  i, StPos : integer;
  s, newStr : String;
Begin
  s := vS + ParseChar;
  i := 1;  StPos := 1;
  While i <= Length(s) do begin
    if ( s[i] = ParseChar ) and ( i > 1 ) then begin
      if TrimOnInsert then
        newStr := Trim(Copy(s, StPos, i - StPos))
      else
        newStr := Copy(s, StPos, i - StPos);
      if ( Length(newStr) > 0 ) then ParsedString.Add(newStr);
      StPos := i + 1;
    end;{if}
    Inc(i);
  end; {while}
End;

function CFileDir.ForceStr(StringtoForceCharIn, StringToForceToFront,
  StringToForceAtEnd: String): String;
var s : String;
Begin
  s := StringtoForceCharIn;
  if ( StringToForceAtEnd <> '' ) and ( ucRight(StringtoForceCharIn, Length(StringToForceAtEnd)) <> StringToForceAtEnd ) then
    s := s + StringToForceAtEnd;
  if ( StringToForceToFront <> '' ) and ( ucLeft(StringtoForceCharIn, Length(StringToForceToFront)) <> StringToForceToFront)  then
    s := StringToForceToFront + s;
  result := s;
End;

function CFileDir.SaveDlgAPI(var SelectedFile: String; DefaultFileTypes, Title,
  DefaultFile, DefaultExt, InitialPath: String): Boolean;
var  sPath, sNewFileName : String;
Begin
  if (( InitialPath='{CURDIR}' ) or ( Length(InitialPath) = 0 )) then
  begin
    InitialPath:= AddPath('.');
  end;
  if Title = '{AppName}' then
    Title := Application.Title
  else
    Title := Title;

  SelectedFile := '';
  sNewFileName := DefaultFile;
  if ( OpenSaveFileDialog(Application.Handle, DefaultExt, DefaultFileTypes, InitialPath, Title, sNewFileName, false) ) then
  begin
    SelectedFile := sNewFileName;
    Result := true;
  end else
    Result := false;
end;

function CFileDir.Slash(StringToForceSlashAtEnd: String): String;
begin
  result := ForceStr( StringToForceSlashAtEnd, '', '\');
end;

function CFileDir.ucLeft( s : String; NumOfLeftChar : integer ) : String;
Begin
  Result := Copy(s, 1, NumOfLeftChar );
End;

function CFileDir.ucRight( s : String; NumOfRightChar : integer ) : String;
Begin
  Result := Copy(s, ((Length(s) - NumOfRightChar) + 1), NumOfRightChar);
End;

function CFileDir.ucExtractFileName(FullPathName: String;
  ExcludeExtention: Boolean): String;
var
  s : String;
  nPos : Integer;
Begin
  result := '';
  try
    s := ExtractFileName( FullPathName );
    if ExcludeExtention then begin
      nPos := PosRev(s, '.', 9999, True);
      if ( nPos > 0 ) then s := Copy(s, 1, nPos - 1);
    end; {if}
    result := s;
  except
    ErrorRaised(UNIT_NAME + ':CFileDir.ucExtractFileName - Error while Extracting File Name. Parms=' + Parms([ 'FullPathName', FullPathName, 'ExcludeExtention', ExcludeExtention ], True), ExceptObject);
    raise;
  end; {try}
End;

function CFileDir.GetPairStrParm(sa: array of String;
  QuoteStrings: boolean; MajorDelimiter, MinorDelimiter: String): String;
var
  i : ShortInt; s, sNew : String;
  bFirstOp : boolean;
Begin
  result := '';
  try
    s := '';  sNew := ''; bFirstOp := True;
    for i := Low(sa) to High(sa) do begin
      s := sa[i];
      if QuoteStrings then s := ForceStr(s, '"', '"');
      if ( i < High(sa)) then begin
        if bFirstOp then
          s := s + MinorDelimiter
        else
          s := s + MajorDelimiter;
      end;
      bFirstOp := not bFirstOp;
      sNew := sNew + s;
    end; {for}
    result := sNew;
  except
    ErrorRaised(UNIT_NAME + ':ucGetStringParm(' + Parms( [ 'v', 'Variant()', 'QuoteStrings', QuoteStrings, 'MajorDelimiter', MajorDelimiter, 'MinorDelimiter', MinorDelimiter], True) + ')', ExceptObject);
  end; {try}
End;

function CFileDir.GetShortFileName(const FileName: TFileName): TFileName;
var
  buffer: array[0..MAX_PATH-1] of char;
begin
  SetString(Result, buffer, GetShortPathName(
    pchar(FileName), buffer, MAX_PATH-1));
end;

function CFileDir.GetStringParm(v: array of Variant; Paired: Boolean;
  MajorDelimiter, MinorDelimiter: String): String;
begin

end;

function CFileDir.ucPos(StringToSearchIn, StringToFind: String;
  StartPos: Cardinal; CaseSensitive: boolean): Cardinal;
var
  nStartPos, cPos : Cardinal;
Begin
  result := 0;
  try
    if (( StartPos > Length(StringToSearchIn)) or ( StartPos < 1 )) then
      nStartPos := 0
    else
      nStartPos := StartPos;
    for cPos := nStartPos to Length(StringToSearchIn) do begin
      if ( CompareString(StringToFind, Copy(StringToSearchIn, cPos, Length(StringToFind)), CaseSensitive) = COMPARED_EQUAL ) then begin
        result := cPos;
        Break;
      end;
    end; {for}
  except
    ErrorRaised(UNIT_NAME + ':CFileDir.ucPos ' + Parms( [ 'StringToSearchIn', StringToSearchIn, 'StringToFind', StringToFind , 'StartPos', StartPos ], True), ExceptObject);
    raise;
  end; {try}
End;

function CFileDir.DeleteFilesInPath( const PathName: String): integer;
var
  slFilesToDelete : TSTringList;
  i, nDelCount : integer;
begin
  Result := 0;
  nDelCount := 0;
  try
    slFilesToDelete := GetFileList( PathName, true );
    for i  := 0 to slFilesToDelete.Count - 1 do begin
      if ( DeleteFile( PChar( slFilesToDelete[i] ) ) ) then
        Inc( nDelCount );
    end;
  finally
    FreeAndNil( slFilesToDelete );
  end;
  Result := nDelCount;
end;

function CFileDir.GetFileList( PathName : String; ShowDirectory : boolean = false) : TStringList;
//Obtain list of all files in a directoy with mask "Pathfile".. wild card characters work
var
  slResult : TStringList;
  nReturnCode : integer;
  bIsDirectory : boolean;
  sCurrent, sPathName : String;
  SearchRec: TSearchRec;
Begin
  bIsDirectory := False;
  slResult := TStringList.Create;
  sPathName := AddPath(PathName);
  try
   try
    //nReturnCode := FindFirst(sPathName, faAnyFile, SearchRec);
    FindFirst(sPathName, faAnyFile, SearchRec);
    //SearchRec.Attr = faDirectory;
    sCurrent := SearchRec.Name;
    While ( sCurrent <> '' ) do begin
      sCurrent := Slash(ExtractFileDir(sPathName)) + sCurrent;
      if ( sCurrent[Length(sCurrent)] <> '.') then begin
        if ( ShowDirectory ) and (bIsDirectory) then sCurrent := sCurrent + '+';
        if ((ShowDirectory) and ( bIsDirectory)) or (not bIsDirectory) then begin
          slResult.Add( sCurrent );
        end; {if}
      end; { if}
      bIsDirectory := False;
      nReturnCode := FindNext(SearchRec);
      if nReturnCode = 0 then begin
        sCurrent := SearchRec.Name;
        if ( SearchRec.Attr = faDirectory ) then bIsDirectory := True;
      end else
        sCurrent := '';
    end; {while}
    slResult.Sort;
   finally
    FindClose( SearchRec.FindHandle );
   end;
  except
    ErrorRaised(UNIT_NAME + ':GetFileList' + Parms( [ 'PathName', PathName,  'ShowDirectory' , ShowDirectory] , True), ExceptObject);
    raise;
  end; {try}
  result := slResult;
End;

Procedure CFileDir.WriteString( StringToWrite, FileToWriteTo : String; AppendToFile : boolean);
var
  F1 : TextFile;
  sFileName, sDir : string;
Begin
  try
    sFileName := AddPath(FileToWriteTo);
    sDir := ExtractFilePath(sFileName);
    if (not DirectoryExists(sDir)) then ForceDirectories(sDir);
    AssignFile(F1, sFileName);
    if ( AppendToFile ) and (FileExists(sFileName)) then
      Append(F1)
    else
      Rewrite(F1);
    Write(F1, StringToWrite);
    CloseFile(F1);
  except
    ErrorRaised(UNIT_NAME + ':WriteString' + Parms( [ 'StringToWrite', StringToWrite, 'FileToWriteTo', 'FileToWriteTo' , 'AppendToFile', AppendToFile ], True), ExceptObject);
    raise;
  end; {try}
End;

Function CFileDir.TextFileToString( FileToReadFrom : String; StripCrlf : Boolean ) : String;
var
  F1 : TextFile;
  sFileName, sInput, s : string;
Begin
  try
    sFileName := AddPath(FileToReadFrom);
    AssignFile(F1, sFileName);
    Reset(F1);
    while not Eof(F1) do begin
      Readln(F1, sInput);
      s := s + sInput;
      if not StripCrlf then s := s + ucCrLf
    end;
    CloseFile(F1);
  except
    ErrorRaised(UNIT_NAME + ':TextFileToString' + Parms( [ 'FileToReadFrom', FileToReadFrom, 'StripCrlf', StripCrlf ], True), ExceptObject);
    raise;
  end; {try}
  TextFileToString := s;
End;

//function CFileDir.BrowseForFolder(var FolderName: String): Boolean;
//Begin
//  Result := BrowseForFolderAPI( 'Browser For Folder', FolderName );
//End;


function CFileDir.BrowseForFolder(var FolderName: String): Boolean;
var
  BrowseInfo  : TBrowseInfo;
  ItemIDList  : PItemIDList;
  DisplayName : array[0..MAX_PATH] of Char;
begin
  Result:=False;
  FillChar(BrowseInfo,SizeOf(BrowseInfo),#0);
  with BrowseInfo do begin
//    hwndOwner:=Application.Handle;
    hwndOwner:=0;
    pszDisplayName:=@DisplayName[0];
    lpszTitle:='Select a folder';
    ulFlags:=BIF_RETURNONLYFSDIRS;
    if ( Length( lg_StartFolder ) > 0 ) then
      lpfn := BrowseForFolderCallBack;
 end;
  ItemIDList:=SHBrowseForFolder(BrowseInfo);
  if Assigned(ItemIDList) then begin
    if SHGetPathFromIDList(ItemIDList,DisplayName) then
       begin
        FolderName:=DisplayName;
        lg_StartFolder := '';
        Result:=True;
       end;
    GlobalFreePtr(ItemIDList);
  end;
  lg_StartFolder := '';
End;

Function CFileDir.OpenDlg(var SelectedFile : String; DefaultFileTypes : String = 'All Files (*.*)|*.*'; Title : String = '{AppName}'; DefaultFile : String = '') : Boolean;
var  dlg : TOpenDialog;  sPath, sNewFileName : String;
Begin
  SelectedFile := '';
  result := false;
  dlg := TOpenDialog.Create(nil);
  try
    dlg.Filter := DefaultFileTypes;
    if Title = '{AppName}' then
      dlg.Title := Application.Title
    else
      dlg.Title := Title;
    sPath := ExtractFilePath(DefaultFile);
    if ( Trim(sPath) = '' ) then sPath := AppDir;
    dlg.InitialDir := sPath;
    dlg.FileName := ExtractFileName(DefaultFile);
    if dlg.Execute then begin
      SelectedFile := dlg.FileName;
      result := True;
    end;
  except
    ErrorRaised(UNIT_NAME + ':OpenDlg', ExceptObject);
    raise;
  end; {try}
  dlg.Free;
End;

Function CFileDir.OpenDlgAPI(var SelectedFile : String; DefaultFileTypes : String = 'All Files (*.*)|*.*'; Title : String = '{AppName}'; DefaultFile : String = ''; DefaultExt : String = 'txt'; InitialPath : String = '{CURDIR}' ) : Boolean;
var  sPath, sNewFileName : String;
Begin
  if (( InitialPath='{CURDIR}' ) or ( Length(InitialPath) = 0 )) then
  begin
    InitialPath:= AddPath('.');
  end;
  if Title = '{AppName}' then
    Title := Application.Title
  else
    Title := Title;

  SelectedFile := '';
  sNewFileName := DefaultFile;
  if ( OpenSaveFileDialog(Application.Handle, DefaultExt, DefaultFileTypes, InitialPath, Title, sNewFileName, True) ) then
  begin
    SelectedFile := sNewFileName;
    Result := true;
  end else
    Result := false;
End;

function CFileDir.GetFileDateTime(FullFileName : String): TDateTime;
var
  FileHandle : integer;
begin
  try
    FileHandle := FileOpen(FullFileName, fmShareDenyNone);
    if ( FileHandle > 0 ) then
      result := FileDateToDateTime(FileGetDate(FileHandle))
    else
      result := 0.0;
    FileClose(FileHandle);
  except
    ErrorRaised(UNIT_NAME + ':OpenDlg', ExceptObject);
    raise;
  end; {try}
end;

function CFileDir.GetSystemTempDir : String;
Begin
  GetSystemTempDir := self.Slash( GetEnvironmentVariable(apiTEMP) );
end;

function CFileDir.GetUserDir: string;
begin
  Result := Slash( GetEnvironmentVariable( apiUSERDIR ) );
end;

procedure CFileDir.GetVersion(const sFileName: string;
  var ReturnVersion: string);
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  ReturnVersion := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(sFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(sFileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    ReturnVersion := IntToStr(dwFileVersionMS shr 16);
    ReturnVersion := ReturnVersion + '.' + IntToStr(dwFileVersionMS and $FFFF);
    ReturnVersion := ReturnVersion + '.' + IntToStr(dwFileVersionLS shr 16);
    ReturnVersion := ReturnVersion + '.' + IntToStr(dwFileVersionLS and $FFFF);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

function CFileDir.GetVersion(const sFileName:string): string;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(sFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(sFileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    Result := IntToStr(dwFileVersionMS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionMS and $FFFF);
    Result := Result + '.' + IntToStr(dwFileVersionLS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionLS and $FFFF);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

Function CFileDir.GetEnvironmentVariable(sVariable : string) : String;
{ Returns the value of an environment variable.. Wrapper around WIN API Call}
var
  pcPath : PChar;
  rc : LongInt;
begin
  Result := '';
  GetMem(pcPath, BUFFER_MAX_LENGTH);
  try
    rc := APIGetEnvironmentVariable(PChar(sVariable), pcPath,LongInt(BUFFER_MAX_LENGTH));
  except
    ErrorRaised('Exception Raised while trying to call function APIGetSystemDir', ExceptObject);
  end;
  if ( rc > 0 ) then Result := StrPas(pcPath);
  FreeMem(pcPath);
end;

function CFileDir.GetSubDirList( PathName : String; MaxDepth : longint = 0 ) : TStringList;
//Obtains all the folders and child folders from a path up to a depth specified on MacDepth
var
  slResult : TStringList;
  i : LongInt;
Begin
  i := 0;
  slResult := TStringList.Create;
  try
    _GetSubDirList(PathName, slResult, i, MaxDepth);
  except
    ErrorRaised(UNIT_NAME + 'GetSubDirList' + Parms( [ 'PathName', PathName, 'MaxDepth', MaxDepth], True), ExceptObject);
    raise;
  end;
  slResult.Sort;
  result := slResult;
End;

function CFileDir._GetSubDirList( PathName : String; DirectoryList : TStringList; CurrentDepth : longint; MaxDepth : longint = 0 ) : longint;
var
  i, nCount : integer;
  slCurLvlDir : TStringList;
Begin

  slCurLvlDir := TStringList.Create;
  i := 0;
  nCount := 0;
  try
    if ( GetChildDirs(PathName, slCurLvlDir, CurrentDepth, MaxDepth) > 0 ) then begin
      Inc(CurrentDepth);
      for i := 0 to slCurLvlDir.Count - 1 do begin
        _GetSubDirList(slCurLvlDir[i], DirectoryList, CurrentDepth, MaxDepth);
      end; {for}
    end;
    DirectoryList.AddStrings(slCurLvlDir);
  except
    ErrorRaised(UNIT_NAME + ':_GetSubDirList' + Parms( [ 'PathName', PathName, 'DirectoryList', 'TStringList' , 'CurrentDepth', CurrentDepth, 'MaxDepth', MaxDepth], True), ExceptObject);
    raise;
  end; {try}
  _GetSubDirList := DirectoryList.Count;
  slCurLvlDir.Destroy;
End;

function CFileDir.GetChildDirs( PathName : String; DirectoryList : TStringList; CurrentDepth : longint; MaxDepth : longint = 0 ) : longint;
var
  nReturnCode, nCount : integer;
  bIsDirectory : boolean;
  sCurrentDir : String;
  SearchRec: TSearchRec;
Begin
  nCount := 0;
  bIsDirectory := False;
  if ( CurrentDepth <= MaxDepth ) then begin
    try
      nReturnCode := FindFirst(Slash(PathName) + '*.*', faDirectory, SearchRec);
      //SearchRec.Attr = faDirectory;
      sCurrentDir := SearchRec.Name;
      While ( sCurrentDir <> '' ) do begin
        sCurrentDir := Slash(PathName) + sCurrentDir;
        if ( sCurrentDir[Length(sCurrentDir)] <> '.') then begin
          //if bIsDirectory then sCurrentDir := sCurrentDir + '+';
          if bIsDirectory then begin
            DirectoryList.Add(sCurrentDir);
            Inc(nCount);
          end; {if}
        end; { if}
        bIsDirectory := False;
        nReturnCode := FindNext(SearchRec);
        if nReturnCode = 0 then begin
          sCurrentDir := SearchRec.Name;
          bIsDirectory := DirectoryExists( Slash(PathName) + sCurrentDir );
          //if ( SearchRec.Attr = faDirectory ) then bIsDirectory := True;
        end else
          sCurrentDir := '';
      end; {while}
    except
      ErrorRaised(UNIT_NAME + ':GetChildDirs' + Parms( [ 'PathName', PathName, 'DirectoryList', 'TStringList' , 'CurrentDepth', CurrentDepth, 'MaxDepth' , MaxDepth] , True), ExceptObject);
      raise;
    end; {try}
  end; {if}
  result := nCount;
End;

function CFileDir.GetSpecialFolder(FolderID : longint) : string;
var
 Path : pchar;
 idList : PItemIDList;
begin
 GetMem(Path, MAX_PATH);
 SHGetSpecialFolderLocation(0, FolderID, idList);
 SHGetPathFromIDList(idList, Path);
 Result := string(Path);
 FreeMem(Path);
end;

function CFileDir.GetTmpInternetDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_INTERNET_CACHE));
end;

function CFileDir.GetCookiesDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_COOKIES));
end;

function CFileDir.GetHistoryDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_HISTORY));
end;

function CFileDir.GetDesktop: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_DESKTOP));
end;

function CFileDir.GetDesktopDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_DESKTOPDIRECTORY));
end;

function CFileDir.GetProgDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_PROGRAMS));
end;

function CFileDir.GetMyDocDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_PERSONAL));
end;

function CFileDir.GetFavDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_FAVORITES));
end;

function CFileDir.GetStartUpDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_STARTUP));
end;

function CFileDir.GetRecentDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_RECENT));
end;

function CFileDir.GetSendToDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_SENDTO));
end;

function CFileDir.GetStartMenuDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_STARTMENU));
end;

function CFileDir.GetNetHoodDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_NETHOOD));
end;

function CFileDir.GetFontsDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_FONTS));
end;

function CFileDir.GetTemplateDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_TEMPLATES));
end;

function CFileDir.GetAppDataDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_APPDATA));
end;

function CFileDir.GetPrintHoodDir: string;
begin
 Result := Slash(GetSpecialFolder(CSIDL_PRINTHOOD));
end;
function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
        lParam, lpData: LPARAM): Integer stdcall;
begin
    if uMsg = BFFM_INITIALIZED then
        SendMessage(Wnd,BFFM_SETSELECTION, 1,
            Integer(@lg_StartFolder[1]));
    result := 0;
end;

function BrowseForFolderAPI(const browseTitle: String; var SelectedFolder : string;
        const initialFolder: String =''): boolean;
var
    browse_info: TBrowseInfo;
    folder: array[0..MAX_PATH] of char;
    find_context: PItemIDList;
begin
    FillChar(browse_info,SizeOf(browse_info),#0);
    lg_StartFolder := initialFolder;
    browse_info.pszDisplayName := @folder[0];
    browse_info.lpszTitle := PChar(browseTitle);
    browse_info.ulFlags := BIF_RETURNONLYFSDIRS;
    browse_info.lpfn := BrowseForFolderCallBack;
    find_context := SHBrowseForFolder(browse_info);
    if Assigned(find_context) then
    begin
        if SHGetPathFromIDList(find_context,folder) then
            SelectedFolder := folder
        else
            SelectedFolder := '';
    end
    else
        SelectedFolder := '';
    Result := ( Length(SelectedFolder) > 0 );

end;

function GetModuleName: string;
var
  NameBuffer: array[ 0..MAX_PATH ] of char;
begin
  ZeroMemory( @NameBuffer, SizeOf( NameBuffer ) );
  GetModuleFileName( HInstance, NameBuffer, Pred( SizeOf( NameBuffer ) ) );
  Result := ExtractFileName(NameBuffer);
end;

Procedure WriteString( StringToWrite, FileToWriteTo : String; AppendToFile : boolean);
var
  fd : CFiledir;
Begin
  try
    fd := CFiledir.Create();
    fd.WriteString( StringToWrite + #13#10, FileToWriteTo, AppendToFile );
  finally
    FreeAndNil( fd );
  end; {try}
End;

END.
