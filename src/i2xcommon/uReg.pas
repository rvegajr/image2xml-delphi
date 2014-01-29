unit uReg;

interface
uses
  Windows, Dialogs, SysUtils, Classes;

const
  UNIT_NAME = 'Reg';
  kernel = 'kernel32.dll';
  user = 'user32.dll';
  rgCLASSES = 'CLASSES_ROOT';
  rgCURRENT_USER = 'CURRENT_USER';
  rgLOCAL_MACHINE = 'LOCAL_MACHINE';
  rgUSERS = 'USERS';
  rgPERF_DATA = 'PERFORMANCE_DATA';
  rgCONFIG = 'CURRENT_CONFIG';
  rgDYN_DATA = 'DYN_DATA';

  regOptionNonVolatile = 0;

  regErrorNone = 0;
  regErrorBadDB = 1;
  regErrorBadKey = 2;
  regErrorCantOpen = 3;
  regErrorCantRead = 4;
  regErrorCantWrite = 5;
  regErrorOutOfMemory = 6;
  regErrorInvalidParameter = 7;
  regErrorAccessDenied = 8;
  regErrorInvalidParameterS = 87;
  regErrorNoMoreItems = 259;
  regKeyAllAccess = $3F;
  regKeyQueryValue = $1;
  regSZ = 1;
  regBinary = 3;
  regDWord = 4;

type

  CReg = class(TObject)
  private
    procedure Emsg( s : string );
    function Parms(v: array of Variant; Paired: Boolean): String;
    function GetStringParm(v: array of Variant; Paired: Boolean;
      MajorDelimiter, MinorDelimiter: String): String;
    function ucRight(s: String; NumOfRightChar: integer): String;
  public
    Procedure RegCreateNewKey( KeyName : String; RootKey : HKEY = HKEY_LOCAL_MACHINE);
    Procedure RegDeleteKey( KeyName : String ; RootKey : HKEY = HKEY_LOCAL_MACHINE);
    Procedure RegDeleteValue( KeyName, ValueName : String ; RootKey : HKEY = HKEY_LOCAL_MACHINE );
    Function RegEnumerateSubKeys( KeyName : String; Keys : TStringList ; RootKey : HKEY = HKEY_LOCAL_MACHINE) : Integer;
    Function RegEnumerateValues( KeyName : String; Values : TStringList ; RootKey : HKEY = HKEY_LOCAL_MACHINE) : Integer;
    Function RegGetKeyValue( KeyName, ValueName : String; RootKey : HKEY  = HKEY_LOCAL_MACHINE) : Variant;
    Function ConvertRootKey( RootKey : String) : LongInt; overload;
    Function ConvertRootKey( RootKey : Cardinal) : String; overload;
    Function RegError( ErrorCode : LongInt) : String;
    Procedure RegSetKeyValue( KeyName, ValueName : String; Data : String; RootKey : HKEY = HKEY_LOCAL_MACHINE); overload;
    Procedure RegSetKeyValue( KeyName, ValueName : String; Data : LongInt; RootKey : HKEY = HKEY_LOCAL_MACHINE); overload;
    Procedure RegSetKeyValue( KeyName, ValueName : String; Data: array of Byte; RootKey : HKEY = HKEY_LOCAL_MACHINE); overload;
    Function RegGet( KeyName, ValueName : String; DefaultValue : String = ''; RootKey : HKEY = HKEY_LOCAL_MACHINE) : String;
    Procedure RegSet( KeyName, ValueName : String; Data : String; RootKey : HKEY = HKEY_LOCAL_MACHINE);
    constructor Create(); overload;
    constructor Create(callingWindowedApp : TObject); overload;
    destructor Destroy; override;
  end;

function APICopyFile(ExistingFile, NewFile: PChar; nFailIfExists: LongInt): LongInt; stdcall;
    external kernel name 'CopyFileA';

implementation

Procedure CReg.RegCreateNewKey( KeyName : String; RootKey : HKEY = HKEY_LOCAL_MACHINE);
Begin
  try

  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'RootKey',RootKey ], True));
    raise;
  end; {try}
End;

Procedure CReg.RegDeleteKey( KeyName : String ; RootKey : HKEY = HKEY_LOCAL_MACHINE);
Begin
  try

  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'RootKey',RootKey ], True));
    raise;
  end; {try}
End;

Procedure CReg.RegDeleteValue( KeyName, ValueName : String ; RootKey : HKEY = HKEY_LOCAL_MACHINE );
Begin
  try

  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'ValueName', ValueName, 'RootKey',RootKey ], True));
    raise;
  end; {try}
End;

Function CReg.RegEnumerateSubKeys( KeyName : String; Keys : TStringList ; RootKey : HKEY = HKEY_LOCAL_MACHINE) : Integer;
Begin
  result := -1;
  try

  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'RootKey',RootKey ], True));
    raise;
  end; {try}
End;

Function CReg.RegEnumerateValues( KeyName : String; Values : TStringList ; RootKey : HKEY = HKEY_LOCAL_MACHINE) : Integer;
Begin
  result := -1;
  try

  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'RootKey',RootKey ], True));
    raise;
  end; {try}
End;

Function CReg.RegGetKeyValue( KeyName, ValueName : String; RootKey : HKEY  = HKEY_LOCAL_MACHINE) : Variant;
var
  lReturnValue, lData, lType, lSize : LongInt;
  lHKey : HKEY;
  vValue : Variant;
  sValue, sKeyName : String;
  pcBuffer : PChar;
Begin

  result := '';
  sKeyName := KeyName;
  try
   vValue := varEmpty;
   lSize := 0;
   lReturnValue := RegOpenKeyEx( RootKey, PChar(sKeyName), 0, regKeyQueryValue, lHKey);
   if lReturnValue = regErrorNone then begin
     try
      lReturnValue := RegQueryValueEx( lHKey, Pchar(ValueName), 0, @lType, 0, @lSize);
      if lReturnValue = regErrorNone then begin
        sValue := StringOfChar(' ', lSize);
        Case lType of
          regSZ : begin
            result := '';
            if ( lSize > 0 ) then begin
               lReturnValue := RegQueryValueEx( lHKey, Pchar(ValueName), 0, @lType, PByte(sValue), @lSize);
//               lReturnValue := RegQueryValueEx( lHKey, Pchar(ValueName), 0, @lType, Pchar(sValue), @lSize);
               result := sValue;
            end;
          end;
          regBinary : begin
            result := 0;
            lReturnValue := RegQueryValueEx( lHKey, Pchar(ValueName), 0, @lType, PByte(sValue), @lSize);
//            lReturnValue := RegQueryValueEx( lHKey, Pchar(ValueName), 0, @lType, Pchar(sValue), @lSize);
            If ( lReturnValue = regErrorNone  ) Then begin
              MoveMemory(PChar(sValue), @lData, SizeOf(lData));
              result := lData;
            end;
          end;
          regDWord : begin
            result := 0;
            lReturnValue := RegQueryValueEx( lHKey, Pchar(ValueName), 0, @lType, PByte(sValue), @lSize);
            //lReturnValue := RegQueryValueEx( lHKey, Pchar(ValueName), 0, @lType, Pchar(sValue), @lSize);
            If ( lReturnValue = regErrorNone  ) Then begin
              MoveMemory(PChar(sValue), @lData, SizeOf(lData));
              result := lData;
            end;
          end; {4:}
        end; {Case}
      end;{ Call returned no errors}
     finally
       RegCloseKey( lHKey );
     end; {try}
   end;
  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'RootKey',RootKey ], True));
    raise;
  end; {try}
End;

Function CReg.RegError( ErrorCode : LongInt) : String;
Begin
  Result := '';
  Case ErrorCode  of
    regErrorNone              : Result := 'No Error';
    regErrorBadDB             : Result := 'Bad DB';
    regErrorBadKey            : Result := 'Invalid Key';
    regErrorCantOpen          : Result := 'Cannot Open Key';
    regErrorCantRead          : Result := 'Cannot Read Key';
    regErrorCantWrite         : Result := 'Cannot write to Key';
    regErrorOutOfMemory       : Result := 'Out of Memory';
    regErrorInvalidParameter  : Result := 'Invalid Parameter';
    regErrorAccessDenied      : Result := 'Access Denied';
    regErrorInvalidParameterS : Result := 'Invalid Parameters';
    regErrorNoMoreItems       : Result := 'No More Items';
  end; {case}
End;

Function CReg.ConvertRootKey( RootKey : Cardinal) : String;
Begin
  result := '';
  try
    Case RootKey  of
     HKEY_CLASSES_ROOT     : result := 'HKEY_CLASSES_ROOT';
     HKEY_CURRENT_USER     : result := 'HKEY_CURRENT_USER';
     HKEY_LOCAL_MACHINE    : result := 'HKEY_LOCAL_MACHINE';
     HKEY_USERS            : result := 'HKEY_USERS';
     HKEY_PERFORMANCE_DATA : result := 'HKEY_PERFORMANCE_DATA';
     HKEY_CURRENT_CONFIG   : result := 'HKEY_CURRENT_CONFIG';
     HKEY_DYN_DATA         : result := 'HKEY_DYN_DATA';
    end; {case}
  except
    EMsg(UNIT_NAME + ':ConvertRootKey' + Parms(['RootKey', RootKey ], True));
    raise;
  end; {try}
End;

Function CReg.ConvertRootKey( RootKey : String) : LongInt;
Begin
  result := 0;
  try
    if UpperCase(ucRight(RootKey, Length(rgCLASSES))) = rgCLASSES then
     result := HKEY_CLASSES_ROOT
    else if UpperCase(ucRight(RootKey, Length(rgCURRENT_USER))) = rgCURRENT_USER then
     result := HKEY_CURRENT_USER
    else if UpperCase(ucRight(RootKey, Length(rgLOCAL_MACHINE))) = rgLOCAL_MACHINE then
     result := HKEY_LOCAL_MACHINE
    else if UpperCase(ucRight(RootKey, Length(rgUSERS))) = rgUSERS then
     result := HKEY_USERS
    else if UpperCase(ucRight(RootKey, Length(rgPERF_DATA))) = rgPERF_DATA then
     result := HKEY_PERFORMANCE_DATA
    else if UpperCase(ucRight(RootKey, Length(rgCONFIG))) = rgCONFIG then
     result := HKEY_CURRENT_CONFIG
    else if UpperCase(ucRight(RootKey, Length(rgDYN_DATA))) = rgDYN_DATA then
     result := HKEY_DYN_DATA
    else
      raise Exception.Create(QuotedStr(RootKey) + ' is an invalid root key.');
  except
    EMsg(UNIT_NAME + ':ConvertRootKey' + Parms(['RootKey', RootKey ], True));
    raise;
  end; {try}
End;

Procedure CReg.RegSetKeyValue( KeyName, ValueName : String; Data : String; RootKey : HKEY = HKEY_LOCAL_MACHINE);
var lHKey : HKEY;
Begin
  try
    RegCreateKeyEx(RootKey, PChar(KeyName), 0, '', regOptionNonVolatile, regKeyAllAccess, nil, lHKey, nil);
    RegSetValueEx(lHKey, PChar(ValueName), 0, regSZ, PChar(Data), Length(Data));
    RegCloseKey( lHKey );
  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'ValueName', ValueName, 'Data', Data, 'RootKey', ConvertRootKey(RootKey) ], True));
    raise;
  end; {try}
End;

Procedure CReg.RegSetKeyValue( KeyName, ValueName : String; Data : LongInt; RootKey : HKEY = HKEY_LOCAL_MACHINE);
var lHKey : HKEY;
Begin
  try
    RegCreateKeyEx(RootKey, PChar(KeyName), 0, '', regOptionNonVolatile, regKeyAllAccess, nil, lHKey, nil);
    RegSetValueEx(lHKey, PChar(ValueName), 0, regSZ, @(Data), SizeOf(Data));
    RegCloseKey( lHKey );
  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'ValueName', ValueName, 'Data', Data, 'RootKey', ConvertRootKey(RootKey) ], True));
    raise;
  end; {try}
End;

Procedure CReg.RegSetKeyValue( KeyName, ValueName : String; Data: array of Byte; RootKey : HKEY = HKEY_LOCAL_MACHINE);
var lHKey : HKEY;
Begin
  try
    RegCreateKeyEx(RootKey, PChar(KeyName), 0, '', regOptionNonVolatile, regKeyAllAccess, nil, lHKey, nil);
    RegSetValueEx(lHKey, PChar(ValueName), 0, regSZ, @(Data), SizeOf(Data));
    RegCloseKey( lHKey );
  except
    EMsg(UNIT_NAME + ':RegCreateNewKey' + Parms(['KeyName', KeyName, 'ValueName', ValueName, 'Data', 'Array of Byte', 'RootKey', ConvertRootKey(RootKey) ], True));
    raise;
  end; {try}
End;

Procedure CReg.RegSet( KeyName, ValueName : String; Data : String; RootKey : HKEY = HKEY_LOCAL_MACHINE);
Begin
  try
    RegSetKeyValue(KeyName, ValueName, Data, RootKey);
  except
    EMsg(UNIT_NAME + ':RegSet' + Parms(['KeyName', KeyName, 'ValueName', ValueName, 'Data', Data, 'RootKey', ConvertRootKey(RootKey) ], True));
    raise;
  end; {try}
End;

Function CReg.RegGet( KeyName, ValueName : String; DefaultValue : String = ''; RootKey : HKEY = HKEY_LOCAL_MACHINE) : String;
Begin
  result := DefaultValue;
  try
    result := Trim( RegGetKeyValue(KeyName, ValueName, RootKey) );
    if ( result = '' ) then result := Trim( DefaultValue );
  except
    raise Exception.Create(UNIT_NAME + ':RegGet' + Parms(['KeyName', KeyName, 'ValueName', ValueName, 'RootKey', ConvertRootKey(RootKey) ], True));
  end; {try}
End;


constructor CReg.Create;
begin

end;

constructor CReg.Create(callingWindowedApp: TObject);
begin

end;

destructor CReg.Destroy;
begin

  inherited;
end;

procedure CReg.Emsg(s: string);
begin
  MessageDlg(s, mtError,[mbOk], 0);
End;

function CReg.Parms(v: array of Variant; Paired: Boolean): String;
Begin
  try
    Parms := GetStringParm(v, Paired, ', ', ' = ');
  except
    EMsg(UNIT_NAME + ':CFileDir.Parms' + '- Error');
    raise;
  end; {try.. except}
End;

function CReg.GetStringParm(v: array of Variant; Paired: Boolean;
  MajorDelimiter, MinorDelimiter: String): String;
begin

End;

function CReg.ucRight( s : String; NumOfRightChar : integer ) : String;
Begin
  Result := Copy(s, ((Length(s) - NumOfRightChar) + 1), NumOfRightChar);
End;

END.
