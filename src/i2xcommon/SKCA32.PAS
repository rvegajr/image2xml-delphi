{*****************************************************************}
{* PROTECTION PLUS 4.4                                            }
{*                                                                }
{* Global constants and declare statements for Delphi 2-5         }
{* Uses SKCA32.DLL                                                }
{* Last Modified 07 Dec 2007                                      }
{*****************************************************************}
unit SKCA32;
interface
Uses WinTypes,
     WinProcs;
Type PBool     = ^WordBool;
     PBoolean  = ^Boolean;
     PByte     = ^Byte;
     PWord     = ^Word;
     PShortInt = ^ShortInt;
     PInteger  = ^Integer;
     PLongInt  = ^LongInt;
     PSingle   = ^Single;
     PDouble   = ^Double;

{###############################################}
{SKCA Result / Error Codes}
Const SWKERR_NONE = 0;
Const SWKERR_WINSOCK_STARTUP_ERROR            = 1;
Const SWKERR_WINSOCK_CANNOT_RESOLVE_HOST      = 2;
Const SWKERR_WINSOCK_CANNOT_CREATE_SOCKET     = 3;
Const SWKERR_WINSOCK_CANNOT_CONNECT_TO_SERVER = 4;
Const SWKERR_WINSOCK_CANNOT_SEND_DATA         = 5;
Const SWKERR_WINSOCK_CANNOT_READ_DATA         = 6;
Const SWKERR_NO_MORE_SOFTWARE_KEYS_AVAILABLE  = 7;
Const SWKERR_INVALID_SERVER_RESPONSE          = 8;
Const SWKERR_CANNOT_ALLOCATE_MEMORY           = 9;
Const SWKERR_WINSOCK_CANNOT_RESOLVE_PROXY     = 10;
{new error codes}
Const SWKERR_WININET_UNAVAILABLE              = 11;
Const SWKERR_WININET_FUNCTION_UNAVAILABLE     = 12;
Const SWKERR_NO_CONNECTION                    = 13;
Const SWKERR_INTERNAL_ERROR                   = 14;
Const SWKERR_WINSOCK_CONNECT_ERROR            = 15;
Const SWKERR_WINSOCK_BUFFER_OVERFLOW          = 16;
Const SWKERR_PARTIAL_CONNECTION               = 17;
Const SWKERR_INVALID_PROXY_LOGIN              = 18;
Const SWKERR_SERVER_DOWN                      = 19;
Const SWKERR_FILE_ERROR                       = 20;
Const SWKERR_FTP_FILENOTFOUND                 = 21;
Const SWKERR_CANCEL                           = 22;
Const SWKERR_ERROR_CREATING_WINDOW            = 23;
Const SWKERR_COULD_NOT_CREATE_FILE            = 24;
Const SWKERR_ITEMNOTFOUND                     = 25;
Const SWKERR_INVALIDPASSWORD                  = 26;
Const SWKERR_UPLOADFAILED                     = 27;
Const SWKERR_CONNECTION_TIMEOUT               = 28;
Const SWKERR_OS_INVALID                       = 29;
Const SWKERR_INVALID_PARAMETERS               = 30;
Const SWKERR_HTTP_FILENOTFOUND                = 31;

{###############################################}
{flags for functions}

{SK_ConnectionState Flags}
Const SK_FULL_CONNECTIVITY                    = 1;
Const SK_WININET_CONNECTIVITY_ONLY            = 2;
Const SK_CONNECTIVITY_QUICKCHECK              = 4;
Const SK_CONNECTIVITY_FULLCHECK               = 8;
Const SK_GET_PROXY                            = 16;
Const SK_PROXY_CONNECTION_TEST                = 32;

{PostRegData flags}
Const SK_NO_OVERWRITE                         = $00;
Const SK_OVERWRITE                            = $01;
Const SK_OPTIN1                               = $02;
Const SK_OPTIN2                               = $04;

{SKint_AsyncSocketProccessCommand flags for socket processing}
Const SK_SOCKET_FLAG_CONNECTIONONLY           = 1;

{ProcEZTrig1 & GetTCDataDlg flags}
Const SK_HIDE_PROXY_BUTTON                    = 1;

{Universal flags}
Const SK_SECURE_CONNECTION                    = 32768;
Const SK_SOCKETS_ONLY                         = 65536;

{SK_FileDownload Flags and Options}
Const SK_FILEOPT_HTTP                         = 1;
Const SK_FILEOPT_FTP                          = 2;

Const SK_FILEFLAG_VALIDATERETRY               = $01;
Const SK_FILEFLAG_NOIDEDIT                    = $02;
Const SK_FILEFLAG_NOPASSEDIT                  = $04;

Const SK_STATUS_FILESIZE                      = 1;
Const SK_STATUS_RECEIVING                     = 2;
Const SK_STATUS_ERROR                         = 3;
Const SK_CALLBACK_CANCEL                      = 4;
Const SK_STATUS_FINISHED                      = 5;
Const SK_STATUS_SENDING                       = 6;

{GETHttp flags}
Const SK_STRIP_HEADER                         = 1;

{SK_Firewall actions}
Const SK_ADD_APP                              = 0;
Const SK_ADD_PORT                             = 1;

{SK_Firewall flags}
Const SK_RETURN_OS_FAILURE                    = $01;

{SK_Firewall Protocols}
Const FW_PROTOCOL_TCP                         = 0;
Const FW_PROTOCOL_UDP                         = 1;

{SK_UpdateCheck flags}
Const SK_VERIFYDAYS                           = $01;

{SK_ProcEZTrig1}
{these are two error codes that can be returned by SK_ProcEZTrig1}
{these two codes are defined in the PPP4.PAS file}
{'these normally mean that one of the seed values do not match on the server and in the license file}
Const ERR_INVALID_REGKEY1                     = 69;
Const ERR_INVALID_REGKEY2                     = 70;


{###############################################}
{SKCA Function prototypes follow}

Function SK_ConnectionState (flags: LongInt; server: PChar; proxy: PChar; proxyuser: PChar; proxypw: PChar; timeout: LongInt): LongInt; stdcall;
Function SK_FileDownload(hwnd: LongInt; flags: LongInt; options: LongInt; server: PChar; port: Integer; url: PChar; savepath: PChar; id: PChar; password: PChar; filesize: LongInt; dlgText: PChar; callback: TFarProc): LongInt; stdcall; {***NOTE: The callback must be a procedure that has three LongInt parameters.***}
Function SK_Firewall(action: LongInt; flags: LongInt; appname: PChar; filename: PChar; port: LongInt; protocol: LongInt): LongInt; stdcall;
Procedure SK_GetErrorStr(number: LongInt; buffer: PChar); stdcall;
Function SK_GetLicenseStatus (server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; PW: PChar; status: PChar; replacedby: PLongInt; licenseupdate: PChar): LongInt; stdcall;
Function SK_GetLicenseStatusEx (flags: LongInt; server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; PW: PChar; status: PChar; replacedby: PLongInt; licenseupdate: PChar): LongInt; stdcall;
Function SK_GetRegData (server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; PW: PChar; companyname: PChar; contactname: PChar; email: PChar; phone: PChar; ud1: PChar; ud2: PChar; ud3: PChar; ud4: PChar; ud5: PChar): LongInt; stdcall;
Function SK_GetRegDataEx (flags: LongInt; server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; PW: PChar; companyname: PChar; contactname: PChar; email: PChar; phone: PChar; ud1: PChar; ud2: PChar; ud3: PChar; ud4: PChar; ud5: PChar): LongInt; stdcall;
Function SK_GetTCData (server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt;Password: PChar; cenum: LongInt; compno: LongInt; result1: PLongInt; result2: PLongInt; licenseupd: PChar): LongInt; stdcall;
Function SK_GetTCDataEx (flags: LongInt; server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt;Password: PChar; cenum: LongInt; compno: LongInt; result1: PLongInt; result2: PLongInt; licenseupd: PChar): LongInt; stdcall;
Function SK_GetTCDataDlg (hwnd: LongInt; lfhandle: LongInt; flags: LongInt; server: PChar; proxy: PChar; url: PChar; usercode1: LongInt; usercode2: LongInt; regkey1: PLongInt; regkey2: PLongInt; licenseid: PLongInt; password: PChar; licenseupd: PChar): LongInt; stdcall;
Function SK_Parse (flags: LongInt; data: PChar; item: LongInt; delimiter: PChar; value: PChar): LongInt; stdcall;
Function SK_PostCounters (server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; counter1: LongInt; counter2: LongInt; counter3: LongInt; counter4: LongInt; counter5: LongInt; licenseupd: PChar): LongInt; stdcall;
Function SK_PostCounterEx (flags: LongInt; server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; counter1: LongInt; counter2: LongInt; counter3: LongInt; counter4: LongInt; counter5: LongInt; licenseupd: PChar): LongInt; stdcall;
Function SK_PostEvalData (server: PChar; proxy: PChar; url: PChar; firstname: PChar; lastname: PChar; email: PChar; phone: PChar; ud1: PChar; ud2: PChar; ud3: PChar; ud4: PChar; ud5: PChar; regid: PLongInt): LongInt; stdcall;
Function SK_PostEvalDataEx (flags: LongInt; server: PChar; proxy: PChar; url: PChar; firstname: PChar; lastname: PChar; email: PChar; phone: PChar; ud1: PChar; ud2: PChar; ud3: PChar; ud4: PChar; ud5: PChar; regid: PLongInt): LongInt; stdcall;
Function SK_PostRegData (flags: LongInt; server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; password: PChar; companyname: PChar; firstname: PChar; lastname: PChar; address1: PChar; address2: PChar; city: PChar; state: PChar; postalcode: PChar; country: PChar; phone: PChar; fax: PChar; email: PChar): LongInt; stdcall;
Function SK_PostStrings (server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; string1: PChar; string2: PChar; string3: PChar; string4: PChar; string5: PChar; licenseupd: PChar): LongInt; stdcall;
Function SK_PostStringEx (flags: LongInt; server: PChar; proxy: PChar; url: PChar; LicenseID: LongInt; string1: PChar; string2: PChar; string3: PChar; string4: PChar; string5: PChar; licenseupd: PChar): LongInt; stdcall;
Function SK_ProcEZTrig1 (hwnd: LongInt; filename: PChar; password: PChar; flags: LongInt; errorcode: PLongInt): LongInt; stdcall;
Function SK_UpdateCheck (flags: LongInt; options: LongInt; server: PChar; port: Integer; url: PChar; productid: LongInt; productname: PChar; languagecode: PChar; ver1: LongInt; ver2: LongInt; ver3: LongInt; ver4: LongInt; action: PChar; data: PChar; datasize: LongInt; msg: PChar; msgsize: LongInt; urlinfo: PChar; latestversion: PChar; releasedate: PChar; licenseid: LongInt; Password: PChar; status: PChar; replacedby: PLongInt; licenseupdate: PChar): LongInt; stdcall;

implementation

Function SK_ConnectionState; external 'SKCA32.dll';
Function SK_FileDownload; external 'SKCA32.dll';
Function SK_Firewall; external 'SKCA32.dll';
Procedure SK_GetErrorStr; external 'SKCA32.dll';
Function SK_GetLicenseStatus; external 'SKCA32.dll';
Function SK_GetLicenseStatusEx; external 'SKCA32.dll';
Function SK_GetRegData; external 'SKCA32.dll';
Function SK_GetRegDataEx; external 'SKCA32.dll';
Function SK_GetTCData; external 'SKCA32.dll';
Function SK_GetTCDataEx; external 'SKCA32.dll';
Function SK_GetTCDataDlg; external 'SKCA32.dll';
Function SK_Parse; external 'SKCA32.dll';
Function SK_PostCounters; external 'SKCA32.dll';
Function SK_PostCounterEx;  external 'SKCA32.dll';
Function SK_PostEvalData; external 'SKCA32.dll';
Function SK_PostEvalDataEx; external 'SKCA32.dll';
Function SK_PostRegData; external 'SKCA32.dll';
Function SK_PostStrings; external 'SKCA32.dll';
Function SK_PostStringEx; external 'SKCA32.dll';
Function SK_ProcEzTrig1; external 'SKCA32.dll';
Function SK_UpdateCheck; external 'SKCA32.dll';


end.