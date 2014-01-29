{*****************************************************************}
{* PROTECTION PLUS 4.4.1.0                                        }
{*                                                                }
{* Global constants and declare statements for Delphi 2-5         }
{* Uses KeyLib32.DLL                                              }
{* Last Modified 21 Oct 2007                                      }
{*****************************************************************}

unit PPP4;

interface

Uses 
     WinTypes,
     WinProcs;
var
     lfhandle: LongInt;
     compno: LongInt;
     semhandle: LongInt;

Type 
     PBool     = ^WordBool;
     PBoolean  = ^Boolean;
     PByte     = ^Byte;
     PWord     = ^Word;
     PShortInt = ^ShortInt;
     PInteger  = ^Integer;
     PLongInt  = ^LongInt;
     PSingle   = ^Single;
     PDouble   = ^Double;

{pp_compno32drivers() flags }
Const CHECK_DRIVER_STATUS = 0;  // Check to see if the driver is loaded.
Const INSTALL_DRIVER      = 1;  // Installs the driver specified.
Const UNINSTALL_DRIVER    = 2;  // Uninstalls the driver specified.

{pp_compno32drivers() algorithm defines }
Const COMPNO32_BIOS = 1;
Const COMPNO64_BIOS = 2;

{ pp_compno() algorithm defines }
Const COMPNO_BIOS      = $0001;
Const COMPNO_HDSERIAL  = $0002;
Const COMPNO_HDLOCK    = $0004;
Const COMPNO_HDTYPE    = $0008;
Const COMPNO_NETNAME   = $0010;
Const COMPNO_MACADDR   = $0020;
Const COMPNO_FILESTART = $0040;
Const COMPNO_WINPRODID = $0080;
Const COMPNO_IPADDR    = $0100;
Const COMPNO_MACADDR_WIN98 = $0200;
Const COMPNO_BIOS16        = $0400;
Const COMPNO_BIOS32        = $0800;
Const COMPNO_MACADDR_WINNB = $1000;
Const COMPNO_BIOS64        = $2000;	
Const COMPNO_MACADDR_4200  = $4000;
{ To suppress the MessageBoxes relating to Machnm1.exe }
Const SUPRESS_ERRORS        = $8000;
Const COMPNO_ENHANCED       = $10000;	//use new compid algs
Const COMPNO_SERVER_MACADDR = $20000;
Const COMPNO_NO_WMI         = $40000; //used with COMPNO_ENHANCED, COPYCHK_ENHANCED, and COPYADD_ENHANCED to omit WMI initialization for use in a DLL

{ pp_copycheck() action codes }
Const ACTION_MANUAL    = 0;
Const ACTION_AUTOADD   = 1;
Const COPYCHK_STRICT   = 2;
Const COPYCHK_ENHANCED = 4;

{ pp_copyadd() flags }
Const COPYADD_ADDDUPE  = 1;
Const COPYADD_ERASEALL = 2;
Const COPYADD_ENHANCED = 4;

{  pp_copydelete() flags }
Const COPYDEL_ALL	= -1;
Const COPYDEL_ENHANCED	= -2;

{ pp_copyget flags (slot) }
Const COPYGET_ENHANCED	= -1;

{ License File type defines }
Const LF_FILE 		= 1;
Const LF_REGISTRY 	= 2;
Const LF_CBOX_VERSA 	= 3;
Const LF_CBOX_560 	= 4;
Const LF_INTERNET 	= 5;

{ License File flag defines }
Const LF_CREATE_NORMAL 	= 0;
Const LF_CREATE_RDONLY 	= 1;
Const LF_CREATE_HIDDEN 	= 2;
Const LF_CREATE_SYSTEM 	= 4;
Const LF_CREATE_MISSING = 8;
Const LFOPEN_NOLOOP 	= 16;
Const LFOPEN_NOCACHE   		= $0020;
Const LFOPEN_OPENALWAYS 	= $0040;  // used in pp_semopen to create file if not there

{ pp_exportfile and import file flag defines }
Const PPP_NOURLENCODE   = 1;
Const PPP_EZTRIGPROCESS = 1;

{ pp_lfcreate() flags }
Const LFCREATE_OVERWRITE = 1;

{ pp_semopen flags }
Const SEM_FILE  = 0;
Const SEM_TCPIP = 1;

{ pp_update flags }
Const UPDDATE_FORCE  = 1;
Const UPDDATE_NOTIME = 2;

{ pp_lfalias() flags }
Const LFALIAS_VERIFY_CHECKSUM 	= 1;
Const LFALIAS_FIND_RECENT 	= 2;
Const LFALIAS_OVERWRITE_OLDER	= 4;
Const LFALIAS_CREATE_MISSING 	= 8;
Const LFALIAS_SCATTER         = 16;
Const LFALIAS_SCATTER_BACK    = $8000000; // high bit
Const LFALIAS_NOCACHE         = 32;

{ pp_eztrig1dlg() flags }
Const EZTRIG1DLG_PROCESS = 1;
Const EZTRIG1DLG_DELAYED = 2;

{ return codes for pp_eztrial1() }
Const EZTRIAL1_ERROR           = 0;
Const EZTRIAL1_RETAIL          = 1;
Const EZTRIAL1_DEMO            = 2;
Const EZTRIAL1_BINDING_FAILED  = 3;
Const EZTRIAL1_EXPIRED         = 4;
Const EZTRIAL1_CLOCKTURNEDBACK = 5;
Const EZTRIAL1_PERIODIC        = 6;
Const EZTRIAL1_EXCEEDED_USERS  = 7;

{ pp_eztrial1ex() flags }
Const EX_TRIAL1EX_NO_NET = 1;

{ pp_gotourl() flags }
Const GOTOURL_BUYNOW = 1;

{ pp_importactfile flags }
Const PP_EZTRIGPROCESS = 1;

{ pp_netopen() flags }
Const NET_SINGLE = 0;
Const NET_DO_NOT_UPDATE	 = 1;
Const NET_ALWAYS_CONNECTED = 2;

{ pp_sysinfo() flags }
Const PP_TERMSERV  = 1;
Const PP_VMWARE	 = 2;
Const PP_VIRTUALPC = 4;
Const PP_64BIT	 = 8;

{ flags for pp_transfer() }
Const PP_TRANSFER_ENHANCED	= -1;

{ miscellaneous defines }
Const NO_FLAGS            = 0;
Const PP_USE_UTC          = 1;
Const TCODE_MAX           = 50;
Const PP_MAX_UDEF_STRINGS = 10;
Const PP_MAX_UDEF_NUMBERS = 5;
Const PP_MAX_UDEF_DATES   = 5;

{ result and error codes }
Const PP_FAILURE = 0;
Const PP_FALSE   = 0;
Const PP_SUCCESS = 1;
Const PP_TRUE    = 1;

Const ERR_INVALID_ATTRIBUTES    = 2;
Const ERR_CANNOT_CHANGE_ATTRIBS = 3;
Const ERR_HFILE_ERROR           = 4;
Const ERR_CANNOT_WRITE_FILE     = 5;
Const ERR_CANNOT_CLOSE_FILE     = 6;
Const ERR_CANNOT_OPEN_FILE      = 7;
Const ERR_CANNOT_READ_FILE      = 8;
Const ERR_CANNOT_CREATE_FILE    = 9;
Const ERR_CANNOT_DELETE_FILE    = 10;
Const ERR_FILE_WAS_CREATED      = 11;
Const ERR_INVALID_PASSWORD      = 12;
Const ERR_WRONG_PASSWORD        = 13;
Const ERR_INCORRECT_PARAMETERS  = 14;
Const ERR_FILE_MISSING          = 15;
Const ERR_MEMORY_ALLOCATION     = 16;
Const ERR_MEMORY_FREE           = 17;
Const ERR_MEMORY_LOCK           = 18;
Const ERR_SLOT_NUM_INVALID      = 19;
Const ERR_SLOT_EMPTY            = 20;
Const ERR_SLOTS_FULL            = 21;
Const ERR_SLOT_ALREADY_ASSIGNED = 22;
Const ERR_NET_LIC_FULL          = 23;
Const ERR_COMPNO_NOT_FOUND      = 24;
Const ERR_VAR_NO_INVALID        = 25;
Const ERR_SOFT_EXPIRATION       = 26;
Const ERR_EXPTYPE_INVALID       = 27;
Const ERR_EXP_DATE_EMPTY        = 28;
Const ERR_STRING_TOO_LONG       = 29;
Const ERR_CURRENT_DATE_OLDER    = 30;
Const ERR_CANNOT_LOCK_FILE      = 31;
Const ERR_WRONG_LF_VERSION      = 32;
Const ERR_CORRUPT_LICENSE_FILE  = 33;
Const ERR_SEM_FILE_LOCKED       = 34;
Const ERR_CORRUPT_CONTROL_FILE  = 35;
Const ERR_WRONG_CF_SERIAL_NUM   = 36;
Const ERR_LF_LOCKED             = 37;
Const ERR_LF_CHECKSUM_INVALID   = 38;
Const ERR_NOT_APPLICABLE        = 39;
Const ERR_NOT_IMPLEMENTED_YET   = 40;
Const ERR_FILE_EXISTS           = 41;
Const ERR_REGISTRY_OPEN         = 42;
Const ERR_REGISTRY_QUERY        = 43;
Const ERR_REGISTRY_CLOSE        = 44;
Const ERR_REGISTRY_READ         = 45;
Const ERR_REGISTRY_SET          = 46;
Const ERR_CBOX_NOT_PRESENT      = 47;
Const ERR_CBOX_WRONG_TYPE       = 48;
Const ERR_CBOX_READ_RAM1_ERROR  = 49;
Const ERR_CBOX_READ_RAM2_ERROR  = 50;
Const ERR_CBOX_WRITE_RAM1_ERROR = 51;
Const ERR_CBOX_WRITE_RAM2_ERROR = 52;
Const ERR_CBOX_ID1_ERROR           = 53;
Const ERR_CBOX_ID2_ERROR           = 54;
Const ERR_CBOX_ID3_ERROR           = 55;
Const ERR_VAR_NOT_AVAILABLE        = 56;
Const ERR_DEMO_HAS_EXPIRED         = 57;
Const ERR_WINSOCK_STARTUP_ERROR    = 58;
Const ERR_WINSOCK_CANNOT_RESOLVE_HOST      = 59;
Const ERR_WINSOCK_CANNOT_CREATE_SOCKET     = 60;
Const ERR_WINSOCK_CANNOT_CONNECT_TO_SERVER = 61;
Const ERR_WINSOCK_CANNOT_SEND_DATA         = 62;
Const ERR_WINSOCK_CANNOT_READ_DATA         = 63;
Const ERR_NO_MORE_SOFTWARE_KEYS_AVAILABLE  = 64;
Const ERR_INVALID_SERVER_RESPONSE          = 65;
Const ERR_CANNOT_ALLOCATE_MEMORY           = 66;
Const ERR_WINSOCK_CANNOT_RESOLVE_PROXY     = 67;
Const ERR_ALIAS_FILE_DOES_NOT_MATCH        = 68;
Const ERR_INVALID_CODE_ENTERED             = 69;
Const ERR_INVALID_REGKEY2_ENTERED          = 70;
Const ERR_ACCESS_DENIED	                   = 71;
Const ERR_NON_WIN32_OS                     = 72;
Const ERR_DRIVER_CORRUPT                   = 73;
Const ERR_MARKED_FOR_DELETE                = 74;
// Const ERR_FAKE_FOR_FOXPRO
Const ERR_INVALID_NETHANDLE                = 75;
Const ERR_NO_MESSAGES                      = 76;
Const ERR_ALREADY_OPEN                     = 77;
Const ERR_INVALID_NETWORK_ADDRESS          = 78;
Const ERR_NON_WIN64_OS                     = 79;
Const ERR_NOT_SUPPORTED_BY_OS              = 80;
Const ERR_COULD_NOT_LOAD_DLL               = 81;
Const ERR_FUNCTION_NOT_AVAILABLE           = 82;
Const ERR_NO_DRIVES_FOUND                  = 83;
Const ERR_INTERNAL_BROADCAST               = 84;
Const ERR_BIND_FAILED	                      = 85;
Const ERR_CANNOT_CREATE_THREAD             = 86;
Const ERR_NETWORK_CONNECTIVITY             = 87;
Const ERR_COULD_NOT_INSTALL_DRIVER         = 88;
Const ERR_NETWORK_RECONNECTED	              = 89;
Const ERR_NET_MULTI_STARTUP                = 90;
Const ERR_COMPNO_ENHANCED	                  = 91;

// Const ERR_SEND_MESSAGE
// Const ERR_READ_MESSAGE

Const ERR_FILE_SIZE_MISMATCH = 100;
Const ERR_FILE_INCORRECT     = 101;

{ compno returns for COMPNO_BIOS32 }
Const ERR_ACCESS_DENIED_BIOS32     = -71;
Const ERR_NON_WIN32_OS_BIOS32      = -72;
Const ERR_DRIVER_CORRUPT_BIOS32    = -73;
Const ERR_MARKED_FOR_DELETE_BIOS32 = -74;


{ file attribute defines }
Const PP_NORMAL = 0;
Const PP_RDONLY = 1;
Const PP_HIDDEN = 2;
Const PP_SYSTEM = 4;

{ standard threshhold - used internally only }
Const PP_STD_THRESHOLD = 20;

{ expiration types }
Const EXP_NONE          = 'N';
Const EXP_EXE_LIMIT     = 'E';
Const EXP_SHAREWARE_VER = 'S';
Const EXP_PAYMENT_LIMIT = 'P';
Const EXP_DEMO_VERSION  = 'D';

{ getvar / setvar definitions - character fields }
Const VAR_COMPANY     = 1;
Const VAR_NAME        = 2;
Const VAR_ADDRESS1    = 3;
Const VAR_ADDRESS2    = 4;
Const VAR_ADDRESS3    = 5;
Const VAR_PHONE1      = 6;
Const VAR_PHONE2      = 7;
Const VAR_SERIAL_TEXT = 8;
Const VAR_EXPIRE_TYPE = 9;
Const VAR_UDEF_CHAR_1 = 10;
Const VAR_UDEF_CHAR_2 = 11;
Const VAR_UDEF_CHAR_3 = 12;
Const VAR_UDEF_CHAR_4 = 13;
Const VAR_UDEF_CHAR_5 = 14;
Const VAR_UDEF_CHAR_6 = 15;
Const VAR_UDEF_CHAR_7 = 16;
Const VAR_UDEF_CHAR_8 = 17;
Const VAR_UDEF_CHAR_9 = 18;
Const VAR_UDEF_CHAR_10 = 19;
Const VAR_EZTRIAL_COMPNO_DRIVE    = 20;
Const VAR_EZTRIAL_SYSDIR_FILENAME = 21;
Const VAR_EZTRIG_COMPNO_DRIVE     = 22;
Const VAR_EZTRIAL_REG_ALIAS1      = 23;
Const VAR_EZTRIAL_REG_ALIAS2      = 24;
Const VAR_EZTRIG_DLG_LABELS       = 25;
Const VAR_AUTOCL_PROXY_ADDR       = 26;
Const VAR_AUTOCL_URL_VISIT        = 27;
Const VAR_AUTOCL_URL_BUYNOW       = 28;
Const VAR_AUTOCL_URL_UNLOCK       = 29;
Const VAR_AUTOCL_URL_REGISTER     = 30;
Const VAR_EZTRIAL_COMPNO_FILE     = 31;
Const VAR_EZTRIG_COMPNO_FILE      = 32;
Const VAR_LICENSEPW               = 33;

{ getvar / setvar definitions - numeric fields }
Const VAR_SERIAL_NUM           = 1;
Const VAR_EXP_COUNT            = 2;
Const VAR_EXP_LIMIT            = 3;
Const VAR_LAN_COUNT            = 4;
Const VAR_LAN_LIMIT            = 5;
Const VAR_INSTALL_COUNT        = 6;
Const VAR_INSTALL_LIMIT        = 7;
Const VAR_AUTHORIZED_COMPS     = 8;
Const VAR_UDEF_NUM_1           = 9;
Const VAR_UDEF_NUM_2           = 10;
Const VAR_UDEF_NUM_3           = 11;
Const VAR_UDEF_NUM_4           = 12;
Const VAR_UDEF_NUM_5           = 13;
Const VAR_LF_CHECKSUM          = 14;
Const VAR_EZTRIAL_SOFT_BINDING = 15;
Const VAR_EZTRIAL_HARD_BINDING = 16;
Const VAR_EZTRIAL_COMPNO_THRESHOLD  = 17;
Const VAR_EZTRIAL_CONVERT_COPIES    = 18;
Const VAR_EZTRIAL_UPDATE_LAST_TIME  = 19;
Const VAR_EZTRIAL_COMPNO_ALGORITHMS = 20;
Const VAR_EZTRIAL_FILE_VERSION      = 21;
Const VAR_EZTRIG_FLAGS              = 22;
Const VAR_EZTRIG_COMPNO_ALGORITHMS  = 23;
Const VAR_EZTRIG_SEED               = 24;
Const VAR_EZTRIG_REGKEY2SEED        = 25;
Const VAR_EZTRIAL_DAYS_TO_RUN       = 26;
Const VAR_EZTRIAL_TIMES_TO_RUN      = 27;
Const VAR_LICENSEID                 = 28;
Const VAR_CUSTOMERID                = 29;

{ getvar / setvar definitions - date fields }
Const VAR_EXP_DATE_SOFT = 1;
Const VAR_EXP_DATE_HARD = 2;
Const VAR_LAST_DATE     = 3;
Const VAR_LAST_TIME     = 4;
Const VAR_UDEF_DATE_1   = 5;
Const VAR_UDEF_DATE_2   = 6;
Const VAR_UDEF_DATE_3   = 7;
Const VAR_UDEF_DATE_4   = 8;
Const VAR_UDEF_DATE_5   = 9;

{ compatibility with previous versions }
Const VAR_PRODUCT     = VAR_UDEF_CHAR_2;
Const VAR_DISTRIBUTOR = VAR_UDEF_CHAR_3;
Const VAR_USER_DEF_1  = VAR_UDEF_CHAR_1;
Const VAR_USER_DEF_2  = VAR_UDEF_NUM_1;
Const VAR_USER_DEF_3  = VAR_UDEF_NUM_2;
Const VAR_USER_DEF_4  = VAR_UDEF_DATE_1;

{ Function prototypes follow }
Procedure pp_adddays (month: PLongInt; day: PLongInt; year: PLongInt; days: LongInt); stdcall;
Function pp_bitclear (bit_field: PLongInt; bit_number: LongInt): LongInt; stdcall;
Function pp_bitset (bit_field: PLongInt; bit_number: LongInt): LongInt; stdcall;
Function pp_bittest (bit_field: LongInt; bit_number: LongInt): LongInt; stdcall;
// Only uncomment this in if you are directed to do so
// Procedure pp_cedate ( enum : LongInt; month : PLongInt; day : PLongInt; year : PLongInt); stdcall;
Function pp_checksum (filename: PChar; checksum: PLongInt): LongInt; stdcall;
Function pp_cenum: LongInt; stdcall;
Function pp_chkvarchar (handle: LongInt; var_no: LongInt): LongInt; stdcall;
Function pp_chkvardate (handle: LongInt; var_no: LongInt): LongInt; stdcall;
Function pp_chkvarnum (handle: LongInt; var_no: LongInt): LongInt; stdcall;
Function pp_compno32drivers( driver : LongInt; flags : LongInt): LongInt; stdcall;
Function pp_compno (cnotype: LongInt; filename: PChar; hard_drive: PChar): LongInt; stdcall;
Function pp_convertv3 (handle: LongInt; v3_cf: PChar; v3_cpf: PChar; v3_sn: LongInt): LongInt; stdcall;
Function pp_copyadd (handle: LongInt; flags: LongInt; comp_num: LongInt): LongInt; stdcall;
Function pp_copycheck (handle: LongInt; action: LongInt; comp_num: LongInt): LongInt; stdcall;
Function pp_copycheckth (handle: LongInt; action: LongInt; comp_num: LongInt; threshold: LongInt): LongInt; stdcall;
Function pp_copydelete (handle: LongInt; comp_num: LongInt): LongInt; stdcall;
Function pp_copyget (handle: LongInt; slot: LongInt; comp_num: PLongInt): LongInt; stdcall;
Function pp_countdec (handle: LongInt; var_no: LongInt): LongInt; stdcall;
Function pp_countinc (handle: LongInt; var_no: LongInt): LongInt; stdcall;
// Only uncomment this in if you are directed to do so
// Function pp_ctcodes (code: LongInt; cenum: LongInt; computer: LongInt; seed: LongInt): LongInt; stdcall;
Function pp_daysleft (handle: LongInt; daysleft: PLongInt): LongInt; stdcall;
Procedure pp_decrypt (iinstr: PChar; pwstr: PChar; ret: PChar); stdcall;
Procedure pp_encrypt (iinstr: PChar; pwstr: PChar; ret: PChar); stdcall;
Procedure pp_errorstr (number: LongInt; buffer: PChar); stdcall;
Function pp_expired (handle: LongInt): LongInt; stdcall;
Function pp_exportactfile( flags : LongInt; lfhandle : LongInt; licensepath : PChar; filepath : PChar; 
                           server : PChar; url : PChar; key : PChar; licenseid : LongInt; password : PChar; 
                           compid : LongInt; version : PChar; productid : LongInt; source : Pchar): LongInt; stdcall;
Function pp_eztrial1 (filename: PChar; password: PChar; errorcode: PLongInt; lfhandle: PLongInt): LongInt; stdcall;
Function pp_eztrial1test (lfhandle: LongInt; errorcode: PLongInt): LongInt; stdcall;
Function pp_eztrial2 ( hwndDlg : LongInt; filename: PChar; password: PChar; flags : LongInt; errorcode : PLongInt; lfhandle: PLongInt): LongInt; stdcall;
Function pp_eztrig1 (hwnd: LongInt; filename: PChar; password: PChar; errorcode: PLongInt): LongInt; stdcall;
Function pp_eztrig1dlg (hwnd: LongInt; handle: LongInt; flags: LongInt; dlg_labels: PChar; usercode1: LongInt;
                         usercode2: LongInt; tcseed: LongInt; regkey2seed: LongInt; tcvalue: PLongInt; tcdata: PLongInt): LongInt; stdcall;
Function pp_eztrig1ex (handle: LongInt; regkey1: LongInt; regkey2: LongInt; flags: LongInt; usercode1: LongInt;
                        usercode2: LongInt; tcseed: LongInt; regkey2seed: LongInt; tcvalue: PLongInt; tcdata: PLongInt): LongInt; stdcall;
Function pp_filedelete (filename: PChar): LongInt; stdcall;
Function pp_getcode (hwnd: LongInt; str_title: PChar; str_cenum: PChar; str_comp: PChar; str_code: PChar): LongInt; stdcall;
Procedure pp_getdate (month: PLongInt; day: PLongInt; year: PLongInt; dayofweek: PLongInt); stdcall;
//Procedure pp_getdateex (flags: LongInt; month: PLongInt; day: PLongInt; year: PLongInt; dayofweek: PLongInt); stdcall;
Procedure pp_gettime (hours: PLongInt; minutes: PLongInt; seconds: PLongInt; hseconds: PLongInt); stdcall;
//Procedure pp_gettimeex (flags: LongInt; hours: PLongInt; minutes: PLongInt; seconds: PLongInt; hseconds: PLongInt); stdcall;
Function pp_getvarchar (handle: LongInt; var_no: LongInt; buffer: PChar): LongInt; stdcall;
Function pp_getvardate (handle: LongInt; var_no: LongInt; month_hours: PLongInt; day_minutes: PLongInt;
                         year_seconds: PLongInt): LongInt; stdcall;
Function pp_getvarnum (handle: LongInt; var_no: LongInt; value: PLongInt): LongInt; stdcall;
Function pp_gotourl (hwnd: LongInt; handle: LongInt; flags: LongInt; url: PChar): LongInt; stdcall;
Function pp_hdserial (drive : PChar): LongInt; stdcall;
Function pp_importactfile( flags : LongInt; lfhandle : LongInt; importtype : LongInt;
                           compid : LongInt; regkey1 : PLongInt; regkey2 : PLongInt;
                           session : PLongInt; licenseupd : PChar): LongInt; stdcall;
Procedure pp_lastday (month: LongInt; day: PLongInt; year: LongInt); stdcall;
Function pp_lanactive (handle: LongInt): LongInt; stdcall;
Function pp_lancheck (handle: LongInt): LongInt; stdcall;
Function pp_libtest (testnum: LongInt): LongInt; stdcall;
Procedure pp_libversion(ver1: PLongInt; ver2: PLongInt; ver3: PLongInt; ver4: PLongInt); stdcall;
Function pp_lfalias (handle: LongInt; filename: PChar; lfflags: LongInt; lftype: LongInt; password: PChar;
                      recenthandle: PLongInt): LongInt; stdcall;
Function pp_lfclose (handle: LongInt): LongInt; stdcall;
Function pp_lfcopy (handle: LongInt; filename: PChar; lftype: LongInt): LongInt; stdcall;
Function pp_lfcreate (filename: PChar; flags: LongInt; lftype: LongInt; password: PChar; attrib: LongInt): LongInt; stdcall;
Function pp_lfdelete (filename: PChar; flags: LongInt; lftype: LongInt; password: PChar): LongInt; stdcall;
Function pp_lflock (mem_handle: LongInt): LongInt; stdcall;
Function pp_lfopen (filename: PChar; lfflags: LongInt; lftype: LongInt; password: PChar; handle: PLongInt): LongInt; stdcall;
Function pp_lfunlock (mem_handle: LongInt): LongInt; stdcall;
Function pp_ndecrypt (number: LongInt; seed: LongInt): LongInt; stdcall;
Function pp_ndecryptx (buffer: PChar; number1: PLongInt; number2: PLongInt; number3: PLongInt; number4: PLongInt;
                        seed: LongInt): LongInt; stdcall;
Function pp_nencrypt (number: LongInt; seed: LongInt): LongInt; stdcall;
Procedure pp_nencryptx (buffer: PChar; number1: LongInt; number2: LongInt; number3: LongInt; number4: LongInt; seed: LongInt); stdcall;

Function pp_netclose (lNetHandle : LongInt): LongInt; stdcall;
Function pp_netopen (network_password : PChar; flags : LongInt; number1 : LongInt; string1 : 
                      PChar; nethandle : PLongInt; errorcode : PLongInt): LongInt; stdcall;
Function pp_nettest (lNetHandle : LongInt): LongInt; stdcall;

Procedure pp_npdate (month: PLongInt; day: PLongInt; year: PLongInt; dop: LongInt); stdcall;
Function pp_password (buffer: PChar): LongInt; stdcall;
Function pp_redir (drive: PChar): LongInt; stdcall;
Function pp_semclose (handle: LongInt): LongInt; stdcall;
Function pp_semcount (handle: LongInt; semtype: LongInt; prefix_server: PChar; name: PChar; number: PLongInt): LongInt; stdcall;
Function pp_semopen (handle: LongInt; semtype: LongInt; prefix_server: PChar; name: PChar; sem_handle: PLongInt): LongInt; stdcall;
Function pp_semtest (handle: LongInt): LongInt; stdcall;
Function pp_semused (handle: LongInt; semtype: LongInt; prefix_server: PChar; name: PChar; number: PLongInt): LongInt; stdcall;
Function pp_setvarchar (handle: LongInt; var_no: LongInt; buffer: PChar): LongInt; stdcall;
Function pp_setvardate (handle: LongInt; var_no: LongInt; month_hours: LongInt; day_minutes: LongInt;
                         year_seconds: LongInt): LongInt; stdcall;
Function pp_setvarnum (handle: LongInt; var_no: LongInt; value: LongInt): LongInt; stdcall;
Function pp_sysinfo(flags: LongInt): LongInt; stdcall;
Function pp_tcode (number: LongInt; cenum: LongInt; computer: LongInt; seed: LongInt): LongInt; stdcall;
Function pp_timercheck (timestamp: LongInt; minutes: LongInt): LongInt; stdcall;
Function pp_timerstart: LongInt; stdcall;
Function pp_transfer (handle: LongInt; filename: PChar; password: PChar; comp_num: LongInt): LongInt; stdcall;
Function pp_upddate (handle: LongInt; flag: LongInt): LongInt; stdcall;
Function pp_valdate (handle: LongInt): LongInt; stdcall;
Function pp_get4108mac: LongInt; stdcall;

{ Other Windows API functions used }
Const SW_SHOWNORMAL = 1;
Function ShellExecute (hwnd: LongInt; lpOperation: PChar; lpFile: PChar; lpParameters: PChar;
  lpDirectory: PChar; nShowCmd: LongInt): LongInt; stdcall;
Function GetWindowsDirectory (buffer: PChar; length: LongInt): LongInt; stdcall;
Function GetSystemDirectory (buffer: PChar; length: LongInt): LongInt; stdcall;

implementation

Procedure pp_adddays; external 'KeyLib32.dll';
Function pp_bitclear; external 'KeyLib32.dll';
Function pp_bitset; external 'KeyLib32.dll';
Function pp_bittest; external 'KeyLib32.dll';
// Only uncomment this in if you are directed to do so
// Function pp_cedate; external 'KeyLib32.dll';
Function pp_cenum; external 'KeyLib32.dll';
Function pp_checksum; external 'KeyLib32.dll';
Function pp_chkvarchar; external 'KeyLib32.dll';
Function pp_chkvardate; external 'KeyLib32.dll';
Function pp_chkvarnum; external 'KeyLib32.dll';
Function pp_compno32drivers; external 'KeyLib32.dll';
Function pp_compno; external 'KeyLib32.dll';
Function pp_convertv3; external 'KeyLib32.dll';
Function pp_copyadd; external 'KeyLib32.dll';
Function pp_copycheck; external 'KeyLib32.dll';
Function pp_copycheckth; external 'KeyLib32.dll';
Function pp_copydelete; external 'KeyLib32.dll';
Function pp_copyget; external 'KeyLib32.dll';
Function pp_countdec; external 'KeyLib32.dll';
Function pp_countinc; external 'KeyLib32.dll';
// Only uncomment this in if you are directed to do so
// Function pp_ctcodes; external 'KeyLib32.dll';}
Function pp_daysleft; external 'KeyLib32.dll';
Procedure pp_decrypt; external 'KeyLib32.dll';
Procedure pp_encrypt; external 'KeyLib32.dll';
Procedure pp_errorstr; external 'KeyLib32.dll';
Function pp_expired; external 'KeyLib32.dll';
Function pp_exportactfile; external 'KeyLib32.dll';
Function pp_eztrial1; external 'KeyLib32.dll';
Function pp_eztrial1test; external 'KeyLib32.dll';
Function pp_eztrial2; external 'KeyLib32.dll';
Function pp_eztrig1; external 'KeyLib32.dll';
Function pp_eztrig1dlg; external 'KeyLib32.dll';
Function pp_eztrig1ex; external 'KeyLib32.dll';
Function pp_filedelete; external 'KeyLib32.dll';
Function pp_getcode; external 'KeyLib32.dll';
Procedure pp_getdate; external 'KeyLib32.dll';
Procedure pp_gettime; external 'KeyLib32.dll';
Function pp_getvarchar; external 'KeyLib32.dll';
Function pp_getvardate; external 'KeyLib32.dll';
Function pp_getvarnum; external 'KeyLib32.dll';
Function pp_gotourl; external 'KeyLib32.dll';
Function pp_hdserial; external 'KeyLib32.dll';
Function pp_importactfile; external 'KeyLib32.dll';
Procedure pp_initlib; external 'KeyLib32.dll';
Procedure pp_lastday; external 'KeyLib32.dll';
Function pp_lanactive; external 'KeyLib32.dll';
Function pp_lancheck; external 'KeyLib32.dll';
Function pp_libtest; external 'KeyLib32.dll';
Procedure pp_libversion; external 'KeyLib32.dll';
Function pp_lfalias; external 'KeyLib32.dll';
Function pp_lfclose; external 'KeyLib32.dll';
Function pp_lfcopy; external 'KeyLib32.dll';
Function pp_lfcreate; external 'KeyLib32.dll';
Function pp_lfdelete; external 'KeyLib32.dll';
Function pp_lflock; external 'KeyLib32.dll';
Function pp_lfopen; external 'KeyLib32.dll';
Function pp_lfunlock; external 'KeyLib32.dll';
Function pp_ndecrypt; external 'KeyLib32.dll';
Function pp_ndecryptx; external 'KeyLib32.dll';
Function pp_nencrypt; external 'KeyLib32.dll';
Procedure pp_nencryptx; external 'KeyLib32.dll';
Function pp_netclose; external 'KeyLib32.dll';
Function pp_netopen; external 'KeyLib32.dll';
Function pp_nettest; external 'KeyLib32.dll';
Procedure pp_npdate; external 'KeyLib32.dll';
Function pp_password; external 'KeyLib32.dll';
Function pp_redir; external 'KeyLib32.dll';
Function pp_semclose; external 'KeyLib32.dll';
Function pp_semcount; external 'KeyLib32.dll';
Function pp_semopen; external 'KeyLib32.dll';
Function pp_semtest; external 'KeyLib32.dll';
Function pp_semused; external 'KeyLib32.dll';
Function pp_setvarchar; external 'KeyLib32.dll';
Function pp_setvardate; external 'KeyLib32.dll';
Function pp_setvarnum; external 'KeyLib32.dll';
Function pp_sysinfo; external 'KeyLib32.dll';
Function pp_tcode; external 'KeyLib32.dll';
Function pp_timercheck; external 'KeyLib32.dll';
Function pp_timerstart; external 'KeyLib32.dll';
Function pp_transfer; external 'KeyLib32.dll';
Function pp_upddate; external 'KeyLib32.dll';
Function pp_valdate; external 'KeyLib32.dll';
Function pp_get4108mac; external 'KeyLib32.dll';

{ Other Windows API functions used }
Function ShellExecute; external 'shell32.dll' name 'ShellExecuteA';
Function GetWindowsDirectory; external 'kernel32.dll' name 'GetWindowsDirectoryA';
Function GetSystemDirectory; external 'kernel' name 'GetSystemDirectoryA';

end.

