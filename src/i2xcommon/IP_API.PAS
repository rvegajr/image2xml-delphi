unit IP_API;
{**************************************************************************}
{
{    Delphi Win32 DLL Import Unit for SoftWareKey.com InstantPLUS API
{     C, cdecl calling conventions
{
{**************************************************************************}

interface
uses
  Wintypes, WinProcs;

{///**Function Declarations** }

function WR_LFGetString(flags: LongInt; 
                        var_no: LongInt; 
                        buffer: PChar): LongInt; cdecl ;
function WR_LFGetNum(flags: LongInt;
                     var_no: LongInt;
                     var value: LongInt): LongInt; cdecl ;
function WR_LFGetDate(flags: LongInt;
                      var_no: LongInt;
                      var month_hours: LongInt;
                      var day_minutes: LongInt;
                      var year_seconds: LongInt): LongInt; cdecl ;
function WR_LFSetString(flags: LongInt;
                        var_no: LongInt;
                        buffer: PChar): LongInt; cdecl ;
function WR_LFSetNum(flags: LongInt;
                     var_no: LongInt;
                     value: LongInt): LongInt; cdecl ;
function WR_LFSetDate(flags: LongInt;
                      var_no: LongInt;
                      month_hours: LongInt;
                      day_minutes: LongInt;
                      year_seconds: LongInt): LongInt; cdecl ;
function WR_Activate(flags: LongInt;
                     action: LongInt;
                     action_flags: LongInt;
                     hWnd: HWND): LongInt; cdecl ;
function WR_TrialRunsLeft(flags: LongInt;
                          var runsleft: LongInt): LongInt; cdecl ;
function WR_TrialDaysLeft(flags: LongInt;
                          var daysleft: LongInt): LongInt; cdecl ;
function WR_IsTrial(flags: LongInt): LongInt; cdecl ;
function WR_GetLFHandle(var lfhandle: LongInt): LongInt; cdecl ;
function WR_GetLicenseID(var licenseid: LongInt): LongInt; cdecl ;
function WR_GetPassword(password: PChar): LongInt; cdecl ;
function WR_ExpireType(flags: LongInt;
                       str1: PChar): LongInt; cdecl ;
function WR_DeactivateLicense(flags: LongInt): LongInt; cdecl ;
function WR_GetProductID(flags: LongInt;
                         var number: LongInt): LongInt; cdecl ;
function WR_GetProdOptionID(flags: LongInt;
                            var number: LongInt): LongInt; cdecl ;
function WR_GetPurchaseUrl(flags: LongInt;
                           str1: PChar): LongInt; cdecl ;
function WR_IsJustActivated(flags: LongInt): LongInt; cdecl ;
function WR_Close(flags: LongInt): LongInt; cdecl ;

implementation

function WR_LFGetString; external 'fake.dll' name 'n1';
function WR_LFGetNum; external 'fake.dll' name 'n2';
function WR_LFGetDate; external 'fake.dll' name 'n3';
function WR_LFSetString; external 'fake.dll' name 'n4';
function WR_LFSetNum; external 'fake.dll' name 'n5';
function WR_LFSetDate; external 'fake.dll' name 'n6';
function WR_Activate; external 'fake.dll' name 'n7';
function WR_TrialRunsLeft; external 'fake.dll' name 'n8';
function WR_TrialDaysLeft; external 'fake.dll' name 'n9';
function WR_IsTrial; external 'fake.dll' name 'n10';
function WR_GetLFHandle; external 'fake.dll' name 'n17';
function WR_GetLicenseID; external 'fake.dll' name 'n18';
function WR_GetPassword; external 'fake.dll' name 'n19';
function WR_ExpireType; external 'fake.dll' name 'r14';
function WR_DeactivateLicense; external 'fake.dll' name 'n25';
function WR_GetProductID; external 'fake.dll' name 'r4';
function WR_GetProdOptionID; external 'fake.dll' name 'r5';
function WR_GetPurchaseUrl; external 'fake.dll' name 'r15';
function WR_IsJustActivated; external 'fake.dll' name 'r2';
function WR_Close; external 'fake.dll' name 'r1';

end.
