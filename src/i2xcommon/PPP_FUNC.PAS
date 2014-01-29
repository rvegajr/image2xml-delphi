unit Ppp_func;

interface

uses
  SysUtils, WinTypes, WinProcs, Classes, ppp4;

const
  p = '{d3299e3b-2e38-4df8-ba66-4d5444814833}';
    
function ProtectTest : boolean;
Procedure DisplayError( Const msg: PChar; code: LongInt );
Procedure ConvertToDemo;
Procedure UpdateDateTimeFields(flag: Boolean);
Procedure TurnOffPayments;
Procedure ExtendPayment;

var
  compno: LongInt;
  demo_ver: Boolean;
  LicenseStatus : string;

implementation

uses
    UII2XMain;

function ProtectTest() : boolean;
var
   res: LongInt;                       {for temporary storage of our res code}
   month, day, year: LongInt;
   exptype: Array[0..1] of Char;
begin
  // Turn off all menu options by default
  frmI2XMain.SKMenuEnable( false );

  // Let's see if this computer it authorized
  //res := pp_copycheck(lfhandle, 0, compno);
  res := pp_copycheckth(lfhandle, COPYCHK_ENHANCED, 0, 20);
  result := false;

  Case res of
    PP_FAILURE:
      begin
      //License File is okay, but this computer is not authorized
      // see if this is in demo mode
        pp_getvarchar( lfhandle, VAR_EXPIRE_TYPE, exptype );
        If exptype[0] = 'D' Then Begin
        // turn on our demo indicator
          pp_getvardate( lfhandle, VAR_EXP_DATE_HARD, @month, @day, @year );
          if (( month = 12 ) and ( year=2050 )) then begin
            ConvertToDemo; // convert this copy to a 20 day demo
            result := ProtectTest;  // re-evaluate the protection
            Exit;
          end;

          LicenseStatus := 'Demo expires:  ' + IntToStr(month) + '/' + IntToSTr(day) + '/' + IntToStr(year);
          frmI2XMain.lblPaymentDate.Caption := LicenseStatus;
          frmI2XMain.lblPaymentDate.Visible := True;
          res := pp_expired(lfhandle);
          Case res of
            PP_FALSE: begin
             //So far, so good.  Let's make sure they didn't turn the clock back
              If pp_valdate(lfhandle) = PP_FALSE Then begin
                LicenseStatus := 'Your clock has been turned back.  Please correct and re-run application';
                MessageBox(0, PChar(LicenseStatus), 'Application Date Error', 0)
              end else begin
                frmI2XMain.SKMenuEnable( false );
                MessageBox(0, PChar(LicenseStatus), 'Image2XML is running in trial mode...', 0);
                demo_ver := True;
                result := true;
              end;
            end;
            PP_TRUE: begin
              LicenseStatus := 'Thank you for using Image2XML,  unfortunately this demo has expired.' + #13#10 + 'Please visit www.image2xml.com and purchase a new license.';
              MessageBox(0, PChar(LicenseStatus), 'Demo has expired', 0);
              frmI2XMain.lblPaymentDate.Caption := 'Demo has expired!';
            end;
        else // This function failed - let's see why
          DisplayError('Expiration test failed - Error #', res);
        end;
      end else begin // This is not a demo but the copy protection failed the test
        ConvertToDemo; // convert this copy to a 20 day demo
        ProtectTest;  // re-evaluate the protection
      end;
    end;
    PP_SUCCESS: begin
      // passed copy protection test - let's see if we are in Payment mode
      pp_getvarchar(lfhandle, VAR_EXPIRE_TYPE, exptype);
      If exptype[0] = 'P' Then Begin // turn on our payment indicator
        pp_getvardate(lfhandle, VAR_EXP_DATE_HARD, @month, @day, @year);
        LicenseStatus := 'Next payment due:  ' + IntToStr(month) + '/' + IntToSTr(day) + '/' + IntToStr(year);
        frmI2XMain.lblPaymentDate.Caption := LicenseStatus;
        frmI2XMain.lblPaymentDate.Visible := True;
        res := pp_expired(lfhandle);
        Case res of

          PP_FALSE: begin  // So far, so good.  Let's make sure they didn't turn the clock back
            If pp_valdate(lfhandle) = PP_FALSE Then begin
              LicenseStatus := 'Your clock has been turned back.  Please correct and re-run application';
              MessageBox(0, PChar(LicenseStatus), 'Application Date Error', 0)
            end else begin
              frmI2XMain. SKMenuEnable( true ); // Turn on all menu options since everything is okay
              result := true;
            End;
          end;

          PP_TRUE: begin // This computer Payment is past due
            LicenseStatus := 'Your quarterly payment is past due.  Please call for assistance.';
            MessageBox(0, PChar(LicenseStatus), 'Application Violation', 0 );
            frmI2XMain.lblPaymentDate.Caption := 'Next payment due:  *PAST DUE*';
          end;

          else
            DisplayError('Expiration test failed - Error #', res);
          end; //Case res of

        end else
          frmI2XMain.SKMenuEnable( true ); // Turn on all menu options since everything is okay
          frmI2XMain.lblPaymentDate.Caption := '';
          result := true;
      End;

    else //Case res
      DisplayError( 'Initialization failed - Error #', res ); // This function failed - let's see why
      frmI2XMain.Close; //We don't want to continue since this is a critical failure

  end; //END Case res of

End;

Procedure ConvertToDemo;
var
   result, month, day, year, dow: LongInt;
begin
     {Set Demo Expiration to 20 days from today}
     pp_getdate(@month, @day, @year, @dow);
     pp_adddays(@month, @day, @year, 30);
     pp_setvarchar(lfhandle, VAR_EXPIRE_TYPE, 'D');
     result := pp_setvardate(lfhandle, VAR_EXP_DATE_HARD, month, day, year);
     {1=success - everything is okay}
     If result = PP_SUCCESS Then
        //MessageBox(0, 'Demo set to 30 days from today', 'Success!', 0)
     Else
         DisplayError('Authorization failed - Error #', result);
end;

Procedure ExtendPayment;
var
   result, month, day, year, dow: LongInt;
begin
     {Extend Payment Expiration for the next quarter - on the 15th of the month}
    pp_getdate(@month, @day, @year, @dow);
    pp_adddays(@month, @day, @year, 60);
    {this will advance us to the next month and find the payment date}
    pp_npdate(@month, @day, @year, 15);
    pp_setvarchar(lfhandle, VAR_EXPIRE_TYPE, 'P');
    result := pp_setvardate(lfhandle, VAR_EXP_DATE_HARD, month, day, year);
    {1=success - everything is okay}
    If result = PP_SUCCESS Then
        MessageBox(0, 'Action complete', 'Success!', 0 )
    Else
        DisplayError( 'Authorization failed - Error #', result);
end;

Procedure TurnOffPayments;
var
   result: LongInt;
begin
    {Release from Payment Expiration}
    result := pp_setvarchar(lfhandle, VAR_EXPIRE_TYPE, 'N');
    {1=success - everything is okay}
    If result = PP_SUCCESS Then
        //MessageBox(0, 'Action complete', 'Success!', 0 )
    Else
        DisplayError( 'Authorization failed - Error #', result);
end;

Procedure UpdateDateTimeFields(flag: Boolean);
var
   result: LongInt;
begin
     {if the user turned the date forward by mistake, ran you program and}
     {then set the clock back to the correct time, they will not be allowed}
     {in because pp_valdate() will fail.  They will call you and you will give}
     {them code 6 to force the last used date/time fields to be set to the}
     {current (and correct) date and time.}
     result := pp_upddate(lfhandle, 1);
     If result <> PP_SUCCESS Then
        DisplayError( 'Update date failed - Error #', result)
     Else
        If flag = True Then MessageBox(0, 'Update successful', 'Success!', 0 );
end;

Procedure DisplayError( Const msg: PChar; code: LongInt );
var
   buffer: Array[0..75] of Char;
   buffer1: Array[0..10] of Char;
begin
    StrCopy( buffer, msg );
    StrPCopy( buffer1, IntToStr( code ) );
    StrCat( buffer, buffer1  );
    MessageBox(0, buffer, 'Application Licensing Error', MB_OK);
end;

initialization
  compno := pp_compno( COMPNO_ENHANCED, '', '' );

end.
