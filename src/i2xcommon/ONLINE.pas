unit ONLINE;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ppp4, ppp_func, SKCA32;

type
  TfrmOnline = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    lblDescCENum: TLabel;
    txtLicID: TEdit;
    txtPassword: TEdit;
    lblCompNoDesc: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOnline: TfrmOnline;

implementation

{$R *.DFM}

procedure TfrmOnline.OKBtnClick(Sender: TObject);
var
   result: Integer;
   tcvalue: LongInt;
   RegKey1: LongInt;
   RegKey2: LongInt;
   bucket: Integer;
   cenum: LongInt;
   LicID: LongInt;
   LicPassword: String;
   LicenseUpdate: String;
    errorbuffer : String[50];
begin
    cenum:=pp_cenum();
    compno:=pp_compno( COMPNO_ENHANCED, '', '' );

    val( txtLicID.Text, LicID, bucket );
    LicPassword:=txtPassword.Text;

    {Check to make sure a License ID and/or Password was entered}
    {If one of them was not entered, exit this procedure}
    if LicID = 0 then {if no License ID was entered}
    begin
      MessageBox(0, 'You must enter a License ID to unlock your software!', 'Sample', 0);
      exit;
    end{end if}
    else if LicPassword = '' then {if no password was entered}
    begin
      MessageBox(0, 'You must enter a Password to unlock your software!', 'Sample', 0);
      exit;
    end;{end else if}

    {get the unlock code from the server over the internet}
    //result := SK_GetTCData('secure.softwarekey.com','','/solo/unlock/getcode.asp',LicID,PChar(LicPassword),cenum,compno,@RegKey1,@RegKey2,@LicenseUpdate);
    result := SK_GetTCData('swk','','/solo/unlock/getcode.asp',
      LicID,
      PChar(LicPassword),
      cenum,compno,
      @RegKey1,@RegKey2,@LicenseUpdate);

    If result = 100 Then begin
        DisplayError('LicenseID and/or Password invalid!', result);
    end else if (( result < 100 ) and ( result <> 0 )) Then begin
        SK_GetErrorStr( result, @errorbuffer );
        DisplayError(@errorbuffer[0], result);
    end else begin
        if result = 0 then begin
          tcvalue := pp_tcode(RegKey1, cenum, compno, 84395 );
          //tcvalue := pp_tcode(RegKey1, cenum, compno, 10385 );
          case tcvalue of
            1: begin
              result := pp_copyadd(lfhandle, COPYADD_ERASEALL, compno);

              If ( result = PP_SUCCESS ) then begin
                result := pp_copyadd(lfhandle, COPYADD_ENHANCED, 0);
                TurnOffPayments;
                UpdateDateTimeFields( False );                                                //It is a good idea to force the update of the last used date/time fields}
                MessageBox(self.Handle, 'Image2XML has been activated!  Thank you!', 'Success!', 0);
                //UpdateDateTimeFields( False );
              end else begin
                DisplayError('Authorization failed - Error #', result);
              end;
            end;
            2: begin
              {This code should authorize this computer and turn off payments}
              result := pp_copyadd(lfhandle, COPYADD_ERASEALL, compno);
                {1=success, 2=computer was already authorized - everything is okay}
              If ( result = PP_SUCCESS ) Or ( result = ERR_SLOT_ALREADY_ASSIGNED ) Then
                { Release from Payment Expiration }
                TurnOffPayments
              Else
                DisplayError('Authorization failed - Error #', result);
            end;

            3: begin
              {This code should de-authorize this computer}
              result := pp_copydelete(lfhandle, compno);
              If result <> PP_SUCCESS Then
                DisplayError('Authorization failed - Error #', result)
              Else
                MessageBox(0, 'Application de-authorization complete', 'Success!', 0);
            end;

            4: begin
              {Set Demo Expiration to 20 days from today}
              ConvertToDemo;

                {It is a good idea to force the update of the last used date/time fields}
              UpdateDateTimeFields( False );
            end;

            5: begin
              {Extend Payment Expiration for the next quarter - on the 15th of the month}
              ExtendPayment;

              {It is a good idea to force the update of the last used date/time fields}
              UpdateDateTimeFields( False );
            end;

            6: begin
              {Release from Payment Expiration}
              TurnOffPayments;
            end;

            7: begin
              {if the user turned the date forward by mistake, ran you program and}
              {then set the clock back to the correct time, they will not be allowed}
              {in because pp_valdate() will fail.  They will call you and you will give}
              {them code 6 to force the last used date/time fields to be set to the}
              {current (and correct) date and time.}
              UpdateDateTimeFields( True );
            end;

            49: begin
                { These codes could be used for something else }
            end;
        Else
        { Invalid code was entered }
          MessageBox(0, 'Image2XML could not validate your license.  Please contact us at support@noctusoft.com for assitance.', 'Activation Unsuccessful', MB_OK);
    end;
  end else
    DisplayError('Authorization failed - Error #', result);
    Close; //Unload the unlock form and re-evaluate the protection mode so the menu options can be updated }
    ProtectTest;
  end;
end;

procedure TfrmOnline.CancelBtnClick(Sender: TObject);
Begin
  close;
End;

END.
