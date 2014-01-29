unit Unlock;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  SysUtils, StdCtrls, ExtCtrls, ppp4, ppp_func;

type
  TUnlockScreen = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    code_entered: TEdit;
    CancelBtn: TBitBtn;
    OKBtn: TBitBtn;
    code_entry: TEdit;
    comp_no: TEdit;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UnlockScreen: TUnlockScreen;
  code_entry_var: LongInt;
  comp_no_var: LongInt;

implementation

uses
    UII2XMain;

{$R *.DFM}

procedure TUnlockScreen.CancelBtnClick(Sender: TObject);
begin
  close;
end;

procedure TUnlockScreen.FormShow(Sender: TObject);
var
   buffer: string;
begin
  //initialize the code entry number
  code_entry_var := pp_cenum;
  str( code_entry_var, buffer );
  code_entry.Text := buffer;

  //initialize the computer number from our global variable
  comp_no_var := compno;
  str( comp_no_var, buffer );
  comp_no.Text := buffer;

  //initialize the code entered field
  code_entered.text := '';
end;

procedure TUnlockScreen.OKBtnClick(Sender: TObject);
var
   result: Integer;
   code_entered_var: LongInt;
   bucket: Integer;
begin
  //see if a valid unlock code was entered
  //first, convert the number entered into a numeric variable
  val( code_entered.text, code_entered_var, bucket );

  result := pp_tcode(code_entered_var, code_entry_var, comp_no_var, 66699 );

  case result of
    1: begin                                                                         //This code should authorize this computer and turn on payments
      result := pp_copyadd(lfhandle, COPYADD_ERASEALL, compno);                       //1=success, 22=computer was already authorized - everything is okay
      result := pp_copyadd(lfhandle, COPYADD_ENHANCED, 0);
      If ( result = PP_SUCCESS ) Or ( result = ERR_SLOT_ALREADY_ASSIGNED ) Then Begin
        TurnOffPayments;
        UpdateDateTimeFields( False );                                                //It is a good idea to force the update of the last used date/time fields}
        MessageBox(0, 'Application authorization complete!', 'Successful!', 0);
      end else
        DisplayError('Authorization failed - Error #', result);
     end;

    2: begin                                                                         //This code should authorize this computer and turn off payments
      result := pp_copyadd(lfhandle, COPYADD_ERASEALL, compno);                       //1=success, 2=computer was already authorized - everything is okay
      if ( result = PP_SUCCESS ) Or ( result = ERR_SLOT_ALREADY_ASSIGNED ) Then       //Release from Payment Expiration
        TurnOffPayments
      Else
        DisplayError('Authorization failed - Error #', result);
    end;

    3: begin //This code should de-authorize this computer
      result := pp_copydelete(lfhandle, compno);
      if result <> PP_SUCCESS then
        DisplayError('Authorization failed - Error #', result)
      else
        MessageBox(0, 'Application de-authorization complete', 'Success!', 0);
    end;

    4: begin                                                                        //Set Demo Expiration to 20 days from today
      ConvertToDemo;
      UpdateDateTimeFields( False );                                                //It is a good idea to force the update of the last used date/time fields}
    end;


    5: begin                                                                        //Extend Payment Expiration for the next quarter - on the 15th of the month
      ExtendPayment;
      UpdateDateTimeFields( False );
    end;

    6: begin //Release from Payment Expiration
      TurnOffPayments;
    end;

    7: begin
          {*if the user turned the date forward by mistake, ran you program and
            then set the clock back to the correct time, they will not be allowed
            in because pp_valdate() will fail.  They will call you and you will give
            them code 6 to force the last used date/time fields to be set to the
            current (and correct) date and time. *}
      UpdateDateTimeFields( True );
    end;

     49: begin  // These codes could be used for something else
     end;

     Else
      MessageBox(0, 'Invalid Code Entered!', 'Invalid Code', MB_OK);
  end;

  // Unload the unlock form and re-evaluate the protection mode so the menu options can be updated
  Close;
  ProtectTest;
End;

END.
