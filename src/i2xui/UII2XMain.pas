unit UII2XMain;

interface

uses
  ppp4,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  UII2XBase,
  ExtCtrls,
  Menus,
  StdActns,
  ActnList,
  ImgList,
  ComCtrls,
  StdCtrls,
  Grids,
  Buttons,
  GR32_Image,
  GR32_RangeBars,
  uI2XOCR,
  uI2XConstants,
  mcmTWAINKernel,
  mcmTWAINIntf,
  mcmTWAIN,
  AppEvnts,
  uImage2XML,
  ppp_func,
  unlock,
  SKCA32,
  UII2XAbout;

type
  TI2XMainUI = class(TI2XBaseUI)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuHelpActivateClick(Sender: TObject);
    procedure mnuHelpActivationActivateClick(Sender: TObject);
    procedure mnuHelpActivationLicenseClick(Sender: TObject);
  private
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure UnlockApp1Click(Sender: TObject);
    procedure UnlockOnline1Click(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmI2XMain: TI2XMainUI;
  PPPLicFn : PChar;

implementation
uses ONLINE;

{$R *.dfm}

procedure TI2XMainUI.FormCreate(Sender: TObject);
begin
  inherited;
//
end;

procedure TI2XMainUI.FormDestroy(Sender: TObject);
begin
  inherited;
//
end;
{*

procedure TI2XMainUI.FormShow(Sender: TObject);
var
   result, lfresult: LongInt;
   daysleft, runsleft : LongInt;
   smsg : string;
begin
//  if ( not ProtectTest() ) then
//    self.Close();

   PPPLicFn:= PChar(ExtractFilePath(Application.ExeName)+ 'i2x.lf' );

   // see if our library responds the way we expect
   if pp_libtest(1518238629) <> 4178757 then
   begin
      MessageBox(0, 'Invalid Library', 'Error', 0 );
      Halt;
   end;

   //Default our options to off.  A retail product will have both the Tools and
   //Help menu enabled.  A demo will have the Help but not the tools menu enabled.
   //You can enable or disable any of your application features here.  We are
   //choosing these just as an example.
   self.SKMenuEnable( false );
   lblPaymentDate.Visible:= False;

   //Now run pp_eztrial1() and see what mode we are running in
   result:= pp_eztrial1(PPPLicFn, p, @lfresult, @lfhandle);

   //Set the mode of the application depending on our result from pp_eztrial1()
   case result of
   0: //We had an error - display a message
      MessageBox(0,PChar('File Error #'+format('%4d',[lfresult])+
         ' occurred - please contact Technical Support'), 'Error',MB_OK);
   1: //This is a retail product, turn on menu option
      begin
        self.SKMenuEnable( true );
        lblPaymentDate.Caption:= 'License Valid!';
        lblPaymentDate.Visible:= false;
      end;
   2: //This is a demo that hasn't expired display a nag message,
      //turn off one menu, and enable the demo days left indicator
      begin
        Help1.Enabled:= true; //Menu option
        result:= pp_daysleft(lfhandle, @daysleft);
        result:= pp_getvarnum(lfhandle, VAR_EXP_LIMIT, @runsleft);

        if ( daysleft = 1 ) then
          smsg := 'This is your last day to use Image2XML.' + #10#13 +
            'If you found the software useful,  please purchase a license by clicking on the top menu Help->Buy Image2XML'
        else
          smsg := 'You have ' + IntToStr( daysleft ) + ' days left in your trial period.' + #10#13 +
            'To purchase a license, click on the top menu Help->Buy Image2XML';

        MessageBox(0,PChar(smsg),'Thank you for using Image2XML!',MB_OK);

         lblPaymentDate.Caption:= 'Days Left = '+Format('%4d',[daysleft]);
         lblPaymentDate.Visible:= true;
         if runsleft >= 0 then
         begin
            lblPaymentDate.Caption:= 'Runs left = '+Format('%4d',[runsleft]);
            lblPaymentDate.Visible:= true;
         end;
      end;
   3: begin //Retail application failed software or hardware binding
      MessageBox(0,'Error 3 - please contact Technical Support','Error',MB_OK);
      self.Close;
   end;
   4: begin //Demo that has expired
      MessageBox(0,'This demo has expired.' + #10#13 + 'Please purchase a valid license online at http://www.image2xml.com.',
         'Demo has expired!',MB_OK);
      self.Close;
   end;
   5: begin //Clock has been turned back on demo
      MessageBox(0,'Your clock has been turned back.  Please correct and re-run the application.',
         'Error',MB_OK);
      self.Close;
   end;
   6: //Valid application in Periodic Mode
      begin
        self.SKMenuEnable( true );
      end;
   7: //Too many network user
      begin
          MessageBox(0,'There are too many network users!','Error',MB_OK);
          self.Close;
      end;
   else
      MessageBox(0,PChar('Unknown error #'+format('%4d',[lfresult])+
         ' Please contact Technical Support'), 'Error',MB_OK);
   end; //Case
end;
   *}

procedure TI2XMainUI.mnuHelpActivateClick(Sender: TObject);
var
  result, lfresult : integer;
begin
  inherited;
{*
  PPPLicFn:= PChar(ExtractFilePath(Application.ExeName)+ 'i2x.lf' );
  result := pp_eztrial2(self.Handle, PPPLicFn, p, 0, 0, 0);
  if ( result = 0 ) then begin
    MessageBox(0,'Action Completed successfully!','Activation Action Completed',MB_OK);
  end else begin
      MessageBox(0,PChar('File Error #'+format('%4d',[result])+
            'occurred - please contact Technical Support'),'License File Error',MB_OK);
  end;
  exit;

  // now run eztrig to possibly change the mode
  result := pp_eztrig1(self.Handle, PPPLicFn, p, @lfresult);
  if ( result = 0 ) then begin
    if ( lfresult <> 0 ) then begin
      MessageBox(0,PChar('File Error #'+format('%4d',[lfresult])+
            'occurred - please contact Technical Support'),'License File Error',MB_OK);
    end else if ( result = ERR_INVALID_CODE_ENTERED ) then begin
      MessageBox(0,PChar('File Error #'+format('%4d',[lfresult])+
            'Could not validate Code'),'License File Error',MB_OK);
    end else
      MessageBox(0,'Action Completed successfully!','Activation Action Completed',MB_OK);
  end;
  *}
end;

procedure TI2XMainUI.mnuHelpActivationActivateClick(Sender: TObject);
begin
  inherited;
  frmOnline.Left := (self.Width - frmOnline.Width) div 2;
  frmOnline.Top := (self.Height - frmOnline.Height) div 2;
  frmOnline.ShowModal;
end;

procedure TI2XMainUI.mnuHelpActivationLicenseClick(Sender: TObject);
begin
  inherited;
  UnlockScreen.Left := (self.Width - UnlockScreen.Width) div 2;
  UnlockScreen.Top := (self.Height - UnlockScreen.Height) div 2;
  UnlockScreen.ShowModal;
end;

procedure TI2XMainUI.FormClose(Sender: TObject);
begin
   // Update the last date/time used fields
   pp_upddate(lfhandle, 0);

   // Always close the handle to the memory resources are freed
   pp_lfclose(lfhandle);
end;

procedure TI2XMainUI.Exit1Click(Sender: TObject);
begin
   Close; //Halt
end;

procedure TI2XMainUI.About1Click(Sender: TObject);
begin
end;

procedure TI2XMainUI.Options1Click(Sender: TObject);
begin
   MessageBox(0,'The Tools/Options menu option was chosen'
                   ,'Tools/Options', MB_OK);
end;

procedure TI2XMainUI.UnlockApp1Click(Sender: TObject);
var
   result, lfresult : LongInt;
begin
end; {Unlock}

procedure TI2XMainUI.UnlockOnline1Click(Sender: TObject);
var
   result, errorcode : LongInt;
begin
end;

Procedure TI2XMainUI.FormShow(Sender: TObject);
var
   res: LongInt;
begin
   PPPLicFn:= PChar(AppPath + 'i2x.lf');

   // see if our library responds the way we expect
   if pp_libtest(1518238629) <> 4178757 then
   begin
      MessageBox(0, 'Invalid Library', 'Error', 0 );
      Close;
   end;

   //Attempt to open the license file
   res := pp_lfopen(PPPLicFn, 0, LF_FILE, p, @lfhandle);
   if res <> PP_SUCCESS then
   begin
      //This function failed - let's see why
      DisplayError('Application Violation - Error #', res);

       //We don't want to continue since this is a critical failure
      Close;
   end;

   // Call the protection module
   if ( not ProtectTest() ) then begin
     Close;
   end;

End;

END.
