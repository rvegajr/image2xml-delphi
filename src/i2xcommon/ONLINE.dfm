object frmOnline: TfrmOnline
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Unlock Online'
  ClientHeight = 98
  ClientWidth = 206
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblDescCENum: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'License ID'
  end
  object lblCompNoDesc: TLabel
    Left = 8
    Top = 32
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object OKBtn: TBitBtn
    Left = 19
    Top = 64
    Width = 81
    Height = 25
    TabOrder = 0
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 107
    Top = 64
    Width = 81
    Height = 25
    TabOrder = 1
    OnClick = CancelBtnClick
    Kind = bkCancel
  end
  object txtLicID: TEdit
    Left = 96
    Top = 8
    Width = 105
    Height = 21
    TabOrder = 2
  end
  object txtPassword: TEdit
    Left = 96
    Top = 32
    Width = 105
    Height = 21
    TabOrder = 3
  end
end
