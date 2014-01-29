object UITWAINDeviceSelect: TUITWAINDeviceSelect
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'TWAIN Device Select'
  ClientHeight = 168
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblInst: TLabel
    Left = 0
    Top = 0
    Width = 234
    Height = 13
    Align = alTop
    Alignment = taCenter
    Caption = 'Double Click Device Below to Select:'
    ExplicitWidth = 172
  end
  object lstTWAINDeviceSelect: TListBox
    Left = 0
    Top = 13
    Width = 234
    Height = 155
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnClick = lstTWAINDeviceSelectClick
    OnDblClick = lstTWAINDeviceSelectDblClick
  end
end
