object OCRProductViewDialog: TOCRProductViewDialog
  Left = 227
  Top = 108
  BorderStyle = bsSingle
  Caption = 'OCR Product View'
  ClientHeight = 298
  ClientWidth = 433
  Color = clBtnFace
  ParentFont = True
  Menu = mnuMain
  OldCreateOrder = True
  PopupMode = pmAuto
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object tv: TTreeView
    Left = 0
    Top = 0
    Width = 433
    Height = 298
    Align = alClient
    BorderWidth = 1
    Indent = 19
    TabOrder = 0
    OnMouseMove = tvMouseMove
    ExplicitHeight = 279
  end
  object mnuMain: TMainMenu
    Left = 344
    Top = 136
    object pmnClose: TMenuItem
      Caption = 'Close'
      OnClick = pmnCloseClick
    end
    object pmnuOptions: TMenuItem
      Caption = 'Options'
      Checked = True
      object pmnuAlwaysOnTop: TMenuItem
        Caption = 'Always On Top'
        Checked = True
        OnClick = pmnuAlwaysOnTopClick
      end
    end
    object mnuViewXML: TMenuItem
      Caption = 'View XML'
      OnClick = mnuViewXMLClick
    end
  end
end
