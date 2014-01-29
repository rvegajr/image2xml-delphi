inherited I2XMainUI: TI2XMainUI
  Caption = 'frmI2XMain'
  ClientWidth = 832
  ExplicitWidth = 840
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlActiveImage: TPanel
    Width = 832
    ExplicitWidth = 832
    inherited lblZoom: TLabel
      Left = 731
      ExplicitLeft = 731
    end
    inherited txtActiveImage: TEdit
      Width = 588
      ExplicitWidth = 588
    end
    inherited bbtnOpenImage: TBitBtn
      Left = 661
      ExplicitLeft = 661
    end
    inherited gbZoom: TGaugeBar
      Left = 761
      ExplicitLeft = 761
    end
    inherited bbtnRefresh: TBitBtn
      Left = 684
      ExplicitLeft = 684
    end
    inherited bbtnImageProcAndOCR: TBitBtn
      Left = 707
      ExplicitLeft = 707
    end
  end
  inherited pnlImageView: TPanel
    Width = 617
    ExplicitWidth = 617
    inherited ImgView: TImgView32
      Width = 615
      ExplicitWidth = 615
    end
  end
  inherited pgctlTemplateData: TPageControl
    Left = 617
    ExplicitLeft = 617
    inherited tabInfo: TTabSheet
      inherited pnlTemplateInfo: TPanel
        DesignSize = (
          187
          752)
        inherited Button1: TButton
          Left = 82
          Top = 348
          ExplicitLeft = 82
          ExplicitTop = 348
        end
      end
    end
    inherited tabJob: TTabSheet
      DesignSize = (
        187
        752)
    end
  end
  inherited bStatus: TStatusBar
    Width = 832
    ExplicitWidth = 832
  end
  inherited MainMenu1: TMainMenu
    inherited help1: TMenuItem
      inherited mnuHelpActivation: TMenuItem
        inherited mnuHelpActivationActivate: TMenuItem
          OnClick = mnuHelpActivationActivateClick
        end
        inherited mnuHelpActivationLicense: TMenuItem
          OnClick = mnuHelpActivationLicenseClick
        end
      end
    end
  end
end
