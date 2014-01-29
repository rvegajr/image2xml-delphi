inherited I2XOptionsUI: TI2XOptionsUI
  Caption = 'Image2XML Options'
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlRight: TPanel
    object pnlPlugins: TPanel
      Left = 56
      Top = 52
      Width = 369
      Height = 137
      Caption = 'pnlPlugins'
      TabOrder = 1
      object grdPlugins: TStringGrid
        Left = 1
        Top = 1
        Width = 367
        Height = 135
        Align = alClient
        ColCount = 2
        DefaultRowHeight = 16
        FixedCols = 0
        TabOrder = 0
      end
    end
    object pnlTWAIN: TPanel
      Left = 77
      Top = 84
      Width = 369
      Height = 137
      Caption = 'pnlTWAIN'
      TabOrder = 2
      object lblDeviceList: TLabel
        Left = 1
        Top = 1
        Width = 367
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'The highlighted device below is the selected TWAIN Device:'
        ExplicitWidth = 286
      end
      object lstTWAINDevices: TListBox
        Left = 1
        Top = 14
        Width = 367
        Height = 122
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = lstTWAINDevicesClick
      end
    end
    object pnlApplication: TPanel
      Left = 96
      Top = 124
      Width = 377
      Height = 149
      TabOrder = 0
      object lblGridSize: TLabel
        Left = 53
        Top = 27
        Width = 41
        Height = 13
        Caption = 'Grid Size'
        FocusControl = txtGridSize
      end
      object lblRootPath: TLabel
        Left = 9
        Top = 51
        Width = 72
        Height = 13
        Caption = 'Job Root Path:'
      end
      object btnJobRootPathSelect: TSpeedButton
        Left = 353
        Top = 48
        Width = 18
        Height = 16
        Hint = 'Select a Root Path for Jobs'
        Glyph.Data = {
          76010000424D760100000000000036000000280000000A0000000A0000000100
          18000000000040010000120B0000120B00000000000000000000658CB2376A9D
          3F72A54679AC4C7FB24C7FB24679AC3F72A5376A9D658CB200005B88B598BEE6
          98BEE698BEE698BEE67FABD96598CC6598CC6598CC36699C00003B6EA13B6EA1
          3B6EA13B6EA15F8CB890B6DD9DC2E89DC2E89DC2E85F8CB800004174A77EAAD8
          7EAAD87EAAD8608FBF4174A74174A74174A74174A74174A70000487BAE8FB7E1
          8FB7E18FB7E18FB7E18FB7E18FB7E18FB7E18FB7E1487BAE00004F82B5A1C5EA
          A1C5EAA1C5EAA1C5EAA1C5EAA1C5EAA1C5EAA1C5EA4F82B500005689BCB2D3F3
          B2D3F3B2D3F3B2D3F3B2D3F3B2D3F3B2D3F3B2D3F35689BC00005C8FC2C1DDFA
          C1DDFAC1DDFAC1DDFAC1DDFAC1DDFAC1DDFAC1DDFA5C8FC2000089AFD696BCE4
          CCE5FFCCE5FF96BCE46194C86194C86194C86194C889AFD60000FFFFFF8CB2D9
          6598CC6598CC8CB2D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnJobRootPathSelectClick
      end
      object chkGridSnap: TCheckBox
        Left = 9
        Top = 3
        Width = 98
        Height = 15
        Caption = 'Toggle Grid Snap'
        Enabled = False
        TabOrder = 0
        OnClick = chkGridSnapClick
      end
      object txtGridSize: TEdit
        Left = 9
        Top = 24
        Width = 23
        Height = 21
        Enabled = False
        TabOrder = 1
        Text = '5'
        OnChange = txtGridSizeChange
      end
      object UpDownGridSize: TUpDown
        Left = 32
        Top = 24
        Width = 15
        Height = 21
        Associate = txtGridSize
        Max = 255
        Position = 5
        TabOrder = 2
      end
      object txtJobRootPath: TEdit
        Left = 83
        Top = 46
        Width = 264
        Height = 21
        TabOrder = 3
      end
    end
  end
  inherited ActionList1: TActionList
    Left = 248
    Top = 256
  end
  inherited ImageList1: TImageList
    Left = 264
    Top = 316
  end
end
