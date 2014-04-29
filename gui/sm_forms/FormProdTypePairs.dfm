inherited FormProdTypePairs: TFormProdTypePairs
  Left = -837
  Top = 90
  Caption = 'Scenario parameters: Production type combinations'
  ClientHeight = 539
  ClientWidth = 755
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D000
    00000D7800000222222220778000022222222030880002222222203007000222
    22222030178002222222203017780222222220B014080720000002B014082003
    BBBBBBB01408038B3333333114080333199999991C080018911111110C080011
    14CCCCCCCC700007C8444444448000084440000000000000000000000000000F
    0000000700000003000000030000000100000000000000000000000000000000
    00008000000080000000C0000000C0010000E0010000E1FF0000FFFF0000}
  OnCanResize = FormCanResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 496
    Width = 755
    TabOrder = 5
    inherited pnlWizardButtons: TPanel
      Left = 357
    end
  end
  object pnlSelectedPairs: TPanel [1]
    Left = 0
    Top = 41
    Width = 230
    Height = 455
    Hint = 'Press CTRL key to select more than one production type'
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 224
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      BorderStyle = bsSingle
      Caption = 'Production type combinations'
      TabOrder = 0
    end
    object lbxSelectedPairs: TListBox
      Left = 1
      Top = 25
      Width = 224
      Height = 425
      Hint = 
        'Press CTRL + left mouse button to select multiple production typ' +
        'es'
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'Cattle - beef'
        'Cattle - dairy'
        'Mixed - beef/dairy'
        'Swine - confinement'
        'Swine - outside')
      MultiSelect = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = lbxSelectedPairsClick
    end
  end
  object pnlCenter: TPanel [2]
    Left = 230
    Top = 41
    Width = 219
    Height = 455
    Align = alLeft
    TabOrder = 1
    object BitBtnAdd: TBitBtn
      Left = 37
      Top = 163
      Width = 164
      Height = 25
      Hint = 'Removes selected combinations from the model'
      Caption = 'Add'
      Enabled = False
      TabOrder = 0
      OnClick = BitBtnAddClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333333333333333333333333333333333FF333333333333
        3000333333FFFFF3F77733333000003000B033333777773777F733330BFBFB00
        E00033337FFF3377F7773333000FBFB0E000333377733337F7773330FBFBFBF0
        E00033F7FFFF3337F7773000000FBFB0E000377777733337F7770BFBFBFBFBF0
        E00073FFFFFFFF37F777300000000FB0E000377777777337F7773333330BFB00
        000033333373FF77777733333330003333333333333777333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
    end
    object BitBtnRemove: TBitBtn
      Left = 37
      Top = 131
      Width = 164
      Height = 25
      Hint = 'Includes selected combinations in the model'
      Caption = 'Remove'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = BitBtnRemoveClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333FFF333333333333000333333333
        3333777FFF3FFFFF33330B000300000333337F777F777773F333000E00BFBFB0
        3333777F773333F7F333000E0BFBF0003333777F7F3337773F33000E0FBFBFBF
        0333777F7F3333FF7FFF000E0BFBF0000003777F7F3337777773000E0FBFBFBF
        BFB0777F7F33FFFFFFF7000E0BF000000003777F7FF777777773000000BFB033
        33337777773FF733333333333300033333333333337773333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      Layout = blGlyphRight
      NumGlyphs = 2
    end
  end
  object pnlPairsAndProdTypes: TPanel [3]
    Left = 449
    Top = 41
    Width = 302
    Height = 455
    Align = alClient
    TabOrder = 2
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 300
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Choose from all combinations'
      TabOrder = 0
    end
    object lbxAllPairs: TListBox
      Left = 1
      Top = 25
      Width = 300
      Height = 192
      Align = alTop
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 1
      OnClick = lbxAllPairsClick
    end
    object pnlProdTypes: TPanel
      Left = 1
      Top = 217
      Width = 300
      Height = 237
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object lbxSource: TListBox
        Left = 0
        Top = 24
        Width = 136
        Height = 213
        Align = alLeft
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbxSourceClick
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 300
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Match production types below'
        TabOrder = 1
      end
      object lbxDest: TListBox
        Left = 136
        Top = 24
        Width = 164
        Height = 213
        Align = alClient
        ItemHeight = 13
        TabOrder = 2
        OnClick = lbxSourceClick
      end
    end
  end
  object pnlCaption: TPanel [4]
    Left = 0
    Top = 0
    Width = 755
    Height = 41
    Align = alTop
    Caption = 'Production type combinations'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  object Panel9: TPanel [5]
    Left = 751
    Top = 41
    Width = 4
    Height = 455
    Align = alRight
    TabOrder = 4
  end
end
