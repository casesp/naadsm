inherited FormProdTypeBase: TFormProdTypeBase
  Left = 1026
  Top = 253
  Caption = 'Scenario parameters'
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
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    TabOrder = 3
    object btnApplyToAll: TButton
      Left = 8
      Top = 8
      Width = 137
      Height = 25
      Caption = '&Apply to all'
      TabOrder = 1
      OnClick = btnApplyToAllClick
    end
    object btnCopy: TButton
      Left = 145
      Top = 8
      Width = 137
      Height = 25
      Caption = 'C&opy...'
      TabOrder = 2
      OnClick = btnCopyClick
    end
  end
  object pnlProdTypes: TPanel [1]
    Left = 0
    Top = 41
    Width = 121
    Height = 477
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 1
    object pnlProdTypeCaption: TPanel
      Left = 1
      Top = 1
      Width = 115
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      BorderStyle = bsSingle
      Caption = 'Production types'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowFrame
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object lbxProdTypes: TListBox
      Left = 1
      Top = 25
      Width = 115
      Height = 447
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'Cattle - beef'
        'Cattle - dairy'
        'Mixed - beef/dairy'
        'Swine - confinement'
        'Swine - outside')
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = lbxProdTypesClick
    end
  end
  object pnlCaption: TPanel [2]
    Left = 0
    Top = 0
    Width = 702
    Height = 41
    Align = alTop
    Caption = 'CHANGE THIS IN DERIVED CLASSES'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object pnlBody: TPanel [3]
    Left = 121
    Top = 41
    Width = 581
    Height = 477
    Align = alClient
    TabOrder = 2
    object pnlHeader: TPanel
      Left = 1
      Top = 1
      Width = 579
      Height = 40
      Align = alTop
      Alignment = taLeftJustify
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object lblProdType: TLabel
        Left = 16
        Top = 8
        Width = 102
        Height = 24
        Caption = 'lblProdType'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
    end
  end
end
