inherited FormProdTypePairBase: TFormProdTypePairBase
  Left = 856
  Caption = 'Scenario parameters'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    object btnApplyToAll: TButton
      Left = 8
      Top = 8
      Width = 137
      Height = 25
      Caption = 'Apply to all'
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
  object pnlCaption: TPanel [1]
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
    TabOrder = 1
  end
  object pnlSelectedPairs: TPanel [2]
    Left = 0
    Top = 41
    Width = 201
    Height = 477
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 2
    object pnlSelectedPairsCaption: TPanel
      Left = 1
      Top = 1
      Width = 195
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
      Width = 195
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
      OnClick = lbxSelectedPairsClick
    end
  end
  object pnlBody: TPanel [3]
    Left = 201
    Top = 41
    Width = 501
    Height = 477
    Align = alClient
    TabOrder = 3
    object pnlHeader: TPanel
      Left = 1
      Top = 1
      Width = 499
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
      object lblProdTypePair: TLabel
        Left = 16
        Top = 8
        Width = 134
        Height = 24
        Caption = 'lblProdTypePair'
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
