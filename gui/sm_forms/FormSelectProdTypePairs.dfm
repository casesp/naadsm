object FormSelectProdTypePairs: TFormSelectProdTypePairs
  Left = 525
  Top = 372
  Width = 405
  Height = 508
  Caption = 'Select production type combinations'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInstructions: TPanel
    Left = 0
    Top = 0
    Width = 397
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblInstructions: TLabel
      Left = 8
      Top = 8
      Width = 359
      Height = 26
      Caption = 
        'To copy the parameters for the current production type combinati' +
        'on to other combinations, select one or more from the list below' +
        ' and click '#39'Copy'#39':'
      WordWrap = True
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 432
    Width = 397
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCopy: TButton
      Left = 112
      Top = 16
      Width = 75
      Height = 25
      Caption = '&Copy'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnCopyClick
    end
    object btnCancel: TButton
      Left = 208
      Top = 16
      Width = 73
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlSelectedPairs: TPanel
    Left = 0
    Top = 57
    Width = 397
    Height = 375
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 2
    object pnlSelectedPairsCaption: TPanel
      Left = 1
      Top = 1
      Width = 391
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
      Width = 391
      Height = 345
      Hint = 'Press CTRL + left mouse button to select multiple combinations'
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
    end
  end
end
