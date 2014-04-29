object FormRegionalSettings: TFormRegionalSettings
  Left = 1014
  Top = 249
  BorderStyle = bsDialog
  Caption = 'NAADSM regional settings'
  ClientHeight = 268
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlOK: TPanel
    Left = 0
    Top = 236
    Width = 325
    Height = 32
    Align = alBottom
    TabOrder = 0
    object btnOK: TButton
      Left = 152
      Top = 4
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnClick
    end
    object btnCancel: TButton
      Left = 240
      Top = 4
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnClick
    end
  end
  object pgcRegionalSettings: TPBPageControl
    Left = 0
    Top = 0
    Width = 325
    Height = 236
    ActivePage = tabCsvOptions
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object tabCsvOptions: TTabSheet
      Caption = ' CSV import/export'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      object lblDecimalSymbolLabel: TLabel
        Left = 24
        Top = 88
        Width = 79
        Height = 13
        Caption = 'Decimal symbol: '
      end
      object lblListSeparatorLabel: TLabel
        Left = 24
        Top = 120
        Width = 66
        Height = 13
        Caption = 'List separator:'
      end
      object lblDecimalSymbol: TLabel
        Left = 112
        Top = 88
        Width = 82
        Height = 13
        Caption = 'lblDecimalSymbol'
        Visible = False
      end
      object lblListSeparator: TLabel
        Left = 112
        Top = 120
        Width = 32
        Height = 13
        Caption = 'Label3'
        Visible = False
      end
      object rdoUseSystem: TRadioButton
        Left = 8
        Top = 16
        Width = 297
        Height = 17
        Caption = 'Use system characters for CSV import/export'
        TabOrder = 0
        OnClick = rdoClick
      end
      object rdoUseCustom: TRadioButton
        Left = 8
        Top = 40
        Width = 297
        Height = 33
        Caption = 'Use specified characters for CSV import/export in NAADSM'
        TabOrder = 1
        WordWrap = True
        OnClick = rdoClick
      end
      object leDecimalSymbol: TEdit
        Left = 136
        Top = 84
        Width = 25
        Height = 21
        MaxLength = 1
        TabOrder = 2
        Visible = False
      end
      object leListSeparator: TEdit
        Left = 136
        Top = 116
        Width = 25
        Height = 21
        MaxLength = 1
        TabOrder = 3
        Visible = False
      end
    end
  end
end
