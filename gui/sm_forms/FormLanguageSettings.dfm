object FormLanguageSettings: TFormLanguageSettings
  Left = 813
  Top = 47
  BorderStyle = bsDialog
  Caption = 'NAADSM Language settings'
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
  object pnlLanguageRdo: TPanel
    Left = 0
    Top = 64
    Width = 325
    Height = 121
    Align = alTop
    TabOrder = 0
    object rdoUseSelected: TRadioButton
      Left = 8
      Top = 34
      Width = 305
      Height = 41
      Caption = 'Always use selected language'
      TabOrder = 1
      WordWrap = True
      OnClick = rdoClick
    end
    object rdoUseSystemDefault: TRadioButton
      Left = 8
      Top = 72
      Width = 305
      Height = 41
      Caption = 'Always use this computer'#39's default language'
      TabOrder = 2
      WordWrap = True
      OnClick = rdoClick
    end
    object rdoAlwaysPrompt: TRadioButton
      Left = 8
      Top = 2
      Width = 305
      Height = 41
      Caption = 'Always prompt user for a language selection'
      Checked = True
      TabOrder = 0
      TabStop = True
      WordWrap = True
      OnClick = rdoClick
    end
  end
  object pnlLanguageSelect: TPanel
    Left = 0
    Top = 0
    Width = 325
    Height = 64
    Align = alTop
    TabOrder = 1
    object lblLanguageSelect: TLabel
      Left = 8
      Top = 7
      Width = 122
      Height = 13
      Caption = 'Please select a language:'
      WordWrap = True
    end
    object cboLanguage: TComboBox
      Left = 88
      Top = 36
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'English'
      Items.Strings = (
        'English'
        'Espa'#241'ol')
    end
  end
  object pnlLanguageCbx: TPanel
    Left = 0
    Top = 185
    Width = 325
    Height = 40
    Align = alTop
    TabOrder = 2
    object cbxUseSelected: TCheckBox
      Left = 8
      Top = 2
      Width = 305
      Height = 33
      Caption = 'Always use selected language'
      TabOrder = 0
      WordWrap = True
    end
  end
  object pnlOK: TPanel
    Left = 0
    Top = 225
    Width = 325
    Height = 43
    Align = alClient
    TabOrder = 3
    object btnOK: TButton
      Left = 152
      Top = 8
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
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnClick
    end
  end
end
