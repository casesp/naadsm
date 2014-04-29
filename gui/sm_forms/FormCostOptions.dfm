inherited FormCostOptions: TFormCostOptions
  Left = 526
  Top = 187
  BorderIcons = [biMaximize]
  Caption = 'Scenario parameters: Cost accounting options'
  ClientHeight = 527
  ClientWidth = 698
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
    Top = 484
    Width = 698
    TabOrder = 2
    inherited pnlWizardButtons: TPanel
      Left = 300
    end
  end
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 0
    Width = 698
    Height = 41
    Align = alTop
    Caption = 'Cost accounting options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object pnlBody: TPanel [2]
    Left = 0
    Top = 41
    Width = 698
    Height = 443
    Align = alClient
    TabOrder = 1
    object grpButtons: TGroupBox
      Left = 80
      Top = 40
      Width = 529
      Height = 105
      Caption = 'For which control measures do you want to track DIRECT COSTS? '
      TabOrder = 0
      TabStop = True
      object cbxDestrCosts: TCheckBox
        Left = 24
        Top = 48
        Width = 409
        Height = 17
        Caption = 'Track costs of destruction'
        TabOrder = 1
        OnClick = valueChanged
      end
      object cbxVaccCosts: TCheckBox
        Left = 24
        Top = 72
        Width = 409
        Height = 17
        Caption = 'Track costs of vaccination'
        TabOrder = 2
        OnClick = valueChanged
      end
      object cbxSurvCosts: TCheckBox
        Left = 24
        Top = 24
        Width = 409
        Height = 17
        Caption = 'Track costs of surveillance in zones'
        TabOrder = 0
        OnClick = valueChanged
      end
    end
  end
end
