inherited FormYesNo: TFormYesNo
  Left = 691
  Top = 16
  BorderIcons = [biMaximize]
  Caption = 'Scenario parameters'
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
    Caption = 'FORM CAPTION'
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
      Left = 104
      Top = 48
      Width = 481
      Height = 137
      Caption = 'grpButtons'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      object btnYes: TRadioButton
        Left = 8
        Top = 24
        Width = 457
        Height = 49
        Caption = 'btnYes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        WordWrap = True
        OnClick = valueChanged
      end
      object btnNo: TRadioButton
        Left = 8
        Top = 80
        Width = 457
        Height = 49
        Caption = 'btnNo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        TabStop = True
        WordWrap = True
        OnClick = valueChanged
      end
    end
  end
end
