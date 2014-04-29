object FormMain: TFormMain
  Left = 586
  Top = 17
  Width = 429
  Height = 497
  Caption = 'NAADSMDiff'
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
  inline fraScenario1: TFrameFileSelector
    Left = 16
    Top = 16
    Width = 376
    Height = 47
    TabOrder = 0
    inherited lblFileName: TLabel
      Width = 99
      Caption = 'Scenario 1 file name:'
    end
  end
  inline fraScenario2: TFrameFileSelector
    Left = 16
    Top = 80
    Width = 376
    Height = 47
    TabOrder = 1
    inherited lblFileName: TLabel
      Width = 99
      Caption = 'Scenario 2 file name:'
    end
  end
  object btnCompareScenario: TButton
    Left = 24
    Top = 144
    Width = 105
    Height = 25
    Caption = 'Compare scenario'
    TabOrder = 2
    OnClick = btnCompareClick
  end
  object btnComparePopulation: TButton
    Left = 152
    Top = 144
    Width = 105
    Height = 25
    Caption = 'Compare population'
    TabOrder = 3
    OnClick = btnCompareClick
  end
  object btnCompareAll: TButton
    Left = 280
    Top = 144
    Width = 105
    Height = 25
    Caption = 'Compare all'
    TabOrder = 4
    OnClick = btnCompareClick
  end
end
