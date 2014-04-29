inherited FormDestrGlobal: TFormDestrGlobal
  Left = 374
  Top = 270
  Caption = 'Scenario parameters: Global destruction options'
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 0
    Width = 702
    Height = 41
    Align = alTop
    Caption = 'Global destruction options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  inline fraParams: TFrameDestrGlobal [2]
    Left = 0
    Top = 41
    Width = 702
    Height = 477
    Align = alClient
    TabOrder = 2
    inherited pnlDestrGlobal: TPanel
      Width = 702
      Height = 477
      inherited pnlUseDestrGlobal: TPanel
        Width = 700
      end
      inherited pnlDestrParams: TPanel
        Width = 700
        Height = 411
      end
    end
  end
end
