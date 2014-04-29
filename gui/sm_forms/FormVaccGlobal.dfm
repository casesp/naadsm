inherited FormVaccGlobal: TFormVaccGlobal
  Left = 491
  Top = 9
  Caption = 'Scenario parameters: Global vaccination options'
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 0
    Width = 702
    Height = 41
    Align = alTop
    Caption = 'Global vaccination options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object pnlBody: TPanel [2]
    Left = 0
    Top = 41
    Width = 702
    Height = 477
    Align = alClient
    TabOrder = 2
    inline fraParams: TFrameVaccGlobal
      Left = 1
      Top = 1
      Width = 700
      Height = 475
      Align = alClient
      TabOrder = 0
      inherited pnlVaccGlobal: TPanel
        Width = 700
        Height = 475
        inherited pnlUseVaccGlobal: TPanel
          Width = 698
        end
        inherited pnlVaccParams: TPanel
          Width = 698
          Height = 401
        end
      end
    end
  end
end
