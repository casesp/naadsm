inherited FormCostsVacc: TFormCostsVacc
  Left = 374
  Top = 360
  Caption = 'Scenario parameters: Costs of vaccination'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Caption = 'Costs of vaccination'
  end
  inherited pnlBody: TPanel
    inline fraParams: TFrameCostsVacc
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
    end
  end
end
