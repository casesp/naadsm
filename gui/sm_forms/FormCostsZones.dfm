inherited FormCostsZones: TFormCostsZones
  Left = 552
  Top = 176
  Caption = 'Scenario parameters: Costs of zone surveillance'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Caption = 'Costs of zone surveillance'
  end
  inherited pnlBody: TPanel
    inline fraParams: TFrameCostsZones
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
      inherited pnlZoneEffects: TPanel
        Width = 563
        inherited pnlZoneEffectsBody: TPanel
          Width = 561
        end
      end
    end
  end
end
