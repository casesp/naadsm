inherited FormZone: TFormZone
  Left = 695
  Top = 319
  Caption = 'Scenario parameters: Zone parameters'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Caption = 'Zone parameters'
  end
  inherited pnlBody: TPanel
    inline fraParams: TFrameZone
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
      inherited pnlZoneTrigger: TPanel
        Width = 563
        inherited cbxIndirectTraceTrigger: TCheckBox
          Width = 505
        end
        inherited cbxDirectTraceTrigger: TCheckBox
          Width = 505
        end
        inherited cbxDetectionTrigger: TCheckBox
          Width = 513
        end
      end
      inherited pnlZoneEffects: TPanel
        Width = 563
        inherited pnlZoneEffectsBody: TPanel
          Width = 561
        end
      end
    end
  end
end
