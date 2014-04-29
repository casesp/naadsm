inherited FormDisease: TFormDisease
  Left = 1157
  Top = 44
  Caption = 'Scenario Parameters: Disease'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Caption = 'Disease'
    TabOrder = 2
  end
  inherited pnlBody: TPanel
    TabOrder = 0
    object sbxAllParams: TScrollBox
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      BevelOuter = bvRaised
      TabOrder = 1
      inline fraParams: TFrameDisease
        Left = 0
        Top = 0
        Width = 559
        Height = 546
        Align = alTop
        TabOrder = 0
        inherited pnlUseDiseaseTransition: TPanel
          Width = 559
        end
        inherited pnlDiseaseParams: TPanel
          Width = 559
          inherited pnlDiseaseStates: TPanel
            Width = 559
          end
          inherited pnlPrevalence: TPanel
            Width = 559
          end
          inherited pnlMortality: TPanel
            Width = 559
          end
        end
      end
    end
  end
end
