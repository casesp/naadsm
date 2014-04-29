inherited FormTracingTesting: TFormTracingTesting
  Left = 598
  Top = 210
  Caption = 'Scenario parameters: Diagnostic testing'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Caption = 'Diagnostic testing'
  end
  inherited pnlBody: TPanel
    inline fraParams: TFrameTracingTesting
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
      inherited pnlTestingOptions: TPanel
        Width = 579
      end
      inherited pnlTestCharacteristics: TPanel
        Width = 579
        Height = 210
      end
    end
  end
end
