inherited FormLocalAreaSpread: TFormLocalAreaSpread
  Left = 732
  Top = 354
  Caption = 'Scenario parameters: Local area spread'
  ClientHeight = 642
  ClientWidth = 784
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 599
    Width = 784
    inherited pnlWizardButtons: TPanel
      Left = 386
    end
  end
  inherited pnlCaption: TPanel
    Width = 784
    Caption = 'Local area spread'
  end
  inherited pnlSelectedPairs: TPanel
    Height = 558
    inherited lbxSelectedPairs: TListBox
      Height = 528
    end
  end
  inherited pnlBody: TPanel
    Width = 583
    Height = 558
    inherited pnlHeader: TPanel
      Width = 581
    end
    inline fraParams: TFrameAirborneOrLocalAreaSpread
      Left = 1
      Top = 41
      Width = 581
      Height = 516
      Align = alClient
      TabOrder = 1
      Visible = False
      inherited pnlParams: TPanel
        Width = 581
        Height = 488
        inherited lblDistBetwUnits: TLabel
          Width = 197
          Height = 13
        end
      end
      inherited pnlUseAirborneSpread: TPanel
        Width = 581
      end
    end
  end
end
