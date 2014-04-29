inherited FormVaccination: TFormVaccination
  Left = 897
  Top = 48
  Caption = 'Scenario parameters: Vaccination'
  ClientHeight = 474
  ClientWidth = 674
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 431
    Width = 674
    inherited pnlWizardButtons: TPanel
      Left = 276
    end
  end
  inherited pnlProdTypes: TPanel
    Height = 390
    TabOrder = 2
    inherited lbxProdTypes: TListBox
      Height = 360
    end
  end
  inherited pnlCaption: TPanel
    Width = 674
    Caption = 'Vaccination'
  end
  inherited pnlBody: TPanel
    Width = 553
    Height = 390
    TabOrder = 1
    inherited pnlHeader: TPanel
      Width = 551
    end
    inline fraParams: TFrameVaccination
      Left = 1
      Top = 41
      Width = 551
      Height = 348
      Align = alClient
      TabOrder = 1
      Visible = False
      inherited pnlRingVacc: TPanel
        Width = 551
        Height = 131
      end
      inherited pnlVaccination: TPanel
        Width = 551
        inherited pnlUseVacc: TPanel
          Width = 549
        end
        inherited pnlVaccParams: TPanel
          Width = 549
        end
      end
    end
  end
end
