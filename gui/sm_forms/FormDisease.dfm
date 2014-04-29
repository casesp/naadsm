inherited FormDisease: TFormDisease
  Left = 592
  Top = 347
  Caption = 'Scenario Parameters: Disease'
  ClientHeight = 473
  ClientWidth = 632
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 430
    Width = 632
    inherited pnlWizardButtons: TPanel
      Left = 234
    end
  end
  inherited pnlProdTypes: TPanel
    Height = 389
    inherited lbxProdTypes: TListBox
      Height = 359
    end
  end
  inherited pnlCaption: TPanel
    Width = 632
    Caption = 'Disease'
    TabOrder = 2
  end
  inherited pnlBody: TPanel
    Width = 511
    Height = 389
    TabOrder = 0
    inherited pnlHeader: TPanel
      Width = 509
    end
    inline fraParams: TFrameDisease
      Left = 1
      Top = 41
      Width = 509
      Height = 347
      HorzScrollBar.Visible = False
      Align = alClient
      TabOrder = 1
      inherited pnlParams: TPanel
        Width = 509
        Height = 347
        inherited pnlUseDiseaseTransition: TPanel
          Width = 507
          inherited cbxTransition: TCheckBox
            OnClick = cbxTransitionClick
          end
        end
        inherited pnlDiseaseParams: TPanel
          Width = 507
          Height = 305
        end
      end
    end
  end
end
