inherited FormCostsDestr: TFormCostsDestr
  Left = 735
  Top = 69
  Caption = 'Scenario parameters: Costs of destruction'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Caption = 'Costs of destruction'
  end
  inherited pnlBody: TPanel
    inline fraParams: TFrameCostsDestr
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
      inherited pnlCostParams: TPanel
        Width = 385
        inherited lblDollars2: TLabel
          Left = 288
        end
        inherited lblDollars1: TLabel
          Left = 288
        end
        inherited lblDollars3: TLabel
          Left = 288
        end
        inherited lblDollars4: TLabel
          Left = 288
        end
        inherited lblDollars5: TLabel
          Left = 288
        end
        inherited rleDestrAppraisalPerUnit: TREEdit
          Left = 296
        end
        inherited rleDestrCleaningPerUnit: TREEdit
          Left = 296
        end
        inherited rleDestrIndemnificationPerAnimal: TREEdit
          Left = 296
        end
        inherited rleDestrEuthanasiaPerAnimal: TREEdit
          Left = 296
        end
        inherited rleDestrDisposalPerAnimal: TREEdit
          Left = 296
        end
      end
    end
  end
end
