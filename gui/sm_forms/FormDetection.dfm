inherited FormDetection: TFormDetection
  Left = 728
  Top = 77
  Caption = 'Scenario parameters: Detection'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlProdTypes: TPanel
    TabOrder = 2
  end
  inherited pnlCaption: TPanel
    Caption = 'Detection'
  end
  inherited pnlBody: TPanel
    TabOrder = 1
    inline fraParams: TFrameDetection
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
      inherited pnlParams: TPanel
        Width = 579
        Height = 435
        inherited pnlUseDetection: TPanel
          Width = 577
          inherited cbxDetect: TCheckBox
            OnClick = cbxDetectClick
          end
        end
        inherited pnlDetectionParams: TPanel
          Width = 577
          Height = 400
        end
      end
    end
  end
end
