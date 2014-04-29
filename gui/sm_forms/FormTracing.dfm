inherited FormTracing: TFormTracing
  Left = 71
  Top = 449
  Caption = 'Scenario parameters: Tracing'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlProdTypes: TPanel
    TabOrder = 0
  end
  inherited pnlCaption: TPanel
    Caption = 'Tracing'
    TabOrder = 2
  end
  inherited pnlBody: TPanel
    TabOrder = 1
    inline fraParams: TFrameTracing
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
      inherited pnlTraceDirectParams: TPanel
        Width = 579
      end
      inherited pnlTraceIndirectParams: TPanel
        Width = 579
      end
      inherited pnlTracingDelayParams: TPanel
        Width = 579
        Height = 138
      end
      inherited pnlTraceTypes: TPanel
        Width = 579
      end
    end
  end
end
