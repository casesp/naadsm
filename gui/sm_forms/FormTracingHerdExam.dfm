inherited FormTracingHerdExam: TFormTracingHerdExam
  Left = -789
  Top = 270
  Caption = 'Scenario parameters: Unit examination for clinical signs'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Caption = 'Unit examination for clinical signs'
  end
  inherited pnlBody: TPanel
    inline fraParams: TFrameTracingHerdExam
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
    end
  end
end
