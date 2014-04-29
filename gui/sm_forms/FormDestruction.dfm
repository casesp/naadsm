inherited FormDestruction: TFormDestruction
  Top = 18
  Caption = 'Scenario parameters: Destruction'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlProdTypes: TPanel
    TabOrder = 0
  end
  inherited pnlCaption: TPanel
    Caption = 'Destruction'
    TabOrder = 2
  end
  inherited pnlBody: TPanel
    TabOrder = 1
    inline fraParams: TFrameDestruction
      Left = 1
      Top = 41
      Width = 579
      Height = 435
      Align = alClient
      TabOrder = 1
    end
  end
end
