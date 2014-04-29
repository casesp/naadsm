inherited FormContactSpread: TFormContactSpread
  Left = 704
  Top = 318
  Caption = 'Scenario parameters: Contact spread'
  ClientWidth = 752
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Width = 752
    inherited pnlWizardButtons: TPanel
      Left = 354
    end
  end
  inherited pnlCaption: TPanel
    Width = 752
    Caption = 'Contact spread'
  end
  inherited pnlBody: TPanel
    Width = 551
    inherited pnlHeader: TPanel
      Width = 549
    end
    object sbxAllParams: TScrollBox
      Left = 1
      Top = 41
      Width = 549
      Height = 435
      Align = alClient
      TabOrder = 1
      inline fraParams: TFrameContactSpreadMaster
        Left = 0
        Top = 0
        Width = 545
        Height = 431
        HorzScrollBar.Visible = False
        Align = alClient
        TabOrder = 0
        inherited pnlDirectContact: TPanel
          Width = 529
          inherited pnlDirect: TPanel
            Width = 527
            inherited pnlDirectParams: TPanel
              Width = 527
              inherited fraDirect: TFrameContactSpread
                Width = 511
                VertScrollBar.Visible = False
                inherited pnlSimpleParams: TPanel
                  Width = 511
                  inherited pnlSimpleParamsTop: TPanel
                    Width = 509
                  end
                  inherited pnlSimpleParamsBottom: TPanel
                    Width = 509
                  end
                end
                inherited pnlCharts: TPanel
                  Width = 511
                end
              end
            end
          end
          inherited pnlUseDirectContact: TPanel
            Width = 527
            inherited cbxIncludeDirect: TCheckBox
              OnClick = fraParamscbxIncludeDirectClick
            end
          end
        end
        inherited pnlIndirectContact: TPanel
          Width = 529
          Height = 0
          inherited pnlUseIndirectContact: TPanel
            Width = 527
            inherited cbxIncludeIndirect: TCheckBox
              OnClick = fraParamscbxIncludeIndirectClick
            end
          end
          inherited pnlIndirect: TPanel
            Width = 527
            Height = 21
            inherited pnlIndirectParams: TPanel
              Width = 527
              Height = 21
              inherited pnlSpacerIndirect: TPanel
                Height = 21
              end
              inherited fraIndirect: TFrameContactSpread
                Width = 511
                Height = 21
                VertScrollBar.Visible = False
                inherited pnlSimpleParams: TPanel
                  Width = 511
                  inherited pnlSimpleParamsTop: TPanel
                    Width = 509
                  end
                  inherited pnlSimpleParamsBottom: TPanel
                    Width = 509
                  end
                end
                inherited pnlCharts: TPanel
                  Width = 511
                  Height = 0
                end
              end
            end
          end
        end
      end
    end
  end
end
