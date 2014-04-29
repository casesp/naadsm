object FrameContactSpreadMaster: TFrameContactSpreadMaster
  Left = 0
  Top = 0
  Width = 472
  Height = 822
  TabOrder = 0
  object pnlDirectContact: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 409
    Align = alTop
    TabOrder = 0
    object pnlDirect: TPanel
      Left = 1
      Top = 29
      Width = 470
      Height = 379
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlDirectParams: TPanel
        Left = 0
        Top = 0
        Width = 470
        Height = 379
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        inline fraDirect: TFrameContactSpread
          Left = 16
          Top = 0
          Width = 454
          Height = 379
          Align = alClient
          TabOrder = 0
          inherited pnlSimpleParams: TPanel
            Width = 454
            inherited pnlSimpleParamsTop: TPanel
              Width = 452
            end
            inherited pnlSimpleParamsBottom: TPanel
              Width = 452
            end
          end
          inherited pnlCharts: TPanel
            Width = 454
            Height = 282
            inherited pnlDistanceDistr: TPanel
              Width = 452
            end
            inherited pnlProportionInShipment: TPanel
              Width = 452
            end
            inherited pnlMovementControl: TPanel
              Width = 452
            end
          end
        end
        object pnlSpacerDirect: TPanel
          Left = 0
          Top = 0
          Width = 16
          Height = 379
          Align = alLeft
          BevelOuter = bvNone
          Constraints.MaxWidth = 16
          Constraints.MinWidth = 16
          TabOrder = 1
        end
      end
    end
    object pnlUseDirectContact: TPanel
      Left = 1
      Top = 1
      Width = 470
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object cbxIncludeDirect: TCheckBox
        Left = 8
        Top = 8
        Width = 369
        Height = 17
        Caption = 'Model direct contact spread'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object pnlIndirectContact: TPanel
    Left = 0
    Top = 409
    Width = 472
    Height = 413
    Align = alClient
    TabOrder = 1
    object pnlUseIndirectContact: TPanel
      Left = 1
      Top = 1
      Width = 470
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object cbxIncludeIndirect: TCheckBox
        Left = 8
        Top = 8
        Width = 505
        Height = 17
        Caption = 'Model indirect contact spread'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
    end
    object pnlIndirect: TPanel
      Left = 1
      Top = 29
      Width = 470
      Height = 383
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pnlIndirectParams: TPanel
        Left = 0
        Top = 0
        Width = 470
        Height = 383
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnlSpacerIndirect: TPanel
          Left = 0
          Top = 0
          Width = 16
          Height = 383
          Align = alLeft
          BevelOuter = bvNone
          Constraints.MaxWidth = 16
          Constraints.MinWidth = 16
          TabOrder = 0
        end
        inline fraIndirect: TFrameContactSpread
          Left = 16
          Top = 0
          Width = 454
          Height = 383
          Align = alClient
          TabOrder = 1
          inherited pnlSimpleParams: TPanel
            Width = 454
            inherited pnlSimpleParamsTop: TPanel
              Width = 452
            end
            inherited pnlSimpleParamsBottom: TPanel
              Width = 452
            end
          end
          inherited pnlCharts: TPanel
            Width = 454
            inherited pnlDistanceDistr: TPanel
              Width = 452
            end
            inherited pnlProportionInShipment: TPanel
              Width = 452
            end
            inherited pnlMovementControl: TPanel
              Width = 452
            end
          end
        end
      end
    end
  end
end
