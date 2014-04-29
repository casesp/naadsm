object FrameCostIterationSummary: TFrameCostIterationSummary
  Left = 0
  Top = 0
  Width = 602
  Height = 558
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Constraints.MinWidth = 555
  TabOrder = 0
  object pbpGraphTableTabs: TPBPageControl
    Left = 0
    Top = 0
    Width = 602
    Height = 558
    ActivePage = tabGraph
    Align = alClient
    TabOrder = 0
    object tabGraph: TTabSheet
      Caption = 'GraphView'
      object pnlChart: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 530
        Align = alClient
        Constraints.MinHeight = 309
        TabOrder = 0
        inline fraChart: TFrameSingleCostCurve
          Left = 1
          Top = 1
          Width = 592
          Height = 528
          Align = alClient
          Constraints.MinHeight = 282
          Constraints.MinWidth = 367
          TabOrder = 0
          inherited pnlCumulCosts: TPanel
            Width = 592
          end
          inherited pnlChart: TPanel
            Width = 592
            Height = 408
            inherited chtCosts: TChart
              Width = 592
              Height = 408
            end
          end
          inherited pnlCostCategories: TPanel
            Top = 433
            Width = 592
          end
        end
      end
    end
    object tabTable: TTabSheet
      Caption = 'Table View'
      ImageIndex = 1
      object pnlTable: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 530
        Align = alClient
        Constraints.MinHeight = 27
        TabOrder = 0
        object pnlTableFrameContainer: TPanel
          Left = 1
          Top = 1
          Width = 592
          Height = 528
          Align = alClient
          BevelOuter = bvLowered
          TabOrder = 0
          inline fraTable: TFrameSingleCostTable
            Left = 1
            Top = 1
            Width = 590
            Height = 526
            Align = alClient
            Constraints.MinHeight = 150
            TabOrder = 0
            inherited pnlShowCumul: TPanel
              Width = 590
            end
            inherited fraGrid: TFrameStringGridBase
              Width = 590
              Height = 501
              inherited stgGrid: TARSyncGrid
                Width = 580
                Height = 501
              end
              inherited pnlSpacer: TPanel
                Left = 580
                Height = 501
              end
            end
          end
        end
      end
    end
  end
end
