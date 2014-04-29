inherited FormOutputStats: TFormOutputStats
  Left = 468
  Top = 85
  Width = 660
  Height = 876
  Caption = 'Output statistics'
  Constraints.MinWidth = 660
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 652
    inherited pnlProdTypes: TPanel
      Width = 318
      inherited cboProdTypes: TComboBox
        Width = 177
      end
      inherited cboZones: TComboBox
        Left = 200
        Width = 108
      end
    end
  end
  object pgcOutputs: TPBPageControl [1]
    Left = 0
    Top = 33
    Width = 652
    Height = 816
    ActivePage = tabPTZoneOutputs
    Align = alClient
    TabOrder = 1
    Visible = False
    OnChange = pgcOutputsChange
    object tabEpiOutputs: TTabSheet
      Caption = '   Epidemiology   '
      inline fraStatsEpi: TFrameOutputStats
        Left = 0
        Top = 0
        Width = 644
        Height = 788
        Align = alClient
        Constraints.MinWidth = 400
        TabOrder = 0
        inherited pnlWholeSection: TPanel
          Width = 644
          Height = 788
          inherited pnlTopSection: TPanel
            Width = 644
            inherited Splitter1: TSplitter
              Width = 644
            end
            inherited pnlTable: TPanel
              Width = 644
              inherited pnlTableHeader: TPanel
                Width = 642
              end
              inherited fraTable: TFrameOutputStatsTable
                Width = 642
                inherited stgGrid: TARSyncGrid
                  Width = 632
                end
                inherited pnlSpacer: TPanel
                  Left = 632
                end
              end
            end
            inherited pnlHistogram: TPanel
              Width = 644
              inherited pnlHistogramHeader: TPanel
                Width = 607
              end
              inherited fraHistogram: TFrameArrayHistogram
                Width = 607
                inherited chtHistogram: TChart
                  Width = 607
                end
              end
            end
          end
        end
      end
    end
    object tabCostOutputs: TTabSheet
      Caption = '   Cost accounting     '
      ImageIndex = 1
      inline fraStatsCost: TFrameOutputStats
        Left = 0
        Top = 0
        Width = 644
        Height = 788
        Align = alClient
        Constraints.MinWidth = 400
        TabOrder = 0
        inherited pnlWholeSection: TPanel
          Width = 644
          Height = 788
          inherited pnlTopSection: TPanel
            Width = 644
            inherited Splitter1: TSplitter
              Width = 644
            end
            inherited pnlTable: TPanel
              Width = 644
              inherited pnlTableHeader: TPanel
                Width = 642
              end
              inherited fraTable: TFrameOutputStatsTable
                Width = 642
                inherited stgGrid: TARSyncGrid
                  Width = 632
                end
                inherited pnlSpacer: TPanel
                  Left = 632
                end
              end
            end
            inherited pnlHistogram: TPanel
              Width = 644
              inherited pnlHistogramHeader: TPanel
                Width = 642
              end
              inherited fraHistogram: TFrameArrayHistogram
                Width = 642
                inherited chtHistogram: TChart
                  Width = 642
                end
                inherited gbxBreaks: TGroupBox
                  Width = 642
                end
                inherited Panel1: TPanel
                  Width = 642
                end
              end
            end
          end
        end
      end
    end
    object tabPTZoneOutputs: TTabSheet
      Caption = 'Zones/production types'
      ImageIndex = 2
      inline fraStatsPTZones: TFrameOutputStats
        Left = 0
        Top = 0
        Width = 644
        Height = 788
        Align = alClient
        Constraints.MinWidth = 400
        TabOrder = 0
        inherited pnlWholeSection: TPanel
          Width = 644
          Height = 788
          inherited pnlTopSection: TPanel
            Width = 644
            inherited Splitter1: TSplitter
              Width = 644
            end
            inherited pnlTable: TPanel
              Width = 644
              inherited pnlTableHeader: TPanel
                Width = 642
              end
              inherited fraTable: TFrameOutputStatsTable
                Width = 642
                inherited stgGrid: TARSyncGrid
                  Width = 632
                end
                inherited pnlSpacer: TPanel
                  Left = 632
                end
              end
            end
            inherited pnlHistogram: TPanel
              Width = 644
              inherited pnlHistogramHeader: TPanel
                Width = 642
              end
              inherited fraHistogram: TFrameArrayHistogram
                Width = 642
                inherited chtHistogram: TChart
                  Width = 642
                end
                inherited gbxBreaks: TGroupBox
                  Width = 642
                end
                inherited Panel1: TPanel
                  Width = 642
                end
              end
            end
          end
        end
      end
    end
    object tabZoneOutputs: TTabSheet
      Caption = 'Zones'
      ImageIndex = 3
      inline fraStatsZones: TFrameOutputStats
        Left = 0
        Top = 0
        Width = 644
        Height = 788
        Align = alClient
        Constraints.MinWidth = 400
        TabOrder = 0
        inherited pnlWholeSection: TPanel
          Width = 644
          Height = 788
          inherited pnlTopSection: TPanel
            Width = 644
            inherited Splitter1: TSplitter
              Width = 644
            end
            inherited pnlTable: TPanel
              Width = 644
              inherited pnlTableHeader: TPanel
                Width = 642
              end
              inherited fraTable: TFrameOutputStatsTable
                Width = 642
                inherited stgGrid: TARSyncGrid
                  Width = 632
                end
                inherited pnlSpacer: TPanel
                  Left = 632
                end
              end
            end
            inherited pnlHistogram: TPanel
              Width = 644
              inherited pnlHistogramHeader: TPanel
                Width = 642
              end
              inherited fraHistogram: TFrameArrayHistogram
                Width = 642
                inherited chtHistogram: TChart
                  Width = 642
                end
                inherited gbxBreaks: TGroupBox
                  Width = 642
                end
                inherited Panel1: TPanel
                  Width = 642
                end
              end
            end
          end
        end
      end
    end
  end
end
