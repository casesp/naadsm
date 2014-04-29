inherited FormOutputStats: TFormOutputStats
  Left = 975
  Top = 106
  Width = 625
  Height = 876
  Caption = 'Output statistics'
  Constraints.MinWidth = 625
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 617
    inherited pnlProdTypes: TPanel
      Width = 315
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
    Width = 617
    Height = 816
    ActivePage = tabEpiOutputs
    Align = alClient
    TabOrder = 1
    Visible = False
    OnChange = pgcOutputsChange
    object tabEpiOutputs: TTabSheet
      Caption = '   Epidemiology   '
      inline fraStatsEpi: TFrameOutputStats
        Left = 0
        Top = 0
        Width = 609
        Height = 788
        Align = alClient
        Constraints.MinWidth = 400
        TabOrder = 0
        inherited pnlWholeSection: TPanel
          Width = 609
          Height = 788
          inherited Splitter2: TSplitter
            Width = 609
          end
          inherited pnlTopSection: TPanel
            Width = 609
            inherited Splitter1: TSplitter
              Width = 609
            end
            inherited pnlTable: TPanel
              Width = 609
              inherited pnlTableHeader: TPanel
                Width = 607
              end
              inherited fraTable: TFrameOutputStatsTable
                Width = 607
                inherited stgGrid: TARSyncGrid
                  Width = 597
                end
                inherited pnlSpacer: TPanel
                  Left = 597
                end
              end
            end
            inherited pnlHistogram: TPanel
              Width = 609
              inherited pnlHistogramHeader: TPanel
                Width = 607
              end
              inherited fraHistogram: TFrameArrayHistogram
                Width = 607
                inherited pnlControls: TPanel
                  Width = 607
                end
                inherited chtHistogram: TChart
                  Width = 607
                end
              end
            end
          end
          inherited pnlConvergence: TPanel
            Width = 609
            Height = 313
            inherited pnlConvergenceHeader: TPanel
              Width = 607
            end
            inherited fraConvergence: TFrameArrayConvergence
              Width = 607
              Height = 286
              inherited pnlControls: TPanel
                Top = 243
                Width = 607
                inherited rdgConvergenceParam: TRadioGroup
                  Width = 605
                end
              end
              inherited chtConvergence: TChart
                Width = 607
                Height = 243
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
        Width = 609
        Height = 788
        Align = alClient
        Constraints.MinWidth = 400
        TabOrder = 0
        inherited pnlWholeSection: TPanel
          Width = 609
          Height = 788
          inherited Splitter2: TSplitter
            Width = 609
          end
          inherited pnlTopSection: TPanel
            Width = 609
            inherited Splitter1: TSplitter
              Width = 609
            end
            inherited pnlTable: TPanel
              Width = 609
              inherited pnlTableHeader: TPanel
                Width = 607
              end
              inherited fraTable: TFrameOutputStatsTable
                Width = 607
                inherited stgGrid: TARSyncGrid
                  Width = 597
                end
                inherited pnlSpacer: TPanel
                  Left = 597
                end
              end
            end
            inherited pnlHistogram: TPanel
              Width = 609
              inherited pnlHistogramHeader: TPanel
                Width = 607
              end
              inherited fraHistogram: TFrameArrayHistogram
                Width = 607
                inherited pnlControls: TPanel
                  Width = 607
                end
                inherited chtHistogram: TChart
                  Width = 607
                end
              end
            end
          end
          inherited pnlConvergence: TPanel
            Width = 609
            Height = 313
            inherited pnlConvergenceHeader: TPanel
              Width = 607
            end
            inherited fraConvergence: TFrameArrayConvergence
              Width = 607
              Height = 286
              inherited pnlControls: TPanel
                Top = 243
                Width = 607
                inherited rdgConvergenceParam: TRadioGroup
                  Width = 605
                end
              end
              inherited chtConvergence: TChart
                Width = 607
                Height = 243
              end
            end
          end
        end
      end
    end
    object tabZoneOutputs: TTabSheet
      Caption = '   Zones'
      ImageIndex = 2
      inline fraStatsZones: TFrameOutputStats
        Left = 0
        Top = 0
        Width = 609
        Height = 788
        Align = alClient
        Constraints.MinWidth = 400
        TabOrder = 0
        inherited pnlWholeSection: TPanel
          Width = 609
          Height = 788
          inherited Splitter2: TSplitter
            Width = 609
          end
          inherited pnlTopSection: TPanel
            Width = 609
            inherited Splitter1: TSplitter
              Width = 609
            end
            inherited pnlTable: TPanel
              Width = 609
              inherited pnlTableHeader: TPanel
                Width = 607
              end
              inherited fraTable: TFrameOutputStatsTable
                Width = 607
                inherited stgGrid: TARSyncGrid
                  Width = 597
                end
                inherited pnlSpacer: TPanel
                  Left = 597
                end
              end
            end
            inherited pnlHistogram: TPanel
              Width = 609
              inherited pnlHistogramHeader: TPanel
                Width = 607
              end
              inherited fraHistogram: TFrameArrayHistogram
                Width = 607
                inherited pnlControls: TPanel
                  Width = 607
                end
                inherited chtHistogram: TChart
                  Width = 607
                end
              end
            end
          end
          inherited pnlConvergence: TPanel
            Width = 609
            Height = 313
            inherited pnlConvergenceHeader: TPanel
              Width = 607
            end
            inherited fraConvergence: TFrameArrayConvergence
              Width = 607
              Height = 286
              inherited pnlControls: TPanel
                Top = 243
                Width = 607
                inherited rdgConvergenceParam: TRadioGroup
                  Width = 605
                end
              end
              inherited chtConvergence: TChart
                Width = 607
                Height = 243
              end
            end
          end
        end
      end
    end
  end
end
