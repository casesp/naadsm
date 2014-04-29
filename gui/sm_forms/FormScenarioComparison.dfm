inherited FormScenarioComparison: TFormScenarioComparison
  Left = 749
  Top = 153
  Width = 889
  Height = 846
  Caption = 'Scenario comparison'
  Constraints.MinWidth = 860
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 881
    inherited pnlProdTypes: TPanel
      Width = 547
    end
    inherited pnlButtons: TPanel
      inherited btnSaveData: TBitBtn
        Enabled = False
      end
      inherited btnPrintData: TBitBtn
        Enabled = False
      end
      inherited btnCopyData: TBitBtn
        Enabled = False
      end
      inherited btnSaveCharts: TBitBtn
        Enabled = False
      end
      inherited btnCopyCharts: TBitBtn
        Enabled = False
      end
      inherited btnPrintCharts: TBitBtn
        Enabled = False
      end
    end
  end
  object pnlFrames: TPanel [1]
    Left = 0
    Top = 33
    Width = 881
    Height = 786
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlB: TPanel
      Left = 410
      Top = 0
      Width = 471
      Height = 786
      Align = alClient
      TabOrder = 0
      object pnlFileNameB: TPanel
        Left = 1
        Top = 1
        Width = 469
        Height = 23
        Align = alTop
        Caption = 'FileNameB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object pgcOutputsB: TPBPageControl
        Left = 1
        Top = 24
        Width = 469
        Height = 761
        ActivePage = tabZoneOutputsB
        Align = alClient
        TabOrder = 1
        Visible = False
        OnMouseUp = pgcOutputsBMouseUp
        object tabEpiOutputsB: TTabSheet
          Caption = '   Epidemiology   '
          inline fraStatsEpiB: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 461
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 461
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 461
                inherited Splitter1: TSplitter
                  Width = 461
                end
                inherited pnlTable: TPanel
                  Width = 461
                  inherited pnlTableHeader: TPanel
                    Width = 459
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 459
                    inherited stgGrid: TARSyncGrid
                      Width = 449
                    end
                    inherited pnlSpacer: TPanel
                      Left = 449
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 461
                  inherited pnlHistogramHeader: TPanel
                    Width = 403
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 403
                    inherited chtHistogram: TChart
                      Width = 403
                    end
                  end
                end
              end
            end
          end
        end
        object tabCostOutputsB: TTabSheet
          Caption = '   Cost accounting     '
          ImageIndex = 1
          inline fraStatsCostB: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 461
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 461
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 461
                inherited Splitter1: TSplitter
                  Width = 461
                end
                inherited pnlTable: TPanel
                  Width = 461
                  inherited pnlTableHeader: TPanel
                    Width = 459
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 459
                    inherited stgGrid: TARSyncGrid
                      Width = 449
                    end
                    inherited pnlSpacer: TPanel
                      Left = 449
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 461
                  inherited pnlHistogramHeader: TPanel
                    Width = 402
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 402
                    inherited chtHistogram: TChart
                      Width = 402
                    end
                  end
                end
              end
            end
          end
        end
        object tabPTZoneOutputsB: TTabSheet
          Caption = 'Zones/production types'
          ImageIndex = 2
          inline fraStatsPTZonesB: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 461
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 461
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 461
                inherited Splitter1: TSplitter
                  Width = 461
                end
                inherited pnlTable: TPanel
                  Width = 461
                  inherited pnlTableHeader: TPanel
                    Width = 459
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 459
                    inherited stgGrid: TARSyncGrid
                      Width = 449
                    end
                    inherited pnlSpacer: TPanel
                      Left = 449
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 461
                  inherited pnlHistogramHeader: TPanel
                    Width = 459
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 459
                    inherited chtHistogram: TChart
                      Width = 459
                    end
                    inherited gbxBreaks: TGroupBox
                      Width = 459
                    end
                    inherited Panel1: TPanel
                      Width = 459
                    end
                  end
                end
              end
            end
          end
        end
        object tabZoneOutputsB: TTabSheet
          Caption = 'Zones'
          ImageIndex = 3
          inline fraStatsZonesB: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 461
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 461
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 461
                inherited Splitter1: TSplitter
                  Width = 461
                end
                inherited pnlTable: TPanel
                  Width = 461
                  inherited pnlTableHeader: TPanel
                    Width = 459
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 459
                    inherited stgGrid: TARSyncGrid
                      Width = 449
                    end
                    inherited pnlSpacer: TPanel
                      Left = 449
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 461
                  inherited pnlHistogramHeader: TPanel
                    Width = 459
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 459
                    inherited chtHistogram: TChart
                      Width = 459
                    end
                    inherited gbxBreaks: TGroupBox
                      Width = 459
                    end
                    inherited Panel1: TPanel
                      Width = 459
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    object pnlA: TPanel
      Left = 0
      Top = 0
      Width = 410
      Height = 786
      Align = alLeft
      TabOrder = 1
      object pnlFileNameA: TPanel
        Left = 1
        Top = 1
        Width = 408
        Height = 23
        Align = alTop
        Caption = 'FileNameA'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object pgcOutputsA: TPBPageControl
        Left = 1
        Top = 24
        Width = 408
        Height = 761
        ActivePage = tabZoneOutputsA
        Align = alClient
        TabOrder = 1
        Visible = False
        OnMouseUp = pgcOutputsAMouseUp
        object tabEpiOutputsA: TTabSheet
          Caption = '   Epidemiology   '
          inline fraStatsEpiA: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 400
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 400
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 400
                inherited Splitter1: TSplitter
                  Width = 400
                end
                inherited pnlTable: TPanel
                  Width = 400
                  inherited pnlTableHeader: TPanel
                    Width = 398
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 398
                    inherited stgGrid: TARSyncGrid
                      Width = 388
                    end
                    inherited pnlSpacer: TPanel
                      Left = 388
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 400
                  inherited pnlHistogramHeader: TPanel
                    Width = 398
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 398
                    inherited chtHistogram: TChart
                      Width = 398
                    end
                    inherited gbxBreaks: TGroupBox
                      Width = 398
                    end
                    inherited Panel1: TPanel
                      Width = 398
                    end
                  end
                end
              end
            end
          end
        end
        object tabCostOutputsA: TTabSheet
          Caption = '   Cost accounting     '
          ImageIndex = 1
          inline fraStatsCostA: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 400
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 400
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 400
                inherited Splitter1: TSplitter
                  Width = 400
                end
                inherited pnlTable: TPanel
                  Width = 400
                  inherited pnlTableHeader: TPanel
                    Width = 398
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 398
                    inherited stgGrid: TARSyncGrid
                      Width = 388
                    end
                    inherited pnlSpacer: TPanel
                      Left = 388
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 400
                  inherited pnlHistogramHeader: TPanel
                    Width = 398
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 398
                    inherited chtHistogram: TChart
                      Width = 398
                    end
                    inherited gbxBreaks: TGroupBox
                      Width = 398
                    end
                    inherited Panel1: TPanel
                      Width = 398
                    end
                  end
                end
              end
            end
          end
        end
        object tabPTZoneOutputsA: TTabSheet
          Caption = 'Zones/production types'
          ImageIndex = 2
          inline fraStatsPTZonesA: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 400
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 400
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 400
                inherited Splitter1: TSplitter
                  Width = 400
                end
                inherited pnlTable: TPanel
                  Width = 400
                  inherited pnlTableHeader: TPanel
                    Width = 398
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 398
                    inherited stgGrid: TARSyncGrid
                      Width = 388
                    end
                    inherited pnlSpacer: TPanel
                      Left = 388
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 400
                  inherited pnlHistogramHeader: TPanel
                    Width = 398
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 398
                    inherited chtHistogram: TChart
                      Width = 398
                    end
                    inherited gbxBreaks: TGroupBox
                      Width = 398
                    end
                    inherited Panel1: TPanel
                      Width = 398
                    end
                  end
                end
              end
            end
          end
        end
        object tabZoneOutputsA: TTabSheet
          Caption = 'Zones'
          ImageIndex = 3
          inline fraStatsZonesA: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 400
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 400
              Height = 733
              inherited pnlTopSection: TPanel
                Width = 400
                inherited Splitter1: TSplitter
                  Width = 400
                end
                inherited pnlTable: TPanel
                  Width = 400
                  inherited pnlTableHeader: TPanel
                    Width = 398
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 398
                    inherited stgGrid: TARSyncGrid
                      Width = 388
                    end
                    inherited pnlSpacer: TPanel
                      Left = 388
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 400
                  inherited pnlHistogramHeader: TPanel
                    Width = 398
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 398
                    inherited chtHistogram: TChart
                      Width = 398
                    end
                    inherited gbxBreaks: TGroupBox
                      Width = 398
                    end
                    inherited Panel1: TPanel
                      Width = 398
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end
end
