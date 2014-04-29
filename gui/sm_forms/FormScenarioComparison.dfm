inherited FormScenarioComparison: TFormScenarioComparison
  Left = 10
  Top = 102
  Width = 833
  Height = 846
  Caption = 'Scenario comparison'
  Constraints.MinWidth = 830
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 825
    inherited pnlProdTypes: TPanel
      Width = 523
    end
  end
  object pnlFrames: TPanel [1]
    Left = 0
    Top = 33
    Width = 825
    Height = 786
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlB: TPanel
      Left = 410
      Top = 0
      Width = 415
      Height = 786
      Align = alClient
      TabOrder = 0
      object pnlFileNameB: TPanel
        Left = 1
        Top = 1
        Width = 413
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
        Width = 413
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
            Width = 405
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 405
              Height = 733
              inherited Splitter2: TSplitter
                Width = 405
              end
              inherited pnlTopSection: TPanel
                Width = 405
                inherited Splitter1: TSplitter
                  Width = 405
                end
                inherited pnlTable: TPanel
                  Width = 405
                  inherited pnlTableHeader: TPanel
                    Width = 403
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 403
                    inherited stgGrid: TARSyncGrid
                      Width = 393
                    end
                    inherited pnlSpacer: TPanel
                      Left = 393
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 405
                  inherited pnlHistogramHeader: TPanel
                    Width = 403
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 403
                    inherited pnlControls: TPanel
                      Width = 403
                    end
                    inherited chtHistogram: TChart
                      Width = 403
                    end
                  end
                end
              end
              inherited pnlConvergence: TPanel
                Width = 405
                Height = 258
                inherited pnlConvergenceHeader: TPanel
                  Width = 403
                end
                inherited fraConvergence: TFrameArrayConvergence
                  Width = 403
                  Height = 231
                  inherited pnlControls: TPanel
                    Top = 188
                    Width = 403
                    inherited rdgConvergenceParam: TRadioGroup
                      Width = 401
                    end
                  end
                  inherited chtConvergence: TChart
                    Width = 403
                    Height = 188
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
            Width = 405
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 405
              Height = 733
              inherited Splitter2: TSplitter
                Width = 405
              end
              inherited pnlTopSection: TPanel
                Width = 405
                inherited Splitter1: TSplitter
                  Width = 404
                end
                inherited pnlTable: TPanel
                  Width = 404
                  inherited pnlTableHeader: TPanel
                    Width = 402
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 402
                    inherited stgGrid: TARSyncGrid
                      Width = 392
                    end
                    inherited pnlSpacer: TPanel
                      Left = 392
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 404
                  inherited pnlHistogramHeader: TPanel
                    Width = 402
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 402
                    inherited pnlControls: TPanel
                      Width = 402
                    end
                    inherited chtHistogram: TChart
                      Width = 402
                    end
                  end
                end
              end
              inherited pnlConvergence: TPanel
                Width = 405
                Height = 258
                inherited pnlConvergenceHeader: TPanel
                  Width = 403
                end
                inherited fraConvergence: TFrameArrayConvergence
                  Width = 403
                  Height = 231
                  inherited pnlControls: TPanel
                    Top = 188
                    Width = 403
                    inherited rdgConvergenceParam: TRadioGroup
                      Width = 401
                    end
                  end
                  inherited chtConvergence: TChart
                    Width = 403
                    Height = 188
                  end
                end
              end
            end
          end
        end
        object tabZoneOutputsB: TTabSheet
          Caption = '   Zones '
          ImageIndex = 2
          inline fraStatsZonesB: TFrameOutputStats
            Left = 0
            Top = 0
            Width = 405
            Height = 733
            Align = alClient
            Constraints.MinWidth = 400
            TabOrder = 0
            inherited pnlWholeSection: TPanel
              Width = 405
              Height = 733
              inherited Splitter2: TSplitter
                Width = 405
              end
              inherited pnlTopSection: TPanel
                Width = 405
                inherited Splitter1: TSplitter
                  Width = 405
                end
                inherited pnlTable: TPanel
                  Width = 405
                  inherited pnlTableHeader: TPanel
                    Width = 403
                  end
                  inherited fraTable: TFrameOutputStatsTable
                    Width = 403
                    inherited stgGrid: TARSyncGrid
                      Width = 393
                    end
                    inherited pnlSpacer: TPanel
                      Left = 393
                    end
                  end
                end
                inherited pnlHistogram: TPanel
                  Width = 405
                  inherited pnlHistogramHeader: TPanel
                    Width = 403
                  end
                  inherited fraHistogram: TFrameArrayHistogram
                    Width = 403
                    inherited pnlControls: TPanel
                      Width = 403
                    end
                    inherited chtHistogram: TChart
                      Width = 403
                    end
                  end
                end
              end
              inherited pnlConvergence: TPanel
                Width = 405
                Height = 258
                inherited pnlConvergenceHeader: TPanel
                  Width = 403
                end
                inherited fraConvergence: TFrameArrayConvergence
                  Width = 403
                  Height = 231
                  inherited pnlControls: TPanel
                    Top = 188
                    Width = 403
                    inherited rdgConvergenceParam: TRadioGroup
                      Width = 401
                    end
                  end
                  inherited chtConvergence: TChart
                    Width = 403
                    Height = 188
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
        ActivePage = tabCostOutputsA
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
              inherited Splitter2: TSplitter
                Width = 400
              end
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
                    inherited pnlControls: TPanel
                      Width = 398
                    end
                    inherited chtHistogram: TChart
                      Width = 398
                    end
                  end
                end
              end
              inherited pnlConvergence: TPanel
                Width = 400
                Height = 258
                inherited pnlConvergenceHeader: TPanel
                  Width = 398
                end
                inherited fraConvergence: TFrameArrayConvergence
                  Width = 398
                  Height = 231
                  inherited pnlControls: TPanel
                    Top = 188
                    Width = 398
                    inherited rdgConvergenceParam: TRadioGroup
                      Width = 396
                    end
                  end
                  inherited chtConvergence: TChart
                    Width = 398
                    Height = 188
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
              inherited Splitter2: TSplitter
                Width = 400
              end
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
                    inherited pnlControls: TPanel
                      Width = 398
                    end
                    inherited chtHistogram: TChart
                      Width = 398
                    end
                  end
                end
              end
              inherited pnlConvergence: TPanel
                Width = 400
                Height = 258
                inherited pnlConvergenceHeader: TPanel
                  Width = 398
                end
                inherited fraConvergence: TFrameArrayConvergence
                  Width = 398
                  Height = 231
                  inherited pnlControls: TPanel
                    Top = 188
                    Width = 398
                    inherited rdgConvergenceParam: TRadioGroup
                      Width = 396
                    end
                  end
                  inherited chtConvergence: TChart
                    Width = 398
                    Height = 188
                  end
                end
              end
            end
          end
        end
        object tabZoneOutputsA: TTabSheet
          Caption = '   Zones  '
          ImageIndex = 2
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
              inherited Splitter2: TSplitter
                Width = 400
              end
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
                    inherited pnlControls: TPanel
                      Width = 398
                    end
                    inherited chtHistogram: TChart
                      Width = 398
                    end
                  end
                end
              end
              inherited pnlConvergence: TPanel
                Width = 400
                Height = 258
                inherited pnlConvergenceHeader: TPanel
                  Width = 398
                end
                inherited fraConvergence: TFrameArrayConvergence
                  Width = 398
                  Height = 231
                  inherited pnlControls: TPanel
                    Top = 188
                    Width = 398
                    inherited rdgConvergenceParam: TRadioGroup
                      Width = 396
                    end
                  end
                  inherited chtConvergence: TChart
                    Width = 398
                    Height = 188
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
