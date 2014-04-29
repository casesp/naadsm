inherited FormIterationSummary: TFormIterationSummary
  Left = -842
  Top = 99
  Width = 755
  Height = 818
  Caption = 'Summary of 1 iteration'
  Constraints.MinHeight = 600
  Constraints.MinWidth = 755
  KeyPreview = True
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 747
    TabOrder = 2
    inherited pnlProdTypes: TPanel
      Width = 445
      object lblIteration: TLabel [0]
        Left = 328
        Top = 8
        Width = 41
        Height = 13
        Caption = 'Iteration:'
      end
      inherited cboProdTypes: TComboBox
        Width = 169
      end
      inherited cboZones: TComboBox
        Left = 192
        Width = 129
        TabOrder = 4
      end
      object cboIteration: TComboBox
        Left = 384
        Top = 4
        Width = 57
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = cboIterationChange
      end
    end
  end
  object pgcOutputs: TPBPageControl [1]
    Left = 0
    Top = 53
    Width = 747
    Height = 738
    ActivePage = tabEpiOutputs
    Align = alClient
    TabOrder = 0
    OnChange = pgcOutputsChange
    object tabEpiOutputs: TTabSheet
      Caption = '   Epidemiology'
      object sbxEpiOutputs: TScrollBox
        Left = 0
        Top = 0
        Width = 739
        Height = 710
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        TabOrder = 0
        inline fraEpiIterationSummary: TFrameEpiIterationSummary
          Left = 0
          Top = 0
          Width = 735
          Height = 706
          Align = alClient
          Constraints.MinWidth = 555
          TabOrder = 0
          Visible = False
          inherited pbpGraphTableTabs: TPBPageControl
            Width = 735
            Height = 706
            inherited tabGraphs: TTabSheet
              inherited pnlInapparentChart: TPanel
                Width = 727
                Height = 363
                Anchors = [akLeft, akTop, akRight, akBottom]
                inherited pnlInapparent: TPanel
                  Width = 725
                  inherited cbxInapparent: TCheckBox
                    OnClick = fraEpiIterationSummarycbxInapparentClick
                  end
                end
                inherited fraInapparent: TFrameSingleEpiCurve
                  Width = 725
                  Height = 343
                  inherited chtCurve: TChart
                    Width = 725
                    Height = 343
                  end
                end
              end
              inherited pnlApparentChart: TPanel
                Top = 363
                Width = 727
                Height = 315
                inherited pnlApparent: TPanel
                  Width = 725
                  inherited cbxApparent: TCheckBox
                    OnClick = fraEpiIterationSummarycbxApparentClick
                  end
                end
                inherited fraApparent: TFrameSingleEpiCurve
                  Width = 725
                  Height = 295
                  inherited chtCurve: TChart
                    Width = 725
                    Height = 295
                  end
                end
              end
            end
            inherited tabTables: TTabSheet
              inherited PanelInf: TPanel
                Width = 727
                inherited pnlInfHeader: TPanel
                  Width = 725
                  inherited cbxInf: TCheckBox
                    OnClick = fraEpiIterationSummarycbxInfClick
                  end
                end
                inherited fraSgInf: TFrameStringGridBase
                  Width = 725
                  inherited stgGrid: TARSyncGrid
                    Width = 715
                  end
                  inherited pnlSpacer: TPanel
                    Left = 715
                  end
                end
              end
              inherited PanelVac: TPanel
                Width = 727
                inherited pnlVacHeader: TPanel
                  Width = 725
                  inherited cbxVac: TCheckBox
                    OnClick = fraEpiIterationSummarycbxVacClick
                  end
                end
                inherited fraSgVac: TFrameStringGridBase
                  Width = 725
                  inherited stgGrid: TARSyncGrid
                    Width = 715
                  end
                  inherited pnlSpacer: TPanel
                    Left = 715
                  end
                end
              end
              inherited pnlAsterisk: TPanel
                Width = 727
                Height = 213
                inherited lblAsterisk: TLabel
                  Width = 717
                  Height = 203
                end
              end
              inherited pnlDestr: TPanel
                Width = 727
                inherited pnlDestrHeader: TPanel
                  Width = 725
                  inherited cbxDestr: TCheckBox
                    OnClick = fraEpiIterationSummarycbxDestrClick
                  end
                end
                inherited fraSgDestr: TFrameStringGridBase
                  Width = 725
                  Anchors = [akLeft, akTop, akRight]
                  inherited stgGrid: TARSyncGrid
                    Width = 715
                    Anchors = [akLeft, akTop, akRight]
                  end
                  inherited pnlSpacer: TPanel
                    Left = 715
                  end
                end
              end
              inherited pnlSurv: TPanel
                Width = 727
                inherited pnlSurvHeader: TPanel
                  Width = 725
                  inherited cbxSurv: TCheckBox
                    Caption = 'Detection'
                    OnClick = fraEpiIterationSummarycbxSurvClick
                  end
                end
                inherited fraSgSurv: TFrameStringGridBase
                  Width = 725
                  inherited stgGrid: TARSyncGrid
                    Width = 715
                  end
                  inherited pnlSpacer: TPanel
                    Left = 715
                  end
                end
              end
              inherited sgHeader: TStringGrid
                Width = 727
              end
            end
          end
        end
      end
    end
    object tabCostOutputs: TTabSheet
      Caption = '   Cost accounting'
      ImageIndex = 1
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 739
        Height = 710
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        TabOrder = 0
        inline fraCostIterationSummary: TFrameCostIterationSummary
          Left = 0
          Top = 0
          Width = 735
          Height = 706
          HorzScrollBar.Visible = False
          VertScrollBar.Visible = False
          Align = alClient
          Constraints.MinWidth = 555
          TabOrder = 0
          inherited pbpGraphTableTabs: TPBPageControl
            Width = 735
            Height = 706
            inherited tabGraph: TTabSheet
              Caption = 'Graph View'
              inherited pnlChart: TPanel
                Width = 783
                Height = 573
                inherited fraChart: TFrameSingleCostCurve
                  Width = 781
                  Height = 571
                  Constraints.MinWidth = 567
                  inherited pnlCumulCosts: TPanel
                    Width = 781
                  end
                  inherited pnlChart: TPanel
                    Width = 781
                    Height = 451
                    inherited chtCosts: TChart
                      Width = 781
                      Height = 451
                    end
                  end
                  inherited pnlCostCategories: TPanel
                    Top = 476
                    Width = 781
                    inherited lineVaccSubtotal: TPanel
                      TabOrder = 18
                    end
                    inherited lineAppraisal: TPanel
                      TabOrder = 16
                    end
                    inherited lineIndemnification: TPanel
                      TabOrder = 17
                    end
                    inherited lineDisposal: TPanel
                      TabOrder = 11
                    end
                    inherited lineVaccSetup: TPanel
                      TabOrder = 13
                    end
                  end
                end
              end
            end
            inherited tabTable: TTabSheet
              inherited pnlTable: TPanel
                Width = 783
                inherited pnlTableFrameContainer: TPanel
                  Width = 781
                  inherited fraTable: TFrameSingleCostTable
                    Width = 779
                    inherited pnlShowCumul: TPanel
                      Width = 779
                    end
                    inherited fraGrid: TFrameStringGridBase
                      Width = 779
                      inherited stgGrid: TARSyncGrid
                        Width = 769
                      end
                      inherited pnlSpacer: TPanel
                        Left = 769
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
  object pnlCaption: TPanel [2]
    Left = 0
    Top = 33
    Width = 747
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 5
    Caption = 'Simulation status: completed/aborted/running'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
end
