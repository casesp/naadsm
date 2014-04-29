inherited FormIterationSummary: TFormIterationSummary
  Left = 421
  Top = 164
  Height = 818
  Caption = 'Summary of 1 iteration'
  Constraints.MinHeight = 600
  Constraints.MinWidth = 755
  KeyPreview = True
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    TabOrder = 2
    inherited pnlProdTypes: TPanel
      inherited cboProdTypes: TComboBox
        Width = 169
      end
      inherited cboZones: TComboBox
        Left = 192
        Width = 129
      end
    end
  end
  object pgcOutputs: TPBPageControl [1]
    Left = 0
    Top = 53
    Width = 747
    Height = 738
    ActivePage = tabCostOutputs
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
                Height = 392
                Anchors = [akLeft, akTop, akRight, akBottom]
                inherited pnlInapparent: TPanel
                  Width = 725
                  inherited cbxInapparent: TCheckBox
                    OnClick = fraEpiIterationSummarycbxInapparentClick
                  end
                end
                inherited fraInapparent: TFrameSingleEpiCurve
                  Width = 725
                  Height = 372
                  inherited chtCurve: TChart
                    Width = 725
                    Height = 372
                  end
                end
              end
              inherited pnlApparentChart: TPanel
                Top = 392
                Width = 727
                Height = 286
                inherited pnlApparent: TPanel
                  Width = 725
                  inherited cbxApparent: TCheckBox
                    Width = 492
                    OnClick = fraEpiIterationSummarycbxApparentClick
                  end
                end
                inherited fraApparent: TFrameSingleEpiCurve
                  Width = 725
                  Height = 266
                  inherited chtCurve: TChart
                    Width = 725
                    Height = 266
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
                Height = 185
                inherited lblAsterisk: TLabel
                  Width = 717
                  Height = 175
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
                Width = 727
                Height = 678
                inherited fraChart: TFrameSingleCostCurve
                  Width = 725
                  Height = 676
                  Constraints.MinWidth = 567
                  inherited pnlCumulCosts: TPanel
                    Width = 725
                  end
                  inherited pnlChart: TPanel
                    Width = 725
                    Height = 539
                    inherited chtCosts: TChart
                      Width = 725
                      Height = 539
                    end
                  end
                  inherited pnlCostCategories: TPanel
                    Top = 564
                    Width = 725
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
                Width = 727
                Height = 678
                inherited pnlTableFrameContainer: TPanel
                  Width = 725
                  Height = 676
                  inherited fraTable: TFrameSingleCostTable
                    Width = 723
                    Height = 674
                    inherited pnlShowCumul: TPanel
                      Width = 723
                    end
                    inherited fraGrid: TFrameStringGridBase
                      Width = 723
                      Height = 649
                      inherited stgGrid: TARSyncGrid
                        Width = 713
                        Height = 649
                      end
                      inherited pnlSpacer: TPanel
                        Left = 713
                        Height = 649
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
