inherited FormSummaryEpiCurves: TFormSummaryEpiCurves
  Left = 1174
  Top = 38
  Width = 726
  Height = 520
  Caption = 'Summary epidemic curves'
  Constraints.MinHeight = 425
  Constraints.MinWidth = 625
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 718
    TabOrder = 2
    inherited pnlProdTypes: TPanel
      Width = 416
    end
  end
  object pnlGlobalOptions: TPanel [1]
    Left = 0
    Top = 444
    Width = 718
    Height = 49
    Align = alBottom
    TabOrder = 0
    object lblDaysPerInterval: TLabel
      Left = 376
      Top = 16
      Width = 131
      Height = 13
      Caption = 'Simulation days per interval:'
    end
    object pnlUnitChoices: TPanel
      Left = 240
      Top = 4
      Width = 113
      Height = 39
      BevelInner = bvLowered
      TabOrder = 0
      object rdoHerds: TRadioButton
        Left = 4
        Top = 5
        Width = 105
        Height = 12
        Caption = 'Units'
        Checked = True
        TabOrder = 0
        TabStop = True
        WordWrap = True
        OnClick = updateCurves
      end
      object rdoAnimals: TRadioButton
        Left = 4
        Top = 22
        Width = 77
        Height = 12
        Caption = 'Animals'
        TabOrder = 1
        WordWrap = True
        OnClick = updateCurves
      end
    end
    object pnlCurveChoices: TPanel
      Left = 12
      Top = 4
      Width = 221
      Height = 39
      BevelInner = bvLowered
      TabOrder = 1
      object rdoActual: TRadioButton
        Left = 4
        Top = 5
        Width = 213
        Height = 12
        Caption = 'Actual epidemic curve'
        Checked = True
        TabOrder = 0
        TabStop = True
        WordWrap = True
        OnClick = updateCurves
      end
      object rdoApparent: TRadioButton
        Left = 4
        Top = 22
        Width = 213
        Height = 12
        Caption = 'Apparent epidemic curve'
        TabOrder = 1
        WordWrap = True
        OnClick = updateCurves
      end
    end
    object speDaysPerInterval: TSpinEdit
      Left = 544
      Top = 12
      Width = 57
      Height = 22
      MaxValue = 100
      MinValue = 1
      TabOrder = 2
      Value = 1
      OnChange = speDaysPerIntervalChange
    end
  end
  object pgcMain: TPageControl [2]
    Left = 0
    Top = 33
    Width = 718
    Height = 411
    ActivePage = tbsSummaryEpiCurveGraph
    Align = alClient
    TabOrder = 1
    object tbsSummaryEpiCurveGraph: TTabSheet
      Caption = 'Graphical view'
      inline fraSummaryEpiCurves: TFrameSummaryEpiCurves
        Left = 0
        Top = 0
        Width = 710
        Height = 383
        Align = alClient
        TabOrder = 0
        inherited pnlSummaryEpiCurveOptions: TPanel
          Height = 383
          inherited pnl3D: TPanel
            Height = 139
            inherited cbx3D: TCheckBox
              OnClick = updateCurves
            end
          end
        end
        inherited chtSummaryEpiCurves: TChart
          Width = 597
          Height = 383
          Legend.Alignment = laBottom
          Legend.Color = clSilver
          Legend.Visible = True
        end
      end
    end
    object tbsSummaryEpiCurveTable: TTabSheet
      Caption = 'Tabular view'
      ImageIndex = 1
      OnResize = tbsSummaryEpiCurveTableResize
      inline fraSummaryEpiCurveTable: TFrameSummaryEpiCurveTable
        Left = 0
        Top = 0
        Width = 710
        Height = 371
        Align = alTop
        TabOrder = 0
        inherited stgGrid: TARSyncGrid
          Width = 700
          Height = 371
        end
        inherited pnlSpacer: TPanel
          Left = 700
          Height = 371
        end
      end
      object spacerPanel: TPanel
        Left = 0
        Top = 371
        Width = 710
        Height = 12
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
end
