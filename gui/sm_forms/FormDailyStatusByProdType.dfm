inherited FormDailyStatusByProdType: TFormDailyStatusByProdType
  Left = -1383
  Top = 236
  Height = 535
  Caption = 'Daily unit status for 1 iteration'
  Constraints.MinWidth = 755
  KeyPreview = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
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
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 33
    Width = 747
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Iteration status: completed/aborted/running'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object pcCharts: TPageControl [2]
    Left = 0
    Top = 53
    Width = 747
    Height = 455
    ActivePage = tsDetectionStatus
    Align = alClient
    TabOrder = 2
    object tsDiseaseStatus: TTabSheet
      Caption = 'Disease Status'
      inline fraDiseaseStatus: TFrameDailyStatusByProdTypeDiseaseStats
        Left = 0
        Top = 0
        Width = 739
        Height = 427
        Align = alClient
        TabOrder = 0
        inherited pnlChartOptions: TPanel
          Top = 402
          Width = 739
        end
        inherited chtOutputs: TChart
          Width = 610
          Height = 402
          MarginLeft = 6
          Legend.ColorWidth = 15
          Legend.LegendStyle = lsSeries
        end
        inherited pnlCheckBoxes: TPanel
          Height = 402
        end
      end
    end
    object tsControlStatus: TTabSheet
      Caption = 'Control Status'
      ImageIndex = 1
      inline fraControlStatus: TFrameDailyStatusByProdTypeControlStats
        Left = 0
        Top = 0
        Width = 739
        Height = 427
        Align = alClient
        TabOrder = 0
        inherited pnlChartOptions: TPanel
          Top = 402
          Width = 739
        end
        inherited chtOutputs: TChart
          Width = 594
          Height = 402
          MarginLeft = 6
          Legend.ColorWidth = 15
          Legend.LegendStyle = lsSeries
        end
        inherited pnlCheckBoxes: TPanel
          Height = 402
        end
      end
    end
    object tsDetectionStatus: TTabSheet
      Caption = 'Detection Status'
      ImageIndex = 2
      inline fraDetectionStatus: TFrameDailyStatusByProdTypeDetectionStats
        Left = 0
        Top = 0
        Width = 739
        Height = 427
        Align = alClient
        TabOrder = 0
        inherited pnlChartOptions: TPanel
          Top = 402
          Width = 739
        end
        inherited chtOutputs: TChart
          Width = 594
          Height = 402
          MarginLeft = 6
          Legend.ColorWidth = 15
          Legend.LegendStyle = lsSeries
        end
        inherited pnlCheckBoxes: TPanel
          Height = 402
        end
      end
    end
  end
  inherited dlgSaveWMF: TSaveDialog
    Left = 392
    Top = 41
  end
  inherited dlgSaveCSV: TSaveDialog
    Left = 424
    Top = 40
  end
  inherited dlgPrint: TPrintDialog
    Left = 456
    Top = 40
  end
end
