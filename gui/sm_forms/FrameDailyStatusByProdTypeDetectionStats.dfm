inherited FrameDailyStatusByProdTypeDetectionStats: TFrameDailyStatusByProdTypeDetectionStats
  Width = 728
  Height = 452
  object pnlChartOptions: TPanel
    Left = 0
    Top = 427
    Width = 728
    Height = 25
    Align = alBottom
    TabOrder = 0
    object cbxThreeD: TCheckBox
      Left = 8
      Top = 4
      Width = 113
      Height = 17
      Caption = '3-D View'
      TabOrder = 0
      OnClick = cbxThreeDClick
    end
    object cbxCumulative: TCheckBox
      Left = 119
      Top = 4
      Width = 114
      Height = 17
      Caption = 'Cumulative'
      TabOrder = 1
      OnClick = cbxCumulativeClick
    end
  end
  object chtOutputs: TChart
    Left = 145
    Top = 0
    Width = 583
    Height = 427
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      '')
    BottomAxis.Title.Caption = 'Day of simulation'
    LeftAxis.Title.Caption = 'Number of units, out of a total of xyz'
    Legend.Alignment = laBottom
    Legend.Color = clSilver
    Align = alClient
    TabOrder = 1
    object SeriesExamined: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clBlack
      Title = 'Examined'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesDetected: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = 4259584
      Title = 'Detected'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesDestroyed: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = 8421440
      Title = 'Destroyed'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesTestTruePos: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clBlue
      Title = 'Test true positive'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesTestTrueNeg: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clRed
      Title = 'Test true negative'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesTestFalsePos: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clAqua
      Title = 'Test false positive'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesTestFalseNeg: TLineSeries
      Marks.ArrowLength = 0
      Marks.Frame.Visible = False
      Marks.Transparent = True
      Marks.Visible = False
      SeriesColor = clPurple
      Title = 'Test false negative'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesPDetection: TPointSeries
      Marks.ArrowLength = 0
      Marks.Frame.Visible = False
      Marks.Transparent = True
      Marks.Visible = False
      SeriesColor = clTeal
      Title = 'First Detection'
      Pointer.Brush.Color = 4259584
      Pointer.HorizSize = 2
      Pointer.InflateMargins = False
      Pointer.Style = psCircle
      Pointer.VertSize = 5
      Pointer.Visible = True
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesPDestruction: TPointSeries
      Marks.ArrowLength = 0
      Marks.Visible = False
      SeriesColor = clGrayText
      Title = 'First destruction'
      Pointer.Brush.Color = 8421440
      Pointer.HorizSize = 2
      Pointer.InflateMargins = True
      Pointer.Style = psCircle
      Pointer.VertSize = 5
      Pointer.Visible = True
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesPVaccination: TPointSeries
      Marks.ArrowLength = 0
      Marks.Visible = False
      SeriesColor = clBlue
      Title = 'First vaccination'
      Pointer.Brush.Color = clYellow
      Pointer.HorizSize = 2
      Pointer.InflateMargins = True
      Pointer.Style = psCircle
      Pointer.VertSize = 5
      Pointer.Visible = True
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object SeriesPOver: TPointSeries
      Marks.ArrowLength = 0
      Marks.Visible = False
      SeriesColor = clRed
      Title = 'Outbreak over'
      Pointer.HorizSize = 2
      Pointer.InflateMargins = True
      Pointer.Style = psCircle
      Pointer.VertSize = 5
      Pointer.Visible = True
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
  end
  object pnlCheckBoxes: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 427
    Align = alLeft
    TabOrder = 2
    object cbxExamined: TCheckBox
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Examined'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbxClick
    end
    object lineExamined: TPanel
      Left = 25
      Top = 24
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clDefault
      TabOrder = 1
    end
    object cbxTestFalseNeg: TCheckBox
      Left = 8
      Top = 200
      Width = 121
      Height = 17
      Caption = 'Test false neg'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbxClick
    end
    object cbxDestroyed: TCheckBox
      Left = 8
      Top = 72
      Width = 113
      Height = 17
      Caption = 'Destroyed'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbxClick
    end
    object lineTestFalseNeg: TPanel
      Left = 25
      Top = 216
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clPurple
      TabOrder = 4
    end
    object lineDestroyed: TPanel
      Left = 25
      Top = 88
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clTeal
      TabOrder = 5
    end
    object cbxDetected: TCheckBox
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Detected'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = cbxClick
    end
    object lineDetected: TPanel
      Left = 25
      Top = 56
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clLime
      TabOrder = 7
    end
    object cbxTestTrueNeg: TCheckBox
      Left = 8
      Top = 136
      Width = 113
      Height = 17
      Caption = 'Test true neg'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = cbxClick
    end
    object lineTestTrueNeg: TPanel
      Left = 25
      Top = 152
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clRed
      TabOrder = 9
    end
    object cbxTestFalsePos: TCheckBox
      Left = 8
      Top = 168
      Width = 121
      Height = 17
      Caption = 'Test false pos'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = cbxClick
    end
    object lineTestFalsePos: TPanel
      Left = 25
      Top = 184
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clAqua
      TabOrder = 11
    end
    object cbxTestTruePos: TCheckBox
      Left = 8
      Top = 104
      Width = 113
      Height = 17
      Caption = 'Test true pos'
      Checked = True
      State = cbChecked
      TabOrder = 12
      OnClick = cbxClick
    end
    object lineTestTruePos: TPanel
      Left = 25
      Top = 120
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 13
    end
    object cbxMilestones: TCheckBox
      Left = 8
      Top = 235
      Width = 81
      Height = 17
      Caption = 'Events'
      Checked = True
      State = cbChecked
      TabOrder = 14
      OnClick = cbxClick
    end
  end
end
