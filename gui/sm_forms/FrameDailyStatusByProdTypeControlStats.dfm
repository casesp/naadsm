inherited FrameDailyStatusByProdTypeControlStats: TFrameDailyStatusByProdTypeControlStats
  Width = 728
  Height = 454
  object pnlChartOptions: TPanel
    Left = 0
    Top = 429
    Width = 728
    Height = 25
    Align = alBottom
    TabOrder = 0
    object cbxThreeD: TCheckBox
      Left = 8
      Top = 4
      Width = 105
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
    Height = 429
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      '')
    BottomAxis.Title.Caption = 'Day of simulation'
    LeftAxis.Title.Caption = 'Number of units, out of a total of xyz'
    Legend.Alignment = laBottom
    Legend.Color = clSilver
    Legend.ShadowColor = 4194368
    Align = alClient
    TabOrder = 1
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
    object SeriesVaccinated: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clYellow
      Title = 'Vaccinated'
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
    object SeriesTraceDirFwd: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clBlue
      Title = 'Traced fwd-Direct'
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
    object SeriesTraceIndFwd: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = 16744448
      Title = 'Traced fwd-Indirect'
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
    object SeriesTraceDirBack: TLineSeries
      Marks.ArrowLength = 0
      Marks.Frame.Visible = False
      Marks.Transparent = True
      Marks.Visible = False
      SeriesColor = clRed
      Title = 'Traced back-Direct'
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
    object SeriesTraceIndBack: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clFuchsia
      Title = 'Traced back-Indirect'
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
    Height = 429
    Align = alLeft
    TabOrder = 2
    object cbxTraceDirBack: TCheckBox
      Left = 8
      Top = 168
      Width = 121
      Height = 17
      Caption = 'Traced back-Direct'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbxClick
    end
    object cbxTraceIndBack: TCheckBox
      Left = 8
      Top = 200
      Width = 129
      Height = 17
      Caption = 'Traced back-Indirect'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbxClick
    end
    object cbxDestroyed: TCheckBox
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Destroyed'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbxClick
    end
    object lineTraceDirBack: TPanel
      Left = 25
      Top = 184
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clRed
      TabOrder = 3
    end
    object lineTraceIndBack: TPanel
      Left = 25
      Top = 216
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clFuchsia
      TabOrder = 4
    end
    object lineDestroyed: TPanel
      Left = 25
      Top = 56
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clTeal
      TabOrder = 5
    end
    object cbxDetected: TCheckBox
      Left = 8
      Top = 8
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
      Top = 24
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clLime
      TabOrder = 7
    end
    object cbxTraceDirFwd: TCheckBox
      Left = 8
      Top = 104
      Width = 113
      Height = 17
      Caption = 'Traced fwd-Direct'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = cbxClick
    end
    object lineTraceDirFwd: TPanel
      Left = 25
      Top = 120
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 9
    end
    object cbxTraceIndFwd: TCheckBox
      Left = 8
      Top = 136
      Width = 121
      Height = 17
      Caption = 'Traced fwd-Indirect'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = cbxClick
    end
    object lineTraceIndFwd: TPanel
      Left = 25
      Top = 152
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clGradientActiveCaption
      TabOrder = 11
    end
    object cbxVaccinated: TCheckBox
      Left = 8
      Top = 72
      Width = 113
      Height = 17
      Caption = 'Vaccinated'
      Checked = True
      State = cbChecked
      TabOrder = 12
      OnClick = cbxClick
    end
    object lineVaccinated: TPanel
      Left = 25
      Top = 88
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clYellow
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
