inherited FrameSummaryEpiCurves: TFrameSummaryEpiCurves
  Width = 699
  Height = 422
  object pnlSummaryEpiCurveOptions: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 422
    Align = alLeft
    TabOrder = 0
    object pnlPercentileOptions: TPanel
      Left = 1
      Top = 49
      Width = 111
      Height = 209
      Align = alTop
      TabOrder = 0
      object cbx95: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 17
        Caption = '95th percentile'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = redrawSeries
      end
      object line95: TPanel
        Left = 24
        Top = 24
        Width = 73
        Height = 2
        BevelOuter = bvNone
        Color = clRed
        TabOrder = 1
      end
      object cbx75: TCheckBox
        Left = 8
        Top = 40
        Width = 97
        Height = 17
        Caption = '75th percentile'
        TabOrder = 2
        OnClick = redrawSeries
      end
      object line75: TPanel
        Left = 24
        Top = 55
        Width = 73
        Height = 2
        BevelOuter = bvNone
        Color = clYellow
        TabOrder = 3
      end
      object cbx50: TCheckBox
        Left = 8
        Top = 72
        Width = 97
        Height = 25
        Caption = '50th percentile (median)'
        Checked = True
        State = cbChecked
        TabOrder = 4
        WordWrap = True
        OnClick = redrawSeries
      end
      object line50: TPanel
        Left = 24
        Top = 98
        Width = 73
        Height = 2
        BevelOuter = bvNone
        Color = clGreen
        TabOrder = 5
      end
      object cbxMean: TCheckBox
        Left = 8
        Top = 112
        Width = 73
        Height = 17
        Caption = 'Mean'
        TabOrder = 6
        OnClick = redrawSeries
      end
      object lineMean: TPanel
        Left = 24
        Top = 126
        Width = 73
        Height = 2
        BevelOuter = bvNone
        Color = clFuchsia
        TabOrder = 7
      end
      object cbx25: TCheckBox
        Left = 8
        Top = 144
        Width = 89
        Height = 17
        Caption = '25th percentile'
        TabOrder = 8
        OnClick = redrawSeries
      end
      object cbx5: TCheckBox
        Left = 8
        Top = 176
        Width = 89
        Height = 17
        Caption = '5th percentile'
        Checked = True
        State = cbChecked
        TabOrder = 9
        OnClick = redrawSeries
      end
      object line25: TPanel
        Left = 24
        Top = 159
        Width = 73
        Height = 2
        BevelOuter = bvNone
        Color = clOlive
        TabOrder = 10
      end
      object line5: TPanel
        Left = 24
        Top = 191
        Width = 73
        Height = 2
        BevelOuter = bvNone
        Color = clBlue
        TabOrder = 11
      end
    end
    object pnlCumulative: TPanel
      Left = 1
      Top = 1
      Width = 111
      Height = 48
      Align = alTop
      TabOrder = 1
      object cbxCumulative: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 33
        Caption = 'Cumulative data'
        TabOrder = 0
        WordWrap = True
        OnClick = redrawSeries
      end
    end
    object pnl3D: TPanel
      Left = 1
      Top = 258
      Width = 111
      Height = 163
      Align = alClient
      TabOrder = 2
      object cbx3D: TCheckBox
        Left = 8
        Top = 8
        Width = 89
        Height = 25
        Caption = '3-dimensional display'
        TabOrder = 0
        WordWrap = True
      end
    end
  end
  object chtSummaryEpiCurves: TChart
    Left = 113
    Top = 0
    Width = 586
    Height = 422
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'TChart')
    BottomAxis.Title.Caption = 'Simulation day'
    LeftAxis.Title.Caption = 'Number of infected units'
    Legend.Visible = False
    View3D = False
    Align = alClient
    TabOrder = 1
    object ser95: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clRed
      Title = '95th percentile'
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
    object ser75: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clYellow
      Title = '75th percentile'
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
    object ser50: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clGreen
      Title = '50th percentile'
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
    object serMean: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clFuchsia
      Title = 'Mean'
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
    object ser25: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clOlive
      Title = '25th percentile'
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
    object ser5: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clBlue
      Title = '5th percentile'
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
  end
end
