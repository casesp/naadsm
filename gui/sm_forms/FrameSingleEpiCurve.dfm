inherited FrameSingleEpiCurve: TFrameSingleEpiCurve
  Width = 460
  object chtCurve: TChart
    Left = 0
    Top = 0
    Width = 460
    Height = 240
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'Apparent Epidemic Curve')
    BottomAxis.Title.Caption = 'Days since start of iteration'
    LeftAxis.Title.Caption = 'Number of case-units'
    Legend.Visible = False
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object series: TBarSeries
      Marks.ArrowLength = 20
      Marks.Visible = False
      SeriesColor = clBlue
      Title = 'seriesApparent'
      BarWidthPercent = 100
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Bar'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
  end
end
