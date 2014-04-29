inherited FrameDailyStatusByProdTypeDiseaseStats: TFrameDailyStatusByProdTypeDiseaseStats
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
      Width = 73
      Height = 17
      Caption = '3-D View'
      TabOrder = 0
      OnClick = cbxThreeDClick
    end
  end
  object chtOutputs: TChart
    Left = 129
    Top = 0
    Width = 599
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
    object SeriesSusc: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clBlack
      Title = 'Susceptible'
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
    object SeriesLatent: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clBlue
      Title = 'Latent'
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
    object SeriesSubClinical: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clFuchsia
      Title = 'Subclinical'
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
    object SeriesClinical: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clRed
      Title = 'Clinical'
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
    object SeriesNatImmune: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clLime
      Title = 'NatImmune'
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
    object SeriesVacImmune: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clAqua
      Title = 'VacImmune'
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
      Marks.ArrowLength = 0
      Marks.Frame.Visible = False
      Marks.Transparent = True
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
    object SeriesPDetection: TPointSeries
      Marks.ArrowLength = 0
      Marks.Frame.Visible = False
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
    Width = 129
    Height = 427
    Align = alLeft
    TabOrder = 2
    object cbxSusceptible: TCheckBox
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Susceptible'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbxClick
    end
    object lineSusceptible: TPanel
      Left = 25
      Top = 24
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clBlack
      TabOrder = 1
    end
    object cbxNatImmune: TCheckBox
      Left = 8
      Top = 136
      Width = 113
      Height = 17
      Caption = 'Nat immune'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbxClick
    end
    object cbxLatent: TCheckBox
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Latent'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbxClick
    end
    object cbxVacImmune: TCheckBox
      Left = 8
      Top = 168
      Width = 113
      Height = 17
      Caption = 'Vac immune'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = cbxClick
    end
    object cbxSubClinical: TCheckBox
      Left = 8
      Top = 72
      Width = 113
      Height = 17
      Caption = 'Subclinical'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = cbxClick
    end
    object cbxDestroyed: TCheckBox
      Left = 8
      Top = 200
      Width = 113
      Height = 17
      Caption = 'Destroyed'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = cbxClick
    end
    object cbxClinical: TCheckBox
      Left = 8
      Top = 104
      Width = 113
      Height = 17
      Caption = 'Clinical'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = cbxClick
    end
    object lineNatImmune: TPanel
      Left = 25
      Top = 152
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clLime
      TabOrder = 8
    end
    object lineLatent: TPanel
      Left = 25
      Top = 56
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 9
    end
    object lineVacImmune: TPanel
      Left = 25
      Top = 184
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clAqua
      TabOrder = 10
    end
    object lineSubclinical: TPanel
      Left = 25
      Top = 88
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clFuchsia
      TabOrder = 11
    end
    object lineDestroyed: TPanel
      Left = 25
      Top = 216
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clTeal
      TabOrder = 12
    end
    object lineClinical: TPanel
      Left = 25
      Top = 120
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clRed
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
