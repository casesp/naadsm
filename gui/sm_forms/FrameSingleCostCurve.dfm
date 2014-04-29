inherited FrameSingleCostCurve: TFrameSingleCostCurve
  Width = 685
  Height = 526
  Constraints.MinHeight = 350
  Constraints.MinWidth = 570
  object pnlCumulCosts: TPanel
    Left = 0
    Top = 0
    Width = 685
    Height = 25
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object cbxCumulCosts: TCheckBox
      Left = 8
      Top = 4
      Width = 185
      Height = 17
      Caption = 'Show cumulative costs'
      TabOrder = 0
      OnClick = cbxCumulCostsClick
    end
    object cbx3D: TCheckBox
      Left = 200
      Top = 4
      Width = 121
      Height = 17
      Caption = '3-D View'
      TabOrder = 1
      OnClick = cbx3DClick
    end
  end
  object pnlChart: TPanel
    Left = 0
    Top = 25
    Width = 685
    Height = 389
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object chtCosts: TChart
      Left = 0
      Top = 0
      Width = 685
      Height = 389
      BackWall.Brush.Color = clWhite
      BackWall.Brush.Style = bsClear
      Title.Text.Strings = (
        'Direct costs of outbreak, by day')
      BottomAxis.Title.Caption = 'Day of outbreak'
      LeftAxis.Title.Caption = 'Cost ($)'
      Legend.Alignment = laBottom
      Legend.Color = clSilver
      View3D = False
      Align = alClient
      BevelOuter = bvLowered
      TabOrder = 0
      Constraints.MinHeight = 150
      object serTotal: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clRed
        Title = 'Total costs'
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
      object serDestrSubtotal: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clGreen
        Title = 'Destruction subtotal'
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
      object serVaccSubtotal: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clYellow
        Title = 'Vaccination subtotal'
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
      object serAppraisal: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clBlue
        Title = 'Destr. appraisal'
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
      object serCAndD: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clOlive
        Title = 'Destr. C & D'
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
      object serEuthanasia: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = 16711808
        Title = 'Destr. euthanasia'
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
      object serIndemnification: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clFuchsia
        Title = 'Destr. indemnification'
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
      object serDisposal: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clTeal
        Title = 'Destr. disposal'
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
      object serVaccSetup: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clMaroon
        Title = 'Vacc. setup'
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
      object serVacc: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clAqua
        Title = 'Vacc. vaccination'
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
  object pnlCostCategories: TPanel
    Left = 0
    Top = 414
    Width = 685
    Height = 112
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 1
    object lblDestrCosts: TLabel
      Left = 8
      Top = 6
      Width = 149
      Height = 13
      Caption = 'Itemized destruction costs'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblVaccCosts: TLabel
      Left = 8
      Top = 42
      Width = 152
      Height = 13
      Caption = 'Itemized vaccination costs'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblInvisible: TLabel
      Left = 360
      Top = 52
      Width = 48
      Height = 13
      Caption = 'lblInvisible'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object Label1: TLabel
      Left = 8
      Top = 78
      Width = 36
      Height = 13
      Caption = 'Totals'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbxAppraisal: TCheckBox
      Left = 8
      Top = 20
      Width = 145
      Height = 17
      Caption = 'Appraisal'
      TabOrder = 0
      OnClick = cbxCategoryClicked
    end
    object cbxCAndD: TCheckBox
      Left = 152
      Top = 20
      Width = 145
      Height = 17
      Caption = 'Cleaning and disinfection'
      TabOrder = 1
      OnClick = cbxCategoryClicked
    end
    object cbxEuthanasia: TCheckBox
      Left = 304
      Top = 20
      Width = 129
      Height = 17
      Caption = 'Euthanasia'
      TabOrder = 2
      OnClick = cbxCategoryClicked
    end
    object cbxIndemnification: TCheckBox
      Left = 392
      Top = 20
      Width = 129
      Height = 17
      Caption = 'Indemnification'
      TabOrder = 3
      OnClick = cbxCategoryClicked
    end
    object cbxDisposal: TCheckBox
      Left = 496
      Top = 20
      Width = 65
      Height = 17
      Caption = 'Disposal'
      TabOrder = 4
      OnClick = cbxCategoryClicked
    end
    object cbxDestrSubtotal: TCheckBox
      Left = 8
      Top = 92
      Width = 145
      Height = 17
      Caption = 'Destruction subtotal'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 5
      OnClick = cbxCategoryClicked
    end
    object cbxVaccSetup: TCheckBox
      Left = 8
      Top = 56
      Width = 201
      Height = 17
      Caption = 'Site setup'
      TabOrder = 6
      OnClick = cbxCategoryClicked
    end
    object cbxVacc: TCheckBox
      Left = 152
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Vaccination'
      TabOrder = 7
      OnClick = cbxCategoryClicked
    end
    object cbxVaccSubtotal: TCheckBox
      Left = 152
      Top = 92
      Width = 145
      Height = 17
      Caption = 'Vaccination subtotal'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 8
      OnClick = cbxCategoryClicked
    end
    object cbxTotal: TCheckBox
      Left = 304
      Top = 92
      Width = 113
      Height = 17
      Caption = 'Total costs'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 9
      OnClick = cbxCategoryClicked
    end
    object lineDestrSubtotal: TPanel
      Left = 24
      Top = 106
      Width = 100
      Height = 2
      BevelOuter = bvNone
      Color = clGreen
      TabOrder = 10
    end
    object lineVaccSubtotal: TPanel
      Left = 168
      Top = 106
      Width = 104
      Height = 2
      BevelOuter = bvNone
      Color = clYellow
      TabOrder = 11
    end
    object lineTotal: TPanel
      Left = 320
      Top = 106
      Width = 60
      Height = 2
      BevelOuter = bvNone
      Color = clRed
      TabOrder = 12
    end
    object lineAppraisal: TPanel
      Left = 24
      Top = 34
      Width = 49
      Height = 2
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 13
    end
    object lineCAndD: TPanel
      Left = 168
      Top = 34
      Width = 121
      Height = 2
      BevelOuter = bvNone
      Color = clOlive
      TabOrder = 14
    end
    object lineEuthanasia: TPanel
      Left = 320
      Top = 34
      Width = 57
      Height = 2
      BevelOuter = bvNone
      Color = clNavy
      TabOrder = 15
    end
    object lineIndemnification: TPanel
      Left = 408
      Top = 34
      Width = 75
      Height = 2
      BevelOuter = bvNone
      Color = clFuchsia
      TabOrder = 16
    end
    object lineDisposal: TPanel
      Left = 512
      Top = 34
      Width = 45
      Height = 2
      BevelOuter = bvNone
      Color = clTeal
      TabOrder = 17
    end
    object lineVaccSetup: TPanel
      Left = 24
      Top = 70
      Width = 54
      Height = 2
      BevelOuter = bvNone
      Color = clMaroon
      TabOrder = 18
    end
    object lineVacc: TPanel
      Left = 168
      Top = 70
      Width = 65
      Height = 2
      BevelOuter = bvNone
      Color = clAqua
      TabOrder = 19
    end
  end
end
