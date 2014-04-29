inherited FrameDailyZoneStatusByProdType: TFrameDailyZoneStatusByProdType
  Width = 523
  object pnlChartOptions: TPanel
    Left = 0
    Top = 215
    Width = 523
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
    end
  end
  object chtOutputs: TChart
    Left = 0
    Top = 0
    Width = 523
    Height = 215
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    MarginLeft = 5
    Title.Text.Strings = (
      '')
    BottomAxis.Title.Caption = 'Day of simulation'
    LeftAxis.Title.Caption = 'Y axis'
    Legend.Alignment = laBottom
    Legend.Color = clSilver
    Legend.ColorWidth = 15
    Align = alClient
    TabOrder = 1
  end
end
