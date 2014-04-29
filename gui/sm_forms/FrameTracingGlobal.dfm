object FrameTracingGlobal: TFrameTracingGlobal
  Left = 0
  Top = 0
  Width = 570
  Height = 430
  TabOrder = 0
  object pnlTracingGlobal: TPanel
    Left = 0
    Top = 0
    Width = 570
    Height = 430
    Align = alClient
    TabOrder = 0
    object pnlUseTracingGlobal: TPanel
      Left = 1
      Top = 1
      Width = 568
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object cbxUseTracing: TCheckBox
        Left = 16
        Top = 8
        Width = 457
        Height = 25
        Caption = 'Conduct tracing for some or all production types'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = cbxUseTracingClick
      end
    end
    object cbxUseHerdExam: TCheckBox
      Left = 24
      Top = 40
      Width = 457
      Height = 25
      Caption = 'Examine some or all traced units for clinical signs of disease'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = cbxUseHerdExamClick
    end
    object cbxUseTesting: TCheckBox
      Left = 24
      Top = 72
      Width = 457
      Height = 25
      Caption = 'Perform diagnostic testing for some or all traced herds'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = cbxUseTestingClick
    end
  end
end
