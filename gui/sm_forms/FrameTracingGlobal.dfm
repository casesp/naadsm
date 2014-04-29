object FrameTracingGlobal: TFrameTracingGlobal
  Left = 0
  Top = 0
  Width = 617
  Height = 157
  TabOrder = 0
  object pnlTracingGlobal: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 157
    Align = alClient
    TabOrder = 0
    object pnlUseTracingGlobal: TPanel
      Left = 1
      Top = 1
      Width = 615
      Height = 64
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object cbxUseTracing: TCheckBox
        Left = 16
        Top = 8
        Width = 529
        Height = 49
        Caption = 
          'Conduct trace-forward (trace-out) investigations for some or all' +
          ' production types'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        WordWrap = True
        OnClick = cbxUseTracingClick
      end
    end
  end
end
