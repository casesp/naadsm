object FrameTracing: TFrameTracing
  Left = 0
  Top = 0
  Width = 555
  Height = 308
  TabOrder = 0
  object lblSurvDirectTracePeriod: TLabel
    Left = 40
    Top = 52
    Width = 60
    Height = 26
    Caption = 'Days before detection:'
    WordWrap = True
  end
  object lblSurvIndirectTracePeriod: TLabel
    Left = 40
    Top = 116
    Width = 60
    Height = 26
    Caption = 'Days before detection:'
    WordWrap = True
  end
  object lblSurvDirectSuccess: TLabel
    Left = 216
    Top = 52
    Width = 90
    Height = 26
    Caption = 'Probability of trace success (0 to 1):'
    WordWrap = True
  end
  object lblSurvIndirectSuccess: TLabel
    Left = 216
    Top = 116
    Width = 90
    Height = 26
    Caption = 'Probability of trace success (0 to 1):'
    WordWrap = True
  end
  object lblTraceForward: TLabel
    Left = 8
    Top = 8
    Width = 225
    Height = 13
    Caption = 'Trace-forward (trace-out) investigations'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object rleSurvDirectTracePeriod: TREEdit
    Left = 144
    Top = 52
    Width = 57
    Height = 21
    EditAlign = eaLeft
    Enabled = False
    TabOrder = 0
    OnExit = processTextEntry
  end
  object rleSurvIndirectTracePeriod: TREEdit
    Left = 144
    Top = 116
    Width = 57
    Height = 21
    EditAlign = eaLeft
    TabOrder = 1
    OnExit = processTextEntry
  end
  object rleSurvDirectSuccess: TREEdit
    Left = 328
    Top = 52
    Width = 57
    Height = 21
    EditAlign = eaLeft
    TabOrder = 2
    OnExit = processTextEntry
    OnKeyDown = rleKeyDown
  end
  object rleSurvIndirectSuccess: TREEdit
    Left = 328
    Top = 116
    Width = 57
    Height = 21
    EditAlign = eaLeft
    TabOrder = 3
    OnExit = processTextEntry
    OnKeyDown = rleKeyDown
  end
  object cbxDirectTrace: TCheckBox
    Left = 24
    Top = 32
    Width = 313
    Height = 17
    Caption = 'Trace direct contacts'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = processClick
  end
  object cbxIndirectTrace: TCheckBox
    Left = 24
    Top = 96
    Width = 337
    Height = 17
    Caption = 'Trace indirect contacts'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = processClick
  end
end
