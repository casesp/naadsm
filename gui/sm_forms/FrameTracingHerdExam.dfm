object FrameTracingHerdExam: TFrameTracingHerdExam
  Left = 0
  Top = 0
  Width = 565
  Height = 430
  TabOrder = 0
  object lblForwardDirectMultiplier: TLabel
    Left = 48
    Top = 62
    Width = 253
    Height = 13
    Caption = 'Multiplier for the probability of observing clinical signs: '
    WordWrap = True
  end
  object lblForwardIndirectMultiplier: TLabel
    Left = 48
    Top = 142
    Width = 253
    Height = 13
    Caption = 'Multiplier for the probability of observing clinical signs: '
    WordWrap = True
  end
  object lblBackDirectMultiplier: TLabel
    Left = 48
    Top = 230
    Width = 253
    Height = 13
    Caption = 'Multiplier for the probability of observing clinical signs: '
    WordWrap = True
  end
  object lblBackIndirectMultiplier: TLabel
    Left = 48
    Top = 318
    Width = 253
    Height = 13
    Caption = 'Multiplier for the probability of observing clinical signs: '
    WordWrap = True
  end
  object cbxForwardDirectExam: TCheckBox
    Left = 24
    Top = 16
    Width = 489
    Height = 41
    Caption = 
      'Examine units identified by TRACE-FORWARD of DIRECT contact for ' +
      'clinical signs of disease'
    TabOrder = 0
    WordWrap = True
    OnClick = processClick
  end
  object cbxForwardIndirectExam: TCheckBox
    Left = 24
    Top = 96
    Width = 489
    Height = 41
    Caption = 
      'Examine units identified by TRACE-FORWARD of INDIRECT contact fo' +
      'r clinical signs of disease'
    TabOrder = 1
    WordWrap = True
    OnClick = processClick
  end
  object cbxBackDirectExam: TCheckBox
    Left = 24
    Top = 176
    Width = 489
    Height = 41
    Caption = 
      'Examine units identified by TRACE-BACK of DIRECT contact for cli' +
      'nical signs of disease'
    TabOrder = 2
    WordWrap = True
    OnClick = processClick
  end
  object cbxBackIndirectExam: TCheckBox
    Left = 24
    Top = 264
    Width = 489
    Height = 41
    Caption = 
      'Examine units identified by TRACE-BACK of INDIRECT contact for c' +
      'linical signs of disease'
    TabOrder = 3
    WordWrap = True
    OnClick = processClick
  end
  object rleForwardDirectMultiplier: TREEdit
    Left = 328
    Top = 60
    Width = 65
    Height = 21
    EditAlign = eaLeft
    TabOrder = 4
    OnExit = processTextEntry
  end
  object rleForwardIndirectMultiplier: TREEdit
    Left = 328
    Top = 140
    Width = 65
    Height = 21
    EditAlign = eaLeft
    TabOrder = 5
    OnExit = processTextEntry
  end
  object rleBackDirectMultiplier: TREEdit
    Left = 328
    Top = 228
    Width = 65
    Height = 21
    EditAlign = eaLeft
    TabOrder = 6
    OnExit = processTextEntry
  end
  object rleBackIndirectMultiplier: TREEdit
    Left = 328
    Top = 316
    Width = 65
    Height = 21
    EditAlign = eaLeft
    TabOrder = 7
    OnExit = processTextEntry
  end
end
