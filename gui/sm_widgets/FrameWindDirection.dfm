object FrameWindDirection: TFrameWindDirection
  Left = 0
  Top = 0
  Width = 289
  Height = 225
  TabOrder = 0
  object lblWindRange: TLabel
    Left = 0
    Top = 8
    Width = 284
    Height = 41
    Caption = 
      'Range of wind direction (0-360 degrees,  0 degrees = north, proc' +
      'eeding clockwise):'
    WordWrap = True
  end
  object lblWindStart: TLabel
    Left = 12
    Top = 64
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object lblWindEnd: TLabel
    Left = 149
    Top = 64
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object lblWindDir: TLabel
    Left = 12
    Top = 102
    Width = 117
    Height = 91
    Caption = 'Wind direction (blue indicates selected range):'
    WordWrap = True
  end
  object pbxWindDir: TPaintBox
    Left = 144
    Top = 102
    Width = 120
    Height = 120
    OnPaint = pbxWindDirPaint
  end
  object rleWindStart: TREEdit
    Left = 64
    Top = 60
    Width = 65
    Height = 21
    EditAlign = eaLeft
    TabOrder = 0
    OnChange = pbxWindDirPaint
    OnExit = processText
    OnKeyUp = startAngleDataEntry
  end
  object rleWindEnd: TREEdit
    Left = 200
    Top = 60
    Width = 65
    Height = 21
    EditAlign = eaLeft
    TabOrder = 1
    OnExit = processText
    OnKeyUp = endAngleDataEntry
  end
end
