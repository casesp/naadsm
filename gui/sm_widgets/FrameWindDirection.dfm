object FrameWindDirection: TFrameWindDirection
  Left = 0
  Top = 0
  Width = 369
  Height = 208
  TabOrder = 0
  object lblWindRange: TLabel
    Left = 8
    Top = 8
    Width = 353
    Height = 33
    Caption = 
      'Area at risk of exposure around an infectious unit located at th' +
      'e center of the circle (0-360 degrees, 0/360 degrees = north, pr' +
      'oceeding clockwise):'
    WordWrap = True
  end
  object lblWindStart: TLabel
    Left = 12
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Start'
  end
  object lblWindEnd: TLabel
    Left = 149
    Top = 48
    Width = 19
    Height = 13
    Caption = 'End'
  end
  object lblWindDir: TLabel
    Left = 12
    Top = 78
    Width = 114
    Height = 39
    Caption = 'Area at risk of exposure (blue indicates affected area):'
    WordWrap = True
  end
  object pbxWindDir: TPaintBox
    Left = 144
    Top = 78
    Width = 120
    Height = 120
    OnPaint = pbxWindDirPaint
  end
  object rleWindStart: TREEdit
    Left = 64
    Top = 44
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
    Top = 44
    Width = 65
    Height = 21
    EditAlign = eaLeft
    TabOrder = 1
    OnExit = processText
    OnKeyUp = endAngleDataEntry
  end
end
