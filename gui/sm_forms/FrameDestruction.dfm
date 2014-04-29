object FrameDestruction: TFrameDestruction
  Left = 0
  Top = 0
  Width = 565
  Height = 430
  TabOrder = 0
  object lblDestrRingRadius: TLabel
    Left = 24
    Top = 136
    Width = 79
    Height = 13
    Caption = 'Ring radius (km):'
  end
  object lblBasicDestrNote: TLabel
    Left = 24
    Top = 40
    Width = 274
    Height = 13
    Caption = '(Units of this production type will be destroyed if detected) '
  end
  object lblTriggerNote: TLabel
    Left = 24
    Top = 96
    Width = 441
    Height = 26
    Caption = 
      '(Units of this and/or other types may be pre-emptively destroyed' +
      ' if they are within the specified ring) '
    WordWrap = True
  end
  object lblTracingNote: TLabel
    Left = 24
    Top = 408
    Width = 292
    Height = 13
    Caption = '* Tracing must be conducted, or this option will be unavailable'
    WordWrap = True
  end
  object lblPreempt: TLabel
    Left = 8
    Top = 176
    Width = 308
    Height = 13
    Caption = 'Pre-emptive destuction of units of this production type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cbxDestrBasic: TCheckBox
    Left = 8
    Top = 8
    Width = 553
    Height = 33
    Caption = 'Destroy detected units of this production type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = processClick
  end
  object cbxDestrDirectForward: TCheckBox
    Left = 24
    Top = 200
    Width = 521
    Height = 33
    Caption = 
      'Destroy units of this production type that have had DIRECT conta' +
      'ct with a detected unit as identified by TRACE FORWARD*'
    TabOrder = 3
    WordWrap = True
    OnClick = processClick
  end
  object cbxDestrIndirectForward: TCheckBox
    Left = 24
    Top = 240
    Width = 521
    Height = 33
    Caption = 
      'Destroy units of this production type that have had INDIRECT con' +
      'tact with a detected unit as identified by TRACE FORWARD*'
    TabOrder = 4
    WordWrap = True
    OnClick = processClick
  end
  object cbxDestrRingTarget: TCheckBox
    Left = 24
    Top = 360
    Width = 521
    Height = 33
    Caption = 
      'Destroy units of this type when they are within a destruction ri' +
      'ng around any unit that is a ring trigger'
    TabOrder = 7
    WordWrap = True
    OnClick = processClick
  end
  object rleDestrRingRadius: TREEdit
    Left = 160
    Top = 136
    Width = 57
    Height = 21
    EditAlign = eaLeft
    TabOrder = 2
    OnExit = processTextEntry
  end
  object cbxDestrRingTrigger: TCheckBox
    Left = 8
    Top = 64
    Width = 545
    Height = 33
    Caption = 
      'Trigger ring destruction around detected units of this productio' +
      'n type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    WordWrap = True
    OnClick = processClick
  end
  object cbxDestrDirectBack: TCheckBox
    Left = 24
    Top = 280
    Width = 521
    Height = 33
    Caption = 
      'Destroy units of this production type that have had DIRECT conta' +
      'ct with a detected unit as identified by TRACE BACK*'
    TabOrder = 5
    WordWrap = True
    OnClick = processClick
  end
  object cbxDestrIndirectBack: TCheckBox
    Left = 24
    Top = 320
    Width = 521
    Height = 33
    Caption = 
      'Destroy units of this production type that have had INDIRECT con' +
      'tact with a detected unit as identified by TRACE BACK*'
    TabOrder = 6
    WordWrap = True
    OnClick = processClick
  end
end
