object FrameDestruction: TFrameDestruction
  Left = 0
  Top = 0
  Width = 485
  Height = 397
  TabOrder = 0
  object lblDestrRingRadius: TLabel
    Left = 24
    Top = 144
    Width = 79
    Height = 13
    Caption = 'Ring radius (km):'
  end
  object lblBasicDestrNote: TLabel
    Left = 24
    Top = 48
    Width = 319
    Height = 13
    Caption = 
      '(Diseased units of this production type will be destroyed if det' +
      'ected) '
  end
  object lblTriggerNote: TLabel
    Left = 24
    Top = 104
    Width = 441
    Height = 26
    Caption = 
      '(Units of this and/or other types may be pre-emptively destroyed' +
      ' if they are within the specified ring) '
    WordWrap = True
  end
  object lblTracingNote: TLabel
    Left = 24
    Top = 360
    Width = 349
    Height = 13
    Caption = 
      '* Tracing must be conducted for this type, or this option will b' +
      'e unavailable'
    WordWrap = True
  end
  object cbxDestrBasic: TCheckBox
    Left = 8
    Top = 8
    Width = 473
    Height = 33
    Caption = 'Destroy detected diseased units of this production type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = processClick
  end
  object cbxDestrPreempt: TCheckBox
    Left = 8
    Top = 176
    Width = 433
    Height = 17
    Caption = 'Pre-emptively destroy units of this production type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = processClick
  end
  object cbxDestrDirect: TCheckBox
    Left = 32
    Top = 208
    Width = 393
    Height = 33
    Caption = 
      'Destroy units of this production type that have had DIRECT conta' +
      'ct with a detected unit as identified by tracing*'
    TabOrder = 4
    WordWrap = True
    OnClick = processClick
  end
  object cbxDestrIndirect: TCheckBox
    Left = 32
    Top = 256
    Width = 393
    Height = 33
    Caption = 
      'Destroy units of this production type that have had INDIRECT con' +
      'tact with a detected unit as identified by tracing*'
    TabOrder = 5
    WordWrap = True
    OnClick = processClick
  end
  object cbxDestrRingTarget: TCheckBox
    Left = 32
    Top = 304
    Width = 401
    Height = 33
    Caption = 
      'Destroy units of this type when they are within a destruction ri' +
      'ng around any unit that is a ring trigger'
    TabOrder = 6
    WordWrap = True
    OnClick = processClick
  end
  object rleDestrRingRadius: TREEdit
    Left = 160
    Top = 144
    Width = 57
    Height = 21
    EditAlign = eaLeft
    TabOrder = 2
    OnExit = processTextEntry
    OnKeyDown = rleKeyDown
  end
  object cbxDestrRingTrigger: TCheckBox
    Left = 8
    Top = 72
    Width = 465
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
end
