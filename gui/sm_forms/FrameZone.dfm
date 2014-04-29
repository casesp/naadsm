object FrameZone: TFrameZone
  Left = 0
  Top = 0
  Width = 498
  Height = 637
  TabOrder = 0
  object pnlZoneTrigger: TPanel
    Left = 0
    Top = 0
    Width = 498
    Height = 105
    Align = alTop
    TabOrder = 0
    object lblZoneCreation: TLabel
      Left = 8
      Top = 8
      Width = 150
      Height = 13
      Caption = 'Creating (triggering) zones'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbxIndirectTraceTrigger: TCheckBox
      Left = 16
      Top = 72
      Width = 401
      Height = 25
      Caption = 'Indirect tracing of units of this type triggers a zone focus'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      WordWrap = True
      OnClick = processTriggerClick
    end
    object cbxDirectTraceTrigger: TCheckBox
      Left = 16
      Top = 48
      Width = 393
      Height = 25
      Caption = 'Direct tracing of units of this type triggers a zone focus'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      WordWrap = True
      OnClick = processTriggerClick
    end
    object cbxDetectionTrigger: TCheckBox
      Left = 16
      Top = 24
      Width = 369
      Height = 25
      Caption = 'Detection of infected units of this type triggers a  zone focus'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      WordWrap = True
      OnClick = processTriggerClick
    end
  end
  object pnlZoneEffects: TPanel
    Left = 0
    Top = 105
    Width = 498
    Height = 485
    Align = alTop
    TabOrder = 1
    object pnlZoneEffectsBody: TPanel
      Left = 1
      Top = 1
      Width = 496
      Height = 483
      Align = alClient
      TabOrder = 0
    end
  end
end
