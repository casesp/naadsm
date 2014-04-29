object FrameAirborneOrLocalAreaSpread: TFrameAirborneOrLocalAreaSpread
  Left = 0
  Top = 0
  Width = 441
  Height = 440
  TabOrder = 0
  object pnlParams: TPanel
    Left = 0
    Top = 28
    Width = 441
    Height = 412
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    object pnlSizeParams: TPanel
      Left = 0
      Top = 0
      Width = 441
      Height = 73
      Align = alTop
      TabOrder = 0
      object lblNInfectiousInSource: TLabel
        Left = 24
        Top = 8
        Width = 272
        Height = 26
        Caption = 
          'Number of infectious (subclinical and clinical) animals in a sou' +
          'rce unit:'
        WordWrap = True
      end
      object lblNSusceptibleInRecipient: TLabel
        Left = 24
        Top = 48
        Width = 229
        Height = 13
        Caption = 'Number of susceptible animals in a recipient unit:'
        WordWrap = True
      end
      object reNInfectiousInSource: TREEdit
        Left = 344
        Top = 8
        Width = 57
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = processText
      end
      object reNSusceptibleInRecipient: TREEdit
        Left = 344
        Top = 48
        Width = 57
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = processText
      end
    end
    object pnlOtherParams: TPanel
      Left = 0
      Top = 73
      Width = 441
      Height = 88
      Align = alTop
      TabOrder = 1
      object lblDistBetwUnits: TLabel
        Left = 24
        Top = 16
        Width = 197
        Height = 13
        Caption = 'Distance (in km) between these two units:'
        WordWrap = True
      end
      object lblProbSpread: TLabel
        Left = 24
        Top = 55
        Width = 285
        Height = 26
        Caption = 
          'Daily probability of xyz spread of disease between these two uni' +
          'ts:'
        WordWrap = True
      end
      object reDistBetwUnits: TREEdit
        Left = 344
        Top = 16
        Width = 57
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = processText
      end
      object reProbSpread: TREEdit
        Left = 320
        Top = 60
        Width = 81
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = processText
      end
    end
  end
  object pnlUseAirborneSpread: TPanel
    Left = 0
    Top = 0
    Width = 441
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object cbxUseSpread: TCheckBox
      Left = 8
      Top = 2
      Width = 441
      Height = 25
      Caption = 'Model xyz spread between these production types'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = cbxUseSpreadClick
    end
  end
end
