object FrameTracing: TFrameTracing
  Left = 0
  Top = 0
  Width = 565
  Height = 430
  TabOrder = 0
  object pnlTraceDirectParams: TPanel
    Left = 0
    Top = 177
    Width = 565
    Height = 67
    Align = alTop
    TabOrder = 0
    object lblSurvDirectTracePeriod: TLabel
      Left = 24
      Top = 28
      Width = 107
      Height = 26
      Caption = 'Days before detection (critical period):'
      WordWrap = True
    end
    object lblSurvDirectSuccess: TLabel
      Left = 232
      Top = 28
      Width = 90
      Height = 26
      Caption = 'Probability of trace success (0 to 1):'
      WordWrap = True
    end
    object lblDirectContactParams: TLabel
      Left = 8
      Top = 8
      Width = 184
      Height = 13
      Caption = 'For tracing of DIRECT contacts:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object rleSurvDirectTracePeriod: TREEdit
      Left = 160
      Top = 28
      Width = 57
      Height = 21
      EditAlign = eaLeft
      Enabled = False
      TabOrder = 0
      OnExit = processTextEntry
    end
    object rleSurvDirectSuccess: TREEdit
      Left = 344
      Top = 28
      Width = 57
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processTextEntry
    end
  end
  object pnlTraceIndirectParams: TPanel
    Left = 0
    Top = 244
    Width = 565
    Height = 66
    Align = alTop
    TabOrder = 1
    object lblIndirectContactParams: TLabel
      Left = 8
      Top = 8
      Width = 197
      Height = 13
      Caption = 'For tracing of INDIRECT contacts:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSurvIndirectTracePeriod: TLabel
      Left = 24
      Top = 28
      Width = 107
      Height = 26
      Caption = 'Days before detection (critical period):'
      WordWrap = True
    end
    object lblSurvIndirectSuccess: TLabel
      Left = 232
      Top = 28
      Width = 90
      Height = 26
      Caption = 'Probability of trace success (0 to 1):'
      WordWrap = True
    end
    object rleSurvIndirectTracePeriod: TREEdit
      Left = 160
      Top = 28
      Width = 57
      Height = 21
      EditAlign = eaLeft
      TabOrder = 0
      OnExit = processTextEntry
    end
    object rleSurvIndirectSuccess: TREEdit
      Left = 344
      Top = 28
      Width = 57
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processTextEntry
    end
  end
  object pnlTracingDelayParams: TPanel
    Left = 0
    Top = 310
    Width = 565
    Height = 120
    Align = alClient
    TabOrder = 2
    object imgPdf: TImage
      Left = 24
      Top = 33
      Width = 16
      Height = 16
      Hint = 'This parameter is a probability density function'
      ParentShowHint = False
      Picture.Data = {
        07544269746D617036030000424D360300000000000036000000280000001000
        000010000000010018000000000000030000120B0000120B0000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000E5E5E5E8E8E8F0F1F1E9F4F5E7F5F5E7F5F5E7F5F5E7F5F5E7F5F5
        E7F5F5E8F4F5EFF1F2E8E8E8E4E4E4000000000000A6A6A6717575602C2FA519
        1BB02023AD2023AD2023AD2023AD2023AF2023A91A1C63282B6A6E6EA2A2A200
        0000000000FFFFFFFDFEFECDC7C8A22D2DFE0000FF0000FF0000FF0000FF0000
        FF0000A72223C8C0BFFBFDFDFFFFFF000000000000FFFFFFFFFFFFFFFFFFCEDC
        DCA81718FF0000FF0000FF0000FF0000B11112C4D0D1FFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFA58787E40000FF0000FF0000E90000
        9C7273FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFD4E0E0AE0C0DFF0000FF0000BB0808CDD4D4FFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFF6FFFF9F4C4DFB0000FE0000A33B3C
        F2FEFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFB2A4A5D40000DA0000A59090FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFF6F69D62639B5859EBF1F1
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFAFFFFF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000}
      ShowHint = True
    end
    object lblTracingDelay: TLabel
      Left = 48
      Top = 36
      Width = 192
      Height = 13
      Caption = 'Delay for carrying out trace investigation:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblAllTraceParams: TLabel
      Left = 8
      Top = 8
      Width = 161
      Height = 13
      Caption = 'For ANY trace investigation:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    inline smcTracingDelay: TFrameSMFunctionEditor
      Left = 32
      Top = 55
      Width = 425
      Height = 50
      Constraints.MinHeight = 50
      Constraints.MinWidth = 410
      TabOrder = 0
    end
  end
  object pnlTraceTypes: TPanel
    Left = 0
    Top = 0
    Width = 565
    Height = 177
    Align = alTop
    TabOrder = 3
    object cbxTraceDirectForward: TCheckBox
      Left = 8
      Top = 8
      Width = 521
      Height = 33
      Caption = 
        'Conduct TRACE FORWARD investigations to search for DIRECT contac' +
        'ts where the reported unit was of this production type and was t' +
        'he source of contact'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      WordWrap = True
      OnClick = processClick
    end
    object cbxTraceIndirectForward: TCheckBox
      Left = 8
      Top = 48
      Width = 521
      Height = 33
      Caption = 
        'Conduct TRACE FORWARD investigations to search for INDIRECT cont' +
        'acts where the reported unit was the SOURCE of contact and was o' +
        'f this production type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      WordWrap = True
      OnClick = processClick
    end
    object cbxTraceDirectBack: TCheckBox
      Left = 8
      Top = 88
      Width = 521
      Height = 33
      Caption = 
        'Conduct TRACE BACK investigations to search for DIRECT contacts ' +
        'where the reported unit was the RECIPIENT of contact and was of ' +
        'this production type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      WordWrap = True
      OnClick = processClick
    end
    object cbxTraceIndirectBack: TCheckBox
      Left = 8
      Top = 128
      Width = 521
      Height = 33
      Caption = 
        'Conduct TRACE BACK investigations to search for INDIRECT contact' +
        's where the reported unit was the RECIPIENT of contact and was o' +
        'f this production type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      WordWrap = True
      OnClick = processClick
    end
  end
end
