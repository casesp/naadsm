object FrameTracingTesting: TFrameTracingTesting
  Left = 0
  Top = 0
  Width = 565
  Height = 430
  TabOrder = 0
  object pnlTestingOptions: TPanel
    Left = 0
    Top = 0
    Width = 565
    Height = 225
    Align = alTop
    TabOrder = 0
    object lblTracingNote: TLabel
      Left = 16
      Top = 200
      Width = 365
      Height = 13
      Caption = 
        '* Unit exams must be conducted for this type, or this option wil' +
        'l be unavailable'
      WordWrap = True
    end
    object cbxTestDirectForward: TCheckBox
      Left = 16
      Top = 8
      Width = 513
      Height = 41
      Caption = 
        'Perform diagnostic testing of units identified by TRACE-FORWARD ' +
        'of DIRECT contact*'
      TabOrder = 0
      WordWrap = True
      OnClick = processClick
    end
    object cbxTestIndirectForward: TCheckBox
      Left = 16
      Top = 48
      Width = 513
      Height = 41
      Caption = 
        'Perform diagnostic testing of units identified by TRACE-FORWARD ' +
        'of INDIRECT contact*'
      TabOrder = 1
      WordWrap = True
      OnClick = processClick
    end
    object cbxTestDirectBack: TCheckBox
      Left = 16
      Top = 88
      Width = 513
      Height = 41
      Caption = 
        'Perform diagnostic testing of units identified by TRACE-BACK of ' +
        'DIRECT contact*'
      TabOrder = 2
      WordWrap = True
      OnClick = processClick
    end
    object cbxTestIndirectBack: TCheckBox
      Left = 16
      Top = 128
      Width = 513
      Height = 41
      Caption = 
        'Perform diagnostic testing of units identified by TRACE-BACK of ' +
        'INDIRECT contact*'
      TabOrder = 3
      WordWrap = True
      OnClick = processClick
    end
  end
  object pnlTestCharacteristics: TPanel
    Left = 0
    Top = 225
    Width = 565
    Height = 205
    Align = alClient
    TabOrder = 1
    object imgPdf: TImage
      Left = 16
      Top = 81
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
    object lblTestingDelay: TLabel
      Left = 40
      Top = 84
      Width = 137
      Height = 13
      Caption = 'Delay in obtaining test results'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblSensitivity: TLabel
      Left = 16
      Top = 14
      Width = 161
      Height = 19
      Caption = 'Unit-level test sensitivity:'
      WordWrap = True
    end
    object lblSpecificity: TLabel
      Left = 16
      Top = 46
      Width = 169
      Height = 19
      Caption = 'Unit-level test specificity:'
      WordWrap = True
    end
    inline smcTestingDelay: TFrameSMFunctionEditor
      Left = 24
      Top = 103
      Width = 425
      Height = 50
      Constraints.MinHeight = 50
      Constraints.MinWidth = 410
      TabOrder = 0
    end
    object rleSensitivity: TREEdit
      Left = 192
      Top = 12
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processTextEntry
    end
    object rleSpecificity: TREEdit
      Left = 192
      Top = 44
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 2
      OnExit = processTextEntry
    end
  end
end
