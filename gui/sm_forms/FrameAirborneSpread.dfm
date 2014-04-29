object FrameAirborneSpread: TFrameAirborneSpread
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
    object lblMaxSpread: TLabel
      Left = 24
      Top = 44
      Width = 269
      Height = 26
      Caption = 
        'Maximum distance spread under these conditions (in km, minimum 1' +
        ' km):'
      WordWrap = True
    end
    object lblProbSpread1km: TLabel
      Left = 24
      Top = 8
      Width = 297
      Height = 26
      Caption = 
        'Probability of spread/contg. day, at 1 km, average unit sizes (0' +
        ' to 1):'
      WordWrap = True
    end
    object lblTransportDelay: TLabel
      Left = 48
      Top = 332
      Width = 114
      Height = 13
      Caption = 'Airborne transport delay:'
      Transparent = False
    end
    object imgPDF: TImage
      Left = 24
      Top = 332
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
    object lblUseExponential: TLabel
      Left = 40
      Top = 0
      Width = 291
      Height = 26
      Caption = 
        '(The rate of disease declines exponentially from the source: a m' +
        'aximum distance of spread is not required.) '
      Visible = False
      WordWrap = True
    end
    object rleProbSpread1km: TREEdit
      Left = 336
      Top = 8
      Width = 57
      Height = 21
      EditAlign = eaLeft
      TabOrder = 0
      OnExit = processText
      OnKeyDown = rleKeyDown
    end
    object rleMaxSpread: TREEdit
      Left = 336
      Top = 44
      Width = 57
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processText
      OnKeyDown = rleKeyDown
    end
    inline fraWindDir: TFrameWindDirection
      Left = 48
      Top = 80
      Width = 305
      Height = 225
      TabOrder = 2
      inherited rleWindStart: TREEdit
        OnExit = processWindDirText
      end
      inherited rleWindEnd: TREEdit
        OnExit = processWindDirText
      end
    end
    inline smcTransportDelay: TFrameSMFunctionEditor
      Left = 32
      Top = 352
      Width = 401
      Height = 50
      Constraints.MinHeight = 50
      Constraints.MinWidth = 390
      TabOrder = 3
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
    object cbxUseAirborneSpread: TCheckBox
      Left = 8
      Top = 2
      Width = 441
      Height = 25
      Caption = 'Model airborne spread between these production types'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = cbxUseAirborneSpreadClick
    end
  end
end
