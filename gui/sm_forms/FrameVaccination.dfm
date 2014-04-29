object FrameVaccination: TFrameVaccination
  Left = 0
  Top = 0
  Width = 462
  Height = 354
  TabOrder = 0
  object pnlRingVacc: TPanel
    Left = 0
    Top = 265
    Width = 462
    Height = 89
    Align = alClient
    TabOrder = 0
    object lblVaccRingRadius: TLabel
      Left = 24
      Top = 52
      Width = 149
      Height = 13
      Caption = 'Radius of vaccination ring (km):'
    end
    object cbxRingVacc: TCheckBox
      Left = 16
      Top = 8
      Width = 417
      Height = 33
      Caption = 
        'Trigger a vaccination ring upon disease detection in units of th' +
        'is production type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      WordWrap = True
      OnClick = processClick
    end
    object rleVaccRingRadius: TREEdit
      Left = 360
      Top = 52
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processTextEntry
    end
  end
  object pnlVaccination: TPanel
    Left = 0
    Top = 0
    Width = 462
    Height = 265
    Align = alTop
    TabOrder = 1
    object pnlUseVacc: TPanel
      Left = 1
      Top = 1
      Width = 460
      Height = 48
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object cbxVaccinate: TCheckBox
        Left = 16
        Top = 8
        Width = 385
        Height = 33
        Caption = 
          'Vaccinate units of this production type as part of disease contr' +
          'ol efforts'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        WordWrap = True
        OnClick = processClick
      end
    end
    object pnlVaccParams: TPanel
      Left = 1
      Top = 49
      Width = 460
      Height = 215
      Align = alClient
      BevelOuter = bvNone
      Constraints.MinWidth = 460
      TabOrder = 1
      object lblVaccImmunePeriod: TLabel
        Left = 48
        Top = 8
        Width = 113
        Height = 13
        Caption = 'Vaccine immune period:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblDaysToImmunity: TLabel
        Left = 24
        Top = 88
        Width = 237
        Height = 13
        Caption = 'Delay in unit immunity following vaccination (days):'
      end
      object lblMinTimeBetwVacc: TLabel
        Left = 24
        Top = 120
        Width = 300
        Height = 39
        Caption = 
          'Minimum time that must elapse after vaccination of a unit of thi' +
          's production type before that unit can be queued for another vac' +
          'cination (days):'
        WordWrap = True
      end
      object imgPdf: TImage
        Left = 24
        Top = 8
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
      inline smcVaccImmunePeriod: TFrameSMFunctionEditor
        Left = 32
        Top = 24
        Width = 409
        Height = 50
        Constraints.MinHeight = 50
        Constraints.MinWidth = 390
        TabOrder = 0
      end
      object rleDaysToImmunity: TREEdit
        Left = 360
        Top = 88
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = processTextEntry
      end
      object rleMinTimeBetwVacc: TREEdit
        Left = 360
        Top = 128
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 2
        OnExit = processTextEntry
      end
      object cbxVaccinateDetected: TCheckBox
        Left = 24
        Top = 176
        Width = 409
        Height = 33
        Caption = 
          'Vaccinate detected units of this production type when they occur' +
          ' in vaccination rings'
        TabOrder = 3
        WordWrap = True
        OnClick = processClick
      end
    end
  end
end
