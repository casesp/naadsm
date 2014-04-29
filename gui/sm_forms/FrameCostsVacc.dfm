object FrameCostsVacc: TFrameCostsVacc
  Left = 0
  Top = 0
  Width = 393
  Height = 401
  TabOrder = 0
  object pnlCostParams: TPanel
    Left = 32
    Top = 8
    Width = 353
    Height = 209
    TabOrder = 0
    object lblVaccAdditionalPerAnimal: TLabel
      Left = 16
      Top = 144
      Width = 241
      Height = 41
      Caption = 
        'Additional cost for each animal vaccinated beyond the threshold ' +
        '(per animal):'
      WordWrap = True
    end
    object lblVaccSetupPerUnit: TLabel
      Left = 16
      Top = 12
      Width = 128
      Height = 13
      Caption = 'Cost of site setup (per unit):'
    end
    object lblVaccBaselinePerAnimal: TLabel
      Left = 16
      Top = 60
      Width = 181
      Height = 13
      Caption = 'Baseline vaccination cost (per animal):'
    end
    object lblVaccThreshold: TLabel
      Left = 16
      Top = 96
      Width = 249
      Height = 33
      Caption = 
        'Number of animals that may be vaccinated before the cost increas' +
        'es:'
      WordWrap = True
    end
    object lblDollars6: TLabel
      Left = 264
      Top = 12
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars7: TLabel
      Left = 264
      Top = 60
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars8: TLabel
      Left = 264
      Top = 148
      Width = 6
      Height = 13
      Caption = '$'
    end
    object rleVaccAdditionalPerAnimal: TREEdit
      Left = 272
      Top = 144
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 3
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleVaccSetupPerUnit: TREEdit
      Left = 272
      Top = 8
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 0
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleVaccBaselinePerAnimal: TREEdit
      Left = 272
      Top = 56
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleVaccThreshold: TREEdit
      Left = 272
      Top = 96
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 2
      OnExit = processTextEntry
    end
  end
  object pnlNoDestruction: TPanel
    Left = 32
    Top = 232
    Width = 321
    Height = 169
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lblNoDestruction: TLabel
      Left = 56
      Top = 40
      Width = 339
      Height = 13
      Caption = '(Vaccination is not used with the selected production type.)'
    end
  end
end
