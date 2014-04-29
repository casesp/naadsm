object FrameCosts: TFrameCosts
  Left = 0
  Top = 0
  Width = 393
  Height = 401
  TabOrder = 0
  object cbxUseCosts: TCheckBox
    Left = 32
    Top = 8
    Width = 329
    Height = 17
    Caption = 'Track direct costs for this production type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = processClick
  end
  object pnlCostParams: TPanel
    Left = 32
    Top = 32
    Width = 329
    Height = 353
    BevelOuter = bvNone
    TabOrder = 1
    object lblDestrAppraisalPerUnit: TLabel
      Left = 32
      Top = 36
      Width = 125
      Height = 13
      Caption = 'Cost of appraisal (per unit):'
    end
    object lblDestrCleaningPerUnit: TLabel
      Left = 32
      Top = 60
      Width = 200
      Height = 13
      Caption = 'Cost of cleaning and disinfection (per unit):'
    end
    object lblDestrIndemnificationPerAnimal: TLabel
      Left = 32
      Top = 84
      Width = 131
      Height = 13
      Caption = 'Indemnification (per animal):'
    end
    object lblDestrDisposalPerAnimal: TLabel
      Left = 32
      Top = 132
      Width = 139
      Height = 13
      Caption = 'Carcass disposal (per animal):'
    end
    object lblDestrEuthanasiaPerAnimal: TLabel
      Left = 32
      Top = 108
      Width = 113
      Height = 13
      Caption = 'Euthanasia (per animal):'
    end
    object lblVaccAdditionalPerAnimal: TLabel
      Left = 32
      Top = 280
      Width = 203
      Height = 26
      Caption = 
        'Additional cost for each animal vaccinated beyond the threshold ' +
        '(per animal):'
      WordWrap = True
    end
    object lblVaccSetupPerUnit: TLabel
      Left = 32
      Top = 196
      Width = 128
      Height = 13
      Caption = 'Cost of site setup (per unit):'
    end
    object lblVaccBaselinePerAnimal: TLabel
      Left = 32
      Top = 220
      Width = 181
      Height = 13
      Caption = 'Baseline vaccination cost (per animal):'
    end
    object lblVaccThreshold: TLabel
      Left = 32
      Top = 248
      Width = 204
      Height = 26
      Caption = 
        'Number of animals that may be vaccinated before the cost increas' +
        'es:'
      WordWrap = True
    end
    object lblDestructionCosts: TLabel
      Left = 16
      Top = 8
      Width = 85
      Height = 13
      Caption = 'Destruction costs:'
    end
    object lblVaccinationCosts: TLabel
      Left = 16
      Top = 168
      Width = 87
      Height = 13
      Caption = 'Vaccination costs:'
    end
    object lblDollars2: TLabel
      Left = 240
      Top = 60
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars1: TLabel
      Left = 240
      Top = 36
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars3: TLabel
      Left = 240
      Top = 84
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars4: TLabel
      Left = 240
      Top = 108
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars5: TLabel
      Left = 240
      Top = 132
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars6: TLabel
      Left = 240
      Top = 196
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars7: TLabel
      Left = 240
      Top = 220
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars8: TLabel
      Left = 240
      Top = 284
      Width = 6
      Height = 13
      Caption = '$'
    end
    object rleDestrAppraisalPerUnit: TREEdit
      Left = 248
      Top = 32
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 0
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleDestrCleaningPerUnit: TREEdit
      Left = 248
      Top = 56
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleDestrIndemnificationPerAnimal: TREEdit
      Left = 248
      Top = 80
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 2
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleDestrEuthanasiaPerAnimal: TREEdit
      Left = 248
      Top = 104
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 3
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleDestrDisposalPerAnimal: TREEdit
      Left = 248
      Top = 128
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 4
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleVaccAdditionalPerAnimal: TREEdit
      Left = 248
      Top = 280
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 8
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleVaccSetupPerUnit: TREEdit
      Left = 248
      Top = 192
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 5
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleVaccBaselinePerAnimal: TREEdit
      Left = 248
      Top = 216
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 6
      OnExit = processTextEntry
      OnKeyDown = rleKeyDown
    end
    object rleVaccThreshold: TREEdit
      Left = 248
      Top = 248
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 7
      OnExit = processTextEntry
    end
  end
end
