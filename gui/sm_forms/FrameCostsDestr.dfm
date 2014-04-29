object FrameCostsDestr: TFrameCostsDestr
  Left = 0
  Top = 0
  Width = 393
  Height = 401
  TabOrder = 0
  object pnlCostParams: TPanel
    Left = 24
    Top = 8
    Width = 353
    Height = 209
    TabOrder = 0
    object lblDestrAppraisalPerUnit: TLabel
      Left = 24
      Top = 20
      Width = 125
      Height = 13
      Caption = 'Cost of appraisal (per unit):'
    end
    object lblDestrCleaningPerUnit: TLabel
      Left = 24
      Top = 52
      Width = 200
      Height = 13
      Caption = 'Cost of cleaning and disinfection (per unit):'
    end
    object lblDestrIndemnificationPerAnimal: TLabel
      Left = 24
      Top = 84
      Width = 131
      Height = 13
      Caption = 'Indemnification (per animal):'
    end
    object lblDestrDisposalPerAnimal: TLabel
      Left = 24
      Top = 148
      Width = 139
      Height = 13
      Caption = 'Carcass disposal (per animal):'
    end
    object lblDestrEuthanasiaPerAnimal: TLabel
      Left = 24
      Top = 116
      Width = 113
      Height = 13
      Caption = 'Euthanasia (per animal):'
    end
    object lblDollars2: TLabel
      Left = 248
      Top = 52
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars1: TLabel
      Left = 248
      Top = 20
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars3: TLabel
      Left = 248
      Top = 84
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars4: TLabel
      Left = 248
      Top = 116
      Width = 6
      Height = 13
      Caption = '$'
    end
    object lblDollars5: TLabel
      Left = 248
      Top = 148
      Width = 6
      Height = 13
      Caption = '$'
    end
    object rleDestrAppraisalPerUnit: TREEdit
      Left = 256
      Top = 16
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 0
      OnExit = processTextEntry
    end
    object rleDestrCleaningPerUnit: TREEdit
      Left = 256
      Top = 48
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 1
      OnExit = processTextEntry
    end
    object rleDestrIndemnificationPerAnimal: TREEdit
      Left = 256
      Top = 80
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 2
      OnExit = processTextEntry
    end
    object rleDestrEuthanasiaPerAnimal: TREEdit
      Left = 256
      Top = 112
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 3
      OnExit = processTextEntry
    end
    object rleDestrDisposalPerAnimal: TREEdit
      Left = 256
      Top = 144
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 4
      OnExit = processTextEntry
    end
  end
  object pnlNoDestruction: TPanel
    Left = 40
    Top = 224
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
      Width = 337
      Height = 13
      Caption = '(Destruction is not used with the selected production type.)'
    end
  end
end
