object FrameCostsDestr: TFrameCostsDestr
  Left = 0
  Top = 0
  Width = 431
  Height = 401
  TabOrder = 0
  object pnlCostParams: TPanel
    Left = 24
    Top = 8
    Width = 393
    Height = 297
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
    object lblCostApplicationHeader: TLabel
      Left = 24
      Top = 176
      Width = 288
      Height = 13
      Caption = 'Cost application for deaths caused by the disease:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblDeadUnitAppraise: TLabel
      Left = 24
      Top = 200
      Width = 147
      Height = 13
      Caption = 'Whether to charge to appraise:'
    end
    object lblDeadUnitClean: TLabel
      Left = 24
      Top = 224
      Width = 133
      Height = 13
      Caption = 'Whether to charge to clean:'
    end
    object lblDeadUnitIndemnify: TLabel
      Left = 24
      Top = 248
      Width = 151
      Height = 13
      Caption = 'Whether to charge to indemnify:'
    end
    object lblDeadUnitDispose: TLabel
      Left = 24
      Top = 272
      Width = 143
      Height = 13
      Caption = 'Whether to charge to dispose:'
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
    object cbxDeadUnitAppraise: TComboBox
      Left = 184
      Top = 192
      Width = 193
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = 'Always applied'
      OnExit = processTextEntry
      Items.Strings = (
        'Always applied'
        'Never applied'
        'Only apply after queued for destruction')
    end
    object cbxDeadUnitClean: TComboBox
      Left = 184
      Top = 216
      Width = 193
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 6
      Text = 'Always applied'
      OnExit = processTextEntry
      Items.Strings = (
        'Always applied'
        'Never applied'
        'Only apply after queued for destruction')
    end
    object cbxDeadUnitIndemnify: TComboBox
      Left = 184
      Top = 240
      Width = 193
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 7
      Text = 'Always applied'
      OnExit = processTextEntry
      Items.Strings = (
        'Always applied'
        'Never applied'
        'Only apply after queued for destruction')
    end
    object cbxDeadUnitDispose: TComboBox
      Left = 184
      Top = 264
      Width = 193
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 8
      Text = 'Always applied'
      OnExit = processTextEntry
      Items.Strings = (
        'Always applied'
        'Never applied'
        'Only apply after queued for destruction')
    end
  end
  object pnlNoDestruction: TPanel
    Left = 24
    Top = 320
    Width = 353
    Height = 73
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lblNoDestruction: TLabel
      Left = 6
      Top = 32
      Width = 337
      Height = 13
      Caption = '(Destruction is not used with the selected production type.)'
    end
  end
end
