inherited FormDailyStatusByProdType: TFormDailyStatusByProdType
  Left = 573
  Top = 136
  Width = 755
  Height = 535
  Caption = 'Daily unit status for 1 iteration'
  Constraints.MinWidth = 755
  KeyPreview = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 747
    inherited pnlProdTypes: TPanel
      Width = 445
      object lblIteration: TLabel [0]
        Left = 328
        Top = 8
        Width = 41
        Height = 13
        Caption = 'Iteration:'
      end
      inherited cboProdTypes: TComboBox
        Width = 169
      end
      inherited cboZones: TComboBox
        Left = 192
        Width = 129
        TabOrder = 4
      end
      object cboIteration: TComboBox
        Left = 384
        Top = 4
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = cboIterationChange
      end
    end
  end
  inline fraBody: TFrameDailyStatusByProdType [1]
    Left = 129
    Top = 53
    Width = 618
    Height = 455
    Align = alClient
    TabOrder = 1
    inherited pnlChartOptions: TPanel
      Top = 430
      Width = 618
      inherited cbxThreeD: TCheckBox
        OnClick = cbxThreeDClick
      end
    end
    inherited chtOutputs: TChart
      Width = 618
      Height = 430
      MarginLeft = 6
      Legend.ColorWidth = 15
      Legend.LegendStyle = lsSeries
    end
  end
  object pnlCheckBoxes: TPanel [2]
    Left = 0
    Top = 53
    Width = 129
    Height = 455
    Align = alLeft
    TabOrder = 2
    object cbxSusceptible: TCheckBox
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Susceptible'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbxSusceptibleClick
    end
    object lineSusceptible: TPanel
      Left = 25
      Top = 24
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clBlack
      TabOrder = 1
    end
    object cbxNatImmune: TCheckBox
      Left = 8
      Top = 136
      Width = 113
      Height = 17
      Caption = 'Nat Immune'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbxNatImmuneClick
    end
    object cbxLatent: TCheckBox
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Latent'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbxLatentClick
    end
    object cbxVacImmune: TCheckBox
      Left = 8
      Top = 168
      Width = 113
      Height = 17
      Caption = 'Vac Immune'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = cbxVacImmuneClick
    end
    object cbxSubClinical: TCheckBox
      Left = 8
      Top = 72
      Width = 113
      Height = 17
      Caption = 'Subclinical'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = cbxSubClinicalClick
    end
    object cbxDestroyed: TCheckBox
      Left = 8
      Top = 200
      Width = 113
      Height = 17
      Caption = 'Destroyed'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = cbxDestroyedClick
    end
    object cbxClinical: TCheckBox
      Left = 8
      Top = 104
      Width = 113
      Height = 17
      Caption = 'Clinical'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = cbxClinicalClick
    end
    object cbxMilestones: TCheckBox
      Left = 8
      Top = 408
      Width = 81
      Height = 17
      Caption = 'Events'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = cbxMilestonesClick
    end
    object lineNatImmune: TPanel
      Left = 25
      Top = 152
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clLime
      TabOrder = 9
    end
    object lineLatent: TPanel
      Left = 25
      Top = 56
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clYellow
      TabOrder = 10
    end
    object lineVacImmune: TPanel
      Left = 25
      Top = 184
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 11
    end
    object lineSubclinical: TPanel
      Left = 25
      Top = 88
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clFuchsia
      TabOrder = 12
    end
    object lineDestroyed: TPanel
      Left = 25
      Top = 216
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 13
    end
    object lineClinical: TPanel
      Left = 25
      Top = 120
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clRed
      TabOrder = 14
    end
    object cbxDetected: TCheckBox
      Left = 8
      Top = 256
      Width = 113
      Height = 17
      Caption = 'Detected'
      Checked = True
      State = cbChecked
      TabOrder = 15
      OnClick = cbxDetectedClick
    end
    object lineDetected: TPanel
      Left = 25
      Top = 272
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clGreen
      TabOrder = 16
    end
    object cbxTraceDir: TCheckBox
      Left = 8
      Top = 288
      Width = 113
      Height = 17
      Caption = 'Traced - Direct'
      Checked = True
      State = cbChecked
      TabOrder = 17
      OnClick = cbxTraceDirClick
    end
    object lineTraceDir: TPanel
      Left = 25
      Top = 304
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clNavy
      TabOrder = 18
    end
    object cbxTraceInd: TCheckBox
      Left = 8
      Top = 320
      Width = 113
      Height = 17
      Caption = 'Traced - Indirect'
      Checked = True
      State = cbChecked
      TabOrder = 19
      OnClick = cbxTraceIndClick
    end
    object lineTraceInd: TPanel
      Left = 25
      Top = 336
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clPurple
      TabOrder = 20
    end
    object cbxVaccinated: TCheckBox
      Left = 8
      Top = 352
      Width = 113
      Height = 17
      Caption = 'Vaccinated'
      Checked = True
      State = cbChecked
      TabOrder = 21
      OnClick = cbxVaccinatedClick
    end
    object lineVaccinated: TPanel
      Left = 25
      Top = 368
      Width = 96
      Height = 2
      BevelOuter = bvNone
      Color = clAqua
      TabOrder = 22
    end
  end
  object pnlCaption: TPanel [3]
    Left = 0
    Top = 33
    Width = 747
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Iteration status: completed/aborted/running'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  inherited dlgSaveWMF: TSaveDialog [4]
  end
  inherited dlgSaveCSV: TSaveDialog [5]
  end
end
