inherited FormGeneralParams: TFormGeneralParams
  Left = 912
  Top = 16
  Hint = 'General description of the simulation session'
  Caption = 'Scenario parameters: Start setup'
  ClientHeight = 452
  ClientWidth = 594
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D000
    00000D7800000222222220778000022222222030880002222222203007000222
    22222030178002222222203017780222222220B014080720000002B014082003
    BBBBBBB01408038B3333333114080333199999991C080018911111110C080011
    14CCCCCCCC700007C8444444448000084440000000000000000000000000000F
    0000000700000003000000030000000100000000000000000000000000000000
    00008000000080000000C0000000C0010000E0010000E1FF0000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 409
    Width = 594
    TabOrder = 2
    inherited pnlWizardButtons: TPanel
      Left = 196
    end
  end
  object pnlHeader: TPanel [1]
    Left = 0
    Top = 0
    Width = 594
    Height = 41
    Align = alTop
    Caption = 'Start setup'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object pnlParams: TPanel [2]
    Left = 0
    Top = 41
    Width = 594
    Height = 368
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvNone
    TabOrder = 1
    object lblDescr: TLabel
      Left = 24
      Top = 15
      Width = 99
      Height = 13
      Caption = 'Scenario description:'
    end
    object lblTimeToRun: TLabel
      Left = 256
      Top = 368
      Width = 59
      Height = 13
      Caption = '(Time to run)'
      Visible = False
    end
    object lblInstructions1: TLabel
      Left = 24
      Top = 263
      Width = 104
      Height = 13
      Caption = 'A simulation runs until:'
    end
    object lblIterations: TLabel
      Left = 24
      Top = 111
      Width = 94
      Height = 13
      Caption = 'Number of iterations'
    end
    object lblInstructions2: TLabel
      Left = 44
      Top = 279
      Width = 311
      Height = 13
      Caption = 
        '1) there are no more incubating or contagious units in the repli' +
        'cate'
    end
    object lblInstructions3: TLabel
      Left = 56
      Top = 295
      Width = 328
      Height = 13
      Caption = 
        'AND all vacination and/or destruction activities have been compl' +
        'eted'
    end
    object lblInstructions5: TLabel
      Left = 36
      Top = 327
      Width = 16
      Height = 13
      Caption = 'OR'
    end
    object lblInstructions4: TLabel
      Left = 56
      Top = 311
      Width = 254
      Height = 13
      Caption = 'AND the stipulated number of iterations are completed'
    end
    object lblIterationNumber: TLabel
      Left = 276
      Top = 111
      Width = 134
      Height = 13
      Caption = 'Currently on iteration number'
      Visible = False
    end
    object lblRepNo: TLabel
      Left = 428
      Top = 111
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Color = clGreen
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Visible = False
    end
    object lblTimeToRun2: TLabel
      Left = 24
      Top = 368
      Width = 149
      Height = 13
      Caption = 'Time to run this set of iterations:'
      Visible = False
    end
    object lblInstructions6: TLabel
      Left = 44
      Top = 343
      Width = 363
      Height = 13
      Caption = 
        '2) the stop button is pressed (the current simluation day will s' +
        'till be completed)'
    end
    object rleIterations: TREEdit
      Left = 152
      Top = 104
      Width = 73
      Height = 21
      InputExpression = '^\d*$'
      EditAlign = eaLeft
      TabOrder = 0
      OnChange = dataChanged
    end
    object mmoDescr: TMemo
      Left = 24
      Top = 32
      Width = 553
      Height = 65
      Lines.Strings = (
        'mmoDescr')
      TabOrder = 1
      OnChange = dataChanged
    end
    object gbxRandomSeed: TGroupBox
      Left = 24
      Top = 136
      Width = 249
      Height = 103
      Caption = 'Random number generator seed '
      TabOrder = 2
      object lblSeedValue: TLabel
        Left = 16
        Top = 76
        Width = 98
        Height = 13
        Caption = 'Seed value (integer):'
      end
      object rdoAutoSeed: TRadioButton
        Left = 8
        Top = 24
        Width = 233
        Height = 17
        Caption = 'Generate seed automatically'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = radioClick
      end
      object rdoSpecifySeed: TRadioButton
        Left = 8
        Top = 48
        Width = 233
        Height = 17
        Caption = 'Specify a seed value'
        TabOrder = 1
        OnClick = radioClick
      end
      object rleSeedValue: TREEdit
        Left = 160
        Top = 72
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 2
        OnChange = dataChanged
      end
    end
    object pnlSeparator: TPanel
      Left = 24
      Top = 248
      Width = 537
      Height = 2
      TabOrder = 3
    end
  end
end
