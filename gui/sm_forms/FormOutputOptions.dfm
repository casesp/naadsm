inherited FormOutputOptions: TFormOutputOptions
  Left = 1167
  Top = 483
  Caption = 'Scenario parameters: Output options'
  ClientHeight = 588
  ClientWidth = 710
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 545
    Width = 710
    inherited pnlWizardButtons: TPanel
      Left = 312
    end
  end
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 0
    Width = 710
    Height = 41
    Align = alTop
    Caption = 'Output options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object pnlBasic: TPanel [2]
    Left = 0
    Top = 41
    Width = 710
    Height = 504
    Align = alClient
    TabOrder = 2
    object lblInstructions: TLabel
      Left = 24
      Top = 8
      Width = 656
      Height = 26
      Caption = 
        'Select optional outputs that you would like to generate and reco' +
        'rd from those listed below.  Some of these selections will resul' +
        't in decreased performance and very large scenario database file' +
        's: use these options with caution.'
      WordWrap = True
    end
    object gbxDailyStates: TGroupBox
      Left = 80
      Top = 56
      Width = 553
      Height = 93
      Caption = 'Daily unit states '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object cbxDailyStates: TCheckBox
        Left = 8
        Top = 20
        Width = 529
        Height = 17
        Caption = 'Write a plain text file containing daily states for all units'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = updateControls
      end
      inline fraDailyStatesFile: TFrameFileSelector
        Left = 20
        Top = 36
        Width = 377
        Height = 49
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        inherited SaveDialog1: TSaveDialog
          DefaultExt = 'txt'
          Filter = 'Plain text file (*.txt)|*.txt|All files (*.*)|*.*'
        end
      end
    end
    object gbxDailyOutputIterations: TGroupBox
      Left = 80
      Top = 156
      Width = 553
      Height = 117
      Caption = 'Daily output for iterations '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object lblDailyOutputIterations: TLabel
        Left = 32
        Top = 84
        Width = 97
        Height = 13
        Caption = 'Number of iterations:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object rdoDailyOutputAll: TRadioButton
        Left = 8
        Top = 20
        Width = 529
        Height = 25
        Caption = 
          'Save all daily output for every iteration (warning: this option ' +
          'may produce very large scenario files)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WordWrap = True
        OnClick = updateControls
      end
      object rdoDailyOutputSpecified: TRadioButton
        Left = 8
        Top = 60
        Width = 481
        Height = 17
        Caption = 'Save all daily output for a specified number of iterations'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = updateControls
      end
      object speDailyOutputIterations: TSpinEdit
        Left = 224
        Top = 84
        Width = 57
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 1000
        MinValue = 3
        ParentFont = False
        TabOrder = 2
        Value = 3
      end
    end
    object gbxEventsAndExposures: TGroupBox
      Left = 80
      Top = 276
      Width = 553
      Height = 101
      Caption = 'Daily events and exposures '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      object cbxEvents: TCheckBox
        Left = 8
        Top = 20
        Width = 529
        Height = 17
        Caption = 
          'Save information for each event (scenario files may be very larg' +
          'e)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WordWrap = True
      end
      object cbxExposures: TCheckBox
        Left = 8
        Top = 60
        Width = 537
        Height = 17
        Caption = 
          'Save information for each exposure and trace (scenario files may' +
          ' be very large)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        WordWrap = True
      end
    end
    object gbxMapOutputs: TGroupBox
      Left = 80
      Top = 384
      Width = 553
      Height = 89
      Caption = 'NAADSMap output'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      object lblNAADSMapFolder: TLabel
        Left = 24
        Top = 64
        Width = 81
        Height = 13
        Caption = 'lblNAADSMapDir'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblNAADSMapLabel: TLabel
        Left = 24
        Top = 44
        Width = 141
        Height = 13
        Caption = 'Folder for NAADSMap output:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object cbxMapOutput: TCheckBox
        Left = 8
        Top = 20
        Width = 313
        Height = 17
        Caption = 'Generate NAADSMap output'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = updateControls
      end
      object btnMapOptions: TButton
        Left = 440
        Top = 16
        Width = 96
        Height = 25
        Caption = 'Options...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Visible = False
      end
      object btnChooseNAADSMapDir: TButton
        Left = 400
        Top = 48
        Width = 137
        Height = 25
        Caption = 'Select folder...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = btnChooseNAADSMapDirClick
      end
    end
  end
  inherited PopupActionBarEx1: TPopupActionBarEx
    Left = 664
    Top = 376
  end
end
