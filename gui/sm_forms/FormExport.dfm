object FormExport: TFormExport
  Left = 594
  Top = 314
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'Export scenario'
  ClientHeight = 470
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSpacer: TPanel
    Left = 0
    Top = 0
    Width = 435
    Height = 8
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object gbxScenarioExport: TGroupBox
    Left = 0
    Top = 101
    Width = 435
    Height = 236
    Align = alTop
    Caption = 'Scenario parameters: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lblScenario: TLabel
      Left = 12
      Top = 176
      Width = 116
      Height = 13
      Caption = 'Scenario parameters file:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object cbxWriteOutputs: TCheckBox
      Left = 12
      Top = 17
      Width = 357
      Height = 21
      Caption = 'Include output specification for NAADSM/SC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = cbxHerdsClick
    end
    object gbxIterationEnd: TGroupBox
      Left = 8
      Top = 44
      Width = 313
      Height = 121
      Caption = 'Iteration end condition: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object rdoOutbreakEnd: TRadioButton
        Left = 16
        Top = 20
        Width = 273
        Height = 17
        Caption = 'End of outbreak (including all control activities)'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rdoEndOptionClick
      end
      object rdoDiseaseEnd: TRadioButton
        Left = 16
        Top = 44
        Width = 273
        Height = 17
        Caption = 'End of active disease phase'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = rdoEndOptionClick
      end
      object rdoFirstDetection: TRadioButton
        Left = 16
        Top = 68
        Width = 273
        Height = 17
        Caption = 'First detection'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = rdoEndOptionClick
      end
      object rdoSpecDay: TRadioButton
        Left = 16
        Top = 92
        Width = 121
        Height = 17
        Caption = 'Specific day'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = rdoEndOptionClick
      end
      inline fraAcceptCancel: TFrameAcceptCancel
        Left = 209
        Top = 88
        Width = 80
        Height = 22
        HorzScrollBar.Visible = False
        VertScrollBar.Visible = False
        TabOrder = 4
        Visible = False
        inherited btnAccept: TBitBtn
          Enabled = True
          OnClick = fraAcceptCancelbtnAcceptClick
        end
        inherited btnCancel: TBitBtn
          Enabled = True
          OnClick = fraAcceptCancelbtnCancelClick
        end
      end
      object rleSpecDay: TREEdit
        Left = 144
        Top = 88
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 5
        Visible = False
        OnEnter = rleSpecDayEnter
        OnExit = rleSpecDayExit
        OnKeyUp = rleSpecDayKeyUp
      end
    end
    object btnScenario: TButton
      Left = 350
      Top = 189
      Width = 75
      Height = 28
      Caption = 'Browse...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnScenarioClick
    end
    object leScenario: TEdit
      Left = 12
      Top = 196
      Width = 325
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 8
    Width = 435
    Height = 81
    Align = alTop
    Caption = 'Files to export: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object cbxScenario: TCheckBox
      Left = 12
      Top = 20
      Width = 261
      Height = 21
      Caption = 'Export scenario parameters file'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = cbxScenarioClick
    end
    object cbxHerds: TCheckBox
      Left = 12
      Top = 52
      Width = 208
      Height = 21
      Caption = 'Export list of units'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = cbxHerdsClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 349
    Width = 435
    Height = 76
    Align = alTop
    Caption = 'List of units: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    object lblHerds: TLabel
      Left = 12
      Top = 20
      Width = 43
      Height = 13
      Caption = 'Units file:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object btnHerds: TButton
      Left = 350
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Browse...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnHerdsClick
    end
    object leHerds: TEdit
      Left = 12
      Top = 36
      Width = 325
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object pnlSpacer2: TPanel
    Left = 0
    Top = 89
    Width = 435
    Height = 12
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
  end
  object pnlSpacer3: TPanel
    Left = 0
    Top = 337
    Width = 435
    Height = 12
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
  end
  object pnlSpacer4: TPanel
    Left = 0
    Top = 425
    Width = 435
    Height = 8
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 6
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 433
    Width = 435
    Height = 37
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 7
    object btnExport: TButton
      Left = 137
      Top = 2
      Width = 74
      Height = 26
      Caption = 'Export'
      Default = True
      TabOrder = 0
      OnClick = btnExportClick
    end
    object btnCancel: TButton
      Left = 220
      Top = 2
      Width = 74
      Height = 26
      Cancel = True
      Caption = 'Cancel'
      Default = True
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 400
    Top = 8
  end
end
