object FormExport: TFormExport
  Left = 535
  Top = 153
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'Export scenario'
  ClientHeight = 269
  ClientWidth = 439
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
  object lblScenario: TLabel
    Left = 12
    Top = 124
    Width = 116
    Height = 13
    Caption = 'Scenario parameters file:'
  end
  object lblHerds: TLabel
    Left = 12
    Top = 175
    Width = 56
    Height = 13
    Caption = 'List of units:'
  end
  object leScenario: TEdit
    Left = 12
    Top = 143
    Width = 325
    Height = 21
    TabOrder = 0
  end
  object leHerds: TEdit
    Left = 12
    Top = 194
    Width = 325
    Height = 21
    TabOrder = 1
  end
  object btnScenario: TButton
    Left = 350
    Top = 141
    Width = 75
    Height = 28
    Caption = 'Browse...'
    TabOrder = 2
    OnClick = btnScenarioClick
  end
  object btnHerds: TButton
    Left = 350
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btnHerdsClick
  end
  object btnExport: TButton
    Left = 269
    Top = 232
    Width = 74
    Height = 26
    Caption = 'Export'
    Default = True
    TabOrder = 4
    OnClick = btnExportClick
  end
  object GroupBox1: TGroupBox
    Left = 20
    Top = 12
    Width = 405
    Height = 101
    Caption = 'Items to import: '
    TabOrder = 5
    object cbxScenario: TCheckBox
      Left = 20
      Top = 20
      Width = 373
      Height = 21
      Caption = 'Export scenario parameters file'
      TabOrder = 0
      OnClick = cbxScenarioClick
    end
    object cbxHerds: TCheckBox
      Left = 20
      Top = 73
      Width = 208
      Height = 21
      Caption = 'Export list of units'
      TabOrder = 2
      OnClick = cbxHerdsClick
    end
    object cbxWriteOutputs: TCheckBox
      Left = 36
      Top = 41
      Width = 357
      Height = 21
      Caption = 'Include output specification for NAADSM/SC'
      TabOrder = 1
      OnClick = cbxHerdsClick
    end
  end
  object btnCancel: TButton
    Left = 351
    Top = 232
    Width = 74
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 6
    OnClick = btnCancelClick
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 256
    Top = 24
  end
end
