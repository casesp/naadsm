object FormImport: TFormImport
  Left = 642
  Top = 55
  Width = 427
  Height = 339
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Import scenario'
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
    Left = 16
    Top = 120
    Width = 116
    Height = 13
    Caption = 'Scenario parameters file:'
  end
  object lblHerds: TLabel
    Left = 16
    Top = 168
    Width = 56
    Height = 13
    Caption = 'List of units:'
  end
  object lblDB: TLabel
    Left = 16
    Top = 216
    Width = 153
    Height = 13
    Caption = 'Scenario database file to create:'
  end
  object Label2: TLabel
    Left = 16
    Top = 8
    Width = 193
    Height = 13
    Caption = 'Import existing XML files to the database '
  end
  object leScenario: TEdit
    Left = 16
    Top = 136
    Width = 313
    Height = 21
    Enabled = False
    TabOrder = 0
    OnKeyDown = leKeyDown
  end
  object leHerds: TEdit
    Left = 16
    Top = 184
    Width = 313
    Height = 21
    Enabled = False
    TabOrder = 1
  end
  object leDB: TEdit
    Left = 16
    Top = 232
    Width = 313
    Height = 21
    Enabled = False
    TabOrder = 2
  end
  object btnScenario: TButton
    Left = 336
    Top = 134
    Width = 73
    Height = 25
    Caption = 'Browse...'
    Enabled = False
    TabOrder = 3
    OnClick = btnScenarioClick
  end
  object btnHerds: TButton
    Left = 336
    Top = 182
    Width = 73
    Height = 25
    Caption = 'Browse...'
    Enabled = False
    TabOrder = 4
    OnClick = btnHerdsClick
  end
  object btnDB: TButton
    Left = 336
    Top = 230
    Width = 73
    Height = 25
    Caption = 'Browse...'
    Enabled = False
    TabOrder = 5
    OnClick = btnDBClick
  end
  object btnImport: TButton
    Left = 264
    Top = 272
    Width = 65
    Height = 25
    Caption = 'Import'
    Default = True
    Enabled = False
    TabOrder = 6
    OnClick = btnImportClick
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 40
    Width = 313
    Height = 65
    Caption = 'Items to import: '
    TabOrder = 7
    object cbxScenario: TCheckBox
      Left = 16
      Top = 16
      Width = 169
      Height = 17
      Caption = 'Import scenario parameters file'
      TabOrder = 0
      OnClick = cbxScenarioClick
    end
    object cbxHerds: TCheckBox
      Left = 16
      Top = 40
      Width = 169
      Height = 17
      Caption = 'Import list of units'
      Enabled = False
      TabOrder = 1
      OnClick = cbxHerdsClick
    end
  end
  object btnCancel: TButton
    Left = 344
    Top = 272
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 8
    OnClick = btnCancelClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    InitialDir = 'c:\bin'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 240
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'mdb'
    Filter = 'NAADSM scenario databases (*.mdb)|*.mdb|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 288
    Top = 8
  end
end
