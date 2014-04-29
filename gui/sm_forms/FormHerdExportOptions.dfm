object FormHerdExportOptions: TFormHerdExportOptions
  Left = 962
  Top = 23
  BorderStyle = bsDialog
  Caption = 'Unit list export options'
  ClientHeight = 391
  ClientWidth = 549
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
  inline fraExportFile: TFrameFileSelector
    Left = 72
    Top = 280
    Width = 377
    Height = 57
    TabOrder = 0
  end
  object rdgFileFormat: TRadioGroup
    Left = 24
    Top = 8
    Width = 497
    Height = 41
    Caption = 'File format '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Export as CSV'
      'Export as XML')
    TabOrder = 1
    OnClick = rdgFileFormatClick
  end
  object gbxCSVOptions: TGroupBox
    Left = 24
    Top = 64
    Width = 497
    Height = 201
    Caption = 'CSV options'
    TabOrder = 2
    object rdgProdTypes: TRadioGroup
      Left = 16
      Top = 16
      Width = 465
      Height = 73
      Caption = 'Production types '
      ItemIndex = 0
      Items.Strings = (
        'Export production types by name'
        'Export production types by number')
      TabOrder = 0
    end
    object rdgInitialStates: TRadioGroup
      Left = 16
      Top = 104
      Width = 465
      Height = 73
      Caption = 'Initial disease states '
      ItemIndex = 0
      Items.Strings = (
        'Export initial states as character codes'
        'Export initial states as numeric codes')
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 352
    Top = 352
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 448
    Top = 352
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object pnlXMLOptions: TPanel
    Left = 16
    Top = 336
    Width = 193
    Height = 41
    BevelOuter = bvNone
    Caption = '(There are no XML options)'
    TabOrder = 5
    Visible = False
  end
end
