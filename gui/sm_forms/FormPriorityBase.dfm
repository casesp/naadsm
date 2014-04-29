inherited FormPriorityBase: TFormPriorityBase
  Left = 470
  Top = 273
  Caption = 'Scenario parameters'
  ClientHeight = 536
  ClientWidth = 688
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
  object pnlCaption: TPanel [0]
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    Caption = 'CHANGE THIS IN DERIVED CLASSES'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object pnlExamplesBottom: TPanel [1]
    Left = 0
    Top = 369
    Width = 688
    Height = 124
    Align = alClient
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 1
    object lblSpacerLeftBottom: TLabel
      Left = 0
      Top = 0
      Width = 40
      Height = 112
      Align = alLeft
      AutoSize = False
    end
    object lblSpacerRight: TLabel
      Left = 648
      Top = 0
      Width = 40
      Height = 112
      Align = alRight
      AutoSize = False
    end
    object mmoExamples: TMemo
      Left = 40
      Top = 0
      Width = 608
      Height = 112
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      Color = clBtnFace
      Lines.Strings = (
        'Memo1')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object pnlSpacer: TPanel
      Left = 0
      Top = 112
      Width = 688
      Height = 12
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object pnlInstructions: TPanel [2]
    Left = 0
    Top = 41
    Width = 688
    Height = 64
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 2
    object lblInstructions: TLabel
      Left = 32
      Top = 10
      Width = 615
      Height = 26
      Caption = 
        'Prioritize by dragging items up or down in both primary or secon' +
        'dary lists.  Drag higher priority items up and lower priority it' +
        'ems down.  Or right-click the item in the white list box and cha' +
        'nge the priority number.'
      WordWrap = True
    end
  end
  object pnlExamplesTop: TPanel [3]
    Left = 0
    Top = 345
    Width = 688
    Height = 24
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 3
    object lblExamples: TLabel
      Left = 40
      Top = 0
      Width = 648
      Height = 24
      Align = alClient
      Caption = 'Priority examples for selections above'
      WordWrap = True
    end
    object lblSpacerLeftTop: TLabel
      Left = 0
      Top = 0
      Width = 40
      Height = 24
      Align = alLeft
      AutoSize = False
    end
  end
  object pnlBody: TPanel [4]
    Left = 0
    Top = 105
    Width = 688
    Height = 240
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object lblPrimary: TLabel
      Left = 160
      Top = 16
      Width = 34
      Height = 13
      Caption = 'Primary'
    end
    object lblSecondary: TLabel
      Left = 336
      Top = 16
      Width = 51
      Height = 13
      Caption = 'Secondary'
    end
    object lblDaysHolding: TLabel
      Left = 392
      Top = 204
      Width = 211
      Height = 13
      Caption = 'The more days holding, the higher the priority'
    end
    object lblNoProdTypes: TLabel
      Left = 112
      Top = 160
      Width = 218
      Height = 13
      Caption = '(No production types use this control measure)'
      Visible = False
    end
    object lblNoReasons: TLabel
      Left = 104
      Top = 184
      Width = 128
      Height = 13
      Caption = '(This activity is never used)'
      Visible = False
    end
    object lbxPrimary: TListBox
      Left = 104
      Top = 32
      Width = 153
      Height = 57
      DragMode = dmAutomatic
      ItemHeight = 13
      Items.Strings = (
        'Reason for destruction'
        'Production type'
        'Days holding')
      TabOrder = 0
      OnClick = lbxPrimaryClick
      OnDragDrop = lbxPrimaryDragDrop
      OnDragOver = lbxPrimaryDragOver
      OnEndDrag = lbxPrimaryEndDrag
      OnMouseDown = lbxPrimaryMouseDown
    end
    object lbxProdType: TListBox
      Left = 304
      Top = 32
      Width = 153
      Height = 150
      DragMode = dmAutomatic
      ItemHeight = 13
      Items.Strings = (
        'Pigs'
        'Cattle'
        'Dairy')
      TabOrder = 1
      OnDragDrop = lbxPrimaryDragDrop
      OnDragOver = lbxPrimaryDragOver
      OnEndDrag = lbxPrimaryEndDrag
      OnMouseDown = lbxPrimaryMouseDown
    end
    object lbxReason: TListBox
      Left = 448
      Top = 21
      Width = 193
      Height = 150
      DragMode = dmAutomatic
      ItemHeight = 13
      Items.Strings = (
        'Detected'
        'Trace forward of direct contact'
        'Trace forward of indirect contact'
        'Ring'
        'Trace back of direct contact'
        'Trace back of indirect contact')
      TabOrder = 2
      OnDragDrop = lbxPrimaryDragDrop
      OnDragOver = lbxPrimaryDragOver
      OnEndDrag = lbxPrimaryEndDrag
      OnMouseDown = lbxPrimaryMouseDown
    end
  end
  inherited pnlBase: TPanel
    Top = 493
    Width = 688
    TabOrder = 5
    inherited pnlWizardButtons: TPanel
      Left = 290
    end
  end
end
