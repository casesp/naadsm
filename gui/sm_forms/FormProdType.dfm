inherited FormProdType: TFormProdType
  Left = 904
  Top = 26
  ActiveControl = pnlProdTypeList
  BorderIcons = [biMaximize]
  Caption = 'Scenario parameters: Production type(s)'
  ClientHeight = 403
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
  inherited pnlBase: TPanel
    Top = 360
    Width = 688
    TabOrder = 3
    inherited pnlWizardButtons: TPanel
      Left = 290
    end
  end
  object pnlProdTypeList: TPanel [1]
    Left = 0
    Top = 41
    Width = 121
    Height = 319
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 0
    object pnlProdTypeListHeader: TPanel
      Left = 1
      Top = 1
      Width = 115
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      BorderStyle = bsSingle
      Caption = 'Production types'
      TabOrder = 0
    end
    object lstProdTypes: TListBox
      Left = 1
      Top = 25
      Width = 115
      Height = 289
      Hint = 
        'Press CTRL + left mouse button to select multiple production typ' +
        'es'
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'Cattle - beef'
        'Cattle - dairy'
        'Mixed - beef/dairy'
        'Swine - confinement'
        'Swine - outside')
      MultiSelect = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = lstProdTypesChange
      OnEnter = lstProdTypesEnter
      OnExit = lstProdTypesExit
      OnKeyDown = lstProdTypesKeyDown
      OnKeyUp = lstProdTypesKeyUp
      OnMouseDown = lstProdTypesMouseDown
      OnMouseMove = lstProdTypesMouseMove
      OnMouseUp = lstProdTypesMouseUp
    end
  end
  object pnlCaption: TPanel [2]
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    Caption = 'Production type(s)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object pnlBody: TPanel [3]
    Left = 121
    Top = 41
    Width = 567
    Height = 319
    Align = alClient
    TabOrder = 2
    object BitBtnAdd: TBitBtn
      Left = 40
      Top = 33
      Width = 233
      Height = 25
      Caption = 'Add production type'
      TabOrder = 0
      OnClick = BitBtnAddClick
    end
    object BitBtnModify: TBitBtn
      Left = 40
      Top = 81
      Width = 233
      Height = 25
      Caption = 'Modify selected production type'
      TabOrder = 1
      OnClick = BitBtnModifyClick
    end
    object BitBtnRemove: TBitBtn
      Left = 40
      Top = 129
      Width = 233
      Height = 25
      Caption = 'Remove selected production type(s)'
      TabOrder = 2
      OnClick = BitBtnRemoveClick
    end
  end
end
