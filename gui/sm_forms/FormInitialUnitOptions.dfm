inherited FormInitialUnitOptions: TFormInitialUnitOptions
  Left = 910
  Top = 457
  BorderIcons = [biMaximize]
  Caption = 'Scenario parameters: Initial unit options'
  ClientHeight = 527
  ClientWidth = 698
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
    Top = 484
    Width = 698
    TabOrder = 2
    inherited pnlWizardButtons: TPanel
      Left = 300
    end
  end
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 0
    Width = 698
    Height = 41
    Align = alTop
    Caption = 'Initial unit options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object pnlBody: TPanel [2]
    Left = 0
    Top = 41
    Width = 698
    Height = 443
    Align = alClient
    TabOrder = 1
    object rdoAssignedUnits: TRadioButton
      Left = 16
      Top = 16
      Width = 473
      Height = 17
      Caption = 
        'Assign specific initial disease state parameters to a specific u' +
        'nit or units (previous window)'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rdoAssignedUnitsClick
    end
    object rdoRandomizedUnits: TRadioButton
      Left = 16
      Top = 40
      Width = 377
      Height = 17
      Caption = 
        'Randomize initially infected units based on the following criter' +
        'ia:'
      TabOrder = 1
      OnClick = rdoAssignedUnitsClick
    end
    object gbxProdTypes: TGroupBox
      Left = 208
      Top = 72
      Width = 233
      Height = 313
      Caption = 'Production types '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      object rdoAnyProdType: TRadioButton
        Left = 8
        Top = 16
        Width = 209
        Height = 17
        Caption = 'Use unit(s) of any production type'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rdoAssignedUnitsClick
      end
      object rdoSelectedProdTypes: TRadioButton
        Left = 8
        Top = 40
        Width = 217
        Height = 17
        Caption = 'Use unit(s) of only the selected type(s):'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = rdoAssignedUnitsClick
      end
      object lstProdTypes: TListBox
        Left = 8
        Top = 64
        Width = 209
        Height = 225
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        MultiSelect = True
        ParentFont = False
        TabOrder = 2
        OnClick = lstProdTypesChanged
        OnDblClick = lstProdTypesChanged
        OnExit = lstProdTypesChanged
      end
    end
    object gbxDzStates: TGroupBox
      Left = 16
      Top = 72
      Width = 169
      Height = 313
      Caption = 'Disease states '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      object lblNInStates: TLabel
        Left = 8
        Top = 24
        Width = 141
        Height = 13
        Caption = 'Number of units in each state:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblLatent: TLabel
        Left = 64
        Top = 52
        Width = 30
        Height = 13
        Caption = 'Latent'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblSubclinical: TLabel
        Left = 64
        Top = 84
        Width = 51
        Height = 13
        Caption = 'Subclinical'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblClinical: TLabel
        Left = 64
        Top = 116
        Width = 33
        Height = 13
        Caption = 'Clinical'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblNatImmune: TLabel
        Left = 64
        Top = 148
        Width = 80
        Height = 13
        Caption = 'Naturally immune'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblVacImmune: TLabel
        Left = 64
        Top = 180
        Width = 78
        Height = 13
        Caption = 'Vaccine immune'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblNote: TLabel
        Left = 8
        Top = 208
        Width = 152
        Height = 39
        Caption = 
          'Note: Initial state durations will be selected from the appropri' +
          'ate distributions.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object rleLatent: TREEdit
        Left = 8
        Top = 48
        Width = 49
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = '1'
      end
      object rleSubclinical: TREEdit
        Left = 8
        Top = 80
        Width = 49
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = '0'
      end
      object rleClinical: TREEdit
        Left = 8
        Top = 112
        Width = 49
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        Text = '0'
      end
      object rleNatImmune: TREEdit
        Left = 8
        Top = 144
        Width = 49
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '0'
      end
      object rleVacImmune: TREEdit
        Left = 8
        Top = 176
        Width = 49
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        Text = '0'
      end
    end
    object gbxGeoRange: TGroupBox
      Left = 464
      Top = 72
      Width = 193
      Height = 313
      Caption = 'Geographic range '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      object lblNorthwest: TLabel
        Left = 24
        Top = 84
        Width = 102
        Height = 13
        Caption = 'Northwest corner:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblLatNW: TLabel
        Left = 32
        Top = 104
        Width = 41
        Height = 13
        Caption = 'Latitude:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblLonNW: TLabel
        Left = 32
        Top = 136
        Width = 50
        Height = 13
        Caption = 'Longitude:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblSoutheast: TLabel
        Left = 24
        Top = 172
        Width = 102
        Height = 13
        Caption = 'Southeast corner:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblLatSE: TLabel
        Left = 32
        Top = 192
        Width = 41
        Height = 13
        Caption = 'Latitude:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblLonSE: TLabel
        Left = 32
        Top = 224
        Width = 50
        Height = 13
        Caption = 'Longitude:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object rleLatNW: TREEdit
        Left = 88
        Top = 104
        Width = 65
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object rleLonNW: TREEdit
        Left = 88
        Top = 136
        Width = 65
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object rleLatSE: TREEdit
        Left = 88
        Top = 192
        Width = 65
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object rleLonSE: TREEdit
        Left = 88
        Top = 224
        Width = 65
        Height = 21
        EditAlign = eaLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object rdoNoRange: TRadioButton
        Left = 8
        Top = 24
        Width = 169
        Height = 17
        Caption = 'Use unit(s) from any location'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        TabStop = True
        OnClick = rdoAssignedUnitsClick
      end
      object rdoRange: TRadioButton
        Left = 8
        Top = 40
        Width = 169
        Height = 41
        Caption = 'Use unit(s) only from within the following rectangular area:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        WordWrap = True
        OnClick = rdoAssignedUnitsClick
      end
    end
  end
end
