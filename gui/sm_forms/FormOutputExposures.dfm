inherited FormOutputExposures: TFormOutputExposures
  Left = 704
  Top = 383
  Width = 700
  Caption = 'Exposures and traces for 1 iteration'
  Constraints.MinHeight = 65
  Constraints.MinWidth = 700
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 692
    Height = 57
    inherited pnlProdTypes: TPanel
      Left = 249
      Width = 442
      Height = 55
      object lblSourcePT: TLabel [0]
        Left = 24
        Top = 6
        Width = 60
        Height = 13
        Caption = 'Source type:'
      end
      object lblRecipientPT: TLabel [1]
        Left = 24
        Top = 30
        Width = 71
        Height = 13
        Caption = 'Recipient type:'
      end
      inherited lblIteration: TLabel
        Left = 296
      end
      inherited cboProdTypes: TComboBox
        Left = 118
        Width = 169
      end
      inherited pnlDecorator3: TPanel
        Height = 48
      end
      inherited pnlDecorator4: TPanel
        Height = 48
      end
      inherited cboZones: TComboBox
        Left = 192
        Top = 20
        Width = 169
        TabOrder = 5
      end
      inherited cboIteration: TComboBox
        Left = 364
      end
      object cboRecipientProdTypes: TComboBox
        Left = 118
        Top = 28
        Width = 169
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnCloseUp = cboRecipientProdTypesCloseUp
      end
    end
    inherited pnlButtons: TPanel
      Width = 120
      Height = 55
      inherited btnSaveCharts: TBitBtn
        Left = 25
        Top = 30
      end
      inherited btnCopyCharts: TBitBtn
        Left = 52
        Top = 30
      end
      inherited btnPrintCharts: TBitBtn
        Left = 79
        Top = 30
      end
      inherited pnlDecorator1: TPanel
        Height = 48
      end
      inherited pnlDecorator2: TPanel
        Height = 48
      end
    end
    inherited pnlMenu: TPanel
      Height = 55
    end
  end
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 57
    Width = 692
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 5
    Caption = 'Iteration status: completed/aborted/running'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object pnlFilterControls: TPanel [2]
    Left = 0
    Top = 105
    Width = 692
    Height = 28
    Align = alTop
    TabOrder = 2
    object lblMainFilter: TLabel
      Left = 8
      Top = 8
      Width = 50
      Height = 13
      Caption = 'Filter by:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblTextEntry: TLabel
      Left = 328
      Top = 8
      Width = 22
      Height = 13
      Caption = 'Day:'
      Visible = False
    end
    object lblExposureType: TLabel
      Left = 328
      Top = 8
      Width = 70
      Height = 13
      Caption = 'Exposure type:'
      Visible = False
    end
    object lblDiseaseState: TLabel
      Left = 384
      Top = 6
      Width = 33
      Height = 13
      Caption = 'Status:'
      Visible = False
    end
    object cboMainFilter: TComboBox
      Left = 152
      Top = 4
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '(No filter)'
      OnChange = cboMainFilterChange
      Items.Strings = (
        '(No filter)'
        'Day'
        'Source unit ID'
        'Source status'
        'Source zone'
        'Exp. type'
        'Is adequate'
        'Recipient unit ID'
        'Recipient status'
        'Recipient zone'
        'Day of initiation')
    end
    object cboExposureType: TComboBox
      Left = 476
      Top = 0
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Visible = False
      OnChange = cboExposuresChange
      Items.Strings = (
        'Exposure, direct'
        'Exposure, indirect'
        'Exposure, airborne'
        'Exposure, local-area'
        'Trace forward, direct'
        'Trace forward, indirect'
        'Trace back, direct'
        'Trace back, indirect')
    end
    object rleTextEntry: TREEdit
      Left = 472
      Top = 4
      Width = 101
      Height = 21
      EditAlign = eaLeft
      TabOrder = 2
      Visible = False
      OnEnter = rleTextEntryEnter
      OnExit = rleTextEntryExit
      OnKeyUp = rleTextEntryKeyUp
    end
    object pnlExposureSuccess: TPanel
      Left = 376
      Top = 8
      Width = 321
      Height = 22
      BevelOuter = bvNone
      TabOrder = 4
      Visible = False
      object rdoSuccess: TRadioButton
        Left = 0
        Top = 4
        Width = 129
        Height = 17
        Caption = 'Successful exposures'
        TabOrder = 0
        OnClick = rdoClick
      end
      object rdoUnsuccess: TRadioButton
        Left = 152
        Top = 4
        Width = 145
        Height = 17
        Caption = 'Unsuccessful exposures'
        TabOrder = 1
        OnClick = rdoClick
      end
    end
    object cboDiseaseState: TComboBox
      Left = 452
      Top = 16
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      Visible = False
      OnChange = cboExposuresChange
      Items.Strings = (
        'Susceptible'
        'Latent'
        'Subclinical'
        'Clinical'
        'Naturally immune'
        'Vaccine immune'
        'Destroyed')
    end
    inline fraAcceptCancel: TFrameAcceptCancel
      Left = 572
      Top = 4
      Width = 49
      Height = 23
      HorzScrollBar.Visible = False
      VertScrollBar.Visible = False
      TabOrder = 3
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
  end
  object pnlEventCounter: TPanel [3]
    Left = 0
    Top = 441
    Width = 692
    Height = 17
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'pnlEventCounter'
    Constraints.MinHeight = 17
    TabOrder = 3
  end
  object stgGrid: TARSortGrid [4]
    Left = 0
    Top = 133
    Width = 692
    Height = 308
    Align = alTop
    ColCount = 4
    DefaultRowHeight = 19
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 4
    IsReadOnly = True
    AlignmentHorz = taLeftJustify
    AlignmentVert = taTopJustify
    ProportionalScrollBars = False
    ExtendedKeys = False
    SortSymbol = sgBendsenArrow
    SortSpacingHor = 2
    SortColumn = 1
    SortOnClick = True
    FooterFont.Charset = DEFAULT_CHARSET
    FooterFont.Color = clWindowText
    FooterFont.Height = -11
    FooterFont.Name = 'MS Sans Serif'
    FooterFont.Style = []
    PrintOptions.Orientation = poPortrait
    PrintOptions.PageTitleMargin = 0
    PrintOptions.PageFooter = 'date|time|page'
    PrintOptions.HeaderSize = 10
    PrintOptions.FooterSize = 7
    PrintOptions.DateFormat = 'd-mmm-yyyy'
    PrintOptions.TimeFormat = 'h:nn'
    PrintOptions.Copies = 0
    PrintOptions.FromRow = 0
    PrintOptions.ToRow = 0
    PrintOptions.PreviewPage = 0
    PrintOptions.BorderStyle = bsNone
    PrintOptions.MarginBottom = 0
    PrintOptions.MarginLeft = 0
    PrintOptions.MarginTop = 0
    PrintOptions.MarginRight = 0
    WordWrap = False
    OnGetCellFormat = stgGridGetCellFormat
    OnBeginSort = stgGridBeginSort
    OnEndSort = stgGridEndSort
  end
  object pnlSortControls: TPanel [5]
    Left = 0
    Top = 77
    Width = 692
    Height = 28
    Align = alTop
    TabOrder = 5
    object lblMainSort: TLabel
      Left = 8
      Top = 8
      Width = 45
      Height = 13
      Caption = 'Sort by:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSortOrder: TLabel
      Left = 328
      Top = 8
      Width = 49
      Height = 13
      Caption = 'Sort order:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object cboMainSort: TComboBox
      Left = 152
      Top = 4
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Day'
      OnChange = sortControlChange
      Items.Strings = (
        'Day'
        'Exp. on day'
        'Source unit ID'
        'Source type'
        'Source status'
        'Source zone'
        'Exp. type'
        'Is adequate'
        'Recipient unit ID'
        'Recipient type'
        'Recipient status'
        'Recipient zone'
        'Day of initiation')
    end
    object rdoAscending: TRadioButton
      Left = 392
      Top = 8
      Width = 89
      Height = 13
      Caption = 'Ascending'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = sortControlChange
    end
    object rdoDescending: TRadioButton
      Left = 480
      Top = 8
      Width = 89
      Height = 13
      Caption = 'Descending'
      TabOrder = 2
      OnClick = sortControlChange
    end
  end
  object spacerPanel: TPanel [6]
    Left = 0
    Top = 458
    Width = 692
    Height = 15
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 6
  end
  inherited dlgSaveWMF: TSaveDialog
    Left = 240
  end
  inherited dlgSaveCSV: TSaveDialog
    Left = 272
  end
  inherited dlgPrint: TPrintDialog
    Left = 304
  end
end
