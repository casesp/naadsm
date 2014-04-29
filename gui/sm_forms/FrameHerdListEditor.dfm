object FrameHerdListEditor: TFrameHerdListEditor
  Left = 0
  Top = 0
  Width = 696
  Height = 401
  TabOrder = 0
  object stgHerds: TARSortGrid
    Left = 0
    Top = 150
    Width = 696
    Height = 226
    Align = alClient
    ColCount = 7
    DefaultRowHeight = 18
    FixedCols = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
    TabOrder = 3
    OnDrawCell = stgHerdsDrawCell
    OnEnter = stgHerdsEnter
    OnExit = stgHerdsExit
    OnKeyDown = stgHerdsKeyDown
    OnKeyUp = stgHerdsKeyUp
    OnMouseDown = stgHerdsMouseDown
    OnMouseWheelDown = stgHerdsMouseWheel
    OnMouseWheelUp = stgHerdsMouseWheel
    OnSelectCell = stgHerdsSelectCell
    OnSetEditText = stgHerdsSetEditText
    IsReadOnly = False
    AlignmentHorz = taLeftJustify
    AlignmentVert = taTopJustify
    ProportionalScrollBars = True
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
    PrintOptions.FromRow = 0
    PrintOptions.ToRow = 0
    PrintOptions.BorderStyle = bsNone
    PrintOptions.MarginBottom = 0
    PrintOptions.MarginLeft = 0
    PrintOptions.MarginTop = 0
    PrintOptions.MarginRight = 0
    WordWrap = False
    OnGetCellFormat = stgHerdsGetCellFormat
    OnBeginSort = stgHerdsBeginSort
    OnEndSort = stgHerdsEndSort
    OnVScroll = stgHerdsScroll
    OnHScroll = stgHerdsScroll
    OnMouseWheel = stgHerdsScroll
    RowHeights = (
      18
      18
      18
      18
      18)
  end
  object cboProdType: TComboBox
    Left = 96
    Top = 128
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cboProdTypeChange
    OnExit = cboExit
    OnKeyDown = cboKeyDown
    OnKeyPress = cboKeyPress
    OnKeyUp = cboKeyUp
  end
  object cboTransitionState: TComboBox
    Left = 240
    Top = 128
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    Visible = False
    OnChange = cboTransitionStateChange
    OnExit = cboExit
    OnKeyDown = cboKeyDown
    OnKeyPress = cboKeyPress
    OnKeyUp = cboKeyUp
  end
  object pnlMessage: TPanel
    Left = 384
    Top = 216
    Width = 177
    Height = 121
    Caption = '(Change this message)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    object lblMessage: TLabel
      Left = 40
      Top = 24
      Width = 64
      Height = 13
      Alignment = taCenter
      Caption = 'lblMessage'
      WordWrap = True
    end
  end
  object pnlBeforeProdType: TPanel
    Left = 16
    Top = 152
    Width = 100
    Height = 50
    Caption = 'pnlBeforeProdType'
    TabOrder = 0
    TabStop = True
    OnEnter = pnlBeforeProdTypeEnter
  end
  object pnlAfterProdType: TPanel
    Left = 176
    Top = 160
    Width = 100
    Height = 50
    Caption = 'pnlAfterProdType'
    TabOrder = 2
    TabStop = True
    OnEnter = pnlAfterProdTypeEnter
  end
  object pnlBeforeDiseaseState: TPanel
    Left = 312
    Top = 156
    Width = 100
    Height = 50
    Caption = 'pnlBeforeDiseaseState'
    TabOrder = 4
    TabStop = True
    OnEnter = pnlBeforeDiseaseStateEnter
  end
  object pnlAfterDiseaseState: TPanel
    Left = 360
    Top = 84
    Width = 100
    Height = 50
    Caption = 'pnlAfterDiseaseState'
    TabOrder = 6
    TabStop = True
    OnEnter = pnlAfterDiseaseStateEnter
  end
  object pnlSortControls: TPanel
    Left = 0
    Top = 0
    Width = 696
    Height = 33
    Align = alTop
    TabOrder = 8
    object pnlSortControlsLeft: TPanel
      Left = 1
      Top = 1
      Width = 344
      Height = 31
      Align = alLeft
      TabOrder = 0
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
      object cboMainSort: TComboBox
        Left = 160
        Top = 6
        Width = 177
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Order'
        OnChange = sortControlChange
        Items.Strings = (
          'Order'
          'ID'
          'Production type'
          'Unit size'
          'Latitude'
          'Longitude'
          'Status'
          'Days left in staus')
      end
    end
    object pnlSortControlsRight: TPanel
      Left = 345
      Top = 1
      Width = 350
      Height = 31
      Align = alClient
      TabOrder = 1
      object lblSortOrder: TLabel
        Left = 8
        Top = 8
        Width = 61
        Height = 13
        Caption = 'Sort order:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rdoAscending: TRadioButton
        Left = 145
        Top = 8
        Width = 89
        Height = 13
        Caption = 'Ascending'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = sortControlChange
      end
      object rdoDescending: TRadioButton
        Left = 233
        Top = 8
        Width = 97
        Height = 13
        Caption = 'Descending'
        TabOrder = 1
        OnClick = sortControlChange
      end
      object btnTest: TButton
        Left = 73
        Top = 8
        Width = 57
        Height = 17
        Caption = 'btnTest'
        TabOrder = 2
        OnClick = btnTestClick
      end
    end
  end
  object pnlFilterControls: TPanel
    Left = 0
    Top = 33
    Width = 696
    Height = 117
    Align = alTop
    TabOrder = 9
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
    object lblHerdSize: TLabel
      Left = 352
      Top = 8
      Width = 43
      Height = 13
      Caption = 'Unit size:'
      Visible = False
    end
    object lblProdTypeFilter: TLabel
      Left = 32
      Top = 48
      Width = 77
      Height = 13
      Caption = 'Production type:'
      Visible = False
    end
    object lblLat: TLabel
      Left = 56
      Top = 32
      Width = 41
      Height = 13
      Caption = 'Latitude:'
      Visible = False
    end
    object lblLon: TLabel
      Left = 112
      Top = 32
      Width = 50
      Height = 13
      Caption = 'Longitude:'
      Visible = False
    end
    object lblDaysLeft: TLabel
      Left = 128
      Top = 48
      Width = 86
      Height = 13
      Caption = 'Days left in status:'
      Visible = False
    end
    object lblStatus: TLabel
      Left = 56
      Top = 72
      Width = 33
      Height = 13
      Caption = 'Status:'
      Visible = False
    end
    object cboMainFilter: TComboBox
      Left = 160
      Top = 4
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '(No filter)'
      OnChange = cboMainFilterChange
      OnEnter = cboMainFilterEnter
      Items.Strings = (
        '(No filter)'
        'Production type'
        'Unit size'
        'Geographic range'
        'Specific latitude'
        'Specific longitude'
        'Status'
        'Days left in status')
    end
    object cboProdTypeFilter: TComboBox
      Left = 260
      Top = 40
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Visible = False
      OnChange = cboFilterChange
    end
    object rleHerdSize: TREEdit
      Left = 532
      Top = 4
      Width = 101
      Height = 21
      EditAlign = eaLeft
      TabOrder = 2
      Visible = False
      OnEnter = rleEnter
      OnExit = rleExit
      OnKeyDown = rleKeyDown
    end
    object rleLat: TREEdit
      Left = 580
      Top = 15
      Width = 101
      Height = 21
      EditAlign = eaLeft
      TabOrder = 3
      Visible = False
      OnEnter = rleEnter
      OnExit = rleExit
      OnKeyDown = rleKeyDown
    end
    inline fraAcceptCancel: TFrameAcceptCancel
      Left = 640
      Top = 3
      Width = 49
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
        OnExit = fraAcceptCancelbtnCancelExit
      end
    end
    object rleLon: TREEdit
      Left = 580
      Top = 47
      Width = 101
      Height = 21
      EditAlign = eaLeft
      TabOrder = 5
      Visible = False
      OnEnter = rleEnter
      OnExit = rleExit
      OnKeyDown = rleKeyDown
    end
    object rleDaysLeft: TREEdit
      Left = 452
      Top = 44
      Width = 101
      Height = 21
      EditAlign = eaLeft
      TabOrder = 6
      Visible = False
      OnEnter = rleEnter
      OnExit = rleExit
      OnKeyDown = rleKeyDown
    end
    object cboStatus: TComboBox
      Left = 420
      Top = 80
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 7
      Visible = False
      OnChange = cboFilterChange
    end
    object pnlGeoRange: TPanel
      Left = 176
      Top = 64
      Width = 321
      Height = 41
      BevelOuter = bvNone
      Color = clGradientActiveCaption
      TabOrder = 8
      Visible = False
      object lblGeoRange: TLabel
        Left = 1
        Top = 4
        Width = 166
        Height = 13
        Caption = 'Please specify a geographic range.'
      end
      object btnGeoRange: TButton
        Left = 8
        Top = 20
        Width = 49
        Height = 18
        Caption = 'Edit...'
        TabOrder = 0
        OnClick = btnGeoRangeClick
      end
    end
  end
  object pnlHerdCounter: TPanel
    Left = 0
    Top = 376
    Width = 696
    Height = 25
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'pnlHerdCounter'
    TabOrder = 10
  end
  object pmnPopup: TPopupMenu
    AutoPopup = False
    Left = 24
    Top = 104
    object mnuRemoveUnits: TMenuItem
      Caption = 'Remove unit'
      OnClick = mnuRemoveUnitsClick
    end
  end
end
