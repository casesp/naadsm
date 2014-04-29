inherited FormOutputEvents: TFormOutputEvents
  Left = 471
  Top = 295
  Width = 755
  Height = 510
  VertScrollBar.Visible = False
  Caption = 'Events for 1 iteration'
  Constraints.MinWidth = 755
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    Width = 747
    inherited pnlProdTypes: TPanel
      Width = 445
      object lblIteration: TLabel [0]
        Left = 328
        Top = 8
        Width = 41
        Height = 13
        Caption = 'Iteration:'
      end
      inherited cboProdTypes: TComboBox
        Width = 169
      end
      inherited cboZones: TComboBox
        Left = 192
        Width = 129
      end
      object cboIteration: TComboBox
        Left = 384
        Top = 4
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = cboIterationChange
      end
    end
  end
  object stgGrid: TARSortGrid [1]
    Left = 0
    Top = 109
    Width = 747
    Height = 340
    Align = alTop
    ColCount = 4
    DefaultRowHeight = 19
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 1
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
  object pnlCaption: TPanel [2]
    Left = 0
    Top = 33
    Width = 747
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
    TabOrder = 2
  end
  object pnlFilterControls: TPanel [3]
    Left = 0
    Top = 81
    Width = 747
    Height = 28
    Align = alTop
    TabOrder = 3
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
    object lblDay: TLabel
      Left = 304
      Top = 8
      Width = 22
      Height = 13
      Caption = 'Day:'
      Visible = False
    end
    object lblEventType: TLabel
      Left = 352
      Top = 8
      Width = 54
      Height = 13
      Caption = 'Event type:'
      Visible = False
    end
    object lblHerdID: TLabel
      Left = 368
      Top = 32
      Width = 36
      Height = 13
      Caption = 'Unit ID:'
      Visible = False
    end
    object cboMainFilter: TComboBox
      Left = 152
      Top = 4
      Width = 137
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
        'Unit ID'
        'Event type')
    end
    object cboEvents: TComboBox
      Left = 404
      Top = 8
      Width = 165
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Visible = False
      OnChange = cboEventsChange
      Items.Strings = (
        'Infections'
        'State changes'
        'Detections'
        'Destructions'
        'Vaccinations'
        'Traces of direct contact'
        'Traces of indirect contact'
        'Zone changes'
        'Creation of zone foci')
    end
    object rleDay: TREEdit
      Left = 396
      Top = 4
      Width = 101
      Height = 21
      EditAlign = eaLeft
      TabOrder = 2
      Visible = False
      OnEnter = rleDayEnter
      OnExit = rleExit
      OnKeyUp = rleKeyUp
    end
    inline fraAcceptCancel: TFrameAcceptCancel
      Left = 496
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
    object rleHerdID: TREEdit
      Left = 412
      Top = 23
      Width = 101
      Height = 21
      EditAlign = eaLeft
      TabOrder = 4
      Visible = False
      OnEnter = rleHerdIDEnter
      OnExit = rleExit
      OnKeyUp = rleKeyUp
    end
  end
  object pnlEventCounter: TPanel [4]
    Left = 0
    Top = 449
    Width = 747
    Height = 18
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'pnlEventCounter'
    Constraints.MinHeight = 18
    TabOrder = 4
  end
  object pnlSortControls: TPanel [5]
    Left = 0
    Top = 53
    Width = 747
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
      Left = 304
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
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Day'
      OnChange = sortControlChange
      Items.Strings = (
        'Day'
        'Event order on day'
        'Unit ID'
        'Unit type'
        'Event type'
        'New state'
        'Successful trace')
    end
    object rdoAscending: TRadioButton
      Left = 384
      Top = 8
      Width = 81
      Height = 13
      Caption = 'Ascending'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = sortControlChange
    end
    object rdoDescending: TRadioButton
      Left = 472
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
    Top = 467
    Width = 747
    Height = 16
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 6
  end
end
