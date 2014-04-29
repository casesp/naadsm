object FrameEpiIterationSummary: TFrameEpiIterationSummary
  Left = 0
  Top = 0
  Width = 555
  Height = 688
  Constraints.MinWidth = 555
  TabOrder = 0
  OnResize = FrameResize
  object pbpGraphTableTabs: TPBPageControl
    Left = 0
    Top = 0
    Width = 555
    Height = 688
    ActivePage = tabGraphs
    Align = alClient
    TabOrder = 0
    object tabGraphs: TTabSheet
      Caption = 'Graphical view'
      object pnlInapparentChart: TPanel
        Left = 0
        Top = 0
        Width = 547
        Height = 240
        Align = alTop
        TabOrder = 0
        object pnlInapparent: TPanel
          Left = 1
          Top = 1
          Width = 545
          Height = 18
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object cbxInapparent: TCheckBox
            Left = 5
            Top = 0
            Width = 404
            Height = 17
            Caption = 'Actual Epidemic Curve -- includes all infections'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbxInapparentClick
          end
        end
        inline fraInapparent: TFrameSingleEpiCurve
          Left = 1
          Top = 19
          Width = 545
          Height = 220
          Align = alClient
          TabOrder = 1
          inherited chtCurve: TChart
            Width = 545
            Height = 220
            Title.Text.Strings = (
              'Actual epidemic curve')
          end
        end
      end
      object pnlApparentChart: TPanel
        Left = 0
        Top = 240
        Width = 547
        Height = 420
        Align = alClient
        TabOrder = 1
        object pnlApparent: TPanel
          Left = 1
          Top = 1
          Width = 545
          Height = 18
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object cbxApparent: TCheckBox
            Left = 5
            Top = 0
            Width = 308
            Height = 17
            Caption = 'Apparent Epidemic Curve -- includes only detected infections'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            State = cbChecked
            TabOrder = 0
            OnClick = cbxApparentClick
          end
        end
        inline fraApparent: TFrameSingleEpiCurve
          Left = 1
          Top = 19
          Width = 545
          Height = 400
          Align = alClient
          TabOrder = 1
          inherited chtCurve: TChart
            Width = 545
            Height = 400
            Title.Text.Strings = (
              'Apparent epidemic curve')
          end
        end
      end
    end
    object tabTables: TTabSheet
      Caption = 'Tabular view'
      ImageIndex = 1
      object PanelInf: TPanel
        Left = 0
        Top = 125
        Width = 547
        Height = 123
        Align = alTop
        TabOrder = 0
        object pnlInfHeader: TPanel
          Left = 1
          Top = 1
          Width = 545
          Height = 18
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object cbxInf: TCheckBox
            Left = 5
            Top = 0
            Width = 404
            Height = 17
            Caption = 'Reasons for infection -- includes all infections'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbxInfClick
          end
        end
        inline fraSgInf: TFrameStringGridBase
          Left = 1
          Top = 19
          Width = 545
          Height = 103
          Align = alClient
          TabOrder = 1
          OnExit = exitStringGrid
          inherited stgGrid: TARSyncGrid
            Width = 535
            Height = 103
            ColCount = 3
            FixedRows = 0
            ScrollBars = ssNone
          end
          inherited pnlSpacer: TPanel
            Left = 535
            Height = 103
          end
        end
      end
      object PanelVac: TPanel
        Left = 0
        Top = 393
        Width = 547
        Height = 72
        Align = alTop
        TabOrder = 1
        object pnlVacHeader: TPanel
          Left = 1
          Top = 1
          Width = 545
          Height = 18
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object cbxVac: TCheckBox
            Left = 5
            Top = 0
            Width = 244
            Height = 17
            Caption = 'Vaccination'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbxVacClick
          end
        end
        inline fraSgVac: TFrameStringGridBase
          Left = 1
          Top = 19
          Width = 545
          Height = 52
          Align = alClient
          TabOrder = 1
          OnExit = exitStringGrid
          inherited stgGrid: TARSyncGrid
            Width = 535
            Height = 52
            ColCount = 3
            RowCount = 2
            FixedRows = 0
            ScrollBars = ssVertical
            RowHeights = (
              19
              19)
          end
          inherited pnlSpacer: TPanel
            Left = 535
            Height = 52
          end
        end
      end
      object pnlAsterisk: TPanel
        Left = 0
        Top = 465
        Width = 547
        Height = 195
        Align = alClient
        Alignment = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        BorderWidth = 5
        TabOrder = 2
        object lblAsterisk: TLabel
          Left = 5
          Top = 5
          Width = 537
          Height = 185
          Align = alClient
          AutoSize = False
          Caption = 
            '  * In the course of a simulation run, these activities may occu' +
            'r more than once on a single unit'
          WordWrap = True
        end
      end
      object pnlDestr: TPanel
        Left = 0
        Top = 248
        Width = 547
        Height = 145
        Align = alTop
        TabOrder = 3
        object pnlDestrHeader: TPanel
          Left = 1
          Top = 1
          Width = 545
          Height = 18
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object cbxDestr: TCheckBox
            Left = 5
            Top = 0
            Width = 241
            Height = 17
            Caption = 'Destruction'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbxDestrClick
          end
        end
        inline fraSgDestr: TFrameStringGridBase
          Left = 1
          Top = 19
          Width = 545
          Height = 125
          Align = alClient
          TabOrder = 1
          OnExit = exitStringGrid
          inherited stgGrid: TARSyncGrid
            Width = 535
            Height = 125
            ColCount = 3
            RowCount = 6
            FixedRows = 0
            ScrollBars = ssNone
            RowHeights = (
              19
              19
              19
              19
              19
              19)
          end
          inherited pnlSpacer: TPanel
            Left = 535
            Height = 125
          end
          inherited StringGrid1: TStringGrid
            DefaultRowHeight = 19
          end
        end
      end
      object pnlSurv: TPanel
        Left = 0
        Top = 28
        Width = 547
        Height = 97
        Align = alTop
        TabOrder = 4
        object pnlSurvHeader: TPanel
          Left = 1
          Top = 1
          Width = 545
          Height = 18
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object cbxSurv: TCheckBox
            Left = 5
            Top = 0
            Width = 228
            Height = 17
            Caption = 'Detection and Tracing'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbxSurvClick
          end
        end
        inline fraSgSurv: TFrameStringGridBase
          Left = 1
          Top = 19
          Width = 545
          Height = 77
          Align = alClient
          TabOrder = 1
          OnExit = exitStringGrid
          inherited stgGrid: TARSyncGrid
            Width = 535
            Height = 77
            ColCount = 3
            RowCount = 3
            FixedRows = 0
            ScrollBars = ssVertical
            RowHeights = (
              19
              19
              19)
          end
          inherited pnlSpacer: TPanel
            Left = 535
            Height = 77
          end
        end
      end
      object sgHeader: TStringGrid
        Left = 0
        Top = 0
        Width = 547
        Height = 28
        Align = alTop
        ColCount = 3
        Constraints.MaxHeight = 28
        Constraints.MinHeight = 28
        FixedCols = 0
        RowCount = 2
        ScrollBars = ssNone
        TabOrder = 5
      end
    end
  end
end
