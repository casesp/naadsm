object FrameOutputStats: TFrameOutputStats
  Left = 0
  Top = 0
  Width = 410
  Height = 820
  Constraints.MinWidth = 400
  TabOrder = 0
  OnResize = FrameResize
  object pnlWholeSection: TPanel
    Left = 0
    Top = 0
    Width = 410
    Height = 820
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 0
      Top = 473
      Width = 410
      Height = 2
      Cursor = crVSplit
      Align = alTop
      MinSize = 20
      ResizeStyle = rsLine
      OnMoved = Splitter2Moved
    end
    object pnlTopSection: TPanel
      Left = 0
      Top = 0
      Width = 410
      Height = 473
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 195
        Width = 410
        Height = 2
        Cursor = crVSplit
        Align = alTop
        MinSize = 20
        ResizeStyle = rsLine
        OnMoved = Splitter1Moved
      end
      object pnlTable: TPanel
        Left = 0
        Top = 0
        Width = 410
        Height = 195
        Align = alTop
        Constraints.MinHeight = 25
        TabOrder = 0
        object pnlTableHeader: TPanel
          Left = 1
          Top = 1
          Width = 408
          Height = 25
          Align = alTop
          Alignment = taLeftJustify
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object lblTable: TLabel
            Left = 16
            Top = 4
            Width = 93
            Height = 13
            Caption = 'Output statistics'
          end
        end
        inline fraTable: TFrameOutputStatsTable
          Left = 1
          Top = 26
          Width = 408
          Height = 168
          Align = alClient
          TabOrder = 1
          inherited stgGrid: TARSyncGrid
            Width = 398
            Height = 168
            OnKeyUp = fraTablestgGridKeyUp
            OnMouseDown = fraTablestgOutputStatsMouseDown
            OnSelectCell = fraTablestgOutputStatsSelectCell
          end
          inherited pnlSpacer: TPanel
            Left = 398
            Height = 168
          end
        end
      end
      object pnlHistogram: TPanel
        Left = 0
        Top = 197
        Width = 410
        Height = 276
        Align = alClient
        Caption = 'Select an output above to display this chart'
        Constraints.MinHeight = 25
        TabOrder = 1
        object pnlHistogramHeader: TPanel
          Left = 1
          Top = 1
          Width = 408
          Height = 25
          Align = alTop
          Alignment = taLeftJustify
          BorderWidth = 5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object cbxHistogram: TCheckBox
            Left = 5
            Top = 4
            Width = 308
            Height = 17
            Caption = 'Histogram'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            State = cbChecked
            TabOrder = 0
            OnClick = cbxClick
          end
        end
        inline fraHistogram: TFrameArrayHistogram
          Left = 1
          Top = 26
          Width = 408
          Height = 249
          Align = alClient
          TabOrder = 1
          inherited pnlControls: TPanel
            Top = 218
            Width = 408
          end
          inherited chtHistogram: TChart
            Width = 408
            Height = 218
          end
        end
      end
    end
    object pnlConvergence: TPanel
      Left = 0
      Top = 475
      Width = 410
      Height = 345
      Align = alClient
      Caption = 'Select an output above to display this chart'
      Constraints.MinHeight = 25
      TabOrder = 1
      object pnlConvergenceHeader: TPanel
        Left = 1
        Top = 1
        Width = 408
        Height = 25
        Align = alTop
        Alignment = taLeftJustify
        BorderWidth = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object cbxConvergence: TCheckBox
          Left = 5
          Top = 4
          Width = 404
          Height = 17
          Caption = 'Convergence'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbxClick
        end
      end
      inline fraConvergence: TFrameArrayConvergence
        Left = 1
        Top = 26
        Width = 408
        Height = 318
        Align = alClient
        TabOrder = 1
        inherited pnlControls: TPanel
          Top = 275
          Width = 408
          inherited rdgConvergenceParam: TRadioGroup
            Width = 406
          end
        end
        inherited chtConvergence: TChart
          Width = 408
          Height = 275
          Align = alClient
        end
      end
    end
  end
end
