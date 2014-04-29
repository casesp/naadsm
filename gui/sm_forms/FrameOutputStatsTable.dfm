inherited FrameOutputStatsTable: TFrameOutputStatsTable
  inherited stgGrid: TARSyncGrid
    FixedCols = 2
    ParentShowHint = False
    PopupMenu = PopupMenu1
    ShowHint = True
    OnMouseDown = stgGridMouseDown
    OnMouseMove = stgGridMouseMove
    OnMouseWheelDown = stgGridMouseWheelDown
    OnMouseWheelUp = stgGridMouseWheelUp
    ColWidths = (
      64
      64
      64
      64
      64)
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Plain text file (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofEnableSizing]
    Title = 'Name of database to save'
    Left = 48
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    Left = 184
    Top = 168
    object Copyrawdatatoclipboard1: TMenuItem
      Caption = 'Copy raw data to clipboard'
      OnClick = Copyrawdatatoclipboard1Click
    end
    object Exportrawdatatofile1: TMenuItem
      Caption = 'Export raw data to file...'
      OnClick = Exportrawdatatofile1Click
    end
  end
end
