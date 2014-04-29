object FrameDestrGlobal: TFrameDestrGlobal
  Left = 0
  Top = 0
  Width = 645
  Height = 212
  TabOrder = 0
  object pnlDestrGlobal: TPanel
    Left = 0
    Top = 0
    Width = 645
    Height = 212
    Align = alClient
    TabOrder = 0
    object pnlUseDestrGlobal: TPanel
      Left = 1
      Top = 1
      Width = 643
      Height = 64
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object cbxUseDestruction: TCheckBox
        Left = 16
        Top = 8
        Width = 593
        Height = 41
        Caption = 
          'Use destruction for disease control for some or all production t' +
          'ypes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        WordWrap = True
        OnClick = cbxUseDestructionClick
      end
    end
    object pnlDestrParams: TPanel
      Left = 1
      Top = 65
      Width = 643
      Height = 146
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lblDestrCapacity: TLabel
        Left = 48
        Top = 56
        Width = 215
        Height = 13
        Caption = 'Destruction capacity (units per day over time):'
      end
      object lblDestrProgramDelay: TLabel
        Left = 24
        Top = 8
        Width = 254
        Height = 13
        Caption = 'Delay before implementing destruction program (days):'
        WordWrap = True
      end
      object imgRel1: TImage
        Left = 24
        Top = 56
        Width = 16
        Height = 16
        Hint = 'This parameter is a relational function'
        ParentShowHint = False
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          000010000000010018000000000000030000120B0000120B0000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000008000008000F6F5F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000080000080000080000080
          00008000008000F1F8F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000FFFFFFFFFFFFFFFFFFF0EFF0008000008000C4E3C5FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFC7DFC7008000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008000008000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFCFCFC008000008000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008000
          F9F7F9FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFEDEDED008000008000FFFFFFF5FAF5E1F1E200
          0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          008000008000008000008000008000000000000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCBE6CCA5D2A6F3F3F300800000800000
          0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
        ShowHint = True
      end
      inline smrDestrCapacity: TFrameSMFunctionEditor
        Left = 32
        Top = 76
        Width = 401
        Height = 57
        Constraints.MinHeight = 45
        Constraints.MinWidth = 390
        TabOrder = 0
      end
      object rleDestrProgramDelay: TREEdit
        Left = 352
        Top = 8
        Width = 81
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = processText
      end
    end
  end
end
