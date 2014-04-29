object FrameZoneProdTypeParams: TFrameZoneProdTypeParams
  Left = 0
  Top = 0
  Width = 447
  Height = 336
  TabOrder = 0
  object pnlDirectMovement: TPanel
    Left = 0
    Top = 24
    Width = 447
    Height = 108
    Align = alTop
    TabOrder = 0
    object pnlUseDirectMovementControl: TPanel
      Left = 1
      Top = 1
      Width = 445
      Height = 24
      Align = alTop
      TabOrder = 0
      object cbxUseDirectMovementControl: TCheckBox
        Left = 16
        Top = 4
        Width = 401
        Height = 17
        Caption = 'Alter direct movement rate for this production type in this zone'
        TabOrder = 0
        OnClick = cbxClick
      end
    end
    object pnlDirectMovementParams: TPanel
      Left = 1
      Top = 25
      Width = 445
      Height = 82
      Align = alClient
      TabOrder = 1
      object lblDirectMovement: TLabel
        Left = 36
        Top = 10
        Width = 377
        Height = 13
        Caption = 'Effect on baseline direct movement rate from units of this type:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object imgRel1: TImage
        Left = 16
        Top = 8
        Width = 16
        Height = 16
        Hint = 'This parameter is a relational function'
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
      end
      inline smrDirectMovement: TFrameSMFunctionEditor
        Left = 24
        Top = 28
        Width = 401
        Height = 50
        Constraints.MinHeight = 45
        Constraints.MinWidth = 390
        TabOrder = 0
      end
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 24
    Align = alTop
    TabOrder = 1
    object lblZoneDescr: TLabel
      Left = 8
      Top = 4
      Width = 96
      Height = 13
      Caption = 'Zone description'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlIndirectMovement: TPanel
    Left = 0
    Top = 132
    Width = 447
    Height = 109
    Align = alTop
    TabOrder = 2
    object pnlUseIndirectMovementControl: TPanel
      Left = 1
      Top = 1
      Width = 445
      Height = 24
      Align = alTop
      TabOrder = 0
      object cbxUseIndirectMovementControl: TCheckBox
        Left = 16
        Top = 4
        Width = 401
        Height = 17
        Caption = 
          'Alter indirect movement rate for this production type in this zo' +
          'ne'
        TabOrder = 0
        OnClick = cbxClick
      end
    end
    object pnlIndirectMovementParams: TPanel
      Left = 1
      Top = 25
      Width = 445
      Height = 83
      Align = alClient
      TabOrder = 1
      object lblIndirectMovement: TLabel
        Left = 36
        Top = 10
        Width = 385
        Height = 13
        Caption = 
          'Effect on baseline indirect movement rate from units of this typ' +
          'e:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object imgRel2: TImage
        Left = 16
        Top = 8
        Width = 16
        Height = 16
        Hint = 'This parameter is a relational function'
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
      end
      inline smrIndirectMovement: TFrameSMFunctionEditor
        Left = 24
        Top = 28
        Width = 401
        Height = 50
        Constraints.MinHeight = 45
        Constraints.MinWidth = 390
        TabOrder = 0
      end
    end
  end
  object pnlDetection: TPanel
    Left = 0
    Top = 241
    Width = 447
    Height = 81
    Align = alTop
    TabOrder = 3
    object pnlUseDetectionMultiplier: TPanel
      Left = 1
      Top = 1
      Width = 445
      Height = 24
      Align = alTop
      TabOrder = 0
      object cbxUseDetectionMultiplier: TCheckBox
        Left = 16
        Top = 4
        Width = 401
        Height = 17
        Caption = 
          'Alter the probability of detection for this production type in t' +
          'his zone'
        TabOrder = 0
        OnClick = cbxClick
      end
    end
    object pnlDetectionParams: TPanel
      Left = 1
      Top = 25
      Width = 445
      Height = 55
      Align = alClient
      TabOrder = 1
      object lblDetectionMultiplier: TLabel
        Left = 24
        Top = 6
        Width = 298
        Height = 39
        Caption = 
          'Multiplier for the probability of observing clinical signs: (Not' +
          'e: probability of reporting an observed clinical unit within a z' +
          'one is 1.)'
        WordWrap = True
      end
      object rleDetectionMultiplier: TREEdit
        Left = 336
        Top = 12
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnKeyDown = rleKeyDown
      end
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 322
    Width = 447
    Height = 2
    Align = alTop
    TabOrder = 4
  end
end
