object FrameSingleCostTable: TFrameSingleCostTable
  Left = 0
  Top = 0
  Width = 578
  Height = 150
  Constraints.MinHeight = 150
  TabOrder = 0
  object pnlShowCumul: TPanel
    Left = 0
    Top = 0
    Width = 578
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object cbxShowCumul: TCheckBox
      Left = 8
      Top = 4
      Width = 241
      Height = 17
      Caption = 'Show cumulative costs'
      TabOrder = 0
      OnClick = cbxShowCumulClick
    end
  end
  inline fraGrid: TFrameStringGridBase
    Left = 0
    Top = 25
    Width = 578
    Height = 125
    Align = alClient
    TabOrder = 1
    inherited stgGrid: TARSyncGrid
      Width = 568
      Height = 125
    end
    inherited pnlSpacer: TPanel
      Left = 568
      Height = 125
    end
  end
end
