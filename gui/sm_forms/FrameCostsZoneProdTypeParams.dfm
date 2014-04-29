object FrameCostsZoneProdTypeParams: TFrameCostsZoneProdTypeParams
  Left = 0
  Top = 0
  Width = 447
  Height = 74
  TabOrder = 0
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 24
    Align = alTop
    TabOrder = 0
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
  object pnlCostParams: TPanel
    Left = 0
    Top = 24
    Width = 447
    Height = 50
    Align = alClient
    TabOrder = 1
    object lblCostPerAnimalDay: TLabel
      Left = 24
      Top = 14
      Width = 184
      Height = 13
      Caption = 'Cost of surveillance per animal per day:'
    end
    object lblDollar: TLabel
      Left = 240
      Top = 14
      Width = 18
      Height = 13
      Caption = '    $'
    end
    object rleCostPerAnimalDay: TREEdit
      Left = 262
      Top = 10
      Width = 65
      Height = 21
      EditAlign = eaLeft
      TabOrder = 0
      OnKeyDown = rleKeyDown
    end
  end
end
