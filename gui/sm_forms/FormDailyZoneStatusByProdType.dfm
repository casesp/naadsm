inherited FormDailyZoneStatusByProdType: TFormDailyZoneStatusByProdType
  Left = 634
  Top = 189
  Height = 480
  Caption = 'Daily zone status for 1 iteration'
  Constraints.MinWidth = 755
  KeyPreview = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlControls: TPanel
    inherited pnlProdTypes: TPanel
      inherited cboProdTypes: TComboBox
        Width = 169
        OnChange = cboProdTypesChange
      end
      inherited cboZones: TComboBox
        Left = 192
        Width = 129
      end
    end
  end
  inline fraBody: TFrameDailyZoneStatusByProdType [1]
    Left = 0
    Top = 53
    Width = 747
    Height = 400
    Align = alClient
    TabOrder = 1
    inherited pnlChartOptions: TPanel
      Top = 375
      Width = 747
      inherited cbxThreeD: TCheckBox
        OnClick = cbxThreeDClick
      end
    end
    inherited chtOutputs: TChart
      Width = 747
      Height = 375
      MarginBottom = 5
      MarginTop = 10
      Legend.LegendStyle = lsSeries
    end
  end
  object Y_AxisSource: TRadioGroup [2]
    Left = 136
    Top = 64
    Width = 521
    Height = 33
    Caption = 'Y-Axis'
    Columns = 6
    ItemIndex = 0
    Items.Strings = (
      'Area'
      'Perimeter'
      '# of units'
      '# of animals'
      'Unit days'
      'Animal days')
    TabOrder = 2
    OnClick = Y_AxisSourceClick
  end
  object pnlCaption: TPanel [3]
    Left = 0
    Top = 33
    Width = 747
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Iteration status: completed/aborted/running'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  inherited dlgSaveWMF: TSaveDialog [4]
  end
  inherited dlgSaveCSV: TSaveDialog [5]
  end
end
