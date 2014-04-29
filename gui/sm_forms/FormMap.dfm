object FormMap: TFormMap
  Left = 730
  Top = 636
  Width = 625
  Height = 501
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Apparent map of units, current/final day'
  Color = clBtnFace
  Constraints.MinHeight = 501
  Constraints.MinWidth = 625
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlLegend: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 64
    Align = alTop
    TabOrder = 0
    object pnlSize: TPanel
      Left = 1
      Top = 1
      Width = 64
      Height = 62
      Align = alLeft
      TabOrder = 0
      object sbSize: TSpeedButton
        Left = 3
        Top = 16
        Width = 54
        Height = 25
        Caption = 'Size'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = sbSizeClick
      end
    end
    object pnlSizeLegend: TPanel
      Left = 65
      Top = 1
      Width = 88
      Height = 62
      Align = alLeft
      TabOrder = 1
      object pbxSizeLegend: TPaintBox
        Left = 1
        Top = 1
        Width = 86
        Height = 60
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
        OnPaint = pbxLegendPaint
      end
    end
    object pnlStatus: TPanel
      Left = 153
      Top = 1
      Width = 64
      Height = 62
      Align = alLeft
      TabOrder = 2
      object sbStatus: TSpeedButton
        Left = 3
        Top = 16
        Width = 54
        Height = 25
        Caption = 'Status'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = sbStatusClick
      end
    end
    object pnlStatusLegend: TPanel
      Left = 217
      Top = 1
      Width = 260
      Height = 62
      Align = alLeft
      Constraints.MinWidth = 260
      TabOrder = 3
      object pbxLegend: TPaintBox
        Left = 1
        Top = 1
        Width = 258
        Height = 60
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'MS Serif'
        Font.Style = []
        ParentFont = False
        OnPaint = pbxLegendPaint
      end
    end
    object pnlProdType: TPanel
      Left = 477
      Top = 1
      Width = 139
      Height = 62
      Align = alClient
      TabOrder = 4
      object pbxZoneLegend: TPaintBox
        Left = 1
        Top = 1
        Width = 137
        Height = 60
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'MS Serif'
        Font.Style = []
        ParentFont = False
        OnPaint = pbxLegendPaint
      end
    end
  end
  object sbxMap: TPanel
    Left = 0
    Top = 64
    Width = 617
    Height = 410
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    FullRepaint = False
    TabOrder = 1
    object ScrollBarH: TScrollBar
      Left = 0
      Top = 393
      Width = 617
      Height = 17
      Align = alBottom
      PageSize = 0
      TabOrder = 0
      OnScroll = ScrollBarHScroll
    end
    object ScrollBarV: TScrollBar
      Left = 598
      Top = 63
      Width = 19
      Height = 330
      Align = alRight
      Kind = sbVertical
      PageSize = 0
      TabOrder = 1
      OnScroll = ScrollBarVScroll
    end
    object Panel1: TPanel
      Left = 35
      Top = 63
      Width = 563
      Height = 330
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 2
      object pbxMap: TImage
        Left = 1
        Top = 1
        Width = 535
        Height = 397
        ParentShowHint = False
        ShowHint = True
        OnMouseDown = pbxMapMouseDown
        OnMouseMove = pbxMapMouseMove
      end
      object pbxInv: TImage
        Left = 96
        Top = 48
        Width = 113
        Height = 65
        Visible = False
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 28
      Width = 617
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 3
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 35
        Height = 35
        Align = alLeft
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 0
        object pbxMapAxis: TPaintBox
          Left = 0
          Top = 0
          Width = 35
          Height = 35
          Align = alClient
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          OnPaint = pbxMapAxisPaint
        end
      end
      object Panel6: TPanel
        Left = 35
        Top = 0
        Width = 582
        Height = 35
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 1
        object pbxMapT: TPaintBox
          Left = 0
          Top = 0
          Width = 321
          Height = 17
          Color = clAqua
          ParentColor = False
          OnPaint = pbxMapTPaint
        end
        object pbxTopBackground: TPaintBox
          Left = 56
          Top = 24
          Width = 73
          Height = 9
          OnPaint = pbxTopBackgroundPaint
        end
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 63
      Width = 35
      Height = 330
      Align = alLeft
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 4
      object pbxMapL: TPaintBox
        Left = 0
        Top = 0
        Width = 17
        Height = 396
        Color = clFuchsia
        ParentColor = False
        OnPaint = pbxMapLPaint
      end
      object pbxLeftBackground: TPaintBox
        Left = 24
        Top = 72
        Width = 9
        Height = 25
        OnPaint = pbxLeftBackgroundPaint
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 617
      Height = 28
      Align = alTop
      TabOrder = 5
      object pnlButtons: TPanel
        Left = 1
        Top = 1
        Width = 96
        Height = 26
        Align = alLeft
        TabOrder = 0
        object sbZoomIn: TSpeedButton
          Left = 8
          Top = 2
          Width = 22
          Height = 22
          Hint = 'Zoom in'
          Glyph.Data = {
            26040000424D2604000000000000360000002800000012000000120000000100
            180000000000F0030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
            0000FF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000000000FF00FF
            0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FF000000000000000000FF00FFFF00FF0000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000
            00000000000000FF00FFFF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00
            FF000000000000000000000000FF00FFFF00FF000000000000000000FF00FFFF
            00FFFF00FFFF00FF0000FF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF
            00FFFF00FF000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FF
            0000FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FF
            000000FF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF00FF000000FF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FF000000FF00FFFF00FFFF00
            FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF
            00FFFF00FFFF00FF0000FF00FF000000FF00FFFF00FF00000000000000000000
            0000000000000000FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FF
            0000FF00FF000000FF00FFFF00FF000000000000000000000000000000000000
            FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FF000000
            FF00FFFF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF0000
            00FF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FF000000FF00FFFF00
            FFFF00FF000000000000FF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FF0000FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            0000FF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF000000
            000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FF
            FF00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FF0000}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbZoomInClick
        end
        object sbZoomOut: TSpeedButton
          Left = 34
          Top = 2
          Width = 22
          Height = 22
          Hint = 'Zoom out'
          Glyph.Data = {
            26040000424D2604000000000000360000002800000012000000120000000100
            180000000000F0030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
            0000FF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000000000FF00FF
            0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FF000000000000000000FF00FFFF00FF0000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000
            00000000000000FF00FFFF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00
            FF000000000000000000000000FF00FFFF00FF000000000000000000FF00FFFF
            00FFFF00FFFF00FF0000FF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF
            00FFFF00FF000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FF
            0000FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FF
            000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FF000000FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF
            00FFFF00FFFF00FF0000FF00FF000000FF00FFFF00FF00000000000000000000
            0000000000000000FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FF
            0000FF00FF000000FF00FFFF00FF000000000000000000000000000000000000
            FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FF000000
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000
            00FF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FF000000FF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FF0000FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            0000FF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF000000
            000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FF
            FF00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF0000FF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FF0000}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbZoomOutClick
        end
        object sbFitToWindow: TSpeedButton
          Left = 60
          Top = 2
          Width = 22
          Height = 22
          Hint = 'Fit to window'
          Glyph.Data = {
            26040000424D2604000000000000360000002800000012000000120000000100
            180000000000F0030000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF0000000000000000
            0000000000000000000000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFF0000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF00FFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
            000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
            00FFFFFFFFFFFFFFFFFF00FFFFFFFFFF0000FFFFFFFFFFFF000000FFFFFFFFFF
            FFFFFFFF000000000000FFFFFF000000000000FFFFFF00000000000000000000
            0000000000FFFFFF0000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFF
            0000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFF0000FFFFFFFFFFFF
            000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
            00FFFFFFFFFFFFFFFFFF000000FFFFFF0000FFFFFFFFFFFF000000FFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFF
            FFFF000000FFFFFF0000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFF
            0000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFF0000FFFFFFFFFFFF
            0000000000000000000000000000000000000000000000000000000000000000
            00FFFFFFFFFFFFFFFFFF000000FFFFFF0000FFFFFFFFFFFF00FFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF000000FFFFFF0000FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFF000000FF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF0000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFF0000000000000000000000000000000000000000
            00000000000000000000000000FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFF0000}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbFitToWindowClick
        end
      end
      object pnlMapInfo: TPanel
        Left = 97
        Top = 1
        Width = 519
        Height = 26
        Align = alClient
        TabOrder = 1
        object pbxMapInfo: TPaintBox
          Left = 1
          Top = 1
          Width = 517
          Height = 24
          Align = alClient
          OnPaint = pbxMapInfoPaint
        end
        object cboProdTypes: TComboBox
          Left = 340
          Top = 1
          Width = 173
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cboProdTypes3Change
        end
      end
    end
  end
end
