object FormLatLonRange: TFormLatLonRange
  Left = 612
  Top = 206
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Lat/lon range selection'
  ClientHeight = 316
  ClientWidth = 458
  Color = clBtnFace
  Constraints.MinWidth = 466
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTopSpacer: TPanel
    Left = 0
    Top = 0
    Width = 458
    Height = 8
    Align = alTop
    BevelOuter = bvNone
    Color = clCream
    TabOrder = 0
  end
  object pnlRadioControls: TPanel
    Left = 0
    Top = 8
    Width = 458
    Height = 97
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object gbxIncludeExclude: TGroupBox
      Left = 137
      Top = 0
      Width = 321
      Height = 97
      Align = alLeft
      Caption = 'Inclusion/exclusion '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object rdoInclude: TRadioButton
        Left = 16
        Top = 20
        Width = 249
        Height = 29
        Caption = 'Select units within the specified area'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        WordWrap = True
        OnClick = rdoIncludeClick
      end
      object rdoExclude: TRadioButton
        Left = 16
        Top = 52
        Width = 297
        Height = 29
        Caption = 'Select units that are NOT within the specified area'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        WordWrap = True
        OnClick = rdoIncludeClick
      end
    end
    object gbxShape: TGroupBox
      Left = 0
      Top = 0
      Width = 137
      Height = 97
      Align = alLeft
      Caption = 'Area shape '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object rdoRect: TRadioButton
        Left = 16
        Top = 28
        Width = 113
        Height = 17
        Caption = 'Rectangular area'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rdoShapeClick
      end
      object rdoCircle: TRadioButton
        Left = 16
        Top = 60
        Width = 113
        Height = 17
        Caption = 'Circular area'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = rdoShapeClick
      end
    end
    object pnlVertSpacer1: TPanel
      Left = 458
      Top = 0
      Width = 8
      Height = 97
      Align = alLeft
      BevelOuter = bvNone
      Color = clSkyBlue
      TabOrder = 2
    end
    object pnlVertSpacer3: TPanel
      Left = 474
      Top = 0
      Width = 0
      Height = 97
      Align = alClient
      BevelOuter = bvNone
      Color = clSkyBlue
      TabOrder = 3
    end
    object pnlVertSpacer2: TPanel
      Left = 466
      Top = 0
      Width = 8
      Height = 97
      Align = alLeft
      BevelOuter = bvNone
      Color = clSkyBlue
      TabOrder = 4
    end
  end
  object pnlHorizSpacer1: TPanel
    Left = 0
    Top = 105
    Width = 458
    Height = 8
    Align = alTop
    BevelOuter = bvNone
    Color = clCream
    TabOrder = 3
  end
  object pnlButtonContainer: TPanel
    Left = 0
    Top = 273
    Width = 458
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object pnlButtons: TPanel
      Left = 128
      Top = 3
      Width = 177
      Height = 36
      BevelOuter = bvNone
      Color = clSkyBlue
      TabOrder = 0
      object btnOK: TButton
        Left = 8
        Top = 6
        Width = 73
        Height = 25
        Caption = '&OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object btnCancel: TButton
        Left = 96
        Top = 6
        Width = 73
        Height = 25
        Cancel = True
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object gbxRect: TGroupBox
    Left = 8
    Top = 88
    Width = 409
    Height = 137
    Caption = 'Rectangular area '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lblNorthwest: TLabel
      Left = 56
      Top = 20
      Width = 102
      Height = 13
      Caption = 'Northwest corner:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblLatNW: TLabel
      Left = 64
      Top = 40
      Width = 41
      Height = 13
      Caption = 'Latitude:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblLonNW: TLabel
      Left = 200
      Top = 40
      Width = 50
      Height = 13
      Caption = 'Longitude:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblSoutheast: TLabel
      Left = 56
      Top = 76
      Width = 102
      Height = 13
      Caption = 'Southeast corner:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblLatSE: TLabel
      Left = 64
      Top = 96
      Width = 41
      Height = 13
      Caption = 'Latitude:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblLonSE: TLabel
      Left = 200
      Top = 96
      Width = 50
      Height = 13
      Caption = 'Longitude:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object rleLatNW: TREEdit
      Left = 112
      Top = 40
      Width = 65
      Height = 21
      EditAlign = eaLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object rleLonNW: TREEdit
      Left = 256
      Top = 40
      Width = 65
      Height = 21
      EditAlign = eaLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object rleLatSE: TREEdit
      Left = 112
      Top = 96
      Width = 65
      Height = 21
      EditAlign = eaLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object rleLonSE: TREEdit
      Left = 256
      Top = 96
      Width = 65
      Height = 21
      EditAlign = eaLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object pnlHorizSpacer2: TPanel
    Left = 0
    Top = 265
    Width = 458
    Height = 8
    Align = alBottom
    BevelOuter = bvNone
    Color = clCream
    TabOrder = 6
  end
  object gbxCircle: TGroupBox
    Left = 64
    Top = 112
    Width = 377
    Height = 129
    Caption = 'Circular area '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    object lblCenter: TLabel
      Left = 56
      Top = 20
      Width = 112
      Height = 13
      Caption = 'Center coordinates:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblLatCenter: TLabel
      Left = 64
      Top = 40
      Width = 41
      Height = 13
      Caption = 'Latitude:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblLonCenter: TLabel
      Left = 200
      Top = 40
      Width = 50
      Height = 13
      Caption = 'Longitude:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblRadius: TLabel
      Left = 56
      Top = 76
      Width = 103
      Height = 13
      Caption = 'Circle radius (km):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object rleLatCenter: TREEdit
      Left = 112
      Top = 40
      Width = 65
      Height = 21
      EditAlign = eaLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object rleLonCenter: TREEdit
      Left = 256
      Top = 40
      Width = 65
      Height = 21
      EditAlign = eaLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object rleRadius: TREEdit
      Left = 112
      Top = 96
      Width = 65
      Height = 21
      EditAlign = eaLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
end
