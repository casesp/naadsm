inherited FormCustomOutputs: TFormCustomOutputs
  Left = 269
  Top = 112
  Caption = 'Custom outputs'
  ClientHeight = 577
  ClientWidth = 765
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 534
    Width = 765
    TabOrder = 2
    inherited pnlWizardButtons: TPanel
      Left = 367
    end
  end
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 0
    Width = 765
    Height = 41
    Align = alTop
    Caption = 'Custom output definitions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object pnlCustomOutputs: TPanel [2]
    Left = 0
    Top = 41
    Width = 153
    Height = 493
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 1
    object pnlCustomOutputsCaption: TPanel
      Left = 1
      Top = 1
      Width = 147
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      BorderStyle = bsSingle
      Caption = 'Custom outputs'
      TabOrder = 0
    end
    object lbxCustomOutputs: TListBox
      Left = 1
      Top = 25
      Width = 147
      Height = 463
      Hint = 'Press CTRL + left mouse button to select multiple definitions'
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'unitsVacc5DaysPostDetection'
        'animalsVacc5DaysPostDetection'
        'unitsDestr5DaysPostDetection'
        'unitsVacc5DaysPostDetection')
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = lbxCustomOutputsClick
    end
  end
  object pnlControls: TPanel [3]
    Left = 153
    Top = 41
    Width = 612
    Height = 493
    Align = alClient
    TabOrder = 3
    object pnlDefEditor: TPanel
      Left = 1
      Top = 1
      Width = 610
      Height = 351
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object lblSql: TLabel
        Left = 16
        Top = 120
        Width = 88
        Height = 13
        Caption = 'SQL statement:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object mmoSql: TMemo
        Left = 8
        Top = 136
        Width = 521
        Height = 209
        TabOrder = 0
        OnChange = mmoSqlChange
      end
      object gbxOutputFrequency: TGroupBox
        Left = 8
        Top = 8
        Width = 321
        Height = 105
        Caption = 'Output frequency: '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object rdoIterationOutput: TRadioButton
          Left = 8
          Top = 24
          Width = 137
          Height = 25
          Caption = 'Generate once per iteration'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          WordWrap = True
          OnClick = rdoBtnClick
        end
        object rdoProdTypeOutput: TRadioButton
          Left = 8
          Top = 60
          Width = 121
          Height = 37
          Caption = 'Generate for every production type for every iteration'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          WordWrap = True
          OnClick = rdoBtnClick
        end
        object rdoZoneOutput: TRadioButton
          Left = 144
          Top = 16
          Width = 145
          Height = 41
          Caption = 'Generate for every zone for every iteration'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          WordWrap = True
          OnClick = rdoBtnClick
        end
        object rdoZonePTOutput: TRadioButton
          Left = 144
          Top = 52
          Width = 153
          Height = 45
          Caption = 
            'Generate for every zone and every production type for every iter' +
            'ation'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          WordWrap = True
          OnClick = rdoBtnClick
        end
      end
      object gbxOutputType: TGroupBox
        Left = 344
        Top = 8
        Width = 185
        Height = 105
        Caption = 'Output type: '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        object rdoInteger: TRadioButton
          Left = 8
          Top = 24
          Width = 121
          Height = 17
          Caption = 'Number - integer'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = rdoBtnClick
        end
        object rdoDouble: TRadioButton
          Left = 8
          Top = 48
          Width = 129
          Height = 17
          Caption = 'Number - floating point'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = rdoBtnClick
        end
        object rdoString: TRadioButton
          Left = 8
          Top = 72
          Width = 121
          Height = 17
          Caption = 'String'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = rdoBtnClick
        end
      end
    end
    object pnlDefController: TPanel
      Left = 1
      Top = 352
      Width = 610
      Height = 140
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnNew: TButton
        Left = 12
        Top = 8
        Width = 253
        Height = 25
        Caption = 'Add definition'
        TabOrder = 0
        OnClick = btnNewClick
      end
      object btnRemove: TButton
        Left = 12
        Top = 40
        Width = 253
        Height = 25
        Caption = 'Remove selected definition'
        TabOrder = 1
        OnClick = btnRemoveClick
      end
      object btnTest: TButton
        Left = 276
        Top = 40
        Width = 253
        Height = 25
        Caption = 'Test current/selected definition'
        TabOrder = 2
      end
      object btnRename: TButton
        Left = 276
        Top = 8
        Width = 253
        Height = 25
        Caption = 'Rename current/selected definition'
        TabOrder = 3
        OnClick = btnRenameClick
      end
      object pnlWarning: TPanel
        Left = 0
        Top = 83
        Width = 610
        Height = 57
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 4
        object lblWarning: TLabel
          Left = 56
          Top = 8
          Width = 418
          Height = 39
          Caption = 
            'This feature of NAADSM/PC is not well documented and may not be ' +
            'fully implemented.  It is intended for experts and others who do' +
            'n'#39't mind living dangerously...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object WarningImage: TImage
          Left = 16
          Top = 12
          Width = 33
          Height = 33
          Picture.Data = {
            07544269746D6170360C0000424D360C00000000000036000000280000002000
            0000200000000100180000000000000C0000120B0000120B0000000000000000
            0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FF000A1A000019000019001335001335001335001335001335
            0013350013350013350013350013350013350013350013350013350013350013
            35001335001335001335001335001335001335001335001335000A1AFF00FFFF
            00FFFF00FF000019004ED2004ED20451D30753D30854D30854D30954D40A55D4
            0A55D40954D40954D40954D40854D30854D30753D30652D30551D30652D30551
            D30451D30451D30350D3024FD2014FD2014FD2014FD2004ED2004ED2000019FF
            00FFFF00FF0000190085E000D3F40CD5F50DD5F50ED5F50FD6F50FD6F510D6F5
            10D6F510D6F50FD6F50FD6F50ED5F50DD5F50DD5F50BD5F409D5F409D5F408D4
            F407D4F406D4F405D4F404D4F403D4F402D3F401D3F400D3F40085E0000019FF
            00FFFF00FF000019007ADD00FEFF0FFEFF13FFFF14FFFF15FFFF16FFFF17FFFF
            16FFFF17FFFF15FFFF15FFFF14FFFF13FFFF12FFFF10FFFF0FFFFF0EFFFF0CFF
            FF0CFFFF09FFFF08FFFF07FFFF06FFFF05FFFF02FEFF00FEFF007ADD000019FF
            00FFFF00FF000019004ED200C5F10EFBFF1AFFFF1BFFFF1DFFFF1FFFFF1FFFFF
            20FFFF20FFFF1FFFFF1FFFFF1CA9A9191919181818166E6E17FFFF14FFFF13FF
            FF11FFFF0FFFFF0DFFFF0BFFFF0AFFFF08FFFF04FBFF00C5F1004ED2000019FF
            00FFFF00FF000019003B9D0063D805EDFC25FEFF2DFFFF30FFFF32FFFF32FFFF
            32FFFF32FFFF32FFFF31FFFF2532322323232020201E1E1E26E3E325FFFF22FF
            FF20FFFF1EFFFF1BFFFF19FFFF17FFFF13FEFF02EDFC0063D8003A9E000019FF
            00FFFF00FFFF00FF000019004ED200A1E919F8FF41FFFF43FFFF45FFFF48FFFF
            48FFFF48FFFF47FFFF46FFFF313C3C2E2E2E2C2C2C2929293AE4E439FFFF37FF
            FF33FFFF30FFFF2CFFFF29FFFF27FFFF0DF7FF00A1E9004ED2000019FF00FFFF
            00FFFF00FFFF00FFFF00FF0000190058D501DDF943FCFF57FFFF58FFFF5BFFFF
            5CFFFF5CFFFF5BFFFF5BFFFF4DB5B53C4646373737449A9A50FFFF4CFFFF49FF
            FF45FFFF40FFFF3DFFFF3AFFFF29FBFF00DDF90058D5000019FF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FF0000190049C5008AE31EF2FF69FFFF6BFFFF6EFFFF
            6EFFFF6FFFFF6EFFFF6EFFFF6CFFFF6BFFFF69FFFF66FFFF62FFFF5FFFFF5CFF
            FF57FFFF53FFFF4DFFFF4AFFFF11F1FF008AE30049C500050DFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FF000019004ED200C4F454F8FF81FFFF80FFFF
            81FFFF82FFFF82FFFF81FFFF81FFFF7CF4F478E9E97AFFFF77FFFF73FFFF6FFF
            FF6BFFFF65FFFF62FFFF3AF7FF00C4F4004ED2000019FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FF0000190045B8006BDA18EBFF8AFEFF92FFFF
            92FFFF94FFFF94FFFF94FFFF92FFFF76A7A76C88888DFFFF8AFFFF87FFFF82FF
            FF7CFFFF78FFFF6DFDFF0EEAFF006BDA0045B7000019FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FF000019004ED200ACEE56F3FFA4FFFF
            A1FFFFA1FFFFA2FFFFA3FFFFA1FFFF798B8B6E6E6E9DFFFF9BFFFF98FFFF93FF
            FF8DFFFF8BFFFF43F2FF00ACEE004ED2000019FF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000190057D513DBFC97FBFF
            AFFFFFAEFFFFAFFFFFAFFFFFA8EEEE7A7A7A7979799EDCDCA9FFFFA5FFFFA1FF
            FF9EFFFF82FBFF0CDAFC0057D5000019FF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000A1A004ED2008DE651ECFF
            BAFFFFB9FFFFBAFFFFBBFFFFA6D1D183838383838398B8B8B6FFFFB3FFFFAFFF
            FFABFFFF43EAFF008DE6004ED2000019FF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000019004ED20ACCF9
            9DF9FFC4FFFFC3FFFFC4FFFFA1B7B78B8B8B8A8A8A939E9EC1FFFFBEFFFFBCFF
            FF92F8FF06CBF9004ED2000019FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00050D0049C50071DD
            3FE4FFC5FEFFCBFFFFCCFFFF9498989191919090908E8E8EC6F8F8C7FFFFBFFE
            FF36E2FF0071DD0049C500050DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000019004ED2
            01ABF192F2FFD3FFFFC7EBEB959595959595949494929292BCDCDCD1FFFF8CF2
            FF01ABF1004ED2000019FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000019
            005FD826DAFFCDFCFFC8E5E5969696969696969696949494B6C9C9CBFCFF24D9
            FF005FD8000019FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF001435
            004ED20290E97CEAFFD4EDED969696979797989898949494C6D9D97AEAFF018F
            E9004ED2001434FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            0000190056D50DBFF9C3F9FFB8C6C6969696969696B8C6C6C1F8FF0DBFF90056
            D5000019FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            000A1A0049C5007CE358DEFFE0FFFFDFFFFFDFFFFFDFFFFF56DEFF007CE30049
            C5000A1AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FF000019004ED202AAF4ABF3FFE1FFFFE1FFFFABF3FF02AAF4004ED20000
            19FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF0000190065DA2ED3FFCBFDFFCBFDFF2ED3FF0065DA000019FF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF000019004ED2009AEE00C7FF00C7FF009AEE004ED2000019FF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FF0000190056D500BFFC00BFFC0056D5000019FF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FF000019004ED2007BE3007BE3004ED2000A1AFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FF000019004ED2004ED2000019FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FF000A1A000A1AFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FF}
          Transparent = True
        end
      end
    end
  end
end
