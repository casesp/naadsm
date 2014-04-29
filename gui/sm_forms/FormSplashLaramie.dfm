object FormSplashLaramie: TFormSplashLaramie
  Left = 736
  Top = 464
  BorderIcons = [biMinimize]
  BorderStyle = bsNone
  Caption = 'NAADSM splash screen'
  ClientHeight = 232
  ClientWidth = 538
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlWholeForm: TPanel
    Left = 0
    Top = 0
    Width = 538
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object pnlAppTitle: TPanel
      Left = 0
      Top = 0
      Width = 538
      Height = 232
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 0
      object lblVersion: TLabel
        Left = 58
        Top = 57
        Width = 199
        Height = 20
        Caption = 'Version 3.0.78 Build 060403'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblAdditionalInfo: TLabel
        Left = 56
        Top = 176
        Width = 313
        Height = 14
        Caption = 'Please see '#39'About NAADSM'#39' for contact and support information.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblAppTitle: TLabel
        Left = 56
        Top = 16
        Width = 301
        Height = 37
        Caption = 'NAADSM-Laramie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -32
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblExplanation: TLabel
        Left = 56
        Top = 88
        Width = 451
        Height = 72
        Caption = 
          'This is an experimental version of the North American Animal Dis' +
          'ease Spread Model.  This version deviates from the published con' +
          'ceptual specification in ways that may significantly affect the ' +
          'simulation outcome. '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = timerTimeout
    Left = 392
    Top = 113
  end
end
