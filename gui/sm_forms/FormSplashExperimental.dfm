object FormSplashExperimental: TFormSplashExperimental
  Left = 542
  Top = 281
  BorderIcons = [biMinimize]
  BorderStyle = bsNone
  Caption = 'NAADSM splash screen'
  ClientHeight = 288
  ClientWidth = 569
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
    Width = 569
    Height = 288
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object pnlAppTitle: TPanel
      Left = 0
      Top = 0
      Width = 569
      Height = 288
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 0
      object pnlLeftSpacer: TPanel
        Left = 0
        Top = 17
        Width = 49
        Height = 271
        Align = alLeft
        TabOrder = 0
      end
      object pnlTopSpacer: TPanel
        Left = 0
        Top = 0
        Width = 569
        Height = 17
        Align = alTop
        TabOrder = 1
      end
      object pnlBody: TPanel
        Left = 49
        Top = 17
        Width = 520
        Height = 271
        Align = alClient
        TabOrder = 2
        object lblAppTitle: TLabel
          Left = 1
          Top = 1
          Width = 518
          Height = 37
          Align = alTop
          Caption = 'NAADSM-Experimental'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -32
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVersion: TLabel
          Left = 1
          Top = 38
          Width = 518
          Height = 20
          Align = alTop
          Caption = 'Version 3.0.78 Build 060403'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblCopyright: TLabel
          Left = 1
          Top = 67
          Width = 518
          Height = 13
          Align = alTop
          Caption = 'Copyright '#169' 2003 - 2013 NAADSM Development Team'
          WordWrap = True
        end
        object lblExplanation: TLabel
          Left = 1
          Top = 89
          Width = 518
          Height = 54
          Align = alTop
          Caption = 
            'This is an experimental version of NAADSM.  This version deviate' +
            's from the published conceptual specification in ways that may s' +
            'ignificantly affect simulation outcomes. '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object lblAdditionalInfo: TLabel
          Left = 1
          Top = 152
          Width = 518
          Height = 14
          Align = alTop
          Caption = 'Please see '#39'About NAADSM'#39' for contact and support information.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object pnlSpacer1: TPanel
          Left = 1
          Top = 58
          Width = 518
          Height = 9
          Align = alTop
          TabOrder = 0
        end
        object pnlSpacer2: TPanel
          Left = 1
          Top = 80
          Width = 518
          Height = 9
          Align = alTop
          TabOrder = 1
        end
        object pnlSpacer3: TPanel
          Left = 1
          Top = 143
          Width = 518
          Height = 9
          Align = alTop
          TabOrder = 2
        end
      end
    end
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = timerTimeout
    Left = 504
    Top = 225
  end
end
