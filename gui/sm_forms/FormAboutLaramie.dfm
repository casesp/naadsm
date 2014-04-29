object FormAboutLaramie: TFormAboutLaramie
  Left = 531
  Top = 66
  BorderStyle = bsDialog
  Caption = 'About NAADSM-Laramie'
  ClientHeight = 371
  ClientWidth = 519
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
  PixelsPerInch = 96
  TextHeight = 13
  object pnlWholeForm: TPanel
    Left = 0
    Top = 0
    Width = 519
    Height = 371
    Align = alClient
    BevelInner = bvLowered
    BevelWidth = 2
    Color = clBlue
    TabOrder = 0
    object pnlBody: TPanel
      Left = 4
      Top = 4
      Width = 511
      Height = 363
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 0
      object pnlButtons: TPanel
        Left = 0
        Top = 330
        Width = 511
        Height = 33
        Align = alBottom
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 0
        object btnOK: TButton
          Left = 416
          Top = 4
          Width = 73
          Height = 25
          Caption = '&OK'
          TabOrder = 0
          OnClick = btnOKClick
        end
        object btnLicense: TButton
          Left = 312
          Top = 4
          Width = 89
          Height = 25
          Caption = '&License...'
          TabOrder = 1
          OnClick = btnLicenseClick
        end
      end
      object pnlAppTitleContainer: TPanel
        Left = 0
        Top = 0
        Width = 511
        Height = 216
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 1
        object lblExplanation: TLabel
          Left = 16
          Top = 120
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
        object pnlAppTitle: TPanel
          Left = 8
          Top = 8
          Width = 337
          Height = 105
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 0
          object lblVersion: TLabel
            Left = 10
            Top = 65
            Width = 199
            Height = 18
            Caption = 'Version 3.0.78 Build 060403'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object lblWebsite: TLabel
            Left = 16
            Top = 88
            Width = 49
            Height = 14
            Cursor = crHandPoint
            Caption = 'lblWebsite'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsUnderline]
            ParentFont = False
            OnClick = lblWebsiteClick
          end
          object lblAppTitle: TLabel
            Left = 8
            Top = 24
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
        end
      end
      object pnlLicense: TPanel
        Left = 0
        Top = 216
        Width = 511
        Height = 114
        Align = alBottom
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 2
        object lblLicenseBlurb: TLabel
          Left = 8
          Top = 52
          Width = 496
          Height = 42
          Caption = 
            'This program is free software; you can redistribute it and/or mo' +
            'dify it under the terms of the GNU General Public License as pub' +
            'lished by the Free Software Foundation; either version 2 of the ' +
            'License, or (at your option) any later version.  For complete li' +
            'cense details, please see below.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object lblCopyright: TLabel
          Left = 112
          Top = 4
          Width = 278
          Height = 28
          Alignment = taCenter
          Caption = 
            'Copyright '#169' 2003 - 2007 Animal Population Health Institute at Co' +
            'lorado State University and University of Guelph'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
      end
    end
  end
end
