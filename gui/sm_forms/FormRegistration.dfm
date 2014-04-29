object FormRegistration: TFormRegistration
  Left = 450
  Top = 282
  BorderStyle = bsDialog
  Caption = 'Please register NAADSM'
  ClientHeight = 271
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 230
    Width = 480
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btnOK: TButton
      Left = 400
      Top = 8
      Width = 73
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      OnClick = btnOKClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 230
    Align = alClient
    TabOrder = 1
    object pnlHeader: TPanel
      Left = 1
      Top = 1
      Width = 478
      Height = 96
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblRegister: TLabel
        Left = 80
        Top = 8
        Width = 387
        Height = 52
        Caption = 
          'It appears that you may not have not registered your copy of NAA' +
          'DSM.  Registration is a good way to stay in touch with the NAADS' +
          'M development team.  The process is simple, and registered users' +
          ' will be updated periodically regarding new releases and feature' +
          's.'
        WordWrap = True
      end
      object imgIcon: TImage
        Left = 24
        Top = 16
        Width = 32
        Height = 32
        Transparent = True
      end
    end
    object pnlBody: TPanel
      Left = 1
      Top = 97
      Width = 478
      Height = 132
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object rdgRegister: TRadioGroup
        Left = 96
        Top = 0
        Width = 289
        Height = 121
        Caption = 'Please select a registration option: '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemIndex = 0
        Items.Strings = (
          'Go to NAADSM website to register now'
          'Remind me again later'
          'I have already registered'
          'Skip registration')
        ParentFont = False
        TabOrder = 0
      end
    end
  end
end
