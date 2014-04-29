object FormAppUpdate: TFormAppUpdate
  Left = 555
  Top = 258
  BorderStyle = bsDialog
  Caption = 'NAADSM updates'
  ClientHeight = 434
  ClientWidth = 517
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
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 517
    Height = 113
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblVersionIDs: TLabel
      Left = 88
      Top = 10
      Width = 347
      Height = 26
      Caption = 
        'An updated version (xxx) of NAADSM is available: you are current' +
        'ly using version yyy.'
      WordWrap = True
    end
    object lblPriority: TLabel
      Left = 88
      Top = 48
      Width = 353
      Height = 26
      Caption = 
        'This update is critical, and highly recommended.  This line shou' +
        'ld wrap this line should wrap.'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 88
      Top = 80
      Width = 331
      Height = 13
      Caption = 
        'A description of the enhancements in the new version is shown be' +
        'low:'
      WordWrap = True
    end
    object imgIcon: TImage
      Left = 24
      Top = 24
      Width = 32
      Height = 32
      Transparent = True
    end
  end
  object mmoNewFeatures: TMemo
    Left = 0
    Top = 113
    Width = 517
    Height = 159
    TabStop = False
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object pnlInstructions: TPanel
    Left = 0
    Top = 272
    Width = 517
    Height = 121
    Align = alBottom
    TabOrder = 2
    object lblInstructions1: TLabel
      Left = 56
      Top = 8
      Width = 233
      Height = 13
      Caption = 'More information about this update is available at:'
    end
    object lblURL: TLabel
      Left = 56
      Top = 24
      Width = 350
      Height = 13
      Cursor = crHandPoint
      Caption = 
        'http://www.naadsm.org/updates.php?version=3.0.79&&fromVersion=3.' +
        '0.78'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = urlClick
    end
    object lblInstructions2: TLabel
      Left = 56
      Top = 48
      Width = 370
      Height = 13
      Caption = 
        'You may go to the NAADSM website now to download the update, if ' +
        'you wish.'
    end
    object lblNoncritical: TLabel
      Left = 56
      Top = 72
      Width = 438
      Height = 26
      Caption = 
        'If you do not want to install this noncritical update, click '#39'Sk' +
        'ip this update'#39' below, and you will not be prompted again until ' +
        'the next update becomes available.'
      WordWrap = True
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 393
    Width = 517
    Height = 41
    Align = alBottom
    TabOrder = 3
    object btnURL: TButton
      Left = 192
      Top = 8
      Width = 177
      Height = 25
      Caption = '&Go to the NAADSM website'
      TabOrder = 2
      OnClick = urlClick
    end
    object Button2: TButton
      Left = 376
      Top = 8
      Width = 129
      Height = 25
      Caption = '&Do not update now'
      TabOrder = 0
      OnClick = btnClick
    end
    object btnNoncritical: TButton
      Left = 8
      Top = 8
      Width = 177
      Height = 25
      Caption = '&Skip this update'
      TabOrder = 1
      OnClick = btnClick
    end
  end
end
