inherited FormSpreadOptions: TFormSpreadOptions
  Left = 622
  Top = 329
  Caption = 'Scenario parameters: Spread options'
  ClientHeight = 449
  ClientWidth = 623
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBase: TPanel
    Top = 406
    Width = 623
    inherited pnlWizardButtons: TPanel
      Left = 225
    end
  end
  object pnlCaption: TPanel [1]
    Left = 0
    Top = 0
    Width = 623
    Height = 41
    Align = alTop
    Caption = 'Spread options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object pnlBody: TPanel [2]
    Left = 0
    Top = 41
    Width = 623
    Height = 365
    Align = alClient
    TabOrder = 2
    object grpSpreadOptions: TGroupBox
      Left = 96
      Top = 16
      Width = 417
      Height = 57
      Caption = 
        'Which type(s) of SPREAD would you like to model during simulatio' +
        'n runs? '
      TabOrder = 0
      object cbxContact: TCheckBox
        Left = 8
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Contact'
        TabOrder = 0
        OnClick = cbxClick
      end
      object cbxAirborne: TCheckBox
        Left = 280
        Top = 24
        Width = 129
        Height = 17
        Caption = 'Airborne'
        TabOrder = 2
        OnClick = cbxClick
      end
      object cbxLocalArea: TCheckBox
        Left = 120
        Top = 24
        Width = 161
        Height = 17
        Caption = 'Local-area spread'
        TabOrder = 1
        OnClick = cbxClick
      end
    end
    object gbxWindParams: TGroupBox
      Left = 96
      Top = 80
      Width = 417
      Height = 273
      Caption = 'For airborne spread:'
      TabOrder = 1
      inline fraWindDirection: TFrameWindDirection
        Left = 48
        Top = 24
        Width = 289
        Height = 225
        TabOrder = 0
        inherited rleWindStart: TREEdit
          OnExit = processWindDirText
        end
        inherited rleWindEnd: TREEdit
          OnExit = processWindDirText
        end
      end
    end
  end
end
