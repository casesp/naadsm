inherited FormSpreadOptions: TFormSpreadOptions
  Left = 744
  Top = 252
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
      Left = 88
      Top = 40
      Width = 417
      Height = 121
      Caption = 
        'Which type(s) of SPREAD would you like to model during simulatio' +
        'n runs? '
      TabOrder = 0
      object rdoAirborne: TRadioButton
        Left = 16
        Top = 48
        Width = 233
        Height = 17
        Caption = 'Airborne'
        TabOrder = 0
        OnClick = rdoClick
      end
      object rdoContact: TRadioButton
        Left = 16
        Top = 24
        Width = 233
        Height = 17
        Caption = 'Contact'
        TabOrder = 1
        OnClick = rdoClick
      end
      object rdoBoth: TRadioButton
        Left = 16
        Top = 72
        Width = 233
        Height = 17
        Caption = 'Both airborne and contact'
        TabOrder = 2
        OnClick = rdoClick
      end
      object rdoNoSpread: TRadioButton
        Left = 16
        Top = 96
        Width = 233
        Height = 17
        Caption = 'No spread'
        TabOrder = 3
        OnClick = rdoClick
      end
    end
    object gbxAirborneDecay: TGroupBox
      Left = 88
      Top = 168
      Width = 417
      Height = 105
      Caption = 'For airborne spread: '
      TabOrder = 1
      object rdoAirborneLinear: TRadioButton
        Left = 16
        Top = 16
        Width = 377
        Height = 33
        Caption = 'Rate of disease transfer declines linearly from source'
        TabOrder = 0
        WordWrap = True
        OnClick = rdoClick
      end
      object rdoAirborneExponential: TRadioButton
        Left = 16
        Top = 56
        Width = 377
        Height = 33
        Caption = 'Rate of disease transfer declines exponentially from source'
        TabOrder = 1
        WordWrap = True
        OnClick = rdoClick
      end
    end
  end
end
