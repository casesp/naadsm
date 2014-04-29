object DialogRunSimException: TDialogRunSimException
  Left = 469
  Top = 173
  Width = 495
  Height = 227
  Caption = 'Simulation error'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblErrorOccurred: TLabel
    Left = 136
    Top = 16
    Width = 213
    Height = 26
    Caption = 
      'An error occurred while this simulation was in progress, and an ' +
      'error file has been created.'
    WordWrap = True
  end
  object lblSendError: TLabel
    Left = 104
    Top = 56
    Width = 293
    Height = 39
    Caption = 
      'We would appreciate your help in solving this problem: please se' +
      'nd the error file (named below) to the NAADSM development team f' +
      'or analysis.  Thank you!'
    WordWrap = True
  end
  object lblErrorFileNameLabel: TLabel
    Left = 104
    Top = 104
    Width = 91
    Height = 13
    Caption = 'Error file name: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblFileName: TLabel
    Left = 104
    Top = 120
    Width = 54
    Height = 13
    Caption = 'lblFileName'
  end
  object btnOK: TButton
    Left = 192
    Top = 152
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
end
