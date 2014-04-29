object FormSMWizardBase: TFormSMWizardBase
  Left = 843
  Top = 437
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'FormSMWizardBase'
  ClientHeight = 561
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBase: TPanel
    Left = 0
    Top = 518
    Width = 702
    Height = 43
    Align = alBottom
    TabOrder = 0
    object pnlWizardButtons: TPanel
      Left = 304
      Top = 1
      Width = 397
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        397
        41)
      object LabelSave: TLabel
        Left = 379
        Top = 0
        Width = 18
        Height = 41
        Align = alRight
        AutoSize = False
        Caption = '*'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Layout = tlBottom
        Visible = False
      end
      object btnFinish: TBitBtn
        Left = 295
        Top = 8
        Width = 74
        Height = 25
        Hint = 'Complete working on the screen by saving the information'
        Anchors = [akTop, akRight]
        Caption = '&Finish'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = wizardButtonClick
      end
      object btnNext: TBitBtn
        Left = 224
        Top = 8
        Width = 71
        Height = 25
        Hint = 'Proceed to the next screen, saving this information'
        Anchors = [akTop, akRight]
        Caption = ' &Next >'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = wizardButtonClick
      end
      object btnBack: TBitBtn
        Left = 76
        Top = 8
        Width = 76
        Height = 25
        Hint = 'Return to the previous screen, saving this information'
        Anchors = [akTop, akRight]
        Caption = '< &Back '
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = wizardButtonClick
      end
      object btnCancel: TBitBtn
        Left = 12
        Top = 8
        Width = 67
        Height = 25
        Hint = 
          'Abort current operations, not saving information on the screen a' +
          'nd return to the main menu'
        Anchors = [akTop, akRight]
        Caption = '&Cancel'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = wizardButtonClick
      end
      object btnSelect: TButton
        Left = 152
        Top = 8
        Width = 74
        Height = 25
        Hint = 'Save this information and select another screen'
        Caption = '&Select...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnMouseUp = btnSelectMouseUp
      end
    end
  end
  object PopupActionBarEx1: TPopupActionBarEx
    Left = 544
    Top = 472
    object Startsetup1: TMenuItem
      Caption = 'Start setup'
    end
  end
end
