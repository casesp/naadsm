object FrameCredits: TFrameCredits
  Left = 0
  Top = 0
  Width = 541
  Height = 607
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 541
    Height = 33
    Align = alTop
    Caption = 'NAADSM model development team:'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 541
    Height = 96
    Align = alTop
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 94
      Align = alLeft
      TabOrder = 0
      object lblContactUS: TLabel
        Left = 8
        Top = 0
        Width = 122
        Height = 13
        Caption = 'Primary contact (US):'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblAffiliationCorso: TLabel
        Left = 8
        Top = 30
        Width = 114
        Height = 13
        Caption = 'USDA-APHIS-VS-CEAH'
      end
      object lblAddress1Corso: TLabel
        Left = 8
        Top = 45
        Width = 202
        Height = 13
        Caption = '2150 Centre Ave., Bldg. B, Mail Stop 2W4 '
      end
      object lblAddress2Corso: TLabel
        Left = 8
        Top = 60
        Width = 132
        Height = 13
        Caption = 'Fort Collins, CO 80526-8117'
      end
      object lblEmailFordeFolle: TLabel
        Left = 8
        Top = 75
        Width = 166
        Height = 13
        Cursor = crHandPoint
        Hint = 'Kim.N.Forde-Folle@aphis.usda.gov@aphis.usda.gov'
        Caption = 'Kim.N.Forde-Folle@aphis.usda.gov'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = mailtoClick
      end
      object lblFordeFolle: TLabel
        Left = 8
        Top = 15
        Width = 86
        Height = 13
        Caption = 'Kim N. Forde-Folle'
      end
    end
    object Panel4: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 94
      Align = alClient
      Alignment = taLeftJustify
      TabOrder = 1
      object lblContactCAN: TLabel
        Left = 8
        Top = 0
        Width = 148
        Height = 13
        Caption = 'Primary contact (Canada):'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDube: TLabel
        Left = 8
        Top = 15
        Width = 67
        Height = 13
        Caption = 'Caroline Dub'#233
      end
      object lblDubeAffiliation: TLabel
        Left = 8
        Top = 30
        Width = 206
        Height = 13
        Caption = 'CFIA-Animal Health and Production Division'
      end
      object lblAddress1Dube: TLabel
        Left = 8
        Top = 45
        Width = 53
        Height = 13
        Caption = '59 Camelot'
      end
      object lblAddress2Dube: TLabel
        Left = 8
        Top = 60
        Width = 101
        Height = 13
        Caption = 'Ottawa, ON K1A 0Y9'
      end
      object lblEmailDube: TLabel
        Left = 8
        Top = 75
        Width = 127
        Height = 13
        Cursor = crHandPoint
        Hint = 'dubecm@inspection.gc.ca'
        Caption = 'dubecm@inspection.gc.ca'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = mailtoClick
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 129
    Width = 541
    Height = 81
    Align = alTop
    TabOrder = 2
    object Panel6: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 79
      Align = alLeft
      Alignment = taLeftJustify
      TabOrder = 0
      object lblSupportPC: TLabel
        Left = 8
        Top = 0
        Width = 192
        Height = 13
        Caption = 'Technical support (Windows/PC):'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblReeves: TLabel
        Left = 8
        Top = 15
        Width = 68
        Height = 13
        Caption = 'Aaron Reeves'
      end
      object lblEmailReeves: TLabel
        Left = 8
        Top = 60
        Width = 143
        Height = 13
        Cursor = crHandPoint
        Hint = 'Aaron.Reeves@colostate.edu'
        Caption = 'Aaron.Reeves@colostate.edu'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = mailtoClick
      end
      object Label7: TLabel
        Left = 8
        Top = 30
        Width = 158
        Height = 13
        Caption = 'Animal Population Health Institute'
      end
      object Label8: TLabel
        Left = 8
        Top = 45
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
    end
    object Panel7: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 79
      Align = alClient
      TabOrder = 1
      object lblSupportParallel: TLabel
        Left = 8
        Top = 0
        Width = 226
        Height = 13
        Caption = 'Technical support (Parallel processing):'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblHarvey: TLabel
        Left = 8
        Top = 15
        Width = 55
        Height = 13
        Caption = 'Neil Harvey'
      end
      object lblEmailHarvey: TLabel
        Left = 8
        Top = 60
        Width = 108
        Height = 13
        Cursor = crHandPoint
        Hint = 'nharvey@uoguelph.ca'
        Caption = 'nharvey@uoguelph.ca'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = mailtoClick
      end
      object Label9: TLabel
        Left = 8
        Top = 30
        Width = 226
        Height = 13
        Caption = 'Department of Computing && Information Science'
      end
      object Label10: TLabel
        Left = 8
        Top = 45
        Width = 95
        Height = 13
        Caption = 'University of Guelph'
      end
    end
  end
  object Panel9: TPanel
    Left = 0
    Top = 210
    Width = 541
    Height = 51
    Align = alTop
    TabOrder = 3
    object Panel10: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 49
      Align = alLeft
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 15
        Width = 114
        Height = 13
        Caption = 'USDA-APHIS-VS-CEAH'
      end
      object lblCorso: TLabel
        Left = 8
        Top = 0
        Width = 80
        Height = 13
        Caption = 'Barbara A. Corso'
      end
    end
    object Panel11: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 49
      Align = alClient
      TabOrder = 1
      object lblHill: TLabel
        Left = 8
        Top = 0
        Width = 61
        Height = 13
        Caption = 'Ashley E. Hill'
      end
      object Label2: TLabel
        Left = 8
        Top = 15
        Width = 158
        Height = 13
        Caption = 'Animal Population Health Institute'
      end
      object Label11: TLabel
        Left = 8
        Top = 30
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
    end
  end
  object Panel12: TPanel
    Left = 0
    Top = 312
    Width = 541
    Height = 51
    Align = alTop
    TabOrder = 4
    object Panel13: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 49
      Align = alLeft
      TabOrder = 0
      object lblSeitzinger: TLabel
        Left = 8
        Top = 0
        Width = 82
        Height = 13
        Caption = 'Ann H. Seitzinger'
      end
      object Label4: TLabel
        Left = 8
        Top = 15
        Width = 196
        Height = 13
        Caption = 'USDA-APHIS-VS-CEAH-NCAHS-NAHMS'
      end
    end
    object Panel14: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 49
      Align = alClient
      TabOrder = 1
      object lblStacey: TLabel
        Left = 8
        Top = 0
        Width = 90
        Height = 13
        Caption = 'Deborah A. Stacey'
      end
      object Label5: TLabel
        Left = 8
        Top = 30
        Width = 95
        Height = 13
        Caption = 'University of Guelph'
      end
      object Label15: TLabel
        Left = 8
        Top = 15
        Width = 226
        Height = 13
        Caption = 'Department of Computing && Information Science'
      end
    end
  end
  object Panel15: TPanel
    Left = 0
    Top = 261
    Width = 541
    Height = 51
    Align = alTop
    TabOrder = 5
    object Panel16: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 49
      Align = alLeft
      TabOrder = 0
      object lblMcNab: TLabel
        Left = 8
        Top = 0
        Width = 83
        Height = 13
        Caption = 'W. Bruce McNab'
      end
      object Label13: TLabel
        Left = 8
        Top = 15
        Width = 208
        Height = 13
        Caption = 'Ministry of Agriculture Food and Rural Affairs'
      end
      object Label14: TLabel
        Left = 8
        Top = 30
        Width = 91
        Height = 13
        Caption = 'Province of Ontario'
      end
    end
    object Panel17: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 49
      Align = alClient
      TabOrder = 1
      object lblSalman: TLabel
        Left = 8
        Top = 0
        Width = 61
        Height = 13
        Caption = 'M.D. Salman'
      end
      object Label3: TLabel
        Left = 8
        Top = 30
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
      object Label12: TLabel
        Left = 8
        Top = 15
        Width = 158
        Height = 13
        Caption = 'Animal Population Health Institute'
      end
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 363
    Width = 541
    Height = 24
    Align = alTop
    Caption = 'Additional programming:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
  end
  object Panel23: TPanel
    Left = 0
    Top = 387
    Width = 541
    Height = 51
    Align = alTop
    TabOrder = 7
    object Panel24: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 49
      Align = alLeft
      TabOrder = 0
      object Label18: TLabel
        Left = 8
        Top = 0
        Width = 71
        Height = 13
        Caption = 'Shaun P. Case'
      end
      object Label19: TLabel
        Left = 8
        Top = 15
        Width = 157
        Height = 13
        Caption = 'Department of Computer Science'
      end
      object Label23: TLabel
        Left = 8
        Top = 30
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
    end
    object Panel25: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 49
      Align = alClient
      TabOrder = 1
      object Label20: TLabel
        Left = 8
        Top = 0
        Width = 69
        Height = 13
        Caption = 'Snehal Shetye'
      end
      object Label21: TLabel
        Left = 8
        Top = 15
        Width = 184
        Height = 13
        Caption = 'Department of Mechanical Engineering'
      end
      object Label22: TLabel
        Left = 8
        Top = 30
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
    end
  end
  object Panel18: TPanel
    Left = 0
    Top = 438
    Width = 541
    Height = 24
    Align = alTop
    Caption = 'Spanish translation:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
  end
  object Panel19: TPanel
    Left = 0
    Top = 537
    Width = 541
    Height = 51
    Align = alTop
    TabOrder = 9
    object Panel20: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 49
      Align = alLeft
      TabOrder = 0
      object lblSchoenbaum: TLabel
        Left = 8
        Top = 0
        Width = 103
        Height = 13
        Caption = 'Mark A. Schoenbaum'
      end
      object Label6: TLabel
        Left = 8
        Top = 15
        Width = 112
        Height = 13
        Caption = 'USDA-APHIS-VS-WRO'
      end
    end
    object Panel21: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 49
      Align = alClient
      TabOrder = 1
      object lblZagmutt: TLabel
        Left = 8
        Top = 0
        Width = 128
        Height = 13
        Caption = 'Francisco Zagmutt-Vergara'
      end
      object Label16: TLabel
        Left = 8
        Top = 15
        Width = 158
        Height = 13
        Caption = 'Animal Population Health Institute'
      end
      object Label17: TLabel
        Left = 8
        Top = 30
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
    end
  end
  object Panel22: TPanel
    Left = 0
    Top = 513
    Width = 541
    Height = 24
    Align = alTop
    Caption = 'Former development team members:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 10
  end
  object Panel26: TPanel
    Left = 0
    Top = 462
    Width = 541
    Height = 51
    Align = alTop
    TabOrder = 11
    object Panel27: TPanel
      Left = 1
      Top = 1
      Width = 256
      Height = 49
      Align = alLeft
      TabOrder = 0
      object Label24: TLabel
        Left = 8
        Top = 0
        Width = 70
        Height = 13
        Caption = 'Celia Antognoli'
      end
      object Label25: TLabel
        Left = 8
        Top = 15
        Width = 158
        Height = 13
        Caption = 'Animal Population Health Institute'
      end
      object Label29: TLabel
        Left = 8
        Top = 30
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
    end
    object Panel28: TPanel
      Left = 257
      Top = 1
      Width = 283
      Height = 49
      Align = alClient
      TabOrder = 1
      object Label26: TLabel
        Left = 8
        Top = 0
        Width = 88
        Height = 13
        Caption = 'Noa Roman-Muniz'
      end
      object Label27: TLabel
        Left = 8
        Top = 15
        Width = 158
        Height = 13
        Caption = 'Animal Population Health Institute'
      end
      object Label28: TLabel
        Left = 8
        Top = 30
        Width = 119
        Height = 13
        Caption = 'Colorado State University'
      end
    end
  end
end
