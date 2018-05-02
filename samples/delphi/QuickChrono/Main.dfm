object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Chrono Demo'
  ClientHeight = 186
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblElapsedTime: TLabel
    Left = 148
    Top = 80
    Width = 253
    Height = 13
    AutoSize = False
  end
  object lblElapsedTimeLong: TLabel
    Left = 148
    Top = 99
    Width = 253
    Height = 13
    AutoSize = False
  end
  object Label3: TLabel
    Left = 119
    Top = 24
    Width = 78
    Height = 29
    Caption = 'Timer:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTimer: TLabel
    Left = 203
    Top = 24
    Width = 108
    Height = 29
    ParentCustomHint = False
    AutoSize = False
    Caption = '00:00:00'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowAccelChar = False
    ShowHint = False
    Transparent = True
  end
  object lblShortFormat: TLabel
    Left = 57
    Top = 80
    Width = 63
    Height = 13
    Caption = 'ElapsedTime:'
  end
  object lblLongFormat: TLabel
    Left = 57
    Top = 99
    Width = 86
    Height = 13
    Caption = 'ElapsedTimeLong:'
  end
  object Label1: TLabel
    Left = 24
    Top = 128
    Width = 84
    Height = 13
    Caption = 'Precission Format'
  end
  object btnChrono: TButton
    Left = 175
    Top = 145
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnChronoClick
  end
  object cbPrecissionFormat: TComboBox
    Left = 24
    Top = 147
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 1
    Text = 'Float'
    Items.Strings = (
      'Float'
      'Round'
      'Truncate')
  end
  object tiChrono: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tiChronoTimer
    Left = 304
    Top = 128
  end
end
