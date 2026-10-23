object ScreenForm: TScreenForm
  Left = 0
  Top = 0
  Caption = 'Screen'
  ClientHeight = 156
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDefaultPosOnly
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 15
  object LabelName: TLabel
    Left = 16
    Top = 16
    Width = 86
    Height = 15
    Caption = 'Customer name'
  end
  object LabelHint: TLabel
    Left = 16
    Top = 100
    Width = 438
    Height = 48
    AutoSize = False
    WordWrap = True
  end
  object EditName: TEdit
    Left = 16
    Top = 36
    Width = 270
    Height = 23
    TabOrder = 0
    Text = 'Ann'
  end
  object ButtonSave: TButton
    Left = 304
    Top = 35
    Width = 150
    Height = 25
    Caption = 'Save'
    Default = True
    TabOrder = 1
    OnClick = ButtonSaveClick
  end
  object CheckRollback: TCheckBox
    Left = 16
    Top = 72
    Width = 300
    Height = 19
    Caption = 'Roll back the transaction at the end'
    TabOrder = 2
  end
end
