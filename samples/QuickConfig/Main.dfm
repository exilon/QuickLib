object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Quick Config Demo'
  ClientHeight = 274
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object meInfo: TMemo
    Left = 16
    Top = 16
    Width = 305
    Height = 209
    TabOrder = 0
  end
  object btnLoad: TButton
    Left = 168
    Top = 241
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 1
    OnClick = btnLoadClick
  end
  object btnSave: TButton
    Left = 249
    Top = 241
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
end
