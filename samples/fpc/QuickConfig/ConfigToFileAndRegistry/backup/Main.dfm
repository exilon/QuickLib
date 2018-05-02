object MainForm: TMainForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  Caption = 'Quick Config Demo'
  ClientHeight = 347
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object meInfo: TMemo
    Left = 8
    Top = 8
    Width = 507
    Height = 273
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnLoadJson: TButton
    Left = 208
    Top = 287
    Width = 145
    Height = 25
    Caption = 'Load From Json'
    TabOrder = 1
    OnClick = btnLoadJsonClick
  end
  object btnSaveJson: TButton
    Left = 359
    Top = 287
    Width = 154
    Height = 25
    Caption = 'Save To Json'
    TabOrder = 2
    OnClick = btnSaveJsonClick
  end
  object btnLoadRegistry: TButton
    Left = 208
    Top = 318
    Width = 145
    Height = 25
    Caption = 'Load From Registry'
    TabOrder = 3
    OnClick = btnLoadRegistryClick
  end
  object SaveRegistry: TButton
    Left = 359
    Top = 318
    Width = 154
    Height = 25
    Caption = 'Save To Registry'
    TabOrder = 4
    OnClick = SaveRegistryClick
  end
end
