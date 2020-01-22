object MainForm: TMainForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  Caption = 'Quick Config Demo'
  ClientHeight = 493
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    720
    493)
  PixelsPerInch = 96
  TextHeight = 13
  object meInfo: TMemo
    Left = 8
    Top = 8
    Width = 704
    Height = 441
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnLoadFile: TButton
    Left = 407
    Top = 464
    Width = 145
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Load From File'
    TabOrder = 1
    OnClick = btnLoadFileClick
  end
  object btnSaveFile: TButton
    Left = 558
    Top = 464
    Width = 154
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save To File'
    TabOrder = 2
    OnClick = btnSaveFileClick
  end
end
