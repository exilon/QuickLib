object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Quick.IOC: scoped services and IOwned (VCL)'
  ClientHeight = 580
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object LabelIntro: TLabel
    Left = 16
    Top = 12
    Width = 748
    Height = 64
    AutoSize = False
    WordWrap = True
  end
  object LabelScope: TLabel
    Left = 16
    Top = 82
    Width = 748
    Height = 15
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object CheckOwnScope: TCheckBox
    Left = 16
    Top = 108
    Width = 560
    Height = 19
    Caption = 'Open the screen with a scope of its own (disconnected from scope A)'
    TabOrder = 0
  end
  object CheckOwnedAudit: TCheckBox
    Left = 16
    Top = 132
    Width = 560
    Height = 19
    Caption = 'Audit repository in an IOwned (a scope of its own for the audit)'
    TabOrder = 1
  end
  object ButtonOpen: TButton
    Left = 16
    Top = 160
    Width = 160
    Height = 28
    Caption = 'Open a screen'
    Default = True
    TabOrder = 2
    OnClick = ButtonOpenClick
  end
  object ButtonDatabase: TButton
    Left = 186
    Top = 160
    Width = 160
    Height = 28
    Caption = 'Show the database'
    TabOrder = 3
    OnClick = ButtonDatabaseClick
  end
  object MemoLog: TMemo
    Left = 16
    Top = 200
    Width = 748
    Height = 364
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
end
