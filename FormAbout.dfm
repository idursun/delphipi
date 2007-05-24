object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About Delphi PI'
  ClientHeight = 189
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 326
    Height = 111
    Alignment = taCenter
    AutoSize = False
    Caption = 'Delphi PI'#13#10'0.1 '#13#10'ibrahim dursun'#13#10'2007'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Bevel1: TBevel
    Left = 8
    Top = 144
    Width = 329
    Height = 9
    Shape = bsTopLine
  end
  object Label2: TLabel
    Left = 104
    Top = 125
    Width = 118
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://www.thexpot.net'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label2Click
  end
  object Button1: TButton
    Left = 262
    Top = 159
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
end
