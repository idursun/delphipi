object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 215
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object rgDelphiVersions: TRadioGroup
    Left = 8
    Top = 8
    Width = 313
    Height = 81
    Caption = 'Installed Delphi Versions'
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 168
    Top = 182
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 249
    Top = 182
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 95
    Width = 313
    Height = 66
    Caption = 'Package Selection Pattern '
    TabOrder = 3
    object Label1: TLabel
      Left = 18
      Top = 28
      Width = 36
      Height = 13
      Caption = 'Pattern'
    end
    object ComboBox1: TComboBox
      Left = 60
      Top = 24
      Width = 221
      Height = 21
      ItemHeight = 0
      TabOrder = 0
      Text = '*.dpk'
    end
  end
end
