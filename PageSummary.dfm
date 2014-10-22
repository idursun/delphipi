inherited SummaryPage: TSummaryPage
  Left = 328
  Top = 271
  Caption = 'SummaryPage'
  ClientHeight = 232
  ClientWidth = 412
  OnCreate = FormCreate
  ExplicitWidth = 412
  ExplicitHeight = 232
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 183
    Width = 312
    Height = 34
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Would you like to save this installation as a script so that you' +
      ' can automate installation of these packages?'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Summary:'
  end
  object btnSave: TButton
    Left = 324
    Top = 183
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object edtSummary: TMemo
    Left = 8
    Top = 27
    Width = 396
    Height = 150
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
