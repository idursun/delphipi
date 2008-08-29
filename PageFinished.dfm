inherited FinishedPage: TFinishedPage
  Caption = 'FinishedPage'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 208
    Width = 418
    Height = 34
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Would you like to save this installation as a script so that you' +
      ' can automate installation of these packages?'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 13
    Top = 9
    Width = 48
    Height = 13
    Caption = 'Summary:'
  end
  object btnSave: TButton
    Left = 432
    Top = 205
    Width = 60
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
    ExplicitLeft = 453
  end
  object edtSummary: TMemo
    Left = 8
    Top = 28
    Width = 484
    Height = 163
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
