inherited SummaryPage: TSummaryPage
  Caption = 'SummaryPage'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 208
    Width = 398
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
    Left = 412
    Top = 203
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object edtSummary: TMemo
    Left = 8
    Top = 28
    Width = 484
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
