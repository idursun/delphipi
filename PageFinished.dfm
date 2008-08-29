inherited FinishedPage: TFinishedPage
  Caption = 'FinishedPage'
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 22
    Top = 21
    Width = 442
    Height = 58
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Would you like to save this installation as a script so that you' +
      ' can '#13#10'automate installation of these packages?'
  end
  object btnSave: TButton
    Left = 22
    Top = 82
    Width = 60
    Height = 37
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
  end
end
