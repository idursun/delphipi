inherited SelectDelphiInstallationPage: TSelectDelphiInstallationPage
  ActiveControl = rgDelphiVersions
  Caption = 'SelectDelphiInstallationPage'
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object rgDelphiVersions: TRadioGroup
    Left = 8
    Top = 8
    Width = 484
    Height = 109
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Installed Delphi Versions'
    TabOrder = 0
    TabStop = True
  end
end
