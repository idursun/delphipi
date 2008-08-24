inherited SelectDelphiInstallationPage: TSelectDelphiInstallationPage
  ActiveControl = rgDelphiVersions
  Caption = 'SelectDelphiInstallationPage'
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 500
  ExplicitHeight = 250
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
    OnClick = rgDelphiVersionsClick
  end
  object grpOutputFolders: TGroupBox
    Left = 8
    Top = 122
    Width = 484
    Height = 120
    Caption = 'Output Folders'
    TabOrder = 1
    object lblBPLOutputFolder: TLabel
      Left = 15
      Top = 29
      Width = 21
      Height = 13
      Caption = 'BPL:'
    end
    object lblDCP: TLabel
      Left = 15
      Top = 59
      Width = 24
      Height = 13
      Caption = 'DCP:'
    end
    object edtBPL: TEdit
      Left = 52
      Top = 24
      Width = 378
      Height = 21
      TabOrder = 0
    end
    object btnBPLBrowse: TButton
      Left = 436
      Top = 22
      Width = 28
      Height = 25
      Caption = '...'
      TabOrder = 1
      OnClick = btnBPLBrowseClick
    end
    object edtDCP: TEdit
      Left = 52
      Top = 53
      Width = 378
      Height = 21
      TabOrder = 2
    end
    object btnDCPBrowse: TButton
      Left = 436
      Top = 51
      Width = 28
      Height = 25
      Caption = '...'
      TabOrder = 3
      OnClick = btnDCPBrowseClick
    end
  end
end
