inherited SelectFoldersPage: TSelectFoldersPage
  Caption = 'SelectFoldersPage'
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 500
  ExplicitHeight = 250
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 90
    Width = 484
    Height = 99
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Pattern to select package files '
    TabOrder = 1
    DesignSize = (
      484
      99)
    object Label3: TLabel
      Left = 12
      Top = 60
      Width = 98
      Height = 13
      Caption = 'Package File Pattern'
    end
    object Label2: TLabel
      Left = 36
      Top = 16
      Width = 408
      Height = 26
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Specify a pattern that matches for the package files that are su' +
        'itable for your delphi installation. ie: *d7.dpk for Delphi 7'
      WordWrap = True
    end
    object Image1: TImage
      Left = 12
      Top = 19
      Width = 18
      Height = 22
    end
    object cbPattern: TComboBox
      Left = 116
      Top = 57
      Width = 358
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      Text = '*.dpk'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 484
    Height = 76
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Select Base Folder contains both Package and Source files'
    TabOrder = 0
    DesignSize = (
      484
      76)
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 56
      Height = 13
      Caption = 'Base Folder'
    end
    object btnSelectFolder: TButton
      Left = 442
      Top = 43
      Width = 32
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 0
      WordWrap = True
      OnClick = btnSelectFolderClick
    end
    object edtBaseFolder: TEdit
      Left = 12
      Top = 43
      Width = 424
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtBaseFolderChange
    end
  end
end
