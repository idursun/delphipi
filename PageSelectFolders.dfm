inherited SelectFoldersPage: TSelectFoldersPage
  Caption = 'SelectFoldersPage'
  OnClose = FormClose
  OnCreate = FormCreate
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
      Width = 221
      Height = 21
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
    Caption = 'Select Base Folder contains both Package and Source files'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 56
      Height = 13
      Caption = 'Base Folder'
    end
    object btnSelectFolder: TButton
      Left = 391
      Top = 32
      Width = 90
      Height = 23
      Caption = '&Select Folder...'
      TabOrder = 0
      OnClick = btnSelectFolderClick
    end
    object edtBaseFolder: TEdit
      Left = 8
      Top = 33
      Width = 380
      Height = 21
      TabOrder = 1
      OnChange = edtBaseFolderChange
    end
  end
end
