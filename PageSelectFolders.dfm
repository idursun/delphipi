inherited SelectFoldersPage: TSelectFoldersPage
  ActiveControl = edtBaseFolder
  Caption = 'SelectFoldersPage'
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpPackagePattern: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 134
    Width = 494
    Height = 79
    Align = alTop
    Caption = 'Pattern to select package files '
    TabOrder = 2
    ExplicitLeft = 8
    ExplicitTop = 79
    ExplicitWidth = 484
    DesignSize = (
      494
      79)
    object Label3: TLabel
      Left = 12
      Top = 51
      Width = 98
      Height = 13
      Caption = 'Package File Pattern'
    end
    object Label2: TLabel
      Left = 36
      Top = 16
      Width = 418
      Height = 26
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Specify a pattern that matches for the package files that are su' +
        'itable for your delphi installation. ie: *d7.dpk for Delphi 7'
      WordWrap = True
      ExplicitWidth = 408
    end
    object imgInfo: TImage
      Left = 12
      Top = 16
      Width = 16
      Height = 16
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
        001008060000001FF3FF610000000467414D410000AFC837058AE90000001974
        455874536F6674776172650041646F626520496D616765526561647971C9653C
        000002DE4944415478DAA5936D48935114C7FF8FAFB939B569E566A66E0E095F
        52D1FAB01484243045D028B050F04B5129FA21B21714853E242488A4465128BD
        510D47504465465343372A34CB395F524B4D5D6A5337F7BCDCEE1ECB0F995FEA
        C0E5C2BDF7FCCE39FF7B0E4308C1FF18F32720A7D1AC66052E8F7042FE8A930D
        E6880002610A109A798EBF6338AB1DDA1090DDF029D7E964EB22FC8962775800
        FCBCDCC5F3053B0783E51B7A271627A94791B16A9F6E1D20ABBE2FD769679BD2
        351269CC7619BECE3A31B560072F10F84BDCA1F4DF04CBAC0D2DAD834BF04081
        B1365BB706C8ACEB55B12C67488F942AA31432740FCF83E358EC0ADB0CD0FBB7
        237360DC18442B7C313E6783FED9C004889062BC76785804ECAF7D571E212595
        99F14A749AADE0594E8C5C752446CCEEFCEDF714047879B8212E4C8E27DD6698
        7B662A4CB78E568980B41A93253F6E4BE4A29DC59875913A53E1E84A54CB5D7E
        E81A985C2D98BE5506FA41EEEF8D1BD73B068DF70B3522407BB163F9747AB84F
        B7791676270796672980C795E35A1170A2DE200269C5F0F460901A1B8A4B358F
        ED26DD498908482E6F5B3E97A1F1793360C5927D913E26AE603875201AE1DB64
        286DA4007AE08279D28F494B52A1BAFA91DDA42FFE0D786139961A1E695D7062
        7CE607789A8140231665C522225886E2BA57347D573F106C0D942124C40FCD57
        5B078D2D25AB2524973D2DDFA9F0AECC4850A1FDC3172A228F6C6D38D21343E1
        46D5EF199AC1E57B5D2E11A08D57A1B5D38C918FA315C696D25511934B5A5458
        E10C99691A65845C8AF6BE7170AE9FE07910BA935FD113A276609675E0F903C3
        0403A418F56786D71A29A9F06E2EB1399A327262A5AA2019FA47A731F3DD0681
        13200F9040131A84CFF336BC7CD8B604C6B3803AEBD6B572D2A19BB920CE3A75
        B442B1774F24027DBDC4B4A7171C78DDD98FB13ECB24C3B81519F565BABFCE82
        CB920F36A809E1F3A868F9101CC12EF11886A1C3846606EC9D6EFD858D87E95F
        EC27BCCF85F03B2A06720000000049454E44AE426082}
    end
    object cbPattern: TComboBox
      Left = 116
      Top = 48
      Width = 368
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      Text = '*.dpk'
      ExplicitWidth = 358
    end
  end
  object grpBaseFolder: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 494
    Height = 65
    Align = alTop
    Caption = 'Select Base Folder contains both Package and Source files'
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 484
    DesignSize = (
      494
      65)
    object Label1: TLabel
      Left = 12
      Top = 16
      Width = 56
      Height = 13
      Caption = 'Base Folder'
    end
    object btnSelectFolder: TButton
      Left = 452
      Top = 35
      Width = 32
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 0
      WordWrap = True
      OnClick = btnSelectFolderClick
      ExplicitLeft = 442
    end
    object edtBaseFolder: TEdit
      Left = 12
      Top = 35
      Width = 434
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtBaseFolderChange
      ExplicitWidth = 424
    end
  end
  object grpDelphiVersion: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 74
    Width = 494
    Height = 54
    Align = alTop
    Caption = 'Installed Delphi Versions'
    TabOrder = 1
    ExplicitLeft = 6
    ExplicitTop = 11
    DesignSize = (
      494
      54)
    object cbDelphiVersions: TComboBox
      Left = 15
      Top = 20
      Width = 459
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbDelphiVersionsChange
    end
  end
end
