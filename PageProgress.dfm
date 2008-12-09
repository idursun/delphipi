inherited ProgressPage: TProgressPage
  Caption = 'ProgressPage'
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 484
    Height = 68
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Overall Progress'
    TabOrder = 0
    DesignSize = (
      484
      68)
    object Label1: TLabel
      Left = 12
      Top = 18
      Width = 44
      Height = 13
      Caption = 'Package:'
    end
    object lblPackage: TLabel
      Left = 62
      Top = 18
      Width = 61
      Height = 13
      Caption = 'lblPackage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblCurrentPackageNo: TLabel
      Left = 401
      Top = 18
      Width = 15
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight, akBottom]
      Caption = '     '
    end
    object ProgressBar: TProgressBar
      Left = 12
      Top = 37
      Width = 404
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 422
      Top = 37
      Width = 59
      Height = 20
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object memo: TRichEdit
    Left = 8
    Top = 82
    Width = 484
    Height = 143
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = TURKISH_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object chkShowFullLog: TCheckBox
    Left = 8
    Top = 231
    Width = 188
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Show Full Log'
    TabOrder = 2
    OnClick = chkShowFullLogClick
  end
end
