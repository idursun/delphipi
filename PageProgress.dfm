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
    object Label2: TLabel
      Left = 364
      Top = 18
      Width = 20
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'File:'
      Visible = False
    end
    object lblFileName: TLabel
      Left = 390
      Top = 18
      Width = 64
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'lblFileName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object lblCurrentPackageNo: TLabel
      Left = 450
      Top = 37
      Width = 15
      Height = 13
      Caption = '     '
    end
    object ProgressBar: TProgressBar
      Left = 12
      Top = 37
      Width = 465
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 82
    Width = 484
    Height = 160
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Compiler Output'
    TabOrder = 1
    object Memo: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 474
      Height = 137
      TabStop = False
      Align = alClient
      Font.Charset = TURKISH_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
end
