inherited ProgressPage: TProgressPage
  Caption = 'ProgressPage'
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 500
  ExplicitHeight = 250
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 484
    Height = 68
    Caption = 'Overall Progress'
    TabOrder = 0
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
      Caption = 'File:'
      Visible = False
    end
    object lblFileName: TLabel
      Left = 390
      Top = 18
      Width = 75
      Height = 13
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
      Width = 31
      Height = 13
      Caption = '     '
    end
    object ProgressBar: TProgressBar
      Left = 12
      Top = 37
      Width = 432
      Height = 17
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 82
    Width = 484
    Height = 160
    Caption = 'Compiler Output'
    TabOrder = 1
    object Memo: TMemo
      Left = 3
      Top = 14
      Width = 478
      Height = 143
      TabStop = False
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
