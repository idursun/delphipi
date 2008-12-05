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
      Left = 448
      Top = 18
      Width = 15
      Height = 13
      Anchors = [akTop, akRight, akBottom]
      Caption = '     '
    end
    object ProgressBar: TProgressBar
      Left = 3
      Top = 37
      Width = 478
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object memo: TRichEdit
    Left = 8
    Top = 82
    Width = 484
    Height = 160
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
end
