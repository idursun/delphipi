inherited SelectCompilerOptions: TSelectCompilerOptions
  Left = 290
  Top = 283
  Caption = 'SelectCompilerOptions'
  ClientHeight = 232
  ClientWidth = 412
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 412
  ExplicitHeight = 232
  PixelsPerInch = 96
  TextHeight = 13
  object grpOutputFolders: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 406
    Height = 142
    Align = alTop
    Caption = 'Compiler Options'
    TabOrder = 0
    DesignSize = (
      406
      142)
    object lblBPLOutputFolder: TLabel
      Left = 15
      Top = 27
      Width = 91
      Height = 13
      Caption = 'BPL Output Folder:'
    end
    object lblDCP: TLabel
      Left = 15
      Top = 56
      Width = 94
      Height = 13
      Caption = 'DCP Output Folder:'
    end
    object lblDCU: TLabel
      Left = 15
      Top = 83
      Width = 95
      Height = 13
      Caption = 'DCU Output Folder:'
    end
    object Label1: TLabel
      Left = 15
      Top = 110
      Width = 95
      Height = 13
      Caption = 'Compiler Directives:'
    end
    object edtBPL: TEdit
      Left = 116
      Top = 24
      Width = 277
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnBPLBrowse: TButton
      Left = 446
      Top = 22
      Width = 28
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnBPLBrowseClick
    end
    object edtDCP: TEdit
      Left = 116
      Top = 53
      Width = 277
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object btnDCPBrowse: TButton
      Left = 446
      Top = 51
      Width = 28
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 3
      OnClick = btnDCPBrowseClick
    end
    object edtDCU: TEdit
      Left = 116
      Top = 80
      Width = 277
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object btnDCUBrowse: TButton
      Left = 446
      Top = 78
      Width = 28
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 5
      OnClick = btnDCUBrowseClick
    end
    object edtConditionals: TEdit
      Left = 116
      Top = 107
      Width = 277
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
    end
  end
end
