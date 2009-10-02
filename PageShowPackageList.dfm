object ShowPackageListPage: TShowPackageListPage
  Left = 233
  Top = 185
  Caption = 'ShowPackageListPage'
  ClientHeight = 337
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    588
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object lblWait: TLabel
    Left = 8
    Top = 8
    Width = 572
    Height = 321
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Please wait while searching folders '
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    Layout = tlCenter
  end
  object fPackageTree: TVirtualStringTree
    Left = 0
    Top = 22
    Width = 588
    Height = 315
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible, hoAutoSpring]
    HintMode = hmHint
    Images = ImageList
    ParentShowHint = False
    PopupMenu = pmSelectPopupMenu
    ShowHint = True
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toMultiSelect]
    TreeOptions.StringOptions = [toSaveCaptions]
    OnChecked = packageTreeChecked
    OnGetText = fPackageTreeGetText
    OnPaintText = fPackageTreePaintText
    OnGetImageIndex = packageTreeGetImageIndex
    OnGetHint = fPackageTreeGetHint
    OnGetNodeDataSize = packageTreeGetNodeDataSize
    OnInitChildren = fPackageTreeInitChildren
    OnInitNode = fPackageTreeInitNode
    OnKeyAction = fPackageTreeKeyAction
    Columns = <
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus]
        Position = 0
        Width = 259
        WideText = 'Package'
      end
      item
        Position = 1
        Width = 220
        WideText = 'Description'
      end
      item
        Position = 2
        Width = 70
        WideText = 'Type'
      end>
  end
  object toolbar: TToolBar
    Left = 0
    Top = 0
    Width = 588
    Height = 22
    AutoSize = True
    Caption = 'toolbar'
    Images = ilActionImages
    TabOrder = 1
    object btnFolderView: TToolButton
      Left = 0
      Top = 0
      Caption = 'btnFolderView'
      DropdownMenu = pmViewStyles
      ImageIndex = 0
      Style = tbsDropDown
    end
    object sepearator1: TToolButton
      Left = 38
      Top = 0
      Width = 8
      Caption = 'sepearator1'
      ImageIndex = 2
      Style = tbsSeparator
    end
  end
  object pmSelectPopupMenu: TPopupMenu
    Left = 292
    Top = 5
    object miSelectAll: TMenuItem
      Caption = 'Select All'
      OnClick = miSelectAllClick
    end
    object miUnselectAll: TMenuItem
      Caption = 'Unselect All'
      OnClick = miUnselectAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miSelectMatching: TMenuItem
      Caption = 'Select Matching...'
      OnClick = miSelectMatchingClick
    end
    object miUnselectMatching: TMenuItem
      Caption = 'Unselect Matching...'
      OnClick = miUnselectMatchingClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Remove1: TMenuItem
      Action = actRemove
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miCollapse: TMenuItem
      Caption = 'Collapse'
      object miCollapseChildren: TMenuItem
        Caption = 'Children'
        OnClick = miCollapseChildrenClick
      end
      object miCollapseAll: TMenuItem
        Caption = 'All'
        OnClick = miCollapseAllClick
      end
    end
    object miExpand: TMenuItem
      Caption = 'Expand'
      object miExpandChildren: TMenuItem
        Caption = 'Children'
        OnClick = miExpandChildrenClick
      end
      object miExpandAll: TMenuItem
        Caption = 'All'
        OnClick = miExpandAllClick
      end
    end
  end
  object ImageList: TImageList
    Left = 350
    Top = 92
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002687C50000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002C8EC8008FCDEB006FB7E200408EC800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002D87
      D8002D88D8002D88D8002D88D8002D88D8002D88D8002D88D8002D88D8002D88
      D8002D87D8002D88D80000000000000000000000000000000000000000000000
      00003295CB0082C4E500CCF4FF00C4EFFF008BD2F1008ACEF0005FA4D7002586
      C400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000338ED900DCF0FA00A7DD
      F4009EDBF40096DAF3008ED8F30086D7F3007FD4F20079D3F20072D2F1006CD0
      F10069CFF100C2EAF800338ED900000000000000000000000000399DCE007DC0
      E000C7EEFC00CCF2FF00A8E8FF0094E0FE0041BAE70045B1E4008ACAEF0082C1
      EB005397D0002384C30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003594DA00EFFAFE00A1E9
      F90091E5F80081E1F70072DEF60063DAF50054D7F40047D3F30039D0F2002ECD
      F10026CBF000CAF2FB003594DA00000000000000000077BDDC00BFE5F600DBF6
      FF00C1EEFF00A5E5FF009FE3FF0094E1FE0046C1EA003AB5E60037AAE20056AF
      E50087C6ED0074B3E4004A90CA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000369ADA00F2FAFD00B3ED
      FA00A4E9F90095E6F80085E2F70076DEF60065DBF50057D7F40049D4F3003BD1
      F20030CEF100CCF2FB003598DA000000000042A8D300E7FBFE00DDF6FF00C1EF
      FF00B7EBFF00ABE8FF00A4E4FF0096E1FE0048C6EB0040BDE9003DB4E60038A9
      E200329FDE006BB6E60083C4EE002485C4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000036A1DA00F6FCFE00C8F2
      FC00B9EFFB00ACECFA009CE8F9008BE3F7007CE0F6006CDCF6005DD9F5004FD6
      F40044D3F300D0F3FC00359FDA000000000044AAD400E2F6FC00D4F3FF00C9F0
      FF00BEEDFF00B3EAFF00ADE7FF007CD9FE0048C7EF0043C4EA0043BEE8003FB5
      E6003AABE30040A7E10083C5EC002687C5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000037A6DA00FEFFFF00F8FD
      FF00F6FDFF00F5FCFF00F3FCFE00D8F6FC0094E6F80085E3F70076DFF60068DB
      F5005CD8F400D7F4FC0035A4DA000000000047ADD500E2F6FC00D7F4FF00CEF2
      FF00C8EFFF00BAEBFF0092DBFB0056C1F10048C2F9003BBDF00047C5EC0045BD
      E90042B5E60047B1E60088CAEE00288AC6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000035ABDA00E8F6FB0094D4
      EF0088CEEE0073C1E900C9E9F600F2FCFE00F3FCFE00F2FCFE00F0FCFE00EFFB
      FE00EEFBFE00FEFFFF0036ABDA000000000049B0D600E2F6FD00DAF4FF00D5F3
      FF00BDEBFF0089D5F70069C9F5004CB4E9008DDAFB008CDCFF0048C4F90038B6
      EC0048BFE8004FBBE8008CD0F0002B8DC7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000036AADA00F1FAFD0094DE
      F50093DCF40081D5F2006ACAED006CCBEA0085D3EF0080D2EF007AD0EF0076CF
      EE0072CFEE00E9F7FB0034AEDA00000000004BB2D700E2F8FD00D4F3FF00B0E4
      FA0086CFF1007FD0F50078D0F5004CB1E400B0E4FA00B6E9FF009BE1FF0078D6
      FE0040BDF5003DB5E90090D5F1002D8FC8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000035AFDA00F7FCFE008EE4
      F80091DEF5009FE0F500ACE1F600EFFBFE00F4FDFE00F3FCFE00F1FCFE00EFFB
      FE00EEFBFE00FAFDFF0036AFDA00000000004DB4D800E1F8FE00CDEBF90092D2
      ED0084CCEB006FBFE50056B1DB003B94C800CEECFA00D9F5FF00B9EAFF0095DF
      FE0077D5FF00A5E4FF0084DCFB002F92C9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000036B3DA00FDFEFE00FEFF
      FF00FEFEFF00FDFEFF00FEFFFF00EAF7FB006BC7E4006BC7E3006BC7E3006BC7
      E30079CDE60074CAE5000000000000000000000000004DB5D900A5D9ED00D2EB
      F500BEDEED0095C9DE0089C3DB0070B8D60069B9DD0090D7F5007FCFF5009DDB
      F800AAE3FA0084CAEC0051A6D500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000034B4D9005EC2E10060C3
      E20060C3E20060C3E2005FC3E2003CB6DB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007EC6
      E000D1EEF700F6FFFF00F0FEFF00CBEDFB0050ADDA008BD7F700AAE1F90095D6
      F20062B2DB00399DCE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004CB4D80092CFE500E6F8FC00E3F6FE00AFDDF200B2E4F70072C0E1003FA4
      D100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004BB2D7009CD5EA0088CCE70045ACD500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFF7F00000000FFFFFC3F00000000
      E003F00F000000008001C0030000000080018001000000008001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      8001000000000000800380010000000080FFE00300000000FFFFF00F00000000
      FFFFFC3F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object ActionList: TActionList
    Images = ilActionImages
    Left = 432
    Top = 64
    object actRemove: TAction
      Caption = 'Remove'
      ShortCut = 46
      OnExecute = actRemoveExecute
      OnUpdate = actRemoveUpdate
    end
    object actChangeViewToTree: TAction
      Category = 'View'
      Caption = 'Tree View'
      ImageIndex = 1
      OnExecute = actChangeViewToTreeExecute
    end
    object actChangeViewToList: TAction
      Category = 'View'
      Caption = 'List View'
      ImageIndex = 0
      OnExecute = actChangeViewToListExecute
    end
  end
  object ilActionImages: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 408
    Top = 184
    Bitmap = {
      494C010102000500040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F4B36A9C28D68FFBF8A66FFBD87
      64FFBA8461FFB8825FFFB37E5CFFB17C5AFFB07B58FFAD7957FFAC7656FFAA75
      54FFA87353FFA87151FF6F4B36A9000000006F4B36A9C28D68FFBF8A66FFBD87
      64FFBA8461FFB8825FFFB37E5CFFB17C5AFFB07B58FFAD7957FFAC7656FFAA75
      54FFA87353FFA87151FF6F4B36A9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C8916CFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA87251FF00000000C8916CFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFDCA67BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA87251FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CA936EFFFFFFFFFFFFAB48FFFDFD
      FDFFB1B1B1FFACACACFFA7A7A7FFA3A3A3FF9E9E9EFF9C9C9CFF9A9A9AFF9898
      98FF989898FFFFFFFFFFA97353FF00000000CA936EFFFFFFFFFFF3F3F3FFBABA
      BAFFBABABAFFFFFFFFFFDCA67BFFFFFFFFFFFEFEFCFFFEFEFCFFFEFEFAFFFEFE
      FAFFFCFCF9FFFFFFFFFFA97353FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CC966FFFFFFFFFFFFEFEFEFFFDFD
      FDFFFDFDFDFFFDFDFDFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFB
      FBFFFBFBFBFFFFFFFFFFAB7554FF00000000CC966FFFFFFFFFFFF3F3F3FFBABA
      BAFFFFFFFFFFFFFFFFFFDCA67BFFFFFFFFFFFDFDFAFFFDFDFAFFFDFDFAFFFCFC
      F7FFFBFBF6FFFFFFFFFFAB7554FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D19B73FFFFFFFFFFFFB152FFFEFE
      FEFFB8B8B8FFB4B4B4FFB0B0B0FFACACACFFA8A8A8FFA5A5A5FFA3A3A3FFA1A1
      A1FFA0A0A0FFFFFFFFFFAF7A58FF00000000D19B73FFFFFFFFFFF3F3F3FFBABA
      BAFFBABABAFFFFFFFFFFDCA67BFFFFFFFFFFFDFDF8FFFBFBF9FFFBFAF7FFFBFA
      F6FFFBF8F4FFFFFFFFFFAF7A58FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D49D75FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFEFEFEFFFEFEFEFFFEFEFEFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
      FDFFFCFCFCFFFFFFFFFFB17C5AFF00000000D49D75FFFFFFFFFFF3F3F3FFBABA
      BAFFFFFFFFFFFFFFFFFFDCA67BFFFFFFFFFFFBF9F7FFFBF9F5FFFBF8F4FFFBF7
      F2FFFBF5F2FFFFFFFFFFB17C5AFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D59F76FFFFFFFFFFFFB85EFFFFFF
      FFFFC0C0C0FFBDBDBDFFB9B9B9FFB6B6B6FFB4B4B4FFB1B1B1FFAFAFAFFFADAD
      ADFFACACACFFFFFFFFFFB47E5CFF00000000D59F76FFFFFFFFFFBABABAFFBABA
      BAFFBABABAFFFFFFFFFFDCA67BFFFFFFFFFFFBF8F4FFFBF7F3FFFBF5F2FFFAF3
      EFFFF8F2ECFFFFFFFFFFB47E5CFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D8A179FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFFEFEFEFFFEFEFEFFFDFD
      FDFFFDFDFDFFFFFFFFFFB6815EFF00000000D8A179FFFFFFFFFFBABABAFFFFFF
      FFFFFFFFFFFFFFFFFFFFDCA67BFFFFFFFFFFFBF6F1FFF8F4EEFFF7F2EBFFF7F0
      EAFFF6ECE8FFFFFFFFFFB6815EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D9A279FFFFFFFFFFFFBE68FFFFFF
      FFFFC7C7C7FFC5C5C5FFC2C2C2FFC0C0C0FFBEBEBEFFBCBCBCFFBABABAFFB8B8
      B8FFB8B8B8FFFFFFFFFFB98460FF00000000D9A279FFFFFFFFFFB5B5B5FFBABA
      BAFFFFFFFFFFFFFFFFFFDCA67BFFFFFFFFFFF7F3EDFFF6EFEAFFF5EBE7FFF3EA
      E4FFF2E7DEFFFFFFFFFFB98460FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBA37AFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFBC8663FF00000000DBA37AFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFDCA67BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFBC8663FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DCA67BFFDCA67BFFDCA67BFFDCA6
      7BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA6
      7BFFDCA67BFFDCA67BFFBF8A66FF00000000DCA67BFFDCA67BFFDCA67BFFDCA6
      7BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA67BFFDCA6
      7BFFDCA67BFFDCA67BFFBF8A66FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBAA83FDE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFBF8E6EFD00000000DBAA83FDE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFBF8E6EFD000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004730236BD4A986F4DCA67BFFDCA5
      7AFFDAA37AFFD8A179FFD59F76FFD49D75FFD29C73FFCF9972FFCE9870FFCB95
      6FFFC9936CFFBB9374F44730236B000000004730236BD4A986F4DCA67BFFDCA5
      7AFFDAA37AFFD8A179FFD59F76FFD49D75FFD29C73FFCF9972FFCE9870FFCB95
      6FFFC9936CFFBB9374F44730236B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object pmViewStyles: TPopupMenu
    Images = ilActionImages
    Left = 200
    Top = 80
    object List1: TMenuItem
      Action = actChangeViewToList
    end
    object FolderTree1: TMenuItem
      Action = actChangeViewToTree
    end
  end
end
