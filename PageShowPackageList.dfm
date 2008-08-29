inherited ShowPackageListPage: TShowPackageListPage
  Caption = 'ShowPackageListPage'
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object packageListView: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 494
    Height = 244
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Description'
        MinWidth = 200
      end
      item
        Caption = 'Name'
        MinWidth = 125
        Width = 125
      end
      item
        Caption = 'Type'
        MinWidth = 60
        Width = 60
      end>
    GridLines = True
    ParentShowHint = False
    PopupMenu = PopupMenu
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnInfoTip = packageListViewInfoTip
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 500
    ExplicitHeight = 250
  end
  object PopupMenu: TPopupMenu
    Left = 236
    Top = 111
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
    object miSelectUsing: TMenuItem
      Caption = 'Select Using...'
      OnClick = miSelectUsingClick
    end
  end
end
