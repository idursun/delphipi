object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About Delphi PI'
  ClientHeight = 224
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 172
    Width = 329
    Height = 9
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 54
    Top = 9
    Width = 232
    Height = 23
    Caption = 'Delphi Package Installer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 264
    Top = 187
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 7
    Top = 45
    Width = 331
    Height = 123
    Lines.Strings = (
      '(Delphi PI 0.11)'
      'Author: ibrahim dursun ( thex@thexpot.net )'
      '2007'
      ''
      'Delphi Package Installer: '
      
        'Delphi PI helps you to compile & install delphi packages to IDE.' +
        ' '
      'Delphi PI:'
      '* automatically resolves any dependency between packages, '
      '* finds required source paths and adds to library path,'
      '* registers design packages.'
      ''
      'All you have to do is selecting a folder that contains both '
      'packages and files.'
      'If the directory structure is like:'
      ''
      'SynEdit\packages'
      'SynEdit\source '
      ''
      'then just select SynEdit folder.'
      ''
      'If package folder contains packages for many Delphi versions '
      'then use a pattern to match packages files suitable for your '
      'Delphi installation or just unselect the ones that are not '
      'suitable '
      'for yours.'
      ''
      'Delphi PI will work those directories and will come up with '
      'a list of packages. After then click Compile.'
      ''
      'Currently only tested with CodeGear Delphi 2007.')
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
