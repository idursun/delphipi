object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 320
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 30
    Top = 123
    Width = 295
    Height = 26
    Caption = 
      'Specify a pattern that matches for the package files that are su' +
      'itable for your delphi installation. ie: *d7.dpk for Delphi 7'
    WordWrap = True
  end
  object Image1: TImage
    Left = 8
    Top = 123
    Width = 18
    Height = 22
    Picture.Data = {
      0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000010
      0000001008060000001FF3FF61000000017352474200AECE1CE9000000046741
      4D410000B18F0BFC6105000000206348524D00007A26000080840000FA000000
      80E8000075300000EA6000003A98000017709CBA513C000002B049444154384F
      A593ED4F526118C6EB5FD13FA173FAD2DAF443736D3648D23E64D6304DD456BA
      0AB3C12AC5447C0314B021186ABE50BE41498A90C0918C9097D0D4930333686B
      AB2F90B52CAF1E68A2B6D6DA3ADB75B6B3F35CBFFBB9EEE77E0E0238F05F4F12
      B057799AE5CC13AA90285711648F357BE3D9324F3C4B36CF66C9DCA2A37799CC
      DFD7EF3373D44BBCE3F240ACBCC78F1E7B0423CC464A7A6B187CF573D0A29918
      2DB6F2F642D200529597D3E24B344FACC0E28DA17B2A0289F135EE0C2D416E5A
      81D1B18E869110E88AF1047D793C0D49017295C18C9CD68568D26C7A11838898
      6EF40730E87A8B41E73AAE1BFC10F60560985983E4A11F74A9314A970C65A4FA
      977CE5C817C4655A2FCC9EF7A8ED0F4148225CD5F9F0F9EB564A553A0FAABA3D
      10DEF7A2FF5918679A9E822AEC13A701D9AD1E566F8D40696271ADC7876A9D17
      555A0F7AED6B3010557431BFA471915801A8A7964117E8D834E08884D91C9B7F
      87DA5E3FA9F41295F7DC643143FE03DB44029513651DB3440E5CD2386164C2A0
      789D9B69002DB66F8EBA3752599395CAD52E62722114F984C4972D94B6DB51D2
      66C3052281C286E13902E02AF602ACACD6CA42FA6811951A37049D4E5C54CEE2
      55F823017C43B16C1AC52437BFC9829A6E064A4B10F4C9F6DD08748D455C24B7
      63D01141A58A8140EE80C5B38EEF3FB65311FC6F3EA048622632A1EB71080537
      4708A06DB789F495D10C5A608CDE1EF6C1605D45A9DC067EF314CE492751546F
      C2D9BA7114D68DA165D84B8E778E6C5F1AA5B9D2DD634C3683E20FF00EE5EB13
      A2010FF4D3CB106A9D64CB4F70BEC18C6A950D6A5300C25E0634A73E41CCFB07
      696734A9023D8FCAEF8A9DBE3501E5E4221E3859A255B49B83C8AB3582E234C4
      686EE39F477907429F526752BC0E1195A760296E539CE234C64945964874985B
      FFF7CBF4FB4DFB97EF9F0D1C9F1AF3BC69FC0000000049454E44AE426082}
  end
  object rgDelphiVersions: TRadioGroup
    Left = 8
    Top = 8
    Width = 313
    Height = 109
    Caption = 'Installed Delphi Versions'
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 169
    Top = 287
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 250
    Top = 287
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 157
    Width = 317
    Height = 63
    Caption = 'Package Selection Pattern '
    TabOrder = 3
    object Label1: TLabel
      Left = 22
      Top = 27
      Width = 36
      Height = 13
      Caption = 'Pattern'
    end
    object ComboBox1: TComboBox
      Left = 64
      Top = 23
      Width = 221
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = '*.dpk'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 226
    Width = 317
    Height = 46
    Caption = 'Help Files'
    Enabled = False
    TabOrder = 4
    object cbInstallHelp: TCheckBox
      Left = 22
      Top = 20
      Width = 263
      Height = 17
      Caption = 'Install Help Files, if you can'
      TabOrder = 0
    end
  end
end
