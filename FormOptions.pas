{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit FormOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JclBorlandTools;

type
  TfrmOptions = class(TForm)
    rgDelphiVersions: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    function GetInstaller: TJclBorRADToolInstallation;
    function GetPattern: String;
    { Private declarations }
  public
    { Public declarations }
    property Installer : TJclBorRADToolInstallation read GetInstaller;
    property Pattern : String read GetPattern;
  private
    var
    installations: JclBorlandTools.TJclBorRADToolInstallations;
  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.dfm}

procedure TfrmOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ComboBox1.Items.IndexOf(combobox1.Text) = -1 then
    ComboBox1.Items.Add(combobox1.Text);
  
  ComboBox1.Items.SaveToFile('patterns.txt');
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  installations := TJclBorRADToolInstallations.Create;

  for I := 0 to installations.Count - 1 do begin
    rgDelphiVersions.Items.Add(installations.Installations[i].Description);
  end;

  if (FileExists('patterns.txt')) then
    ComboBox1.Items.LoadFromFile('patterns.txt');

  if rgDelphiVersions.Items.Count > 0 then 
    rgDelphiVersions.ItemIndex := 0;
end;

function TfrmOptions.GetInstaller: TJclBorRADToolInstallation;
begin
  Result := nil;
  if rgDelphiVersions.ItemIndex <> -1 then
    Result := installations.Installations[rgDelphiVersions.ItemIndex];
end;

function TfrmOptions.GetPattern: String;
begin
  Result := ComboBox1.Text;
end;

end.
