{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageSelectFolders;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ExtCtrls, WizardIntfs;

type
  TSelectFoldersPage = class(TWizardPage)
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cbPattern: TComboBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    btnSelectFolder: TButton;
    edtBaseFolder: TEdit;
    Label2: TLabel;
    Image1: TImage;
    procedure btnSelectFolderClick(Sender: TObject);
    procedure edtBaseFolderChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure UpdateWizardState(const wizard: IWizard); override;
  end;

var
  SelectFoldersPage: TSelectFoldersPage;

implementation
uses FileCtrl, WizardData, gnugettext;
{$R *.dfm}

{ TSelectFoldersPage }

procedure TSelectFoldersPage.UpdateWizardState(const wizard: IWizard);
var
  button: TButton;
begin
  inherited;
  wizard.SetHeader(_('Select Folders'));
  wizard.SetDescription(_('Please select folders which contains the packages that you want to install'));

  button := wizard.GetButton(wbtNext);
  button.Enabled := edtBaseFolder.Text <> '';
end;

procedure TSelectFoldersPage.btnSelectFolderClick(Sender: TObject);
var
  directory: string;
begin
  inherited;
  directory := edtBaseFolder.Text;
  if SelectDirectory(_('Select the folder where packages are'),'',directory) then begin
    edtBaseFolder.Text := directory;
    wizard.UpdateInterface;
  end;
end;

procedure TSelectFoldersPage.edtBaseFolderChange(Sender: TObject);
begin
  inherited;
  wizard.UpdateInterface;
end;

procedure TSelectFoldersPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  data : TWizardData;
begin
  inherited;
  data := TWizardData(wizard.GetData);// as IWizardData;
  if (data = nil) then exit;
  data.SetBaseFolder(edtBaseFolder.Text);
  data.SetPattern(cbPattern.Text);

  if cbPattern.Items.IndexOf(cbPattern.Text) = -1 then
    cbPattern.Items.Add(cbPattern.Text);

  cbPattern.Items.SaveToFile('patterns.txt');
end;

procedure TSelectFoldersPage.FormCreate(Sender: TObject);
var
  data : TWizardData;
begin
  inherited;
  TranslateComponent(self);
  data := TWizardData(wizard.GetData);
  if (data = nil) then exit;
  edtBaseFolder.Text := data.BaseFolder;
  cbPattern.Text := data.Pattern;

  if (FileExists('patterns.txt')) then
    cbPattern.Items.LoadFromFile('patterns.txt');
end;

end.
