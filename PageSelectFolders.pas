{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageSelectFolders;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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
    constructor Create(Owner: TComponent; const compilationData: TCompilationData); override; 
    procedure UpdateWizardState; override;
  end;

var
  SelectFoldersPage: TSelectFoldersPage;

implementation
uses FileCtrl, gnugettext, FormWizard;
{$R *.dfm}

{ TSelectFoldersPage }

procedure TSelectFoldersPage.UpdateWizardState;
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
    UpdateWizardState;
  end;
end;

constructor TSelectFoldersPage.Create(Owner: TComponent;
  const compilationData: TCompilationData);
begin
  inherited;
  fCompilationData := compilationData;
end;

procedure TSelectFoldersPage.edtBaseFolderChange(Sender: TObject);
begin
  inherited;
  UpdateWizardState;
end;

procedure TSelectFoldersPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  if cbPattern.Items.IndexOf(cbPattern.Text) = -1 then
    cbPattern.Items.Add(cbPattern.Text);

  FCompilationData.BaseFolder := edtBaseFolder.Text;
  FCompilationData.Pattern := cbPattern.Text;
  fCompilationData.SourceFilePaths.Clear;
  fCompilationData.PackageList.Clear; 
  cbPattern.Items.SaveToFile('patterns.txt');
end;

procedure TSelectFoldersPage.FormCreate(Sender: TObject);
begin
  inherited;
  TranslateComponent(self);

  edtBaseFolder.Text := FCompilationData.BaseFolder;
  cbPattern.Text := FCompilationData.Pattern;

  if (FileExists('patterns.txt')) then
    cbPattern.Items.LoadFromFile('patterns.txt');
end;

end.
