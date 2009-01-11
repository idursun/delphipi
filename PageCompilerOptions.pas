{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageCompilerOptions;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ExtCtrls, WizardIntfs;

type
  TSelectCompilerOptions = class(TWizardPage)
    grpOutputFolders: TGroupBox;
    lblBPLOutputFolder: TLabel;
    edtBPL: TEdit;
    btnBPLBrowse: TButton;
    lblDCP: TLabel;
    edtDCP: TEdit;
    btnDCPBrowse: TButton;
    lblDCU: TLabel;
    edtDCU: TEdit;
    btnDCUBrowse: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnFolderBrowseClick(Sender: TObject);
  private
    procedure SaveInstallationOutputFolders;
    procedure ShowInstallationOutputFolders;
    procedure BrowseFolder(var folder: string);
  public
    constructor Create(Owner: TComponent; const compilationData: TCompilationData); override; 
    procedure UpdateWizardState; override;
    function CanShowPage: Boolean; override;
  end;

var
  SelectCompilerOptions: TSelectCompilerOptions;

implementation
uses gnugettext, FileCtrl;
{$R *.dfm}

{ TSelectDelphiInstallationPage }

constructor TSelectCompilerOptions.Create(Owner: TComponent;
  const compilationData: TCompilationData);
begin
  inherited;
  FCompilationData := compilationData;
end;

procedure TSelectCompilerOptions.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  SaveInstallationOutputFolders;
end;

procedure TSelectCompilerOptions.FormCreate(Sender: TObject);
begin
  inherited;
  TranslateComponent(self);
  ShowInstallationOutputFolders;
end;

procedure TSelectCompilerOptions.BrowseFolder(var folder:string);
var
  directory: string;
begin
  directory := '';
  if SelectDirectory(_('Select output folder'),'',directory) then begin
     folder := directory;
  end;
end;

procedure TSelectCompilerOptions.btnFolderBrowseClick(Sender: TObject);
var
  path: string;
  editBox: TEdit;
begin
  inherited;
  editBox := TEdit(sender);
  path := editBox.Text;
  BrowseFolder(path);
  editBox.Text := path;
end;

function TSelectCompilerOptions.CanShowPage: Boolean;
begin
   Result := True;
  //Result := FCompilationData.Scripting;
end;

procedure TSelectCompilerOptions.SaveInstallationOutputFolders;
begin
  Assert(Assigned(fCompilationData), 'Compilation data is null');
  fCompilationData.BPLOutputFolder := edtBPL.Text;
  fCompilationData.DCPOutputFolder := edtDCP.Text;
  fCompilationData.DCUOutputFolder := edtDCU.Text;
end;

procedure TSelectCompilerOptions.ShowInstallationOutputFolders;
begin
  Assert(Assigned(fCompilationData), 'Compilation data is null');
  edtBPL.Text:= fCompilationData.BPLOutputFolder;
  edtDCP.Text:= fCompilationData.DCPOutputFolder;
  edtDCU.Text:= fCompilationData.DCUOutputFolder;
end;

procedure TSelectCompilerOptions.UpdateWizardState;
begin
  inherited;
  wizard.SetHeader(_('Select Output Folders and Compiler Conditionals'));
  wizard.SetDescription(_('Please select output folders and compiler conditionals that will affect the compilation'));
end;

end.
