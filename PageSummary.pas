{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit PageSummary;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls;

type
  TSummaryPage = class(TWizardPage)
    Label1: TLabel;
    btnSave: TButton;
    edtSummary: TMemo;
    Label2: TLabel;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function Indented(const line: string):string;
    procedure AddBaseFolder(const summary: TStringList);
    procedure AddDelphiVersion(const summary: TStringList);
    procedure AddPackageList(const summary: TStringList);
    procedure AddSourcePathList(const summary: TStringList);
    procedure AddHelpFileList(const summary: TStringList);
  public
    procedure UpdateWizardState; override;
    
  end;

var
  SummaryPage: TSummaryPage;

implementation
uses ScriptPersister,WizardIntfs, PackageInfo, gnugettext;
{$R *.dfm}

{ TFinishedPage }


procedure TSummaryPage.AddBaseFolder(const summary: TStringList);
begin
   summary.Add(_('Base Folder:'));
   summary.Add(Indented(fCompilationData.BaseFolder));
end;

procedure TSummaryPage.AddDelphiVersion(const summary: TStringList);
begin
  summary.Add(_('Delphi Version:'));
  summary.Add(Indented(fCompilationData.Installation.IDEVersionNumberStr));
end;

procedure TSummaryPage.AddHelpFileList(const summary: TStringList);
var
  helpfile: string;
begin
 summary.Add(_('Help Files:'));
 for helpfile in fCompilationData.HelpFiles do
    summary.Add(Indented(helpfile));
end;

procedure TSummaryPage.AddPackageList(const summary: TStringList);
var
  I: Integer;
  erroredPackages: TStringList;
  packageName: String;
begin
  summary.Add(_('Package List:'));
  erroredPackages := TStringList.Create;
  try
    for I := 0 to fCompilationData.PackageList.Count - 1 do
    with fCompilationData.PackageList[i] do
    begin
      summary.Add(Indented(FileName));
      if Status = psError then
        erroredPackages.Add(FileName);
    end;

    if erroredPackages.Count > 0 then
    begin
      summary.Add(_('However, following packages had some errors:'));
      for packageName in erroredPackages do
        summary.Add(Indented(packageName));
    end;
  finally
    erroredPackages.Free;
  end;
end;

procedure TSummaryPage.AddSourcePathList(const summary: TStringList);
//var
//  path: string;
begin
// summary.Add(_('Source File Paths:'));
// for path in fCompilationData.SourceFilePaths do
//    summary.Add(Indented(path));
end;

procedure TSummaryPage.btnSaveClick(Sender: TObject);
var
  dialog: TSaveDialog;
  scripter: TScriptPersister;
begin
  dialog := TSaveDialog.Create(self);
  try
    if dialog.Execute then begin
      scripter := TScriptPersister.Create;
      try
        scripter.Save(fCompilationData, dialog.FileName);
      finally
        scripter.Free;
      end;
    end;
  finally
    dialog.Free;
  end;
end;

procedure TSummaryPage.FormCreate(Sender: TObject);
var
  summary: TStringList;
begin
  inherited;
  if fCompilationData = nil then exit;
  summary := TStringList.Create;
  try
    AddBaseFolder(summary);
    AddDelphiVersion(summary);
    AddPackageList(summary);
    AddSourcePathList(summary);
    AddHelpFileList(summary);
  finally
    edtSummary.Lines.Assign(summary);
    summary.Free;
  end;
end;

function TSummaryPage.Indented(const line: string): string;
begin
  Result := '  ' + line;
end;

procedure TSummaryPage.UpdateWizardState;
begin
  inherited;
  fWizardController.SetHeader(_('Finished'));
  fWizardController.SetDescription(_('Installation Summary'));
  with fWizardController.GetAction(wbtNext) do
    Caption := _('&Exit');

  with fWizardController.GetAction(wbtBack) do
    Visible := false;
end;

end.
