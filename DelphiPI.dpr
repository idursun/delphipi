program DelphiPI;

uses
  Forms,
  PackageInfo in 'PackageInfo.pas',
  FormAbout in 'FormAbout.pas' {frmAbout},
  PackageCompiler in 'PackageCompiler.pas',
  CompileInfo in 'CompileInfo.pas',
  FormWizard in 'FormWizard.pas' {frmWizard},
  PageBase in 'PageBase.pas' {WizardPage},
  PageSelectFolders in 'PageSelectFolders.pas' {SelectFoldersPage},
  PageSelectDelphiInstallation in 'PageSelectDelphiInstallation.pas' {SelectDelphiInstallationPage},
  PageProgress in 'PageProgress.pas' {ProgressPage},
  WizardIntfs in 'WizardIntfs.pas',
  PageShowPackageList in 'PageShowPackageList.pas' {ShowPackageListPage},
  WizardData in 'WizardData.pas',
  PageInstallHelpFiles in 'PageInstallHelpFiles.pas' {InstallHelpFilesPage},
  PageFinished in 'PageFinished.pas' {FinishedPage};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi PI';
  Application.CreateForm(TfrmWizard, frmWizard);
  Application.Run;
end.
