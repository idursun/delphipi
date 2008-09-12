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
  PageInstallHelpFiles in 'PageInstallHelpFiles.pas' {InstallHelpFilesPage},
  PageSummary in 'PageSummary.pas' {SummaryPage},
  ConsoleRunner in 'ConsoleRunner.pas',
  CompileThread in 'CompileThread.pas',
  CompilationData in 'CompilationData.pas',
  ScriptPersister in 'ScriptPersister.pas',
  RegExpr in 'libs\RegExpr.pas',
  gnugettext in 'libs\gnugettext.pas';

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi Package Installer';
  Application.CreateForm(TfrmWizard, frmWizard);
  Application.Run;
end.
