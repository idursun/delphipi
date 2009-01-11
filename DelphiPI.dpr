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
  PageProgress in 'PageProgress.pas' {ProgressPage},
  WizardIntfs in 'WizardIntfs.pas',
  PageShowPackageList in 'PageShowPackageList.pas' {ShowPackageListPage},
  PageInstallHelpFiles in 'PageInstallHelpFiles.pas' {InstallHelpFilesPage},
  PageSummary in 'PageSummary.pas' {SummaryPage},
  CompileThread in 'CompileThread.pas',
  CompilationData in 'CompilationData.pas',
  ScriptPersister in 'ScriptPersister.pas',
  gnugettext in 'libs\gnugettext.pas',
  Utils in 'Utils.pas',
  PackageInfoFactory in 'PackageInfoFactory.pas',
  PackageList in 'PackageList.pas',
  PackageDependencyVerifier in 'PackageDependencyVerifier.pas',
  ProgressMonitor in 'ProgressMonitor.pas',
  MonitoredPackageCompiler in 'MonitoredPackageCompiler.pas',
  PageCompilerOptions in 'PageCompilerOptions.pas' {SelectCompilerOptions};

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi Package Installer';
  Application.CreateForm(TfrmWizard, frmWizard);
  Application.Run;
end.
