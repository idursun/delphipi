program DelphiPITests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestPackageList in 'TestPackageList.pas',
  PackageInfo in '..\PackageInfo.pas',
  PackageList in '..\PackageList.pas',
  TestPackageInfo in 'TestPackageInfo.pas',
  TestPackageDependencyVerifier in 'TestPackageDependencyVerifier.pas',
  PackageDependencyVerifier in '..\PackageDependencyVerifier.pas',
  CompilationData in '..\CompilationData.pas',
  gnugettext in '..\libs\gnugettext.pas',
  InstalledPackageResolver in '..\InstalledPackageResolver.pas',
  TestTreeModel in 'TestTreeModel.pas',
  TreeModel in '..\TreeModel.pas',
  TreeViewModel in '..\TreeViewModel.pas',
  DelphiVersionTreeViewModel in '..\DelphiVersionTreeViewModel.pas',
  TestDelphiVersionTreeViewModel in 'TestDelphiVersionTreeViewModel.pas',
  Utils in '..\Utils.pas',
  TestDelphiVersionGuess in 'TestDelphiVersionGuess.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

