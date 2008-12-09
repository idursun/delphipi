program DelphiPIConsole;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  CompilationData in 'CompilationData.pas',
  CompileInfo in 'CompileInfo.pas',
  CompileThread in 'CompileThread.pas',
  ConsoleRunner in 'ConsoleRunner.pas',
  MonitoredPackageCompiler in 'MonitoredPackageCompiler.pas',
  PackageCompiler in 'PackageCompiler.pas',
  PackageDependencyVerifier in 'PackageDependencyVerifier.pas',
  PackageInfo in 'PackageInfo.pas',
  PackageInfoFactory in 'PackageInfoFactory.pas',
  PackageList in 'PackageList.pas',
  ProgressMonitor in 'ProgressMonitor.pas',
  ScriptPersister in 'ScriptPersister.pas',
  Utils in 'Utils.pas',
  gnugettext in 'libs\gnugettext.pas',
  ConsoleProgressMonitor in 'ConsoleProgressMonitor.pas';

var
  consoleRunner : TConsoleRunner;  
begin
  try
    consoleRunner := TConsoleRunner.Create;
    try
      consoleRunner.Run;
      Readln;
    finally
      consoleRunner.Free;
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.

