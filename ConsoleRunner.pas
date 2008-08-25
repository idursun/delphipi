{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit ConsoleRunner;

interface
uses PackageInfo, gnugettext;
type

  TConsoleRunner = class
  private
  protected
    procedure PackageEventHandler(const packageInfo: TPackageInfo; status:TPackageStatus);
  public
    constructor Create(const params:array of string);
  end;
  
implementation

uses CompilationData, ScriptPersister, PackageCompiler;
{ TConsoleRunner }

constructor TConsoleRunner.Create(const params:array of string);
var
  compilationData: TCompilationData;
  scripter : TScriptPersister;
  packageCompiler: TPackageCompiler;
begin
  scripter := TScriptPersister.Create;
  try
    compilationData := scripter.Load(params[1]);
  finally
    scripter.Free;
  end;

  packageCompiler := TPackageCompiler.Create(compilationData);
  try
    packageCompiler.OnPackageEvent := PackageEventHandler;
    packageCompiler.Compile;
  finally
    packageCompiler.Free;
  end;
end;

procedure TConsoleRunner.PackageEventHandler(
  const packageInfo: TPackageInfo; status: TPackageStatus);
begin
   if status = psCompiling then
      WriteLn(_('Compiling:') + ' '+ packageInfo.PackageName);
   if status = psSuccess then
      WriteLn(_('Compiled :') + ' '+ packageInfo.PackageName);
end;

end.
