{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit ConsoleRunner;

interface
type

  TConsoleRunner = class
  private
  protected
  public
    constructor Create(const params:array of string);
  end;
  
implementation
uses CompilationData, ScriptPersister;
{ TConsoleRunner }

constructor TConsoleRunner.Create(const params:array of string);
var
  compilationData: TCompilationData;
begin
 // compilationData:= TScriptPersister.Load(params[1]);
end;

end.
