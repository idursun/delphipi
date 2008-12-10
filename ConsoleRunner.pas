{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit ConsoleRunner;

interface
uses Classes, PackageInfo, CompilationData, ConsoleProgressMonitor;
type

  TConsoleRunner = class
  private
    fCompilationData: TCompilationData;
    fMonitor: TConsoleProgressMonitor;
    fParameters: TStringList;
    fOutputLevel: TConsoleOutputLevel;
    procedure DisplayHelp;
  protected
    procedure DisplayHeader; virtual;
    procedure ParseArguments;virtual;
  public
    constructor Create;
    procedure Run;
    destructor Destroy; override;
    property OutputLevel: TConsoleOutputLevel read fOutputLevel;
  end;
  
implementation

uses gnugettext, MonitoredPackageCompiler, ScriptPersister, PackageCompiler, Utils, JclConsole;

procedure WriteLine(color: TJclScreenFontColor; text: string);
begin
  TJclConsole.Default.Screens[0].Writeln(text, TJclScreenTextAttribute.Create(color));  
end;

{ TConsoleRunner }
constructor TConsoleRunner.Create;
begin
  fParameters := TStringList.Create;
end;

procedure TConsoleRunner.Run;
var
  scripter : TScriptPersister;
  packageCompiler: TMonitoredPackageCompiler;
begin
  DisplayHeader;
  ParseArguments;
  if fParameters.Count = 0 then
  begin
    DisplayHelp;
    exit;
  end;
  
  scripter := TScriptPersister.Create;
  try
    fCompilationData := scripter.Load(fParameters[0]);
    fMonitor := TConsoleProgressMonitor.Create;
    fMonitor.OutputLevel := Self.OutputLevel;
  finally
    scripter.Free;
  end;
  
  packageCompiler := TMonitoredPackageCompiler.Create(fCompilationData);
  packageCompiler.Monitor := fMonitor;
  try
    packageCompiler.Compile;
  finally
    packageCompiler.Free;
  end;
end;

destructor TConsoleRunner.Destroy;
begin
  fParameters.Free;
  inherited;
end;

procedure TConsoleRunner.DisplayHeader;
begin
  WriteLn('DelphiPI Console ' + Utils.VERSION);
  WriteLn('By ' + Utils.AUTHOR);
end;

procedure TConsoleRunner.DisplayHelp;
begin
  WriteLn('Usage:');
  WriteLn('DelphiPIConsole.exe [ScriptFile] [OutputLevel]');
  WriteLn('ScriptFile : script file saved by the DelphiPI which contains necessary information about compilation');
  WriteLn('OutputLevel: during compilation how much info should be displayed');
  WriteLn(#9'0 means silent');
  WriteLn(#9'1 means brief');
  WriteLn(#9'2 means full');
end;


procedure TConsoleRunner.ParseArguments;
var
  i:integer;
begin
  fParameters.Clear;
  fOutputLevel := TConsoleOutputLevel.colBrief;
  for I := 1 to ParamCount do
    fParameters.Add(ParamStr(i));
end;

end.
