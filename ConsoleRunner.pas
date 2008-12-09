{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit ConsoleRunner;

interface
uses PackageInfo, CompilationData, ConsoleProgressMonitor;
type

  TConsoleRunner = class
  private
    fCompilationData: TCompilationData;
    fMonitor: TConsoleProgressMonitor;
  protected
    procedure DisplayHeader; virtual;
  public
    constructor Create;
    procedure Run;
  end;
  
implementation

uses Classes,gnugettext, MonitoredPackageCompiler, ScriptPersister, PackageCompiler, Utils;

type
  TConsoleParameterParser = class
  private
    fValues : TStringList;
    fOutputLevel: TConsoleOutputLevel;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse;

    property OutputLevel: TConsoleOutputLevel read fOutputLevel;
  end;

{ TConsoleRunner }

constructor TConsoleRunner.Create;
var
  scripter : TScriptPersister;
  parser : TConsoleParameterParser;
begin
   scripter := TScriptPersister.Create;
   parser := TConsoleParameterParser.Create;
   parser.Parse;  
  try
    fCompilationData := scripter.Load(parser.fValues[0]);
    fMonitor := TConsoleProgressMonitor.Create;
    fMonitor.OutputLevel := parser.OutputLevel;
    
  finally
    scripter.Free;
    parser.Free;
  end;
end;

procedure TConsoleRunner.Run;
var
   packageCompiler: TMonitoredPackageCompiler;
begin
  DisplayHeader;
  
  packageCompiler := TMonitoredPackageCompiler.Create(fCompilationData);
  packageCompiler.Monitor := fMonitor;
  try
    packageCompiler.Compile;
  finally
    packageCompiler.Free;
  end;
end;

procedure TConsoleRunner.DisplayHeader;
begin
  WriteLn('DelphiPI Console ' + Utils.VERSION);
  WriteLn('By ' + Utils.AUTHOR);
end;

{ TConsoleParameterParser }

constructor TConsoleParameterParser.Create;
begin
  fValues := TStringList.Create;
end;

destructor TConsoleParameterParser.Destroy;
begin
  fValues.Free;
  inherited;
end;

procedure TConsoleParameterParser.Parse;
var
  I: Integer;
begin
  fValues.Clear;
  fOutputLevel := TConsoleOutputLevel.colBrief;
  for I := 1 to ParamCount do
    fValues.Add(ParamStr(i));
end;

end.
