{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
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
    fOutputLevel: TConsoleOutputLevel;
    fScriptFile: String;
    procedure DisplayHelp;
  protected
    procedure DisplayHeader; virtual;
    procedure ParseArguments;virtual;
  public
    procedure Run;
    property OutputLevel: TConsoleOutputLevel read fOutputLevel;
  end;

implementation

uses SysUtils,  gnugettext, MonitoredPackageCompiler, ScriptPersister, PackageCompiler,
     Utils, JclConsole, StrUtils;

procedure WriteLine(color: TJclScreenFontColor; text: string);
begin
  TJclConsole.Default.Screens[0].Writeln(text, TJclScreenTextAttribute.Create(color));
end;

{ TConsoleRunner }

//TODO Refactor this
procedure TConsoleRunner.Run;
var
  scripter : TScriptPersister;
  packageCompiler: TMonitoredPackageCompiler;
begin
  DisplayHeader;
  try
    ParseArguments;
  except
    on ex : Exception do begin
       WriteLine(fclRed, ex.Message);
       DisplayHelp;
       exit;
    end;
  end;

  scripter := TScriptPersister.Create;
  try
    fCompilationData := scripter.Load(fScriptFile);
    fMonitor := TConsoleProgressMonitor.Create;
    fMonitor.OutputLevel := Self.OutputLevel;
  finally
    scripter.Free;
  end;
  
  packageCompiler := TMonitoredPackageCompiler.Create(fCompilationData, fMonitor);
  try
    packageCompiler.Compile;
  finally
    packageCompiler.Free;
  end;
end;

procedure TConsoleRunner.DisplayHeader;
begin
  WriteLine(fclWhite, 'DelphiPI Console ' + Utils.VERSION);
  WriteLine(fclWhite, 'By ' + Utils.AUTHOR);
end;

procedure TConsoleRunner.DisplayHelp;
begin
  WriteLine(fclWhite, 'Usage:');
  WriteLine(fclWhite, 'DelphiPIConsole.exe [ScriptFile] [OutputLevel]');
  WriteLine(fclWhite, 'ScriptFile : script file saved by the DelphiPI which contains necessary information about compilation');
  WriteLine(fclWhite, 'OutputLevel: during compilation how much info should be displayed');
  WriteLine(fclWhite, #9'0 means silent');
  WriteLine(fclWhite, #9'1 means brief');
  WriteLine(fclWhite, #9'2 means full');
end;


procedure TConsoleRunner.ParseArguments;
var
  i:integer;
  param: string;
begin
  fOutputLevel := TConsoleOutputLevel.colBrief;

  if ParamCount = 0 then
    raise Exception.Create('Requires Script File.');

  fScriptFile := ParamStr(1);

  if not FileExists(fScriptFile) then
     raise Exception.Create('File does not exist: ' + fScriptFile);

  i := 2;

  while i < ParamCount do
  begin
    param := UpperCase(ParamStr(i));
    // Logging level
//    if param = '-L' then
//    begin
//       i := i+1;
//       param :=  UpperCase(ParamStr(i));
       if (param = '0') or (param = 'SILENT') then
          fOutputLevel := TConsoleOutputLevel.colSilent
       else if (param='1') or (param = 'BRIEF') then
          fOutputLevel := TConsoleOutputLevel.colBrief
       else if (param='2') or (param = 'FULL') then
          fOutputLevel := TConsoleOutputLevel.colFull
       else
         raise Exception.Create(param + ' is not a valid value for OutputLogLevel');
    end;
//
//  end;
end;

end.
