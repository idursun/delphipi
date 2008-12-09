{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit ConsoleProgressMonitor;

interface
uses PackageInfo, ProgressMonitor;
type
  TConsoleOutputLevel = (colSilent, colBrief, colFull);
  TConsoleProgressMonitor = class(TInterfacedObject, IProgressMonitor)
  private
    fOutputLevel: TConsoleOutputLevel;
    fStartTime: TDateTime;
  published
  public
    procedure CompilerOutput(const line: string);
    procedure Finished;
    procedure Log(const text: string);
    procedure PackageProcessed(const packageInfo: TPackageInfo;
      status: TPackageStatus);
    procedure Started;
    property OutputLevel: TConsoleOutputLevel read fOutputLevel write fOutputLevel;
  end;

implementation
uses DateUtils, SysUtils;

{ TConsoleProgressMonitor }

procedure TConsoleProgressMonitor.CompilerOutput(const line: string);
begin
  if OutputLevel = colSilent then exit;
  if OutputLevel = TConsoleOutputLevel.colFull then
    WriteLn(line)
  else if Pos('Fatal:', line) > 0 then
    WriteLn(line);
end;

procedure TConsoleProgressMonitor.Finished;
begin
  WriteLn('Completed in ' + floattostr(MilliSecondsBetween(GetTime, fStartTime)) + ' ms');
end;

procedure TConsoleProgressMonitor.Log(const text: string);
begin
  if OutputLevel = TConsoleOutputLevel.colFull then
    WriteLn(text);
end;

procedure TConsoleProgressMonitor.PackageProcessed(
  const packageInfo: TPackageInfo; status: TPackageStatus);
begin
  if OutputLevel = TConsoleOutputLevel.colSilent then
    exit;
    
  case status of
    psNone: ;
    psCompiling: WriteLn(packageInfo.PackageName + ' : Compiling');
    psInstalling: WriteLn(packageInfo.PackageName + ' : Installing');
    psSuccess: begin
      WriteLn(packageInfo.PackageName + ' is successfuly compiled'); WriteLn;
    end;
    psError: begin
      WriteLn(packageInfo.PackageName + ' is errored');
      WriteLn;
    end;
  end;
end;

procedure TConsoleProgressMonitor.Started;
begin
  WriteLn('Starting');
  fStartTime := GetTime;
end;


end.
