{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit MonitoredPackageCompiler;

interface
uses progressmonitor, packagecompiler, packageinfo, CompilationData;

type
  TPackageCompileEvent = procedure(const package: TPackageInfo; status: TPackageStatus) of object;
  TMonitoredPackageCompiler = class(TPackageCompiler)
  private
    fMonitor: IProgressMonitor;
  protected
    procedure RaiseEvent(const packageInfo: TPackageInfo; status: TPackageStatus);virtual;
    procedure PrepareExtraOptions; override;
    
  public
    constructor Create(const compilationData: TCompilationData; const aMonitor: IProgressMonitor); reintroduce;
    procedure ResolveSourcePaths; override;
    procedure CompilerOutputCallback(const line:string); virtual;
    function CompilePackage(const packageInfo: TPackageInfo): boolean; override;
    function InstallPackage(const packageInfo: TPackageInfo): boolean; override;
    procedure Compile; override;
  end;

implementation
uses Classes;
constructor TMonitoredPackageCompiler.Create(const compilationData:TCompilationData; const aMonitor: IProgressMonitor);
begin
  Assert(aMonitor <> nil,'monitor cannot be null'); { TODO -oidursun -c : a default null implementation can be set }
  inherited Create(compilationData);
  fMonitor := aMonitor;
end;

procedure TMonitoredPackageCompiler.Compile;
begin
  Installation.DCC32.OutputCallback := CompilerOutputCallback;
  fMonitor.Started;
  fMonitor.Log('-=Started');
  inherited;
  fMonitor.Finished;
  fMonitor.Log('-=Finished');
end;

function TMonitoredPackageCompiler.CompilePackage(
  const packageInfo: TPackageInfo): boolean;
begin
  fMonitor.Log('-=Compiling: ' + packageInfo.PackageName);
  fMonitor.Log('Required :'+packageInfo.RequiredPackageList.DelimitedText);
  fMonitor.Log('Contains :'+packageInfo.ContainedFileList.DelimitedText);

  RaiseEvent(packageInfo, psCompiling);
  Result := inherited CompilePackage(packageInfo);
  if not Result then
    RaiseEvent(packageInfo, psError)
  else
    RaiseEvent(packageInfo, psSuccess);  
  fMonitor.Log('Finished');
end;

procedure TMonitoredPackageCompiler.CompilerOutputCallback(const line: string);
begin
  fMonitor.CompilerOutput(line);
end;

function TMonitoredPackageCompiler.InstallPackage(
  const packageInfo: TPackageInfo): boolean;
begin
   Result := inherited InstallPackage(packageInfo);
end;

procedure TMonitoredPackageCompiler.PrepareExtraOptions;
var
  line: string;
begin
  inherited;
  fMonitor.Log('-=All Path:');
  for line in AllPaths do
    fMonitor.Log(line);

  fMonitor.Log('-=Compiler Options:');
  fMonitor.Log(fExtraOptions);
end;

procedure TMonitoredPackageCompiler.RaiseEvent(const packageInfo: TPackageInfo;
  status: TPackageStatus);
begin
  fMonitor.PackageProcessed(packageInfo, status);
  if (status = psSuccess) or (status = psError) then
    packageInfo.Status := status;
end;

procedure TMonitoredPackageCompiler.ResolveSourcePaths;
var
  path: string;
begin
  inherited;
  fMonitor.Log('-=Source Paths:');
  for path in SourceFilePaths do
    fMonitor.Log(path);
end;

end.
