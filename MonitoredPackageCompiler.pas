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
  TLoggedPackageCompiler = class(TPackageCompiler)
  private
    fProgressMonitor: IProgressMonitor;
  protected
    procedure RaiseEvent(const packageInfo: TPackageInfo; status: TPackageStatus);virtual;
  public
    constructor Create(const fCompilationData: TCompilationData);override;
    procedure ResolveSourcePaths; override;
    procedure CompilerOutputCallback(const line:string); virtual;
    function CompilePackage(const packageInfo: TPackageInfo): boolean; override;
    function InstallPackage(const packageInfo: TPackageInfo): boolean; override;
    procedure Compile; override;
    property Monitor: IProgressMonitor read fProgressMonitor write fProgressMonitor;
  end;

implementation
 
constructor TLoggedPackageCompiler.Create(const fCompilationData:TCompilationData);
begin
  inherited Create(fCompilationData);
  Installation.DCC32.OutputCallback := self.CompilerOutputCallback;
end;

procedure TLoggedPackageCompiler.Compile;
begin
  Monitor.Started;
  inherited;
  Monitor.Finished;
end;

function TLoggedPackageCompiler.CompilePackage(
  const packageInfo: TPackageInfo): boolean;
begin
  RaiseEvent(packageInfo, psCompiling);
  Result := inherited CompilePackage(packageInfo);
  if not Result then
    RaiseEvent(packageInfo, psError)
  else
    RaiseEvent(packageInfo, psSuccess);  
end;

procedure TLoggedPackageCompiler.CompilerOutputCallback(const line: string);
begin
  Monitor.CompilerOutput(line);
end;

function TLoggedPackageCompiler.InstallPackage(
  const packageInfo: TPackageInfo): boolean;
begin
   Result := inherited InstallPackage(packageInfo);
end;

procedure TLoggedPackageCompiler.RaiseEvent(const packageInfo: TPackageInfo;
  status: TPackageStatus);
begin
  if Assigned(Monitor) then
  begin
   Monitor.PackageProcessed(packageInfo, status);
   if (status = psSuccess) or (status = psError) then
      packageInfo.Status := status;
  end;
end;

procedure TLoggedPackageCompiler.ResolveSourcePaths;
begin
  inherited;
end;

end.
