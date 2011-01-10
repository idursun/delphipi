unit CompileThread;
interface
uses ProgressMonitor, PackageInfo, MonitoredPackageCompiler, Classes, JclIDEUtils, CompilationData;

type
  TCompileThread = class(TThread, IProgressMonitor)
  private
    FRefCount : integer;
    fMonitor : IProgressMonitor;
    fCompilationData: TCompilationData;
    fCancel: boolean;
    fCompiler: TMonitoredPackageCompiler;
    procedure SetMonitor(const Value: IProgressMonitor);
    procedure SetCancel(const Value: boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(const compilationData: TCompilationData);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    procedure CompilerOutput(const line: string);
    procedure Finished;
    procedure Log(const text: string);
    procedure PackageProcessed(const packageInfo: TPackageInfo;
      status: TPackageStatus);
    procedure Started;

    property Monitor: IProgressMonitor read FMonitor write SetMonitor;
    property Cancel: boolean read fCancel write SetCancel;
  end;

implementation

constructor TCompileThread.Create(const compilationData: TCompilationData);
begin
  inherited Create(true);
  Assert(compilationData <> nil);
  fCompilationData := compilationData;
end;

procedure TCompileThread.Execute;
begin
  inherited;
  fCompiler := TMonitoredPackageCompiler.Create(fCompilationData, Self);
  try
    fCompiler.Compile;
  finally
    fCompiler.Free;
  end;
end;

procedure TCompileThread.Log(const text: string);
begin
  Synchronize(procedure
  begin
    Monitor.Log(text);
  end);
end; 
 
procedure TCompileThread.CompilerOutput(const line: string);
begin
  Synchronize(procedure
  begin
    Monitor.CompilerOutput(line);
  end);
end;

procedure TCompileThread.Finished;
begin
  Synchronize(procedure
  begin
    Monitor.Finished;
  end);
end;

procedure TCompileThread.PackageProcessed(const packageInfo: TPackageInfo;
  status: TPackageStatus);
begin
  Synchronize(procedure
  begin
    Monitor.PackageProcessed(packageInfo, status);
  end);
end;

procedure TCompileThread.Started;
begin
  Synchronize(procedure
  begin
    Monitor.Started;
  end);
end;

procedure TCompileThread.SetCancel(const Value: boolean);
begin
  fCancel := Value;
  if fCompiler <> nil then
    fCompiler.Cancel := true;
end;

procedure TCompileThread.SetMonitor(const Value: IProgressMonitor);
begin
  FMonitor := Value;
end;

function TCompileThread.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCompileThread._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TCompileThread._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if Result = 0 then
    Destroy;
end;

end.
