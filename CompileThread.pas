unit CompileThread;
interface
uses PackageInfo, PackageCompiler, Classes, JclBorlandTools, CompilationData;

type
 IProgressMonitor = interface
    ['{04616FA5-F839-4ADC-A4FF-2DDAAC02E597}']
    function GetStepNo: Integer;
    procedure SetStepNo(const value: Integer);
    function GetPackageName: String;
    procedure SetPackageName(const value : string);
    procedure PackageCompiled(const packageInfo: TPackageInfo; status: TPackageStatus);
    property StepNo: Integer read GetStepNo write SetStepNo;
    property PackageName: String read GetPackageName write SetPackageName;
 end;
  
  TCompileThread = class(TThread)
  private
    fStepNo, fSucceededs, fErroreds: Integer;
    fPackageName: String;
    fPackageInfo : TPackageInfo;
    fStatus : TPackageStatus;
    fMonitor : IProgressMonitor;
    fCompilationData: TCompilationData;
    fCancel: boolean;
    fCompiler: TPackageCompiler;
    procedure SetMonitor(const Value: IProgressMonitor);
    procedure SetCancel(const Value: boolean);
  protected
    procedure Execute; override;
    procedure UpdateMonitor;
    procedure PackageEventHandler(const package: TPackageInfo; status: TPackageStatus);
  public
    constructor Create(const compilationData: TCompilationData);
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
  fCompiler := TPackageCompiler.Create(fCompilationData);
  try
    fCompiler.OnPackageEvent := self.PackageEventHandler;
    fCompiler.Compile;
  finally
    fCompiler.Free;
  end;
end;

procedure TCompileThread.PackageEventHandler(const package: TPackageInfo;
  status: TPackageStatus);
begin
   //TODO: reorganize code, there is dublicate info
   if status = psCompiling then
     fPackageName := package.PackageName;
   if (status = psSuccess) or (status = psError) then
     fStepNo := fStepNo + 1;

   fStatus := status;
   fPackageInfo := package;

   if status = psError then inc(fErroreds);
   if status = psSuccess then inc(fSucceededs);

   Synchronize(UpdateMonitor);
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

procedure TCompileThread.UpdateMonitor;
begin
   if fMonitor <> nil then
   begin
     fMonitor.StepNo := fStepNo;
     fMonitor.PackageName := fPackageName;
     fMonitor.PackageCompiled(fPackageInfo, fStatus);
   end;
end;
end.
