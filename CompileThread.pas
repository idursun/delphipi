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
    property StepNo: Integer read GetStepNo write SetStepNo;
    property PackageName: String read GetPackageName write SetPackageName;
  end;
  
  TCompileThread = class(TThread)
  private
    fStepNo: Integer;
    fPackageName: String;
    fMonitor : IProgressMonitor;
    fCompilationData: TCompilationData;
    procedure SetMonitor(const Value: IProgressMonitor);
  protected
    procedure Execute; override;
    procedure UpdateMonitor;
    procedure PackageEventHandler(const package: TPackageInfo; status: TPackageStatus);
  public
    constructor Create(const compilationData: TCompilationData);
    property Monitor: IProgressMonitor read FMonitor write SetMonitor;
  end;

implementation


constructor TCompileThread.Create(const compilationData: TCompilationData);
begin
  inherited Create(true);
  Assert(compilationData <> nil);
  fCompilationData := compilationData;
end;

procedure TCompileThread.Execute;
var
  compiler: TPackageCompiler;
begin
  inherited;
  compiler := TPackageCompiler.Create(fCompilationData);
  try
    compiler.OnPackageEvent := self.PackageEventHandler;
    compiler.Compile;
  finally
    compiler.Free;
  end;
end;

procedure TCompileThread.PackageEventHandler(const package: TPackageInfo;
  status: TPackageStatus);
begin
   if status = psCompiling then
     fPackageName := package.PackageName;

   if (status = psSuccess) or (status = psError) then
     fStepNo := fStepNo + 1;

   Synchronize(UpdateMonitor);
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
   end;
end;
end.
