unit CompileThread;
interface
uses WizardData, PackageInfo, PackageCompiler, Classes, JclBorlandTools;

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
    fPackageList: TPackageList;
    fInstallation: TJclBorRADToolInstallation;
    procedure SetMonitor(const Value: IProgressMonitor);
  protected
    procedure Execute; override;
    procedure UpdateMonitor;
  public
    constructor Create(const packageList : TPackageList; const installation: TJclBorRADToolInstallation);
    property Monitor: IProgressMonitor read FMonitor write SetMonitor;
  end;

implementation

constructor TCompileThread.Create(const packageList : TPackageList; const installation: TJclBorRADToolInstallation);
begin
  inherited Create(true);
  Assert(packageList <> nil);
  Assert(installation <> nil);

  fPackageList := packageList;
  fInstallation := installation;
end;

procedure TCompileThread.Execute;
var
  info : TPackageInfo;
  compiler: TPackageCompiler;
  sourceList: TStringList;
  i : integer;
  compileSuccessful : Boolean;
begin
  inherited;
  sourceList := TStringList.Create;
  compiler := TPackageCompiler.Create(fInstallation);
  try
    fPackageList.GetSourcePaths(sourceList);
    compiler.AddSourcePathsToIDE(sourceList);
    for i := 0 to fPackageList.Count - 1 do begin
      info := fPackageList[i];
      fStepNo := fStepNo + 1;
      fPackageName := info.PackageName;
      Synchronize(UpdateMonitor);
      compileSuccessful := compiler.CompilePackage(info);
      if compileSuccessful and (not info.RunOnly) then
        compiler.InstallPackage(info);
        
      if not compileSuccessful then
      begin
         // count & report errors
      end; 
    end;
  finally
    compiler.Free;
    sourceList.Free;
  end;
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
