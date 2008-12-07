unit TestPackageDependencyVerifier;
interface

uses
  TestFramework, CompilationData, PackageList,  Classes, 
        PackageDependencyVerifier, PackageInfo;

type
  // Test methods for class TPackageDependencyVerifier
  TestTPackageDependencyVerifier = class(TTestCase)
  strict private
    SUT: TPackageDependencyVerifier;
    fCompilationData: TCompilationData;
  private
  protected
  published
    procedure When_a_required_package_is_missing_then_packages_depending_on_it_should_be_set_as_missing;
    procedure When_a_package_cannot_be_compiled_then_packages_depending_on_it_should_be_marked_as_missing;
  end;

implementation
uses Generics.Collections;
type
  TStringArr = array of string;
  
  TFakeCompilationData = class(TCompilationData)
  private
    procedure Dispose;
  public
    procedure GetIdePackages(const list: TStringList); override;
    function CreatePackage(name: string; requireds: string ): TPackageInfo;
    destructor Destroy; override;
    function GetIdeVersionSuffix: string; override;
  end;

procedure TestTPackageDependencyVerifier.When_a_required_package_is_missing_then_packages_depending_on_it_should_be_set_as_missing;
begin
  fCompilationData := TFakeCompilationData.Create;
  with fCompilationData as TFakeCompilationData do 
  try
    CreatePackage('package1','an ide package');
    CreatePackage('package2','package1');
    CreatePackage('package3','missing package');
    
    Sut := TPackageDependencyVerifier.Create(fCompilationData);
    Sut.Initialize;
    Sut.Verify;
    
    CheckEqualsString('',Sut.MissingPackages['package1']);
    CheckEqualsString('',Sut.MissingPackages['package2']);
    CheckEqualsString('missing package',Sut.MissingPackages['package3']);  

    Sut.Free;
  finally
     Free;
  end;
end;

procedure TestTPackageDependencyVerifier.When_a_package_cannot_be_compiled_then_packages_depending_on_it_should_be_marked_as_missing;
begin
  fCompilationData := TFakeCompilationData.Create;
  with fCompilationData as TFakeCompilationData do 
  try
    CreatePackage('package1','an ide package,missing ide package');
    CreatePackage('package2','package1');
    
    Sut := TPackageDependencyVerifier.Create(fCompilationData);
    Sut.Verify;
    
    CheckEqualsString('missing ide package',Sut.MissingPackages['package1']);
    CheckEqualsString('package1 requires "missing ide package"',Sut.MissingPackages['package2']);

    Sut.Free;
  finally
     Free;
  end;
end;

{ TFakeCompilationData }

function TFakeCompilationData.CreatePackage(name: string; requireds: string): TPackageInfo;
var
  req: string;
  words: TStrings;
begin
 Result := TPackageInfo.Create(name);
 words := TStringList.Create;
 ExtractStrings([','],[' '],PWideChar(requireds), words);
  for req in words do
  begin
     Result.RequiredPackageList.Add(req); 
  end;
  PackageList.Add(Result);
  words.Free;
end;

destructor TFakeCompilationData.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TFakeCompilationData.Dispose;
var
  I: Integer;
begin
  for I := 0 to PackageList.Count - 1 do
    PackageList[i].Free;
end;

procedure TFakeCompilationData.GetIdePackages(const list: TStringList);
begin
  list.Add('an ide package');
end;

function TFakeCompilationData.GetIdeVersionSuffix: string;
begin
  Result := 'd7';
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPackageDependencyVerifier.Suite);
end.
