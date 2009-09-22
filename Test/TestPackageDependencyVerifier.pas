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
    function CreatePackage(name: string; requireds: string ): TPackageInfo;
  published
    procedure When_a_required_package_is_missing_then_packages_depending_on_it_should_be_set_as_missing;
    procedure When_a_package_cannot_be_compiled_then_packages_depending_on_it_should_be_marked_as_missing;
    procedure When_a_package_is_removed_then_next_verfication_should_only_check_custom_packages;
  end;

implementation
uses Generics.Collections, InstalledPackageResolver;
type
  TStringArr = array of string;

  TFakeInstalledPackageResolver = class(TInstalledPackageResolver)
  public
    constructor Create; override;
  end;
  
procedure TestTPackageDependencyVerifier.When_a_required_package_is_missing_then_packages_depending_on_it_should_be_set_as_missing;
var
  list: TObjectList<TPackageInfo>;
begin
  list := TObjectList<TPackageInfo>.Create;
  list.OwnsObjects := True;
  try
    list.Add(CreatePackage('package1','an ide package'));
    list.Add(CreatePackage('package2','package1'));
    list.Add(CreatePackage('package3','missing package'));

    Sut := TPackageDependencyVerifier.Create;
    try    
      Sut.Verify(list, TFakeInstalledPackageResolver.Create);
    
      CheckEqualsString('',Sut.MissingPackages['package1']);
      CheckEqualsString('',Sut.MissingPackages['package2']);
      CheckEqualsString('missing package',Sut.MissingPackages['package3']);
    finally
      SUT.Free;
    end;          
  finally
     list.Free;
  end;
end;

function TestTPackageDependencyVerifier.CreatePackage(name, requireds: string): TPackageInfo;
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
  words.Free;
end;

procedure TestTPackageDependencyVerifier.When_a_package_cannot_be_compiled_then_packages_depending_on_it_should_be_marked_as_missing;
var
  list: TObjectList<TPackageInfo>;
begin
  list := TObjectList<TPackageInfo>.Create;
  try
    list.Add(CreatePackage('package1','an ide package,missing ide package'));
    list.Add(CreatePackage('package2','package1'));

    Sut := TPackageDependencyVerifier.Create;
    try    
      Sut.Verify(list, TFakeInstalledPackageResolver.Create);
    
      CheckEqualsString('missing ide package',Sut.MissingPackages['package1']);
      CheckEqualsString('package1 requires missing ide package',Sut.MissingPackages['package2']);
    finally      
      Sut.Free;
    end;          
  finally
     list.Free;
  end;
end;

procedure TestTPackageDependencyVerifier.When_a_package_is_removed_then_next_verfication_should_only_check_custom_packages;
var
  list: TObjectList<TPackageInfo>;
begin
  list := TObjectList<TPackageInfo>.Create;
  list.Add(CreatePackage('package1','an ide package'));
  list.Add(CreatePackage('package2','package1'));
  list.Add(CreatePackage('package3','package2'));
  try

    Sut := TPackageDependencyVerifier.Create;
    Sut.Verify(list, TFakeInstalledPackageResolver.Create);

    CheckEqualsString('',Sut.MissingPackages['package1']);
    CheckEqualsString('',Sut.MissingPackages['package2']);

    list.Delete(0); // remove package1

    Sut.Verify(list, TFakeInstalledPackageResolver.Create);
    
    CheckEqualsString('',Sut.MissingPackages['package1']);
    CheckNotEqualsString('',Sut.MissingPackages['package3']);

    Sut.Free;
  finally
     list.Free;
  end;
end;

{ TFakeInstalledPackageResolver }

constructor TFakeInstalledPackageResolver.Create;
begin
  inherited;
  InstalledPackages.Add('an ide package');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPackageDependencyVerifier.Suite);
end.
