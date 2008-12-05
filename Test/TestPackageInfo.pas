unit TestPackageInfo;
interface

uses
  TestFramework, Classes, StrUtils, PackageInfo;

type
        // Test methods for class TPackageInfo
        
        TestTPackageInfo = class(TTestCase)
        strict private
                FPackageInfo: TPackageInfo;
                base1, base2: TPackageInfo;
        public
                procedure SetUp; override;
                procedure TearDown; override;
        published
                procedure TestDependsOn;
        end;

implementation

procedure TestTPackageInfo.SetUp;
begin
  FPackageInfo := TPackageInfo.Create('package1');
  base1 := TPackageInfo.Create('base1');
  base2 := TPackageInfo.Create('base2');
        
  FPackageInfo.RequiredPackageList.Add(base1.PackageName);
  FPackageInfo.RequiredPackageList.Add(base2.PackageName);
end;

procedure TestTPackageInfo.TearDown;
begin
  base1.Free;
  base2.Free;
  FPackageInfo.Free;
  FPackageInfo := nil;
end;

procedure TestTPackageInfo.TestDependsOn;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FPackageInfo.DependsOn(base1);
  CheckTrue(ReturnValue, 'should confirm dependency to base1');

  ReturnValue := FPackageInfo.DependsOn(base2);
  CheckTrue(ReturnValue, 'should confirm dependency base2');
  
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPackageInfo.Suite);
end.

