unit TestPackageList;
interface

uses
  TestFramework, Classes, StrUtils, PackageInfo, PackageList;

type
        TestTPackageList = class(TTestCase)
        strict private
                package1, package2, package3: TPackageInfo;
                FPackageList: TPackageList;
                procedure EstablishContext;
        public
                procedure SetUp; override;
                procedure TearDown; override;
                
        published
                procedure When_list_is_cleared;
                procedure When_a_package_is_added;
                procedure When_a_package_is_removed;
                procedure When_a_package_is_searched;
        end;

implementation

procedure TestTPackageList.SetUp;
begin
  FPackageList := TPackageList.Create;
  EstablishContext;
end;


procedure TestTPackageList.EstablishContext;
begin
  package1 := TPackageInfo.Create('package1');
  package2 := TPackageInfo.Create('package2');
  package3 := TPackageInfo.Create('package3');
  
  FPackageList.Add(package1);
  FPackageList.Add(package2);
  FPackageList.Add(package3);
end;

procedure TestTPackageList.TearDown;
begin
  package1.Free;
  package2.Free;
  package3.Free;
  
  FPackageList.Free;
  FPackageList := nil;
end;

procedure TestTPackageList.When_list_is_cleared;
begin
  FPackageList.Clear;
  CheckEquals(0, FPackageList.Count, 'list count should be 0');
end;

procedure TestTPackageList.When_a_package_is_added;
var
  item: TPackageInfo;
  oldCount: Integer;
begin
  item := TPackageInfo.Create;
  item.PackageName := 'added package';
  oldCount := FPackageList.Count;
  
  FPackageList.Add(item);
  
  CheckNotEquals(-1, FPackageList.IndexOf('added package'), 'should find in the list');
  CheckEquals(oldCount + 1, FPackageList.Count,'list count should increase by 1');
end;

procedure TestTPackageList.When_a_package_is_removed;
var
  item: TPackageInfo;
  oldCount: integer;
begin
  item := package2;
  oldCount := FPackageList.Count;
  FPackageList.Remove(item);

  CheckEquals(oldCount-1, FPackageList.Count, 'list count should decrease by 1');
  CheckEquals(-1, FPackageList.IndexOf(item.PackageName), 'it should not be in the list');
end;

procedure TestTPackageList.When_a_package_is_searched;
var
  ReturnValue: Integer;
  PackageName: string;
begin
  PackageName := 'package2';
  ReturnValue := FPackageList.IndexOf(PackageName);
  CheckEquals(1, ReturnValue,'should return index of searched package');

  PackageName := 'PackAge2';
  ReturnValue := FPackageList.IndexOf(PackageName);
  CheckEquals(1, ReturnValue,'should perform case insensitive search');

  PackageName := 'missing package';
  ReturnValue := FPackageList.IndexOf(PackageName);
  CheckEquals(-1, ReturnValue, 'should return -1 if package is not found');
end;

initialization
  RegisterTest(TestTPackageList.Suite);
end.

