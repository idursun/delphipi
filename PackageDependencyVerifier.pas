{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
  License : GNU General Public License 2.0
  ** }
unit PackageDependencyVerifier;

interface

uses Classes, PackageList, PackageInfo, CompilationData, InstalledPackageResolver;

type

  TPackageDependencyVerifier = class
  private
    fCompilationData: TCompilationData;
    fMissingPackages: TStringList;
    fInstalledPackageResolver: TInstalledPackageResolver;
    function GetMissingPackage(key: string): String;
  public
    constructor Create(const CompilationData: TCompilationData; const installedPackageResolver: TInstalledPackageResolver);
    destructor Destroy; override;

    procedure Initialize; virtual;
    procedure Verify; virtual;

    property MissingPackages[key: string]: String read GetMissingPackage;
  end;


implementation

uses SysUtils, JclFileUtils, gnugettext;

{ TPackageDependencyVerifier }
constructor TPackageDependencyVerifier.Create(const CompilationData: TCompilationData; const installedPackageResolver: TInstalledPackageResolver);
begin
  fCompilationData := CompilationData;
  fMissingPackages := TStringList.Create;
  fInstalledPackageResolver := TInstalledPackageResolver.Create;
end;

destructor TPackageDependencyVerifier.Destroy;
begin
  fMissingPackages.Free;
  inherited;
end;

function TPackageDependencyVerifier.GetMissingPackage(key: string): String;
begin
  Result := fMissingPackages.Values[key];
end;

procedure TPackageDependencyVerifier.Verify;
var
  requiredPackage: string;
  i: Integer;
  package: TPackageInfo;
  allPackages: TStringList;
begin
  fMissingPackages.Clear;
  allPackages := TStringList.Create;
  allPackages.AddStrings(fInstalledPackageResolver.ExistentPackages);
  try

    for I := 0 to fCompilationData.PackageList.Count - 1 do
      allPackages.Add(UpperCase(fCompilationData.PackageList[i].packageName));

    for i := 0 to fCompilationData.PackageList.Count - 1 do
    begin
      package := fCompilationData.PackageList[i];
      fMissingPackages.Values[package.packageName] := '';

      for requiredPackage in package.RequiredPackageList do
      begin
        if fMissingPackages.Values[requiredPackage] <> '' then
          fMissingPackages.Values[package.packageName] := Format(_('%s requires %s'), [requiredPackage, fMissingPackages.Values[requiredPackage]]);

        if allPackages.IndexOf(UpperCase(requiredPackage)) = -1 then
          fMissingPackages.Values[package.packageName] := requiredPackage;
      end;
    end;

  finally
    allPackages.Free;
  end;
end;

procedure TPackageDependencyVerifier.Initialize;
begin
  Assert(fInstalledPackageResolver <> nil, 'resolver cannot be null');
  fInstalledPackageResolver.Clear;
  fInstalledPackageResolver.AddDefaultPackageList;
  fInstalledPackageResolver.AddIDEPackageList(fCompilationData);
end;


end.
