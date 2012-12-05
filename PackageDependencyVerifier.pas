{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (ibrahimdursun gmail)
  License : GNU General Public License 2.0
  ** }
unit PackageDependencyVerifier;

interface

uses Classes, PackageList, PackageInfo, CompilationData, InstalledPackageResolver, Generics.Collections;

type
  TPackageDependencyVerifier = class
  private
    fMissingPackages: TStringList;
    function GetMissingPackage(key: string): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Verify(const selectedPackages: TList<TPackageInfo>; const installedPackageResolver: TInstalledPackageResolver); virtual;
    property MissingPackages[key: string]: String read GetMissingPackage;
  end;

implementation
uses SysUtils, JclFileUtils, gnugettext;

{ TPackageDependencyVerifier }
constructor TPackageDependencyVerifier.Create;
begin
  fMissingPackages := TStringList.Create;
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

procedure TPackageDependencyVerifier.Verify(const selectedPackages: TList<TPackageInfo>; const installedPackageResolver: TInstalledPackageResolver);
var
  requiredPackage: string;
  i: Integer;
  package: TPackageInfo;
  allPackages: TStringList;
begin
  fMissingPackages.Clear;
  allPackages := TStringList.Create;
  try
    allPackages.AddStrings(installedPackageResolver.InstalledPackages);
    for I := 0 to selectedPackages.Count - 1 do
      allPackages.Add(UpperCase(selectedPackages[i].PackageName));

    for i := 0 to selectedPackages.Count - 1 do
    begin
      package := selectedPackages[i];
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

end.



