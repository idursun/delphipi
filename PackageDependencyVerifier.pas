{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
  License : GNU General Public License 2.0
  ** }
unit PackageDependencyVerifier;

interface

uses Classes, PackageList, PackageInfo, CompilationData;

type

  TPackageDependencyVerifier = class
  private
    fExistentPackageList: TStringList;
    fCompilationData: TCompilationData;
    fMissingPackages: TStringList;
    function GetMissingPackage(key: string): String;
  protected
    function GetVersionSuffix: string; virtual;
    function RemoveVersionSuffix(const name, suffix: string): string; virtual;
    procedure AddDefaultPackageList; virtual;
    procedure AddIDEPackageList; virtual;
  public
    constructor Create(const CompilationData: TCompilationData);
    destructor Destroy; override;

    procedure Initialize; virtual;
    procedure Verify; virtual;

    property MissingPackages[key: string]: String read GetMissingPackage;
  end;

implementation

uses SysUtils, JclFileUtils, gnugettext;

{ TPackageDependencyVerifier }
constructor TPackageDependencyVerifier.Create(const CompilationData: TCompilationData);
begin
  fCompilationData := CompilationData;
  fExistentPackageList := TStringList.Create;
  fMissingPackages := TStringList.Create();
end;

destructor TPackageDependencyVerifier.Destroy;
begin
  fExistentPackageList.Free;
  fMissingPackages.Free;
  inherited;
end;

function TPackageDependencyVerifier.GetMissingPackage(key: string): String;
begin
  Result := fMissingPackages.Values[key];
end;

function TPackageDependencyVerifier.GetVersionSuffix: string;
begin
  Result := fCompilationData.GetIdeVersionSuffix;
  Result := Copy(Result, 2, Length(Result) - 1) + '0';
end;

function TPackageDependencyVerifier.RemoveVersionSuffix(const name, suffix: string): string;
begin
  Result := name;
  Delete(Result, Length(Result) - Length(suffix) + 1, Length(suffix))
end;

procedure TPackageDependencyVerifier.AddDefaultPackageList;
var
  systemPath, searchPath, entry, versionSuffix: string;
  filePattern: string;
  packageName: string;
  internalList: TStringList;
begin

  Assert(fCompilationData <> nil, 'Compilation Data cannot be null');
  Assert(fCompilationData.Installation <> nil, 'Installation cannot be null');

  systemPath := GetEnvironmentVariable('WINDIR') + '\System32\';
  versionSuffix := GetVersionSuffix;
  filePattern := '*' + versionSuffix + '.bpl';
  searchPath := PathAppend(systemPath, filePattern);
  internalList := TStringList.Create;
  try
    BuildFileList(searchPath, faAnyFile, internalList);
    BuildFileList(PathAppend(fCompilationData.Installation.LibFolderName, filePattern), faAnyFile, internalList);
    BuildFileList(PathAppend(fCompilationData.Installation.BinFolderName, filePattern), faAnyFile, internalList);
    for entry in internalList do
    begin
      packageName := PathExtractFileNameNoExt(entry);
      packageName := UpperCase(packageName);
      packageName := RemoveVersionSuffix(packageName, versionSuffix);
      fExistentPackageList.Add(packageName);
    end;
  finally
    internalList.Free;
  end;
  fExistentPackageList.Add('DESIGNIDE');
end;

procedure TPackageDependencyVerifier.AddIDEPackageList;
var
  i: integer;
  idePackageName: string;
  list: TStringList;
begin
  Assert(fCompilationData <> nil, 'Compilation Data cannot be null');
  Assert(fCompilationData.Installation <> nil, 'Installation cannot be null');

  list := TStringList.Create;
  try
    fCompilationData.GetIdePackages(list);
    for i := 0 to list.Count - 1 do
    begin
      idePackageName := PathExtractFileNameNoExt(list[i]);
      fExistentPackageList.Add(UpperCase(idePackageName));
    end;
  finally
    list.Free;
  end;
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
  allPackages.AddStrings(fExistentPackageList);
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
  fExistentPackageList.Clear;
  AddDefaultPackageList;
  AddIDEPackageList;
end;

end.
