{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit InstalledPackageResolver;

interface
uses Classes, CompilationData;
type

  IInstalledPackageResolver = interface
    procedure AddDefaultPackageList;
    function GetInstalledPackages: TStringList;
    procedure AddIDEPackageList(const CompilationData: TCompilationData);
    procedure Clear;
  end;

  TInstalledPackageResolver = class(TInterfacedObject, IInstalledPackageResolver)
  private
    fVersionSuffix: string;
    fSearchFolders: TStringList;
    fInstalledPackages: TStringList;
    function RemoveVersionSuffix(const name, suffix: string): string;
    function GetInstalledPackages: TStringList;
  protected
  public
    constructor Create; overload; virtual;
    constructor Create(const CompilationData: TCompilationData); overload; virtual;
    destructor Destroy; override;
    procedure AddDefaultPackageList; virtual;
    procedure AddIDEPackageList(const CompilationData: TCompilationData); virtual;
    procedure Clear;
    property InstalledPackages: TStringList read GetInstalledPackages;
  end;
implementation

uses SysUtils, JclFileUtils, JclIDEUtils;
constructor TInstalledPackageResolver.Create;
begin
  inherited Create;
  fSearchFolders := TStringList.Create;
  fInstalledPackages := TStringList.Create;
end;

constructor TInstalledPackageResolver.Create(const CompilationData: TCompilationData);
var
  systemPath, versionPattern: string;
  filePattern: string;
begin
  Assert(CompilationData <> nil, 'Compilation Data cannot be null');
  Assert(CompilationData.Installation <> nil, 'Installation cannot be null');

  Create;

  fVersionSuffix := CompilationData.GetIdeVersionSuffix;
  versionPattern := Copy(fVersionSuffix, 2, Length(fVersionSuffix) - 1) + '0';
  filePattern := '*' + versionPattern + '.bpl';

  systemPath := GetEnvironmentVariable('WINDIR') + '\System32\';
  fSearchFolders.Add(PathAppend(systemPath, filePattern));
  fSearchFolders.Add(PathAppend(CompilationData.Installation.LibFolderName[bpWin32], '*.bpl'));
  fSearchFolders.Add(PathAppend(CompilationData.Installation.BinFolderName, '*.bpl'));
end;

procedure TInstalledPackageResolver.AddDefaultPackageList;
var
  packageName: string;
  internalList: TStringList;
  path, entry: string;
begin
  internalList := TStringList.Create;
  try
    for path in fSearchFolders do
      BuildFileList(path, faAnyFile, internalList);

    for entry in internalList do
    begin
      packageName := PathExtractFileNameNoExt(entry);
      packageName := UpperCase(packageName);
      packageName := RemoveVersionSuffix(packageName, fVersionSuffix);
      fInstalledPackages.Add(packageName);
    end;

  finally
    internalList.Free;
  end;
  fInstalledPackages.Add('DESIGNIDE');
end;

procedure TInstalledPackageResolver.AddIDEPackageList(const CompilationData: TCompilationData);
var
  i: integer;
  idePackageName: string;
  list: TStringList;
begin
  list := TStringList.Create;
  try
    CompilationData.GetIdePackages(list);

   for i := 0 to CompilationData.Installation.IdePackages.Count - 1 do
     list.Add(CompilationData.Installation.IdePackages.PackageFileNames[i]);

    for i := 0 to list.Count - 1 do
    begin
      idePackageName := PathExtractFileNameNoExt(list[i]);
      fInstalledPackages.Add(UpperCase(idePackageName));
    end;
  finally
    list.Free;
  end;
end;

procedure TInstalledPackageResolver.Clear;
begin
  fInstalledPackages.Clear;
end;

destructor TInstalledPackageResolver.Destroy;
begin
  FreeAndNil(fInstalledPackages);
  FreeAndNil(fSearchFolders);
  inherited;
end;

function TInstalledPackageResolver.GetInstalledPackages: TStringList;
begin
  Result := fInstalledPackages;
end;

function TInstalledPackageResolver.RemoveVersionSuffix(const name, suffix: string): string;
begin
  Result := name;
  Delete(Result, Length(Result) - Length(suffix) + 1, Length(suffix))
end;
end.

