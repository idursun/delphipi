{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageDependencyVerifier;

interface
uses Classes, Generics.Collections, PackageList, PackageInfo,  CompilationData;
type
 
  TPackageDependencyVerifier = class
  private
    fExistentPackageList: TStringList;
    fCompilationData: TCompilationData;
    fMissingPackages: TDictionary<string, string>;
    function GetMissingPackage(key:string): String;
  protected 
    function GetVersionSuffix:string; virtual;
    function RemoveVersionSuffix(const name, suffix: string):string; virtual;
    procedure AddDefaultPackageList(); virtual;
    procedure AddCustomPackageList(const list:TPackageList); virtual;
    procedure AddIDEPackageList; virtual;
    procedure CheckMissingDependencies; virtual;
  public
    constructor Create(const compilationData: TCompilationData);
    destructor Destroy; override;
    
    procedure Initialize; virtual;
    procedure Verify;

    property MissingPackages[key:string]: String read GetMissingPackage;
  end;
  
implementation
uses SysUtils, JclFileUtils;

{ TPackageDependencyVerifier }
constructor TPackageDependencyVerifier.Create(const compilationData: TCompilationData);
begin
  fCompilationData := compilationData;
  fExistentPackageList := TStringList.Create;
  fMissingPackages := TDictionary<string, string>.Create();
end;

destructor TPackageDependencyVerifier.Destroy;
begin
  fExistentPackageList.Free;
  fMissingPackages.Free;
  inherited;
end;

function TPackageDependencyVerifier.GetMissingPackage(key:string): String;
begin
  Result :='';
  if fMissingPackages.ContainsKey(key) then
    Result := fMissingPackages[key];
end;

function TPackageDependencyVerifier.GetVersionSuffix: string;
begin
  Result := fCompilationData.GetIdeVersionSuffix;
  Result := Copy(Result, 2, Length(Result)-1) + '0';
end;

function TPackageDependencyVerifier.RemoveVersionSuffix(const name, suffix: string): string;
begin
  Result := name;
  Delete(Result, Length(Result)-length(suffix)+1, length(suffix))
end;

procedure TPackageDependencyVerifier.AddDefaultPackageList;
var
  systemPath,searchPath, entry,versionSuffix: string;
  packageName: string;
  internalList: TStringList;
begin
   systemPath := GetEnvironmentVariable('WINDIR') + '\System32\';
   versionSuffix :=  GetVersionSuffix;
   searchPath := systemPath + '*' + versionSuffix + '.bpl';
   internalList := TStringList.Create;
   try
     BuildFileList(searchPath, faAnyFile, internalList);
     for entry in internalList   do
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
  list := TStringList.Create;
  try
    fCompilationData.GetIdePackages(list);
    for i := 0 to list.Count-1 do
    begin
      idePackageName := PathExtractFileNameNoExt(list[i]);
      fExistentPackageList.Add(UpperCase(idePackageName));
    end;  
  finally
    list.Free;
  end;
end;

procedure TPackageDependencyVerifier.AddCustomPackageList(const list: TPackageList);
var
  I: Integer;
begin
  for I := 0 to list.Count - 1 do
     fExistentPackageList.Add(UpperCase(list[i].PackageName)); 
end;

procedure TPackageDependencyVerifier.CheckMissingDependencies;
var
  requiredPackage: string;
  i: Integer;
  value: string;
  package: TPackageInfo;
  allPackages: TStringList;
begin
  fMissingPackages.Clear;
  allPackages := TStringList.Create;
  allPackages.AddStrings(fExistentPackageList);
  try
   
    for I := 0 to fCompilationData.PackageList.Count - 1 do
      allPackages.Add(UpperCase(fCompilationData.PackageList[i].PackageName)); 
     
    for i := 0 to fCompilationData.PackageList.Count - 1 do
    begin
      package:= fCompilationData.PackageList[i];
      fMissingPackages.Add(package.PackageName,'');
  
      for requiredPackage in package.RequiredPackageList do begin
        if fMissingPackages.TryGetValue(requiredPackage, value) then
           if value <> '' then
             fMissingPackages[package.PackageName] := requiredPackage + ' requires "' + value+'"';
      
        if allPackages.IndexOf(UpperCase(requiredPackage)) = -1 then
          fMissingPackages[package.PackageName] := requiredPackage;
      end;
    end;
    
  finally
    allPackages.Free;
  end;
end;

procedure TPackageDependencyVerifier.Verify;
var
  I: Integer;
begin
  AddCustomPackageList(fCompilationData.PackageList);  
  CheckMissingDependencies;
end;

procedure TPackageDependencyVerifier.Initialize;
begin
  fExistentPackageList.Clear;
  AddDefaultPackageList;
  AddIDEPackageList;
end;

end.
