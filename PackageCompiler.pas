{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageCompiler;

interface
uses JclBorlandTools, PackageInfo, PackageList, SysUtils, Classes, CompilationData;

type
   TPackageCompileEvent = procedure(const package: TPackageInfo; status: TPackageStatus) of object;
   TPackageCompiler = class
   private
     fInstallation : TJclBorRADToolInstallation;
     fCompilationData: TCompilationData;
     fPackageList: TPackageList;
     fPackageCompileEvent: TPackageCompileEvent;
     fCancel: boolean;
    procedure ResolveHelpFiles(const compilationData: TCompilationData);
    procedure AddSourcePathsToIDE(const sourceFilePaths: TStrings; const installation: TJclBorRADToolInstallation);
   protected
     function GetExtraOptions: String; virtual;
     function ConvertToShortPaths(const paths : TStringList): string;
     procedure RaiseEvent(const packageInfo: TPackageInfo; status: TPackageStatus);virtual;
   public
     constructor Create(const compilationData: TCompilationData);
     procedure Compile;
     function CompilePackage(const packageInfo : TPackageInfo): Boolean; virtual;
     function InstallPackage(const packageInfo : TPackageInfo): Boolean; virtual;
     procedure ResolveSourcePaths;

     //Events
     property OnPackageEvent: TPackageCompileEvent read fPackageCompileEvent write fPackageCompileEvent;
     //Properties
     property Cancel: boolean read fCancel write fCancel;
   end;

implementation

uses JclFileUtils, JclStrings;
{ TPackageCompiler }

constructor TPackageCompiler.Create(const compilationData: TCompilationData);
begin
  fCompilationData := compilationData;
  fInstallation := fCompilationData.Installation;
  fPackageList := fCompilationData.PackageList;
end;

procedure TPackageCompiler.Compile;
var
  i: integer;
  info: TPackageInfo;
  compilationSuccessful: boolean;
begin
  try
    if fCompilationData.SourceFilePaths.Count = 0 then
      ResolveSourcePaths;
      
    AddSourcePathsToIDE(fCompilationData.SourceFilePaths, fCompilationData.Installation);
      
    if fCompilationData.HelpFiles.Count = 0 then
      ResolveHelpFiles(fCompilationData);

    fPackageList.SortList;
    for i := 0 to fPackageList.Count - 1 do begin
      info := fPackageList[i];
      RaiseEvent(info, psCompiling);
      compilationSuccessful := CompilePackage(info);

      if compilationSuccessful and (not info.RunOnly) then
      begin
        RaiseEvent(info, psInstalling);
        InstallPackage(info);
      end;
      if compilationSuccessful then
        RaiseEvent(info, psSuccess)
      else
        RaiseEvent(info, psError);

      if fCancel then
        break;
    end;
  finally
    
  end;
end;

function TPackageCompiler.CompilePackage(
  const packageInfo: TPackageInfo): Boolean;
var
  ExtraOptions : String;
begin
  ExtraOptions := GetExtraOptions;
  Result := fInstallation.DCC32.MakePackage(
                packageInfo.filename,
                fCompilationData.BPLOutputFolder,
                fCompilationData.DCPOutputFolder,
                ExtraOptions);
end;


function TPackageCompiler.GetExtraOptions: String;
var
  shortPaths : string;
  pathList : TStringList;
begin
  pathList := TStringList.Create;
  try
    ExtractStrings([';'],[' '],PWideChar(fInstallation.LibrarySearchPath),pathList);
    pathList.Add(fInstallation.BPLOutputPath);
    shortPaths := ConvertToShortPaths(pathList);
  finally
    pathList.Free;
  end;

  Result := '-B -Q';
  Result := Result + ' -I'+shortPaths+'';
  Result := Result + ' -U'+shortPaths+'';
  Result := Result + ' -O'+shortPaths+'';
  Result := Result + ' -R'+shortPaths+'';
end;

function TPackageCompiler.ConvertToShortPaths(const paths : TStringList):string;
var
  path : string;
begin
  Result := '';
  for path in paths do
  begin
    Result := Result + StrDoubleQuote(PathGetShortName(StrTrimQuotes(path))) + ';';
  end;
end;

function TPackageCompiler.InstallPackage(
  const packageInfo: TPackageInfo): Boolean;
var
  BPLFileName : String;  
begin
  BPLFileName := PathAddSeparator(fInstallation.BPLOutputPath) + PathExtractFileNameNoExt(packageInfo.FileName) + packageInfo.Suffix + '.bpl';
  Result := fInstallation.RegisterPackage(BPLFileName, packageInfo.Description);
end;

procedure TPackageCompiler.RaiseEvent(const packageInfo: TPackageInfo;
  status: TPackageStatus);
begin
   if Assigned(fPackageCompileEvent) then
   begin
     if (status = psSuccess) or (status = psError) then
        packageInfo.Status := status;
        
     fPackageCompileEvent(packageInfo, status);
   end;
end;

//TODO: refactor -- finds paths of files have .pas extension and then matches pas files in packages, removes remaining paths 
procedure TPackageCompiler.ResolveSourcePaths;
var
  i,j: integer;
  files, containedFiles: TStringList;
begin
  Assert(assigned(fCompilationData.SourceFilePaths));
  fCompilationData.SourceFilePaths.Clear;

  files := TStringList.Create;
  files.Sorted := true;
  files.Duplicates := dupIgnore;

  containedFiles := TStringList.Create;
  containedFiles.Sorted := true;
  containedFiles.Duplicates := dupIgnore;

  fCompilationData.SourceFilePaths.Sorted := true;
  fCompilationData.SourceFilePaths.Duplicates := dupIgnore;
     
  for i := 0 to fPackageList.Count - 1 do begin
    fCompilationData.SourceFilePaths.Add(ExtractFileDir(fCompilationData.PackageList[i].FileName));
    for j := 0 to fPackageList[i].ContainedFileList.Count - 1 do
      containedFiles.Add(ExtractFileName(fCompilationData.PackageList[i].ContainedFileList[j]));
  end;

  AdvBuildFileList(fCompilationData.BaseFolder+'\*.pas',
           faAnyFile,
           files,
           amAny,
           [flFullnames, flRecursive],
           '', nil);

  for I := 0 to files.count - 1 do begin
    if containedFiles.IndexOf(ExtractFileName(files[i])) > 0 then
      fCompilationData.SourceFilePaths.Add(ExtractFileDir(files[i]));
  end;
end;

procedure TPackageCompiler.ResolveHelpFiles(const compilationData: TCompilationData);
var
  files: TStringList;
  filename: string;
begin
  assert(assigned(fCompilationData));

  files := TStringList.Create;
  try
    AdvBuildFileList(compilationData.BaseFolder +'\*.hlp',
           faAnyFile,
           files,
           amAny,
           [flFullnames, flRecursive],
           '', nil);
     for filename in files do
       compilationData.HelpFiles.Add(filename);
  finally
    files.Free;
  end;
end;

procedure TPackageCompiler.AddSourcePathsToIDE(const sourceFilePaths: TStrings; const installation: TJclBorRADToolInstallation);
var
  path : string;
begin
  Assert(assigned(sourceFilePaths));
  Assert(assigned(installation));
  
  for path in sourceFilePaths do
    installation.AddToLibrarySearchPath(path);
end;


end.
