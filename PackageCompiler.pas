{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageCompiler;

interface
uses JclBorlandTools, PackageInfo, SysUtils, Classes, CompilationData;

type
   TPackageCompileEvent = procedure(const package: TPackageInfo; status: TPackageStatus) of object;
   TPackageCompiler = class
   private
     fInstallation : TJclBorRADToolInstallation;
     fCompilationData: TCompilationData;
     fPackageList: TPackageList;
     fPackageCompileEvent: TPackageCompileEvent;
   protected
     function GetExtraOptions: String; virtual;
     function GetShortPaths(paths: string): string;
     procedure RaiseEvent(const packageInfo: TPackageInfo; status: TPackageStatus);virtual;
   public
     constructor Create(const compilationData: TCompilationData);
     procedure Compile;
     function CompilePackage(const packageInfo : TPackageInfo): Boolean; virtual;
     function InstallPackage(const packageInfo : TPackageInfo): Boolean; virtual;
     procedure AddSourcePathsToIDE(const sourcePaths: TStringList);
     //Events
     property OnPackageEvent: TPackageCompileEvent read fPackageCompileEvent write fPackageCompileEvent;
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

procedure TPackageCompiler.AddSourcePathsToIDE(const sourcePaths: TStringList);
var
  path : string;
begin
  Assert(assigned(sourcePaths));
  for path in sourcePaths do
  begin
    fInstallation.AddToLibrarySearchPath(path);
  end;
end;

procedure TPackageCompiler.Compile;
var
  sourceList: TStringList;
  i: integer;
  info: TPackageInfo;
  compileSuccessful: boolean;
begin
  sourceList := TStringList.Create;
  try
    fPackageList.GetSourcePaths(sourceList);
    AddSourcePathsToIDE(sourceList);
    for i := 0 to fPackageList.Count - 1 do begin
      info := fPackageList[i];
      RaiseEvent(info, psCompiling);
      compileSuccessful := CompilePackage(info);

      if compileSuccessful and (not info.RunOnly) then
      begin
        RaiseEvent(info, psInstalling);
        InstallPackage(info);
      end;
      if compileSuccessful then
        RaiseEvent(info, psSuccess)
      else
        RaiseEvent(info, psError);
    end;
  finally
    sourceList.Free;
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
  paths : string;
begin
  paths := GetShortPaths(fInstallation.LibrarySearchPath);
  Result := '-B';
  Result := Result + #13#10 +'-I"'+paths+'"';
  Result := Result + #13#10+ '-U"'+paths+'"';
  Result := Result + #13#10+ '-O"'+paths+'"';
  Result := Result + #13#10+ '-R"'+paths+'"';
end;

function TPackageCompiler.GetShortPaths(paths : string):string;
var
  pathList : TStringList;
  path : string;
begin
  Result := '';
  pathList := TStringList.Create;
  try
    ExtractStrings([';'],[' '],PAnsiChar(paths),pathList);
    for path in pathList do
    begin
      Result := Result + StrDoubleQuote(PathGetShortName(StrTrimQuotes(path))) + ';';
    end;
  finally
    pathList.Free;
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

end.
