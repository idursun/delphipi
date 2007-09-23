{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageCompiler;

interface
uses JclBorlandTools, PackageInfo, SysUtils, Classes;

type
   TPackageCompiler = class
   private
     installation : TJclBorRADToolInstallation;
   protected
     function GetExtraOptions: String; virtual;
     function GetShortPaths(paths: string): string;
   public
     constructor Create(const inst: TJclBorRadToolInstallation);
     function CompilePackage(const packageInfo : TPackageInfo): Boolean; virtual;
     function InstallPackage(const packageInfo : TPackageInfo): Boolean; virtual;
     procedure AddSourcePathsToIDE(const sourcePaths: TStringList);
   end;

implementation

uses JclFileUtils, JclStrings;
{ TPackageCompiler }

procedure TPackageCompiler.AddSourcePathsToIDE(const sourcePaths: TStringList);
var
  path : string;
begin
  Assert(assigned(sourcePaths));
  for path in sourcePaths do
  begin
    installation.AddToLibrarySearchPath(path);
  end;
end;

function TPackageCompiler.CompilePackage(
  const packageInfo: TPackageInfo): Boolean;
var
  ExtraOptions : String;
begin
  ExtraOptions := GetExtraOptions;
  Result := installation.DCC32.MakePackage(
                packageInfo.filename,
                installation.BPLOutputPath,
                installation.DCPOutputPath,
                ExtraOptions);
end;

constructor TPackageCompiler.Create(const inst: TJclBorRadToolInstallation);
begin
  self.installation := inst; 
end;

function TPackageCompiler.GetExtraOptions: String;
var
  paths : string;
begin
      Result := '-B';
      paths := GetShortPaths(installation.LibrarySearchPath);
      Result := Result + #13#10 +'-I"'+paths+'"';
      Result := Result + #13#10+ '-U"'+paths+'"';
      Result := Result + #13#10+ '-O"'+paths+'"';
      Result := Result + #13#10+ '-R"'+paths+'"';

end;

function TPackageCompiler.GetShortPaths(paths : string):string;
var
  PathList : TStringList;
  path : string;
begin
  Result := '';
  PathList := TStringList.Create;
  try
    ExtractStrings([';'],[' '],PAnsiChar(paths),PathList);
    for path in PathList do
    begin
      Result := Result + StrDoubleQuote(PathGetShortName(StrTrimQuotes(path))) + ';';
    end;
  finally
    PathList.Free;
  end;
end;

function TPackageCompiler.InstallPackage(
  const packageInfo: TPackageInfo): Boolean;
var
  BPLFileName : String;  
begin
  BPLFileName := PathAddSeparator(installation.BPLOutputPath) + PathExtractFileNameNoExt(packageInfo.FileName) + packageInfo.Suffix + '.bpl';
  Result := installation.RegisterPackage(BPLFileName, packageInfo.Description);
end;

end.
