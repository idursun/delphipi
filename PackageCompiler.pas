unit PackageCompiler;

interface
uses JclBorlandTools, PackageInfo;

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
   end;
   
implementation

uses JclFileUtils, Classes;
{ TPackageCompiler }

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
begin
      Result := '-B';
      //paths := GetShortPaths(inst.LibrarySearchPath);
      Result := Result + #13#10 +'-I"'+installation.LibrarySearchPath+'"';
      Result := Result + #13#10+ '-U"'+installation.LibrarySearchPath+'"';
      Result := Result + #13#10+ '-O"'+installation.LibrarySearchPath+'"';
      Result := Result + #13#10+ '-R"'+installation.LibrarySearchPath+'"';

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
       Result := Result + PathGetShortName(path) + ';';
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
  installation.RegisterPackage(BPLFileName, packageInfo.Description);
end;

end.
