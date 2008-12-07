{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageInfoFactory;

interface

uses SysUtils, StrUtils, Classes, PackageInfo;
type

  TPackageInfoFactory = class
  private
    procedure ReadInfo(const packageInfo: TPackageInfo; const lines: TStrings);
    function ClearStr(Str: string):string;
  public
    function CreatePackageInfo(const packageFileName: string): TPackageInfo; overload;
    function CreatePackageInfo(const packageFileContent: TStrings): TPackageInfo; overload;
  end;
implementation

resourcestring
  StrRUNONLY = '{$RUNONLY';
  StrDESCRIPTION = '{$DESCRIPTION ';
  StrSUFFIX = '{$LIBSUFFIX ';
  StrPackage = 'package';
  StrRequires = 'requires';
  StrContains = 'contains';
  
{ TPackageInfoFactory }

function TPackageInfoFactory.ClearStr(Str: string): string;
begin
  Result := Trim(Str);
  Result := ReplaceStr(Result, ',', '');
  Result := ReplaceStr(Result, ';', '');
end;

function TPackageInfoFactory.CreatePackageInfo(
  const packageFileName: string): TPackageInfo;
var
  lines: TStrings;  
begin
  lines := TStringList.Create;
  try
    lines.LoadFromFile(packageFileName);
    result := CreatePackageInfo(lines);
    Result.FileName := packageFileName;
  finally
    lines.Free;
  end;
end;

function TPackageInfoFactory.CreatePackageInfo(
  const packageFileContent: TStrings): TPackageInfo;
begin
  assert(packageFileContent <> nil);
  Result := TPackageInfo.Create();
  ReadInfo(Result, packageFileContent);
end;

// ugly code, but implementing a full blown parser here is like nailing with a sledge hammer
procedure TPackageInfoFactory.ReadInfo(const packageInfo: TPackageInfo; const lines: TStrings);
var
  I: Integer;
  Str : String;
  RequiresBlock,ContainsBlock: Boolean;
begin
  assert(lines <> nil);
  assert(packageInfo <> nil);
  
  RequiresBlock := False;
  ContainsBlock := False;
  packageInfo.RunOnly := False;
  for I := 0 to lines.Count - 1 do begin
    Str := TrimRight(lines[i]);
    if Pos(StrRUNONLY+'}',str) = 1 then
      packageInfo.RunOnly := True;

    if (Pos(StrRUNONLY,str) = 1) and (Pos('ON',Str)>0) then
      packageInfo.RunOnly := True;

    if Pos(StrDESCRIPTION,str) > 0 then
      packageInfo.Description := Copy(Str,Length(StrDescription)+2,Pos('}',str)-Length(StrDescription)-3);
    if Pos(StrSUFFIX,str) > 0 then
      packageInfo.Suffix := Copy(Str,Length(StrSUFFIX)+2,Pos('}',str)-Length(StrSUFFIX)-3);

    if Pos(StrPackage,str) = 1 then
      packageInfo.PackageName := Trim(Copy(Str,Length(strPackage)+2,Length(Str)- Length(strPackage) -2));

    if Pos(StrRequires,str) = 1 then RequiresBlock := True;
    if RequiresBlock then begin
      if not StartsStr('{$', Str) then
        packageInfo.RequiredPackageList.Add(ClearStr(Str));
    end;
    if (RequiresBlock) and (pos(';',str) > 0) then
      RequiresBlock := False;

    if Pos(StrContains,str) = 1 then ContainsBlock := True;

    if ContainsBlock then
      if Pos(' in ',Str) > 0 then
        packageInfo.ContainedFileList.Add(Copy(Str,Pos('''',Str)+1,Length(Str) - Pos('''',Str)-2))
      else
        packageInfo.ContainedFileList.Add(ClearStr(str)+ '.pas');

    if (ContainsBlock) and (pos(';',str) > 0) then
      ContainsBlock := False;
  end;

  if packageInfo.RequiredPackageList.Count > 0 then
    packageInfo.RequiredPackageList.Delete(0);
  if packageInfo.ContainedFileList.Count > 0 then
    packageInfo.ContainedFileList.Delete(0);
end;

end.
