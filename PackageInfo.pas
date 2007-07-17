{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageInfo;
interface
uses  Classes, StrUtils;

type
  IPackageInfo = interface
    ['{2AAFEE24-4BAB-4193-AD5D-3080CD4A47FB}']
    function Requires: TStringList;
    function Contains: TStringList;
    function RunOnly: Boolean;
    function PackageName: String;
    function Description: String;
    function FileName: String;
    procedure SetPackageName(const packageName: String);
    procedure SetDescription(const description: String);
  end;

  TPackageInfo = class(TInterfacedObject, IPackageInfo)
  private
    pfile : TStringList;
    FRequires: TStringList;
    FContains : TStringList;
    FDescription : String;
    FSuffix : String;
    FRunOnly : Boolean;
    FPackageName : String;
    FFileName: String;
    procedure ReadInfo;
    function ClearStr(Str: string):string;
  public
    constructor Create(const packagefile: String);overload;
    constructor Create;overload;
    destructor Destroy; override;
    function Description: string;
    function PackageName: string;
    function RunOnly: Boolean;
    function Suffix: string;
    function Requires: TStringList; overload;
    function DoesRequire(const package: TPackageInfo): Boolean; overload;
    function Contains: TStringList;
    procedure SetDescription(const description: string);
    procedure SetPackageName(const packageName: string);
    procedure SetFileName(const filename:string);

    function FileName: string;
  end;

  TPackageList = class(TList)
  private

    FInitialFolder : String;
    function get(I: Integer): TPackageInfo;
    procedure put(I: Integer; const Value: TPackageInfo);
    class procedure SearchFolder(var Result: TPackageList; const Folder: string);

  public
    property Item[I : Integer]: TPackageInfo read get write put;default;
    procedure Add(const item : TPackageInfo); overload;
    function IndexOf(const PackageName: String):Integer; overload;
    class function LoadFromFolder(const Folder: String):TPackageList;
    procedure SortList();
    procedure GetSourceList(var sourceList: TStringList);
    property InitialFolder: String read FInitialFolder; 
  end;

implementation
uses  SysUtils, JclFileUtils, contnrs;

resourcestring
  StrRUNONLY = '{$RUNONLY';
  StrDESCRIPTION = '{$DESCRIPTION ';
  StrSUFFIX = '{$LIBSUFFIX ';
  StrPackage = 'package';
  StrRequires = 'requires';
  StrContains = 'contains';

Function SimplifyPath(const Str: String):String;
var
  Strs : TStringList;
  Stack : TStack;
  i : integer;
begin
  Result := '';
  Strs := TStringList.Create;
  Stack := TStack.Create;
  try
     ExtractStrings(['\'],[' '],PAnsiChar(ExcludeTrailingBackslash(Str)),Strs);
     for i := 0 to strs.Count-1 do
       if (Strs[i] <> '..') and (Strs[i] <> '') then
         stack.Push(PAnsiChar(Strs[i]))
       else
         stack.Pop;
     while stack.Count > 0 do
       Result := PAnsiChar(stack.Pop) + '\' + Result;
  finally
    Strs.Free;
    Stack.Free;
  end;
  Result := ExcludeTrailingBackslash(Result);
end;

constructor TPackageInfo.Create(const packagefile: string);
begin
  Create;
  pfile := TStringList.Create;
  pfile.LoadFromFile(packagefile);
  FFilename := packagefile;
  ReadInfo;
end;

function TPackageInfo.Contains: TStringList;
begin
  if not assigned(FContains) then
    FContains := TStringList.Create;
  Result := FContains;
end;

constructor TPackageInfo.Create;
begin
  FRequires := TStringList.Create;
  FContains := TStringList.Create;
end;

function TPackageInfo.Description: string;
begin
  Result := FDescription;
end;

destructor TPackageInfo.Destroy;
begin
  pfile.clear;
  Requires.free;
  Contains.Free;
  inherited;
end;

function TPackageInfo.FileName: string;
begin
  Result := FFileName;
end;

function TPackageInfo.PackageName: string;
begin
  Result := FPackageName;
end;

function TPackageInfo.ClearStr(Str: string):String;
begin
  Result := Trim(Str);
  Result := ReplaceStr(Result, ',', '');
  Result := ReplaceStr(Result, ';', '');
end;

procedure TPackageInfo.ReadInfo;
var
  I: Integer;
  Str : String;
  RequiresBlock,ContainsBlock: Boolean;
begin
  RequiresBlock := False;
  ContainsBlock := False;
  FRunOnly := False;
  for I := 0 to pfile.Count - 1 do begin
    Str := TrimRight(pfile[i]);
    if Pos(StrRUNONLY+'}',str) = 1 then
      FRunOnly := True;

    if (Pos(StrRUNONLY,str) = 1) and (Pos('ON',Str)>0) then
      FRunOnly := True;

    if Pos(StrDESCRIPTION,str) > 0 then
      FDescription := Copy(Str,Length(StrDescription)+2,Pos('}',str)-Length(StrDescription)-3);
    if Pos(StrSUFFIX,str) > 0 then
      FSuffix := Copy(Str,Length(StrSUFFIX)+2,Pos('}',str)-Length(StrSUFFIX)-3);

    if Pos(StrPackage,str) = 1 then
      FPackageName := Trim(Copy(Str,Length(strPackage)+2,Length(Str)- Length(strPackage) -2));

    if Pos(StrRequires,str) = 1 then RequiresBlock := True;
    if RequiresBlock then begin
      Requires.Add(ClearStr(Str));
    end;
    if (RequiresBlock) and (pos(';',str) > 0) then
      RequiresBlock := False;

    if Pos(StrContains,str) = 1 then ContainsBlock := True;
    
    if ContainsBlock then
      if Pos(' in ',Str) > 0 then
        Contains.Add(Copy(Str,Pos('''',Str)+1,Length(Str) - Pos('''',Str)-2))
      else
        Contains.Add(ClearStr(str)+ '.pas');

    if (ContainsBlock) and (pos(';',str) > 0) then
      ContainsBlock := False;
  end;
  
  if Requires.Count > 0 then
    Requires.Delete(0);
  if Contains.Count > 0 then
    Contains.Delete(0);
end;

function TPackageInfo.DoesRequire(const package: TPackageInfo): Boolean;
begin
  assert(package <> nil);
  Result := Requires.IndexOf(package.packageName) > -1;
end;

function TPackageInfo.Requires: TStringList;
begin
  if not assigned(FRequires) then
    FRequires := TStringList.Create;
  Result := FRequires;
end;

function TPackageInfo.RunOnly: Boolean;
begin
  Result := FRunOnly;
end;

procedure TPackageInfo.SetDescription(const description: string);
begin
  FDescription := description;
end;

procedure TPackageInfo.SetFileName(const filename: string);
begin
  FFileName := filename;
end;

procedure TPackageInfo.SetPackageName(const packageName: string);
begin
  FPackageName := packageName;
end;

function TPackageInfo.Suffix: string;
begin
  Result := FSuffix;
end;

{ TPackageList }

procedure TPackageList.Add(const item: TPackageInfo);
begin
  inherited add(item);
end;

function TPackageList.get(I: Integer): TPackageInfo;
begin
  Result := TPackageInfo(inherited get(i));
end;

procedure TPackageList.GetSourceList(var sourceList: TStringList);
var
  i : integer;
  j: Integer;
  files, containedFiles  : TStringList;
begin
  Assert(assigned(sourceList));

  files := TStringList.Create;
  files.Sorted := true;
  files.Duplicates := dupIgnore;

  containedFiles := TStringList.Create;
  containedFiles.Sorted := true;
  containedFiles.Duplicates := dupIgnore;

  sourceList.Sorted := true;
  sourceList.Duplicates := dupIgnore;

  
  for I := 0 to Count - 1 do
    for j := 0 to self[i].Contains.Count - 1 do
      containedFiles.Add(ExtractFileName(Self[i].Contains[j]));

  AdvBuildFileList(FInitialFolder+'*.pas',faAnyFile,files,amAny, [flFullnames, flRecursive], '', nil);
  
  for I := 0 to files.count - 1 do begin
    if containedFiles.IndexOf(ExtractFileName(files[i])) > 0 then
      sourceList.Add(ExtractFilePath(files[i]));
  end;
end;

function TPackageList.IndexOf(const PackageName: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to count - 1 do begin
    if Get(i).PackageName = PackageName then begin
      result := i;
      break;
    end;
  end;
end;

class function TPackageList.LoadFromFolder(const Folder: String):TPackageList;
begin
  Result := TPackageList.Create;
  SearchFolder(Result, Folder);
  Result.FInitialFolder := ExtractFilePath(Folder);
end;

procedure TPackageList.put(I: Integer; const Value: TPackageInfo);
begin
  inherited put(i,value);
end;

class procedure TPackageList.SearchFolder(var Result: TPackageList; const Folder: string);
var
  str: string;
  packageList: TStringList;
  info: TPackageInfo;
begin
  packageList := TStringList.Create;
  try
    AdvBuildFileList(Folder, faAnyFile, packageList, amAny, [flFullnames, flRecursive], '', nil);
    for str in packageList do
    begin
      info := TPackageInfo.Create(str);
      Result.Add(info);
    end;
  finally
    packageList.Free;
  end;
end;

procedure TPackageList.SortList();
var
  tmp1,tmp2 : TPackageInfo;
  i: Integer;
  j: Integer;
  packagename: string;
  changed : boolean;
begin
  changed := true;
  while changed do begin
    changed := false;
    for I := 0 to Count - 1 do
    begin
      tmp1 := Self[i];
      for packagename in tmp1.Requires do
      begin
        j := indexOf(packagename);
        if (j > i) then
        begin
          Self.Exchange(i,j);
          changed := true;
        end;
      end;
    end;
  end;
end;


end.
