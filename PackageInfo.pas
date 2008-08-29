{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageInfo;
interface
uses  Classes, StrUtils;

type

  TPackageStatus = (psNone, psCompiling, psInstalling, psSuccess, psError);
  TPackageInfo = class(TInterfacedObject)
  private
    pfile : TStringList;
    FRequires: TStringList;
    FContains : TStringList;
    FDescription : String;
    FSuffix : String;
    FRunOnly : Boolean;
    FPackageName : String;
    FFileName: String;
    fStatus: TPackageStatus;
    procedure ReadInfo;
    function ClearStr(Str: string):string;
    function GetContainedFileList: TStringList;
    function GetRequiredPackageList: TStringList; overload;
  public
    constructor Create(const packagefile: String);overload;
    constructor Create;overload;
    destructor Destroy; override;
    function DependsOn(const package: TPackageInfo): Boolean; overload;
    
    property Description: string read FDescription;
    property PackageName: string read FPackageName;
    property RunOnly: Boolean read FRunOnly;
    property Suffix: string read FSuffix;
    property RequiredPackageList: TStringList read GetRequiredPackageList;
    property ContainedFileList: TStringList read GetContainedFileList;
    property FileName: string read FFileName;
    property Status: TPackageStatus read fStatus write fStatus;
  end;

  TPackageList = class(TList)
  private
    FInitialFolder : String;
    function get(I: Integer): TPackageInfo;
    procedure put(I: Integer; const Value: TPackageInfo);
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Item[I : Integer]: TPackageInfo read get write put;default;
    procedure Add(const item : TPackageInfo); overload;
    procedure Remove(const item: TPackageInfo); overload;
    function IndexOf(const PackageName: String):Integer; overload;
    procedure SortList();
    
    property InitialFolder: String read FInitialFolder write FInitialFolder;
  end;

implementation
uses  SysUtils, contnrs;

resourcestring
  StrRUNONLY = '{$RUNONLY';
  StrDESCRIPTION = '{$DESCRIPTION ';
  StrSUFFIX = '{$LIBSUFFIX ';
  StrPackage = 'package';
  StrRequires = 'requires';
  StrContains = 'contains';

function SimplifyPath(const Str: string):string;
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

function TPackageInfo.GetContainedFileList: TStringList;
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

destructor TPackageInfo.Destroy;
begin
  pfile.clear;
  RequiredPackageList.free;
  ContainedFileList.Free;
  inherited;
end;

function TPackageInfo.ClearStr(Str: string):String;
begin
  Result := Trim(Str);
  Result := ReplaceStr(Result, ',', '');
  Result := ReplaceStr(Result, ';', '');
end;

// ugly code, but implementing a full blown parser here is like nailing with a sledge hammer
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
      RequiredPackageList.Add(ClearStr(Str));
    end;
    if (RequiresBlock) and (pos(';',str) > 0) then
      RequiresBlock := False;

    if Pos(StrContains,str) = 1 then ContainsBlock := True;

    if ContainsBlock then
      if Pos(' in ',Str) > 0 then
        ContainedFileList.Add(Copy(Str,Pos('''',Str)+1,Length(Str) - Pos('''',Str)-2))
      else
        ContainedFileList.Add(ClearStr(str)+ '.pas');

    if (ContainsBlock) and (pos(';',str) > 0) then
      ContainsBlock := False;
  end;

  if RequiredPackageList.Count > 0 then
    RequiredPackageList.Delete(0);
  if ContainedFileList.Count > 0 then
    ContainedFileList.Delete(0);
end;

function TPackageInfo.DependsOn(const package: TPackageInfo): Boolean;
begin
  assert(package <> nil);
  Result := RequiredPackageList.IndexOf(package.packageName) > -1;
end;

function TPackageInfo.GetRequiredPackageList: TStringList;
begin
  if not assigned(FRequires) then
    FRequires := TStringList.Create;
  Result := FRequires;
end;

{ TPackageList }

procedure TPackageList.Add(const item: TPackageInfo);
begin
  inherited add(item);
end;

procedure TPackageList.Clear;
var
  I: Integer;
  package: TPackageInfo;
begin
  inherited;
  for I := 0 to self.Count - 1 do
  begin
    package := self[i] as TPackageInfo;
    if package <> nil then
       package.Free;
  end;
end;

destructor TPackageList.Destroy;
begin

  inherited;
end;

function TPackageList.get(I: Integer): TPackageInfo;
begin
  Result := TPackageInfo(inherited get(i));
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

procedure TPackageList.put(I: Integer; const Value: TPackageInfo);
begin
  inherited put(i,value);
end;

procedure TPackageList.Remove(const item: TPackageInfo);
var i :integer;
begin
  i := IndexOf(item.PackageName);
  Delete(i);
end;  

procedure TPackageList.SortList();
var
  tmp : TPackageInfo;
  i,j: Integer;
  packagename: string;
  changed : boolean;
begin
  changed := true;
  while changed do begin
    changed := false;
    for i := 0 to Count - 1 do
    begin
      tmp := Self[i];
      for packagename in tmp.RequiredPackageList do
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
