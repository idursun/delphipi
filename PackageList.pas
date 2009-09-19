{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageList;

interface
uses SysUtils, Classes, PackageInfo;
type

  TPackageList = class(TList)
  private
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
  end;
implementation

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
  for I := 0 to count - 1 do
  begin
    if UpperCase(Get(i).PackageName) = UpperCase(PackageName) then 
      exit(i);
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

//TODO: encapsulate in another class, sorting list by considering dependencies
procedure TPackageList.SortList();
var
  tmp : TPackageInfo;
  i,j: Integer;
  packagename: string;
  changed : boolean;
begin
  changed := true;
  while changed do
  begin
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
