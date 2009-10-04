{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PackageList;

interface
uses SysUtils, Classes, PackageInfo, Generics.Collections;
type

  TPackageList = class(TObjectList<TPackageInfo>)
  private
  public
    constructor Create;
    function IndexOf(const PackageName: String): Integer; overload;
    procedure SortList;
  end;
implementation
uses Generics.Defaults;
type

  TPackageInfoComparer = class(TInterfacedObject, IComparer<TPackageInfo>)
    function Compare(const Left, Right: TPackageInfo): Integer;
  end;

{ TPackageList }

constructor TPackageList.Create;
begin
  inherited Create(TPackageInfoComparer.Create);
  OwnsObjects := true;
end;

function TPackageList.IndexOf(const PackageName: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to count - 1 do
  begin
    if UpperCase(Self[i].PackageName) = UpperCase(PackageName) then
      exit(i);
  end;
end;

procedure TPackageList.SortList;
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

{ TPackageInfoComparer }

function TPackageInfoComparer.Compare(const Left, Right: TPackageInfo): Integer;
begin
   Result := CompareStr( UpperCase(Left.PackageName), UpperCase(Right.PackageName) );
end;

end.
