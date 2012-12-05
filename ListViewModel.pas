{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (ibrahimdursun gmail)
  License : GNU General Public License 2.0
  ** }
unit ListViewModel;

interface
uses Classes, Generics.Collections, TreeModel, PackageInfo;
type
  TListViewModel<T:INode> = class(TTreeModelBase<T>)
  private
    fList: TList<T>;
  public
    constructor Create(list: TList<T>); virtual;
    function GetChild(const parent: T; index: Integer): T; override;
    function GetChildCount(const parent: T): Integer; override;
  end;

implementation

{ TListViewModel<T> }

constructor TListViewModel<T>.Create(list: TList<T>);
begin
  fList := list;
end;

function TListViewModel<T>.GetChild(const parent: T; index: Integer): T;
begin
  Result := default(T);
  if index < fList.Count then
    Result := fList[index];
end;

function TListViewModel<T>.GetChildCount(const parent: T): Integer;
begin
  Result := 0;
  if parent = nil then
    Result := fList.Count
end;

end.
