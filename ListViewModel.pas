unit ListViewModel;

interface
uses Classes, Generics.Collections, TreeModel, PackageInfo;
type
  TListViewModel<T:INode> = class(TTreeView<T>)
  private
    fList: TList<T>;
  public
    constructor Create(list: TList<T>); virtual;
    destructor Destroy; override;
    function GetChild(const parent: T; index: Integer): T; override;
    function GetChildCount(const parent: T): Integer; override;
  end;

implementation

{ TListViewModel<T> }

constructor TListViewModel<T>.Create(list: TList<T>);
begin
  fList := list;
end;

destructor TListViewModel<T>.Destroy;
begin

  inherited;
end;

function TListViewModel<T>.GetChild(const parent: T; index: Integer): T;
begin
  Result := default(T);
  if index < fList.Count then
    Result := fList[index];
end;

function TListViewModel<T>.GetChildCount(const parent: T): Integer;
begin
  Result := fList.Count;
end;

end.
