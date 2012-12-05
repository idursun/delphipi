{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit DelphiVersionTreeViewModel;

interface
uses Classes, SysUtils, TreeModel, Utils, Generics.Collections;
type

  TDelphiVersionTreeViewModel<T: INode> = class(TTreeModelBase<T>)
  private
    fNodes: TList<T>;
    function FindDelphiVersionIndexByName(const delphiVersionName: string):Integer;
  protected
    function GetChildren(const parent: T): TList<T>;
  public
    constructor Create(const nodes: TList<T>); virtual;
    function GetChild(const parent: T; index: Integer): T; override;
    function GetChildCount(const parent: T): Integer; override;
  end;

  TCachedDelphiVersionTreeViewModel<T:INode> = class(TDelphiVersionTreeViewModel<T>)
  private
    fLastNodeCount: Integer;
    fCache: array[DELPHI_VERSION_5..DELPHI_LAST_VERSION] of TList<T>;
    procedure RefillCache;
  public
    function GetChild(const parent: T; index: Integer): T; override;
    function GetChildCount(const parent: T): Integer; override;
  end;

implementation
uses JclStrings, JclFileUtils, RegularExpressions;

{ TDelphiVersionTreeViewModel<T> }
constructor TDelphiVersionTreeViewModel<T>.Create(const nodes: TList<T>);
begin
  fNodes := nodes;
end;

function TDelphiVersionTreeViewModel<T>.FindDelphiVersionIndexByName(
  const delphiVersionName: string): Integer;
var
  I: Integer;
begin
  Result := DELPHI_VERSION_UNKNOWN;
  for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
    if StrCompare(VersionNames[i], delphiVersionName) = 0 then
      Result := i;
end;

function TDelphiVersionTreeViewModel<T>.GetChild(const parent: T;
  index: Integer): T;
var
 ret: TList<T>;
begin
  Result := default(T);
  ret := GetChildren(parent);
  if ret.Count > index then
    Result := ret[index];
  ret.Free;
end;

function TDelphiVersionTreeViewModel<T>.GetChildCount(const parent: T): Integer;
var
 ret: TList<T>;
begin
   ret := GetChildren(Parent);
   Result := ret.Count;
   ret.Free;
end;

function TDelphiVersionTreeViewModel<T>.GetChildren(const parent: T): TList<T>;
var
  I: Integer;
  path : string;
  matches: array[DELPHI_VERSION_5..DELPHI_LAST_VERSION] of integer;
  node: T;
  ver, delphiVersionIndex: integer;
begin
  FillChar(matches, Length(matches)*sizeof(Integer), 0);
  Result := TList<T>.Create;
  if parent = nil then
  begin
    for node in fNodes do begin
      path := node.GetNodePath;
      ver := Utils.GuessDelphiVersion(path);
      if ver <> -1 then
        Inc(matches[ver]);
    end;
    for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
      if matches[i] > 0 then
        Result.Add(DoCreateLogicalNode(VersionNames[i],VersionNames[i]));
  end else begin
    delphiVersionIndex := FindDelphiVersionIndexByName(parent.GetNodePath);
    for node in fNodes do begin
      path := node.GetNodePath;
      ver := Utils.GuessDelphiVersion(path);
      if ver = delphiVersionIndex then
        Result.Add(node);
    end;
  end;
end;

procedure TCachedDelphiVersionTreeViewModel<T>.RefillCache;
var
  I: Integer;
  versionIndex: Integer;
  parents, children: TList<T>;
  parentNode: T;
begin
  for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
  begin
    if Assigned(fCache[i]) then
      FreeAndNil(fCache[i]);
    fCache[i] := TList<T>.Create;
  end;

  parents := inherited GetChildren(default(T));
  try
    for parentNode in parents do
    begin
      children := inherited GetChildren(parentNode);
      try
        versionIndex := FindDelphiVersionIndexByName(parentNode.GetNodePath);
        fCache[versionIndex].AddRange(children);
      finally
        children.Free;
      end;
    end;
  finally
    parents.Free;
  end;

  fLastNodeCount := fNodes.Count;
end;

function TCachedDelphiVersionTreeViewModel<T>.GetChild(const parent: T;
  index: Integer): T;
var
  versionIndex: Integer;
begin
  if fLastNodeCount <> fNodes.Count then
    RefillCache;

  Result := default(T);
  if parent = nil then begin
    Result := inherited GetChild(parent, index);
  end else begin
    versionIndex := FindDelphiVersionIndexByName(parent.GetNodePath);
    if versionIndex in [DELPHI_VERSION_5..DELPHI_LAST_VERSION] then
      if index < fCache[versionIndex].Count then
        Result := fCache[versionIndex][index];
  end;
end;

function TCachedDelphiVersionTreeViewModel<T>.GetChildCount(
  const parent: T): Integer;
var
  versionIndex : Integer;
begin
  if fLastNodeCount <> fNodes.Count then
    RefillCache;

  if parent <> nil then begin
    versionIndex := FindDelphiVersionIndexByName(parent.GetNodePath);
    if versionIndex <> -1 then
      Result := fCache[versionIndex].Count;
  end else begin
    Result := inherited GetChildCount(default(T));
  end;
end;

end.
