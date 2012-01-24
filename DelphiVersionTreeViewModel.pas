{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit DelphiVersionTreeViewModel;

interface
uses Classes, SysUtils, TreeModel, Utils, Generics.Collections;
type

  TDelphiVersionTreeViewModel<T: INode> = class(TTreeModelBase<T>)
  private
    fNodes: TList<T>;
    type
      TDelphiVersionArray = array[DELPHI_VERSION_5..DELPHI_LAST_VERSION] of TStringList;
    class procedure AddDelphiPattern(const version: integer; const pattern: string; weight: integer=1);
    function GuessDelphiVersion(name: string;  patterns: TDelphiVersionArray): integer;
    function FindDelphiVersionIndexByName(const delphiVersionName: string):Integer;
    class var patterns : TDelphiVersionArray;
    class constructor Initialize;
    class destructor Deinitialize;
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
function TDelphiVersionTreeViewModel<T>.GuessDelphiVersion(name: string;  patterns: TDelphiVersionArray): integer;
var
  key: string;
  matches: array[DELPHI_VERSION_5..DELPHI_LAST_VERSION] of Integer;
  suffices : TStringList;
  pattern : string;
  max, maxi, i, index : integer;
begin
  Result := -1;
  FillChar(matches, Length(matches)*sizeof(Integer),0 );
  name := JclFileUtils.PathExtractFileNameNoExt(name);
  for i := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
  begin
    for pattern in  patterns[i] do
    begin
       index := Pos(UpperCase(pattern), UpperCase(name));
       if index <> 0 then
         matches[i] := matches[i] + index;
    end;
  end;

  max := 0;
  maxi := -1;
  for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
    if matches[i] > max then
    begin
      max := matches[i];
      maxi := i;
    end;
  Result := maxi;
end;

class procedure TDelphiVersionTreeViewModel<T>.AddDelphiPattern(
  const version: integer; const pattern: string; weight: integer=1);
var
  I: Integer;
begin
  for I := 1 to weight do
    patterns[version].Add(pattern);
end;

constructor TDelphiVersionTreeViewModel<T>.Create(const nodes: TList<T>);
begin
  fNodes := nodes;
end;

class destructor TDelphiVersionTreeViewModel<T>.Deinitialize;
var
 i:integer;
begin
  for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
    patterns[I].Free;
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
      ver := GuessDelphiVersion(path, self.patterns);
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
      ver := GuessDelphiVersion(path, self.patterns);
      if ver = delphiVersionIndex then
        Result.Add(node);
    end;
  end;
end;

class constructor TDelphiVersionTreeViewModel<T>.Initialize;
var
  i:integer;
begin
   for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
    patterns[I] := TStringList.Create;

  AddDelphiPattern(DELPHI_VERSION_5, '5', 2);
  AddDelphiPattern(DELPHI_VERSION_5, 'r5', 2);
  AddDelphiPattern(DELPHI_VERSION_5, 'd5', 2);
  AddDelphiPattern(DELPHI_VERSION_5, '50');
  AddDelphiPattern(DELPHI_VERSION_5, 'd50');
  AddDelphiPattern(DELPHI_VERSION_5, 'delphi5');
  AddDelphiPattern(DELPHI_VERSION_5, '_5');

  AddDelphiPattern(DELPHI_VERSION_6, '_6');
  AddDelphiPattern(DELPHI_VERSION_6, '6', 2);
  AddDelphiPattern(DELPHI_VERSION_6, '60');
  AddDelphiPattern(DELPHI_VERSION_6, 'r6');
  AddDelphiPattern(DELPHI_VERSION_6, 'r60');
  AddDelphiPattern(DELPHI_VERSION_6, 'd6');
  AddDelphiPattern(DELPHI_VERSION_6, 'd60');
  AddDelphiPattern(DELPHI_VERSION_6, 'delphi6');

  AddDelphiPattern(DELPHI_VERSION_7, '7');
  AddDelphiPattern(DELPHI_VERSION_7, 'r7');
  AddDelphiPattern(DELPHI_VERSION_7, 'd7');
  AddDelphiPattern(DELPHI_VERSION_7, '70');
  AddDelphiPattern(DELPHI_VERSION_7, 'r70');
  AddDelphiPattern(DELPHI_VERSION_7, 'd70');
  AddDelphiPattern(DELPHI_VERSION_7, 'delphi7');
  AddDelphiPattern(DELPHI_VERSION_7, '_7');
  AddDelphiPattern(DELPHI_VERSION_7, '_70');

  AddDelphiPattern(DELPHI_VERSION_8, 'D8');
  AddDelphiPattern(DELPHI_VERSION_8, 'D80');
  AddDelphiPattern(DELPHI_VERSION_8, 'NET');
  AddDelphiPattern(DELPHI_VERSION_8, '8');
  AddDelphiPattern(DELPHI_VERSION_8, '80');

  AddDelphiPattern(DELPHI_VERSION_2005, '_9');
  AddDelphiPattern(DELPHI_VERSION_2005, '_90');
  AddDelphiPattern(DELPHI_VERSION_2005, '9');
  AddDelphiPattern(DELPHI_VERSION_2005, '90');
  AddDelphiPattern(DELPHI_VERSION_2005, 'r9');
  AddDelphiPattern(DELPHI_VERSION_2005, 'd9');
  AddDelphiPattern(DELPHI_VERSION_2005, 'r90');
  AddDelphiPattern(DELPHI_VERSION_2005, 'd90');
  AddDelphiPattern(DELPHI_VERSION_2005, 'delphi2005');
  AddDelphiPattern(DELPHI_VERSION_2005, '2005', 2);

  AddDelphiPattern(DELPHI_VERSION_2006, '_10');
  AddDelphiPattern(DELPHI_VERSION_2006, '_100');
  AddDelphiPattern(DELPHI_VERSION_2006, '10');
  AddDelphiPattern(DELPHI_VERSION_2006, 'r10');
  AddDelphiPattern(DELPHI_VERSION_2006, 'd10');
  AddDelphiPattern(DELPHI_VERSION_2006, '100');
  AddDelphiPattern(DELPHI_VERSION_2006, 'r100');
  AddDelphiPattern(DELPHI_VERSION_2006, 'd100');
  AddDelphiPattern(DELPHI_VERSION_2006, '2006',2);
  AddDelphiPattern(DELPHI_VERSION_2006, 'd2006');
  AddDelphiPattern(DELPHI_VERSION_2006, 'delphi2006');

  AddDelphiPattern(DELPHI_VERSION_2007, '11');
  AddDelphiPattern(DELPHI_VERSION_2007, 'r11');
  AddDelphiPattern(DELPHI_VERSION_2007, 'd11');
  AddDelphiPattern(DELPHI_VERSION_2007, '110');
  AddDelphiPattern(DELPHI_VERSION_2007, 'r110');
  AddDelphiPattern(DELPHI_VERSION_2007, 'd110');
  AddDelphiPattern(DELPHI_VERSION_2007, '_11');
  AddDelphiPattern(DELPHI_VERSION_2007, '_110');
  AddDelphiPattern(DELPHI_VERSION_2007, '2007', 2);
  AddDelphiPattern(DELPHI_VERSION_2007, 'd2007');
  AddDelphiPattern(DELPHI_VERSION_2007, 'delphi2007');

  AddDelphiPattern(DELPHI_VERSION_2009,'12');
  AddDelphiPattern(DELPHI_VERSION_2009,'r12');
  AddDelphiPattern(DELPHI_VERSION_2009,'d12');
  AddDelphiPattern(DELPHI_VERSION_2009,'120');
  AddDelphiPattern(DELPHI_VERSION_2009,'d2009');
  AddDelphiPattern(DELPHI_VERSION_2009,'2009');
  AddDelphiPattern(DELPHI_VERSION_2009,'delphi2009');
  AddDelphiPattern(DELPHI_VERSION_2009,'_12');

  AddDelphiPattern(DELPHI_VERSION_2010,'13');
  AddDelphiPattern(DELPHI_VERSION_2010,'14');
  AddDelphiPattern(DELPHI_VERSION_2010,'r13');
  AddDelphiPattern(DELPHI_VERSION_2010,'d13');
  AddDelphiPattern(DELPHI_VERSION_2010,'r14');
  AddDelphiPattern(DELPHI_VERSION_2010,'d14');
  AddDelphiPattern(DELPHI_VERSION_2010,'13');
  AddDelphiPattern(DELPHI_VERSION_2010,'_13');
  AddDelphiPattern(DELPHI_VERSION_2010,'d2010');
  AddDelphiPattern(DELPHI_VERSION_2010,'2010',2);
  AddDelphiPattern(DELPHI_VERSION_2010,'delphi2010');
  AddDelphiPattern(DELPHI_VERSION_2010,'_14');

  AddDelphiPattern(DELPHI_XE,'15');
  AddDelphiPattern(DELPHI_XE,'d15');
  AddDelphiPattern(DELPHI_XE,'r15');
  AddDelphiPattern(DELPHI_XE,'150',2);
  AddDelphiPattern(DELPHI_XE,'d15');
  AddDelphiPattern(DELPHI_XE,'_15');

  AddDelphiPattern(DELPHI_XE2,'16');
  AddDelphiPattern(DELPHI_XE2,'d16',2);
  AddDelphiPattern(DELPHI_XE2,'160');
  AddDelphiPattern(DELPHI_XE2,'d16');
  AddDelphiPattern(DELPHI_XE2,'_16');
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
