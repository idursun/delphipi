unit DelphiVersionTreeViewModel;

interface
uses Classes, SysUtils, TreeModel, Utils, Generics.Collections;

const
  DELPHI_VERSION_UNKNOWN = -1;
  DELPHI_VERSION_5 = 0;
  DELPHI_VERSION_6 = 1;
  DELPHI_VERSION_7 = 2;
  DELPHI_VERSION_8 = 3;
  DELPHI_VERSION_2005 = 4;
  DELPHI_VERSION_2006 = 5;
  DELPHI_VERSION_2007 = 6;
  DELPHI_VERSION_2009 = 7;
  DELPHI_VERSION_2010 = 8;
  VersionNames: array[DELPHI_VERSION_UNKNOWN..DELPHI_VERSION_2010] of string = ('Unknown', 'Delphi 5','Delphi 6','Delphi 7','Delphi 8','Delphi 2005', 'Delphi 2006', 'Delphi 2007','Delphi 2009','Delphi 2010');

type

  TDelphiVersionTreeViewModel<T: INode> = class(TTreeModelBase<T>)
  private
    fNodes: TList<T>;
    type
      TDelphiVersionArray = array[DELPHI_VERSION_5..DELPHI_VERSION_2010] of TStringList;
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
    fCache: array[DELPHI_VERSION_5..DELPHI_VERSION_2010] of TList<T>;
    procedure RefillCache;
  public
    function GetChild(const parent: T; index: Integer): T; override;
    function GetChildCount(const parent: T): Integer; override;
  end;

implementation
uses JclStrings, JclFileUtils;

{ TDelphiVersionTreeViewModel<T> }
function TDelphiVersionTreeViewModel<T>.GuessDelphiVersion(name: string;  patterns: TDelphiVersionArray): integer;
var
  key: string;
  matches: array[DELPHI_VERSION_5..DELPHI_VERSION_2010] of Integer;
  suffices : TStringList;
  pattern : string;
  max, maxi, i, index : integer;
begin
  Result := -1;
  FillChar(matches, Length(matches)*sizeof(Integer),0 );
  name := JclFileUtils.PathExtractFileNameNoExt(name);
  for i := DELPHI_VERSION_5 to DELPHI_VERSION_2010 do
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
  for I := DELPHI_VERSION_5 to DELPHI_VERSION_2010 do
    if matches[i] > max then
    begin
      max := matches[i];
      maxi := i;
    end;
  Result := maxi;
end;

constructor TDelphiVersionTreeViewModel<T>.Create(const nodes: TList<T>);
begin
  fNodes := nodes;
end;

class destructor TDelphiVersionTreeViewModel<T>.Deinitialize;
var
 i:integer;
begin
  for I := DELPHI_VERSION_5 to DELPHI_VERSION_2010 do
    patterns[I].Free;
end;

function TDelphiVersionTreeViewModel<T>.FindDelphiVersionIndexByName(
  const delphiVersionName: string): Integer;
var
  I: Integer;
begin
  Result := DELPHI_VERSION_UNKNOWN;
  for I := DELPHI_VERSION_5 to DELPHI_VERSION_2010 do
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
  matches: array[DELPHI_VERSION_5..DELPHI_VERSION_2010] of integer;
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
    for I := DELPHI_VERSION_5 to DELPHI_VERSION_2010 do
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
   for I := DELPHI_VERSION_5 to DELPHI_VERSION_2010 do
    patterns[I] := TStringList.Create;

  patterns[DELPHI_VERSION_5].Add('5');
  patterns[DELPHI_VERSION_5].Add('r5');
  patterns[DELPHI_VERSION_5].Add('d5');
  patterns[DELPHI_VERSION_5].Add('50');
  patterns[DELPHI_VERSION_5].Add('d5');
  patterns[DELPHI_VERSION_5].Add('d50');
  patterns[DELPHI_VERSION_5].Add('delphi5');
  patterns[DELPHI_VERSION_5].Add('_5');

  patterns[DELPHI_VERSION_6].Add('6');
  patterns[DELPHI_VERSION_6].Add('r6');
  patterns[DELPHI_VERSION_6].Add('d6');
  patterns[DELPHI_VERSION_6].Add('60');
  patterns[DELPHI_VERSION_6].Add('d6');
  patterns[DELPHI_VERSION_6].Add('d60');
  patterns[DELPHI_VERSION_6].Add('delphi6');
  patterns[DELPHI_VERSION_6].Add('_6');

  patterns[DELPHI_VERSION_7].Add('7');
  patterns[DELPHI_VERSION_7].Add('r7');
  patterns[DELPHI_VERSION_7].Add('d7');
  patterns[DELPHI_VERSION_7].Add('70');
  patterns[DELPHI_VERSION_7].Add('d7');
  patterns[DELPHI_VERSION_7].Add('d70');
  patterns[DELPHI_VERSION_7].Add('delphi7');
  patterns[DELPHI_VERSION_7].Add('_7');

  patterns[DELPHI_VERSION_2005].Add('9');
  patterns[DELPHI_VERSION_2005].Add('r9');
  patterns[DELPHI_VERSION_2005].Add('d9');
  patterns[DELPHI_VERSION_2005].Add('90');
  patterns[DELPHI_VERSION_2005].Add('d9');
  patterns[DELPHI_VERSION_2005].Add('delphi2005');
  patterns[DELPHI_VERSION_2005].Add('d2005');
  patterns[DELPHI_VERSION_2005].Add('2005');
  patterns[DELPHI_VERSION_2005].Add('2005'); // added twice on purpose
  patterns[DELPHI_VERSION_2005].Add('_9');

  patterns[DELPHI_VERSION_2006].Add('10');
  patterns[DELPHI_VERSION_2006].Add('r10');
  patterns[DELPHI_VERSION_2006].Add('d10');
  patterns[DELPHI_VERSION_2006].Add('100');
  patterns[DELPHI_VERSION_2006].Add('d10');
  patterns[DELPHI_VERSION_2006].Add('2006');
  patterns[DELPHI_VERSION_2006].Add('2006'); // added twice on purpose
  patterns[DELPHI_VERSION_2006].Add('d2006');
  patterns[DELPHI_VERSION_2006].Add('delphi2006');
  patterns[DELPHI_VERSION_2006].Add('_10');

  patterns[DELPHI_VERSION_2007].Add('11');
  patterns[DELPHI_VERSION_2007].Add('r11');
  patterns[DELPHI_VERSION_2007].Add('d11');
  patterns[DELPHI_VERSION_2007].Add('110');
  patterns[DELPHI_VERSION_2007].Add('d11');
  patterns[DELPHI_VERSION_2007].Add('2007');
  patterns[DELPHI_VERSION_2007].Add('2007'); // added twice on purpose
  patterns[DELPHI_VERSION_2007].Add('d2007');
  patterns[DELPHI_VERSION_2007].Add('delphi2007');
  patterns[DELPHI_VERSION_2007].Add('_11');

  patterns[DELPHI_VERSION_2009].Add('12');
  patterns[DELPHI_VERSION_2009].Add('r12');
  patterns[DELPHI_VERSION_2009].Add('d12');
  patterns[DELPHI_VERSION_2009].Add('120');
  patterns[DELPHI_VERSION_2009].Add('d2009');
  patterns[DELPHI_VERSION_2009].Add('2009');
  patterns[DELPHI_VERSION_2009].Add('2009');
  patterns[DELPHI_VERSION_2009].Add('delphi2009');
  patterns[DELPHI_VERSION_2009].Add('_12');

  patterns[DELPHI_VERSION_2010].Add('13');
  patterns[DELPHI_VERSION_2010].Add('14');
  patterns[DELPHI_VERSION_2010].Add('d13');
  patterns[DELPHI_VERSION_2010].Add('d14');
  patterns[DELPHI_VERSION_2010].Add('r13');
  patterns[DELPHI_VERSION_2010].Add('d14');
  patterns[DELPHI_VERSION_2010].Add('130');
  patterns[DELPHI_VERSION_2010].Add('140');
  patterns[DELPHI_VERSION_2010].Add('2010');
  patterns[DELPHI_VERSION_2010].Add('d2010');
  patterns[DELPHI_VERSION_2010].Add('delphi2010');
  patterns[DELPHI_VERSION_2010].Add('d13');
  patterns[DELPHI_VERSION_2010].Add('d14');
  patterns[DELPHI_VERSION_2010].Add('_13');
  patterns[DELPHI_VERSION_2010].Add('_14');
end;

procedure TCachedDelphiVersionTreeViewModel<T>.RefillCache;
var
  I: Integer;
  versionIndex: Integer;
  parents, children: TList<T>;
  parentNode: T;
begin
  for I := DELPHI_VERSION_5 to DELPHI_VERSION_2010 do
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
    if versionIndex in [DELPHI_VERSION_5..DELPHI_VERSION_2010] then
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
