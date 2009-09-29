unit TreeModel;
interface

uses Classes, StrUtils, Generics.Collections;
type
  TDynStrArray = array of string;

  INode = interface
     function GetDisplayName: string;
     function GetData: TObject;
     function GetNodePath: string;
  end;

  TTreeView<T: INode > = class
  public
    function GetChild(const parent: T; index: Integer): T; virtual; abstract;
    function GetChildCount(const parent: T): integer; virtual; abstract;
  end;

  TBasicTreeModel<T : INode, class> = class(TTreeView<T>)
  private
    fNodes : TList<T>;
    function SplitString(const str:string; delimiter:string='\'):TDynStrArray;
  public
    constructor Create(const nodes: TList<T>);
    function GetChild(const parent: T; index: Integer): T; override;
    function GetChildCount(const parent: T): integer; override;
  end;

implementation
uses JclStrings;
function TBasicTreeModel<T>.SplitString(const str:string; delimiter:string='\'):TDynStrArray;
var
  I, lastIndex: Integer;
  words : TStringList;
begin
  words := TStringList.Create;
  try
    ExtractStrings(['\'], [], PWideChar(str), words);
    SetLength(Result, words.Count);
    for I := 0 to words.Count - 1 do
      Result[i] := words[i];
  finally
    words.Free;
  end;
end;

{ TBasicTreeModel }
constructor TBasicTreeModel<T>.Create(const nodes: TList<T>);
begin
  fNodes := nodes;
end;

function TBasicTreeModel<T>.GetChild(const parent: T; index: Integer): T;
var
  prefix: string;
  node: T;
  nodePath: string;
  words: TDynStrArray;
  I: Integer;
  currentLevel, nextLevel : integer;
  nodes: TList<T>;
begin
  Result := default(T);

  prefix := '';
  currentLevel := 0;
  nextLevel := 0;
  if parent <> nil then
  begin
    prefix := parent.GetNodePath;
    currentLevel := StrCharCount(prefix, '\');
    nextLevel := currentLevel + 1;
  end;

  nodes := TList<T>.Create;
  try
    for node in fNodes do
    begin
      nodePath := node.GetNodePath;
      if not StartsStr(prefix, nodePath) then
        Continue;

      words := SplitString(nodePath);
      if Length(words) = 0 then
        Continue;

      if Length(words) <= nextLevel then
        Continue;

      nodes.Add(node);
    end;
  finally
    if nodes.Count > index then
      Result := nodes[index];
    nodes.Free;
  end;

end;

function TBasicTreeModel<T>.GetChildCount(const parent: T): integer;
var
  prefix: string;
  node: T;
  nodePath: string;
  words: TDynStrArray;
  I: Integer;
  list: TStringList;
  currentLevel, nextLevel : integer;
begin
  Result := 0;
  prefix := '';
  currentLevel := 0;
  nextLevel := 0;
  if parent <> nil then
  begin
    prefix := parent.GetNodePath;
    currentLevel := StrCharCount(prefix, '\');
    nextLevel := currentLevel + 1;
  end;

  list := TStringList.Create;
  list.Sorted := true;
  list.Duplicates := TDuplicates.dupIgnore;
  try
    for node in fNodes do
    begin
      nodePath := node.GetNodePath;
      if not StartsStr(prefix, nodePath) then
        Continue;

      words := SplitString(nodePath);
      if Length(words) = 0 then
        Continue;
      if Length(words) < nextLevel then
        Continue;

      list.Add(words[nextLevel]);
    end;
  finally
    Result := list.Count;
    list.Free;
  end;
end;
end.
