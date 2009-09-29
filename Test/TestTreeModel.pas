unit TestTreeModel;

interface

uses
  TestFramework, TreeModel, Classes, Generics.Collections;

type

  TBasicNode = class(TInterfacedObject, INode)
  private
   fName: string;
  public
    constructor Create(name: string);
    function GetData: TObject;
    function GetDisplayName: string;
    function GetNodePath: string;
  end;

  TestTTreeModel = class(TTestCase)
  strict private
    FItems: TObjectList<TBasicNode>;
    FTreeModel: TBasicTreeModel<TBasicNode>;
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Should_return_root_nodes_if_parent_is_nil;
    procedure Should_return_child_nodes_of_specified_parent;
    procedure Should_return_child_node_at_index_0;
    procedure Should_return_child_node_at_index_1;
  end;

implementation
{ TBasicNode }

constructor TBasicNode.Create(name: string);
begin
  fName := name;
end;

function TBasicNode.GetData: TObject;
begin
   Result := nil;
end;

function TBasicNode.getDisplayName: string;
begin
  Result := fName;
end;

function TBasicNode.GetNodePath: string;
begin
  Result := fName;
end;

procedure TestTTreeModel.SetUp;
begin
  FItems := TObjectList<TBasicNode>.Create;
  FItems.Add(TBasicNode.Create('a'));
  FItems.Add(TBasicNode.Create('a\1'));
  FItems.Add(TBasicNode.Create('a\2'));
  FItems.Add(TBasicNode.Create('a\2\3'));
  FItems.Add(TBasicNode.Create('b'));
  FItems.Add(TBasicNode.Create('b\1'));
  FTreeModel := TBasicTreeModel<TBasicNode>.Create(fItems);
end;

procedure TestTTreeModel.TearDown;
begin
  FTreeModel.Free;
  FTreeModel := nil;
end;

procedure TestTTreeModel.Should_return_root_nodes_if_parent_is_nil;
var
  childCount:integer;
begin
  childCount := FTreeModel.GetChildCount(nil);
  CheckEquals(2, childCount, 'node count at level 0 was wrong');
end;

procedure TestTTreeModel.Should_return_child_nodes_of_specified_parent;
var
  childCount:integer;
begin
  childCount := FTreeModel.GetChildCount(TBasicNode.Create('a'));
  CheckEquals(3, childCount, 'child counf of node a was wrong');
end;


procedure TestTTreeModel.Should_return_child_node_at_index_0;
var
  actual: TBasicNode;
begin
  actual := FTreeModel.GetChild(TBasicNode.Create('a'), 0);
  CheckNotNull(actual, 'returned should not be null');
  CheckEquals('a\1', actual.GetNodePath, 'returned node is wrong');
end;

procedure TestTTreeModel.Should_return_child_node_at_index_1;
var
  actual: TBasicNode;
begin
  actual := FTreeModel.GetChild(TBasicNode.Create('a'), 1);
  CheckNotNull(actual, 'returned should not be null');
  CheckEquals('a\2', actual.GetNodePath, 'returned node is wrong');
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTTreeModel.Suite);

end.
