unit TestTreeModel;

interface

uses
  TestFramework, TreeModel, Classes, Generics.Collections;

type

  TDumbNode = class(TInterfacedObject, INode)
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
    FItems: TObjectList<TDumbNode>;
    FTreeModel: TTreeViewModel<TDumbNode>;
  private
    function CreateLogicalNode(name, path:string):TDumbNode;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Should_return_root_nodes_if_parent_is_nil;
    procedure Should_return_child_nodes_of_specified_parent;
    procedure Should_return_child_node_at_index_0;
    procedure Should_return_child_node_at_index_1;
    procedure Should_return_count_of_logical_children;
    procedure Should_return_logical_node_if_there_are_not_immediate_children;
    procedure Should_return_count_of_logical_nodes_under_a_parent;
    procedure Should_return_child_at_index_1_under_a_parent;
  end;

implementation
{ TBasicNode }

constructor TDumbNode.Create(name: string);
begin
  fName := name;
end;

function TDumbNode.GetData: TObject;
begin
   Result := nil;
end;

function TDumbNode.getDisplayName: string;
begin
  Result := fName;
end;

function TDumbNode.GetNodePath: string;
begin
  Result := fName;
end;

function TestTTreeModel.CreateLogicalNode(name, path: string): TDumbNode;
begin
  Result := TDumbNode.Create(path);
end;

procedure TestTTreeModel.SetUp;
begin
  FItems := TObjectList<TDumbNode>.Create;
  FItems.Add(TDumbNode.Create('a'));
  FItems.Add(TDumbNode.Create('a\1'));
  FItems.Add(TDumbNode.Create('a\2'));
  FItems.Add(TDumbNode.Create('a\2\3'));
  FItems.Add(TDumbNode.Create('b'));
  FItems.Add(TDumbNode.Create('b\1\1'));
  FItems.Add(TDumbNode.Create('b\1\2'));
  FItems.Add(TDumbNode.Create('c\1\1'));
  FItems.Add(TDumbNode.Create('c\1\2'));
  FItems.Add(TDumbNode.Create('c\2\1'));
  FTreeModel := TTreeViewModel<TDumbNode>.Create(fItems);
  FTreeModel.OnCreateLogicalNode := CreateLogicalNode;
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
  CheckEquals(3, childCount, 'node count at level 0 was wrong');
end;

procedure TestTTreeModel.Should_return_child_nodes_of_specified_parent;
var
  childCount:integer;
begin
  childCount := FTreeModel.GetChildCount(TDumbNode.Create('a'));
  CheckEquals(2, childCount, 'child count of node a was wrong');
end;


procedure TestTTreeModel.Should_return_child_node_at_index_0;
var
  actual: TDumbNode;
begin
  actual := FTreeModel.GetChild(TDumbNode.Create('a'), 0);
  CheckNotNull(actual, 'returned should not be null');
  CheckEquals('a\1', actual.GetNodePath, 'returned node is wrong');
end;

procedure TestTTreeModel.Should_return_child_node_at_index_1;
var
  actual: TDumbNode;
begin
  actual := FTreeModel.GetChild(TDumbNode.Create('a'), 1);
  CheckNotNull(actual, 'returned should not be null');
  CheckEquals('a\2', actual.GetNodePath, 'returned node is wrong');
end;

procedure TestTTreeModel.Should_return_count_of_logical_children;
var
  count : integer;
begin
  count := FTreeModel.GetChildCount(TDumbNode.Create('b'));
  CheckEquals(1,count, 'logical children count was wrong');
end;

procedure TestTTreeModel.Should_return_logical_node_if_there_are_not_immediate_children;
var
  node : TDumbNode;
begin
  node := FTreeModel.GetChild(TDumbNode.Create('b'), 0);
  CheckNotNull(node, 'logical node should not be null');
  CheckEquals('b\1',node.GetNodePath, 'logical node path was wrong');
end;

procedure TestTTreeModel.Should_return_count_of_logical_nodes_under_a_parent;
var
  count: integer;
begin
  count := FTreeModel.GetChildCount(TDumbNode.Create('c'));
  CheckEquals(2, count, 'logical node count was wrong');
end;

procedure TestTTreeModel.Should_return_child_at_index_1_under_a_parent;
var
  node: TDumbNode;
begin
  node := FTreeModel.GetChild(TDumbNode.Create('c'),1);
  CheckEquals('c\2', node.GetNodePath, 'logical child was wrong');
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTTreeModel.Suite);

end.
