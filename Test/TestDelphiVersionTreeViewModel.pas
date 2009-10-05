unit TestDelphiVersionTreeViewModel;
interface

uses
  TestFramework, TreeModel, Utils, Generics.Collections, Classes,
  DelphiVersionTreeViewModel, SysUtils;

type

  TBasicNode = class(TInterfacedObject, INode)
  private
    fPath: string;
  public
    constructor Create(const path: string);
    function GetDisplayName: string;
    function GetData: TObject;
    function GetNodePath: string;
  end;

  TestTDelphiVersionTreeViewModel = class(TTestCase)
  strict private
    fModel: TDelphiVersionTreeViewModel<TBasicNode>;
    fNodes: TObjectList<TBasicNode>;
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Should_return_only_recognized_delphi_version_count;
    procedure Should_return_nodes_under_specified_delphi_version;
    procedure Should_return_node_at_1_for_Delphi_7;
    procedure Should_return_node_at_1_for_Delphi_2009;
  end;

implementation

procedure TestTDelphiVersionTreeViewModel.SetUp;
begin
  fNodes := TObjectList<TBasicNode>.Create;
  fModel := TDelphiVersionTreeViewModel<TBasicNode>.Create(fNodes);
end;

procedure TestTDelphiVersionTreeViewModel.TearDown;
begin
  fModel.Free;
  fModel := nil;
  fNodes.Free;
end;

procedure TestTDelphiVersionTreeViewModel.Should_return_only_recognized_delphi_version_count;
var
  actual : integer;
begin
  fNodes.Add(TBasicNode.Create('p70'));
  fNodes.Add(TBasicNode.Create('D70'));
  fNodes.Add(TBasicNode.Create('G70'));
  fNodes.Add(TBasicNode.Create('p60'));
  actual := fModel.GetChildCount(nil);
  CheckEquals(2,actual, 'Delphi version count was wrong');
end;

procedure TestTDelphiVersionTreeViewModel.Should_return_nodes_under_specified_delphi_version;
var
  actual : integer;
begin
  fNodes.Add(TBasicNode.Create('p70'));
  fNodes.Add(TBasicNode.Create('D70'));
  fNodes.Add(TBasicNode.Create('G70'));
  fNodes.Add(TBasicNode.Create('p60'));
  actual := fModel.GetChildCount(TBasicNode.Create('Delphi 7'));
  CheckEquals(3,actual, 'package count of Delphi 7 was wrong');
end;

procedure TestTDelphiVersionTreeViewModel.Should_return_node_at_1_for_Delphi_7;
var
  actual : TBasicNode;
begin
  fNodes.Add(TBasicNode.Create('p70'));
  fNodes.Add(TBasicNode.Create('D70'));
  fNodes.Add(TBasicNode.Create('G70'));
  fNodes.Add(TBasicNode.Create('p60'));
  actual := fModel.GetChild(TBasicNode.Create('Delphi 7'), 1);
  CheckEquals('D70',actual.GetDisplayName, 'returned package was wrong');
end;

procedure TestTDelphiVersionTreeViewModel.Should_return_node_at_1_for_Delphi_2009;
var
  actual : TBasicNode;
begin
  fNodes.Add(TBasicNode.Create('p70'));
  fNodes.Add(TBasicNode.Create('D70'));
  fNodes.Add(TBasicNode.Create('pd12'));
  fNodes.Add(TBasicNode.Create('p12'));
  fNodes.Add(TBasicNode.Create('p_12'));
  actual := fModel.GetChild(TBasicNode.Create('Delphi 2009'), 2);
  CheckEquals('p12',actual.GetDisplayName, 'returned package was wrong');
end;


{ TBasicNode }

constructor TBasicNode.Create(const path: string);
begin
  fPath := path;
end;

function TBasicNode.GetData: TObject;
begin
  Result := nil;
end;

function TBasicNode.GetDisplayName: string;
begin
  Result := fPath;
end;

function TBasicNode.GetNodePath: string;
begin
  Result := fPath;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTDelphiVersionTreeViewModel.Suite);
end.

