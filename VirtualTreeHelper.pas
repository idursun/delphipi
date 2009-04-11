unit VirtualTreeHelper;

interface
uses VirtualTrees, Classes, SysUtils, PackageInfo;
type
  PNodeData = ^TNodeData;
  TNodeData = record
    Name: string;
    Info: TPackageInfo;
    MissingPackageName: string;
  end;

  TVirtualTreeHelper = class helper for TBaseVirtualTree
  private
    procedure InternalTraverse(node: PVirtualNode; level: integer;
      handler: TProc<PVirtualNode>);
  public
    procedure Traverse(node: PVirtualNode; handler: TProc<PVirtualNode>); overload;
    procedure Traverse(handler: TProc<PVirtualNode>); overload;
    procedure TraverseData(handler: TProc<PNodeData>);
    procedure TraverseWithData(handler: TProc<PVirtualNode, PNodeData>);
  end;
implementation

{ TVirtualTreeHelper }

procedure TVirtualTreeHelper.InternalTraverse(node: PVirtualNode; level: integer; handler: TProc<PVirtualNode>);
begin
  if node = nil then exit;
  
  handler(node);

  if node.ChildCount > 0 then
    InternalTraverse(node.FirstChild, level, handler);

  if (node.NextSibling <> nil) and (node <> node.NextSibling) and (level < GetNodeLevel(node)) then
    InternalTraverse(node.NextSibling, level, handler);
end;

procedure TVirtualTreeHelper.Traverse(node: PVirtualNode;
  handler: TProc<PVirtualNode>);
begin
  if node = RootNode then
    InternalTraverse(node, -1, handler)
  else
    InternalTraverse(node, GetNodeLevel(node), handler);
end;


procedure TVirtualTreeHelper.TraverseWithData(handler: TProc<PVirtualNode, PNodeData>);
begin
  InternalTraverse(Self.RootNode, -1, procedure (node: PVirtualNode) 
  var
    data: PNodeData;
  begin
    data := Self.GetNodeData(node);
    if (data <> nil) and (data.Info <> nil) then
      handler(node, data);
  end);
end;


procedure TVirtualTreeHelper.Traverse(handler:TProc<PVirtualNode>);
begin
  Traverse(RootNode, handler);
end;

procedure TVirtualTreeHelper.TraverseData(handler: TProc<PNodeData>);
var
  data: PNodeData;
begin
  Traverse(procedure (node: PVirtualNode)
  begin
    data := self.GetNodeData(node);
    if (data <> nil) and (data.Info <> nil)  then
      handler(data); 
  end);
end;
end.
