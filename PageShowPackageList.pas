{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
  License : GNU General Public License 2.0
  ** }
unit PageShowPackageList;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Controls, Forms,
  Dialogs, PageBase, ComCtrls, PackageInfo, ImgList, WizardIntfs, Menus, VirtualTrees, PackageDependencyVerifier,
  ActnList, InstalledPackageResolver, TreeModel, ListViewModel, ToolWin, Generics.Collections, TreeNodes;

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Node: TTreeNode;
  end;

  TPackageViewType = (pvtTree, pvtList);

  TShowPackageListPage = class(TWizardPage)
    pmSelectPopupMenu: TPopupMenu;
    miSelectAll: TMenuItem;
    miUnselectAll: TMenuItem;
    miSelectMatching: TMenuItem;
    N1: TMenuItem;
    ImageList: TImageList;
    miUnselectMatching: TMenuItem;
    fPackageTree: TVirtualStringTree;
    lblWait: TLabel;
    N2: TMenuItem;
    miCollapse: TMenuItem;
    miExpand: TMenuItem;
    miCollapseChildren: TMenuItem;
    miCollapseAll: TMenuItem;
    miExpandChildren: TMenuItem;
    miExpandAll: TMenuItem;
    ActionList: TActionList;
    actRemove: TAction;
    Remove1: TMenuItem;
    N3: TMenuItem;
    toolbar: TToolBar;
    btnFolderView: TToolButton;
    sepearator1: TToolButton;
    ilActionImages: TImageList;
    actChangeViewToTree: TAction;
    actChangeViewToList: TAction;
    actSelectAll: TAction;
    actUnselectAll: TAction;
    actSelectMatching: TAction;
    actUnselectMatching: TAction;
    actExpand: TAction;
    actCollapse: TAction;
    btnChangeToListView: TToolButton;
    btnAddFolder: TToolButton;
    actAddPackagesFromFolder: TAction;
    actAddPackage: TAction;
    btnAddPackage: TToolButton;
    seperator2: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure packageTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure packageTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure packageTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure fPackageTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure fPackageTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure fPackageTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure fPackageTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure fPackageTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure fPackageTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

    procedure actRemoveExecute(Sender: TObject);
    procedure actRemoveUpdate(Sender: TObject);
    procedure actChangeViewToTreeExecute(Sender: TObject);
    procedure actChangeViewToListExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actUnselectAllExecute(Sender: TObject);
    procedure actSelectMatchingExecute(Sender: TObject);
    procedure actUnselectMatchingExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actExpandExecute(Sender: TObject);
  private
    packageLoadThread: TThread;
    fSelectMask: string;
    fDependencyVerifier: TPackageDependencyVerifier;
    fInstalledPackageResolver: TInstalledPackageResolver;
    fModel: TTreeModelBase<TTreeNode>;
    fNodes: TList<TTreeNode>;
    procedure PackageLoadCompleted(Sender: TObject);
    procedure ChangeState(Node: PVirtualNode; checkState: TCheckState; recursive:Boolean = true);
    procedure VerifyDependencies;
    procedure SetView(const viewType: TPackageViewType);
  private
    class var criticalSection: TRTLCriticalSection;
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create(Owner: TComponent; const CompilationData: TCompilationData); override;
    procedure UpdateWizardState; override;
  end;

implementation

uses JclFileUtils, gnugettext, Utils, TreeViewModel, PackageLoadThread;
{$R *.dfm}

var
  threadWorking: Boolean;

type
  TVirtualTreeHelper = class helper for TBaseVirtualTree
  private
    procedure InternalTraverse(Node: PVirtualNode; handler: TProc<PVirtualNode>);
  public
    procedure Traverse(Node: PVirtualNode; handler: TProc<PVirtualNode>); overload;
    procedure Traverse(handler: TProc<PVirtualNode>); overload;
    procedure TraverseWithData(handler: TProc<PVirtualNode, PNodeData>);
  end;

  { TShowPackageListPage }
constructor TShowPackageListPage.Create(Owner: TComponent; const CompilationData: TCompilationData);
begin
  inherited;
  fCompilationData := CompilationData;

  if not DirectoryExists(fCompilationData.BaseFolder) then
    exit;

  fInstalledPackageResolver := TInstalledPackageResolver.Create(CompilationData);
  fInstalledPackageResolver.AddDefaultPackageList;
  fInstalledPackageResolver.AddIDEPackageList(CompilationData);
  fDependencyVerifier := TPackageDependencyVerifier.Create;
  fNodes := TList<TTreeNode>.Create;

  SetView(pvtTree);

  packageLoadThread := TPackageLoadThread.Create(fCompilationData, fNodes);
  with packageLoadThread do
  begin
    OnTerminate := PackageLoadCompleted;
    lblWait.Visible := true;
    threadWorking := true;
    fPackageTree.Visible := false;
    Resume;
  end;
end;

class constructor TShowPackageListPage.Create;
begin
  InitializeCriticalSection(criticalSection);
end;

class destructor TShowPackageListPage.Destroy;
begin
  DeleteCriticalSection(criticalSection);
end;

procedure TShowPackageListPage.FormClose(Sender: TObject; var Action: TCloseAction);
var
  node: TTreeNode;
begin
  inherited;
  if threadWorking then
  begin
    TPackageLoadThread(packageLoadThread).Active := false;
    packageLoadThread.WaitFor;
    packageLoadThread.Free;
    exit;
  end;

  for node in fNodes do
  begin
    if node.Selected then
      fCompilationData.PackageList.Add(node.GetData as TPackageInfo);
  end;

  FreeAndNil(fDependencyVerifier);
  FreeAndNil(fInstalledPackageResolver);
  FreeAndNil(fModel);
  FreeAndNil(fNodes);
end;

procedure TShowPackageListPage.FormCreate(Sender: TObject);
begin
  inherited;
  TranslateComponent(Self);
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
begin
  threadWorking := false;
  if TPackageLoadThread(packageLoadThread).Active then
  begin
    fPackageTree.BeginUpdate;
    try
      fPackageTree.RootNodeCount := fModel.GetChildCount(nil);
      fPackageTree.FullExpand;
      VerifyDependencies;
    finally
      fPackageTree.EndUpdate;
      lblWait.Visible := false;
      fPackageTree.Visible := true;
    end;
    UpdateWizardState;
  end;
end;

procedure TShowPackageListPage.ChangeState(Node: PVirtualNode; checkState: TCheckState; recursive:Boolean = true);
var
  data: PNodeData;
  child: PVirtualNode;
begin
  if Node = nil then
    exit;

  Node.checkState := checkState;
  data := fPackageTree.GetNodeData(Node);
  if data <> nil then
    data.Node.Selected := checkState = csCheckedNormal;

  if not recursive then
    exit;

  child := Node.FirstChild;
  while child <> nil do
  begin
    ChangeState(child, checkState, true);
    child := child.NextSibling;
  end;
end;

procedure TShowPackageListPage.UpdateWizardState;
var
  button: TButton;
  selectedPackageCount: integer;
  modelNode: TTreeNode;
begin
  inherited;
  wizard.SetHeader(_('Select Packages'));
  wizard.SetDescription(_('Select packages that you want to compile.'));
  button := wizard.GetButton(wbtNext);
  button.Enabled := (not threadWorking);
  if not threadWorking then
  begin
    button := wizard.GetButton(wbtNext);
    button.Caption := _('Compile');
    selectedPackageCount := 0;
    for modelNode in fNodes do
    begin
      if modelNode.Selected then
        inc(selectedPackageCount);
    end;
    button.Enabled := selectedPackageCount > 0;
  end;
end;

procedure TShowPackageListPage.VerifyDependencies;
var
  selectedPackages: TList<TPackageInfo>;
  info: TPackageInfo;
  modelNode: TTreeNode;
begin
  selectedPackages := TList<TPackageInfo>.Create;
  try
    for modelNode in fNodes do
    begin
      if modelNode.Selected then
        selectedPackages.Add(modelNode.GetData as TPackageInfo);
    end;
    fDependencyVerifier.Verify(selectedPackages, fInstalledPackageResolver);
  finally
    selectedPackages.Free;
  end;

  fPackageTree.BeginUpdate;
  try
    for modelNode in fNodes do
    begin
      if modelNode.NodeType <> ntPackage then
        continue;
      info := modelNode.GetData as TPackageInfo;
      TPackageTreeNode(modelNode).MissingPackageName := fDependencyVerifier.MissingPackages[info.PackageName];
    end;
  finally
    fPackageTree.EndUpdate;
  end;
end;

procedure TShowPackageListPage.packageTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  fPackageTree.BeginUpdate;
  try
    ChangeState(Node, Node.checkState, true);
    VerifyDependencies;
    fPackageTree.InvalidateChildren(Node, true);
  finally
    fPackageTree.EndUpdate;
  end;
  UpdateWizardState;
end;

procedure TShowPackageListPage.packageTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := sizeof(TNodeData);
end;

procedure TShowPackageListPage.SetView(const viewType: TPackageViewType);
begin
  EnterCriticalSection(criticalSection);
  try
    if assigned(fModel) then
      FreeAndNil(fModel);

    case viewType of
      pvtTree:
        fModel := TTreeViewModel<TTreeNode>.Create(fNodes);
      pvtList:
        fModel := TListViewModel<TTreeNode>.Create(fNodes);
    else
      raise Exception.Create('Invalid View');
    end;

    fModel.OnCreateLogicalNode := function(name, path: string): TTreeNode
    begin
      Result := TTreeNode.Create(name, path);
    end;

  finally
    LeaveCriticalSection(criticalSection);
  end;
end;

procedure TShowPackageListPage.packageTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  data: PNodeData;
begin
  if Column <> 0 then
    exit;

  data := Sender.GetNodeData(Node);
  case data.Node.NodeType of
    ntFolder:
      ImageIndex := 0;
    ntPackage:
      ImageIndex := 1;
  end;
end;

procedure TShowPackageListPage.actChangeViewToListExecute(Sender: TObject);
begin
  inherited;
  fPackageTree.BeginUpdate;
  try
    fPackageTree.Clear;
    SetView(pvtList);
    fPackageTree.RootNodeCount := fModel.GetChildCount(nil);
    fPackageTree.FullExpand;
    VerifyDependencies;
  finally
    fPackageTree.EndUpdate;
  end;
end;

procedure TShowPackageListPage.actChangeViewToTreeExecute(Sender: TObject);
begin
  inherited;
  fPackageTree.BeginUpdate;
  try
    fPackageTree.Clear;
    SetView(pvtTree);
    fPackageTree.RootNodeCount := fModel.GetChildCount(nil);
    fPackageTree.FullExpand;
    VerifyDependencies;
  finally
    fPackageTree.EndUpdate;
  end;
end;

procedure TShowPackageListPage.actCollapseExecute(Sender: TObject);
begin
  fPackageTree.FullCollapse(fPackageTree.FocusedNode);
end;

procedure TShowPackageListPage.actExpandExecute(Sender: TObject);
begin
  fPackageTree.FullExpand(fPackageTree.FocusedNode);
end;

procedure TShowPackageListPage.actRemoveExecute(Sender: TObject);
begin
  if fPackageTree.SelectedCount > 0 then
  begin
    fPackageTree.DeleteSelectedNodes;
    VerifyDependencies;
  end;
end;

procedure TShowPackageListPage.actRemoveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := fPackageTree.SelectedCount > 0;
end;

procedure TShowPackageListPage.actSelectAllExecute(Sender: TObject);
begin
  fPackageTree.BeginUpdate;
  try
    ChangeState(fPackageTree.RootNode, csCheckedNormal, true);
    VerifyDependencies;
  finally
    fPackageTree.EndUpdate;
  end;
  UpdateWizardState;
end;

procedure TShowPackageListPage.actSelectMatchingExecute(Sender: TObject);
var
  value: string;
begin
  fPackageTree.BeginUpdate;
  try
    value := fCompilationData.Pattern;
    if InputQuery(_('Select Matching...'), _('File Mask'), value) then
    begin
      fSelectMask := value;
      fPackageTree.TraverseWithData(procedure(Node: PVirtualNode; data: PNodeData)
      var
        info: TPackageInfo;
      begin
        info := data.Node.GetData as TPackageInfo;
        if IsFileNameMatch(info.PackageName + '.dpk', fSelectMask) then
          ChangeState(Node, csCheckedNormal, false);
      end);
      VerifyDependencies;
    end;
  finally
    fPackageTree.EndUpdate;
  end;
  UpdateWizardState;
end;

procedure TShowPackageListPage.actUnselectAllExecute(Sender: TObject);
begin
  fPackageTree.BeginUpdate;
  try
    ChangeState(fPackageTree.RootNode, csUncheckedNormal, true);
    VerifyDependencies;
  finally
    fPackageTree.EndUpdate;
  end;
  UpdateWizardState;
end;

procedure TShowPackageListPage.actUnselectMatchingExecute(Sender: TObject);
var
  value: string;
begin
  fPackageTree.BeginUpdate;
  try
    value := fCompilationData.Pattern;
    if InputQuery(_('Unselect Matching...'), _('File Mask'), value) then
    begin
      fSelectMask := value;
      fPackageTree.TraverseWithData(procedure(Node: PVirtualNode; data: PNodeData)
      var info: TPackageInfo;
      begin
        info := data.Node.GetData as TPackageInfo;
        if IsFileNameMatch(info.PackageName + '.dpk', fSelectMask) then
          ChangeState(Node, csUncheckedNormal, false);
      end);
      VerifyDependencies;
    end;
  finally
    fPackageTree.EndUpdate;
  end;
  UpdateWizardState;
end;

procedure TShowPackageListPage.fPackageTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

var
  data: PNodeData;
  _type: string;
  info: TPackageInfo;
  builder: TStringBuilder;
  modelNode: TPackageTreeNode;
begin
  data := Sender.GetNodeData(Node);
  if data.Node.NodeType <> ntPackage then
    exit;

  modelNode := TPackageTreeNode(data.Node);
  info := modelNode.GetData as TPackageInfo;
  _type := _('Designtime Package');
  if (info.RunOnly) then
    _type := _('Runtime Package');

  builder := TStringBuilder.Create;
  try
    builder.Append(_('FullPath: ') + info.FileName).AppendLine;
    builder.Append(_('Description: ') + info.Description).AppendLine;
    builder.Append(_('Type: ') + _type).AppendLine;
    builder.Append(_('Requires: ')).AppendLine;
    builder.Append(info.RequiredPackageList.Text).AppendLine;
    if modelNode.MissingPackageName <> '' then
      builder.Append(_('Missing Package: ') + modelNode.MissingPackageName);
    HintText := builder.ToString;
  finally
    builder.Free;
  end;
end;

procedure TShowPackageListPage.fPackageTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PNodeData;
  info: TPackageInfo;
begin
  CellText := '';
  data := Sender.GetNodeData(Node);
  info := data.Node.GetData as TPackageInfo;

  case Column of
    0:
      CellText := data.Node.GetDisplayName;
    1:
      if data.Node.NodeType = ntPackage then
        CellText := info.Description;
    2:
      if data.Node.NodeType = ntPackage then
      begin
        if info.RunOnly then
          CellText := _('runtime')
        else
          CellText := _('design');
      end;
  end;
end;

procedure TShowPackageListPage.fPackageTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  data: PNodeData;
begin
  data := Sender.GetNodeData(Node);
  if data.Node.NodeType = ntFolder then
  begin
    ChildCount := fModel.GetChildCount(data.Node);
  end;
end;

procedure TShowPackageListPage.fPackageTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  data: PNodeData;
  parentData: PNodeData;
  parentNodeData: TTreeNode;
begin
  Node.CheckType := ctCheckBox;

  parentData := Sender.GetNodeData(ParentNode);
  parentNodeData := nil;
  if parentData <> nil then
    parentNodeData := parentData.Node;

  data := Sender.GetNodeData(Node);
  data.Node := fModel.GetChild(parentNodeData, Node.Index);
  if data.Node.Selected then
    Node.checkState := csCheckedNormal;

  if fModel.GetChildCount(data.Node) > 0 then
    Include(InitialStates, ivsHasChildren);
end;

procedure TShowPackageListPage.fPackageTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  if CharCode = VK_DELETE then
  begin
    actRemove.Execute;
  end;
end;

procedure TShowPackageListPage.fPackageTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  data: PNodeData;
begin
  inherited;

  if fPackageTree.FocusedNode = Node then
  begin
    TargetCanvas.Font.Color := clWhite;
    exit;
  end;

  data := Sender.GetNodeData(Node);
  if Column = 0 then
  begin
    if (data.Node.NodeType = ntPackage) and (TPackageTreeNode(data.Node).MissingPackageName <> '') then
      TargetCanvas.Font.Color := clRed
    else
      TargetCanvas.Font.Color := clBlack;
  end;
end;

{ TVirtualTreeHelper }

procedure TVirtualTreeHelper.InternalTraverse(Node: PVirtualNode; handler: TProc<PVirtualNode>);
var
  queue: TQueue<PVirtualNode>;
  current, child: PVirtualNode;
begin
  queue := TQueue<PVirtualNode>.Create;
  queue.Enqueue(Node);
  try
    while queue.Count > 0 do
    begin
      current := queue.Dequeue;
      if current.FirstChild <> nil then
      begin
        child := current.FirstChild;
        repeat
          queue.Enqueue(child);
          child := child.NextSibling;
        until child = nil;
      end;

      handler(current);
    end;
  finally
    queue.Free;
  end;
end;

procedure TVirtualTreeHelper.Traverse(Node: PVirtualNode; handler: TProc<PVirtualNode>);
begin
  InternalTraverse(Node, handler)
end;

procedure TVirtualTreeHelper.Traverse(handler: TProc<PVirtualNode>);
begin
  Traverse(RootNode, handler);
end;

procedure TVirtualTreeHelper.TraverseWithData(handler: TProc<PVirtualNode, PNodeData>);
begin
  InternalTraverse(Self.RootNode, procedure(Node: PVirtualNode)
  var
    data: PNodeData;
  begin
    data := Self.GetNodeData(Node);
    if (data <> nil) and (data.Node <> nil) and (data.Node.GetData <> nil) then
      handler(Node, data);
  end);
end;

end.

