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
  ActnList, InstalledPackageResolver, TreeModel, ListViewModel, ToolWin, Generics.Collections;

type

  TNodeType = (ntNode, ntFolder, ntPackage);
  TTreeNode = class(TInterfacedObject, INode)
  private
    fName: string;
    fPath: string;
    fSelected: Boolean;
    fNodeType: TNodeType;
  public
    constructor Create(const name, path: string);
    function GetData: TObject; virtual;
    function GetDisplayName: string;  virtual;
    function GetNodePath: string; virtual;
    property NodeType: TNodeType read fNodeType;
    property Selected: Boolean read fSelected write fSelected;
  end;

  TPackageTreeNode = class(TTreeNode)
  private
    fInfo: TPackageInfo;
  public
    constructor Create(const info: TPackageInfo); virtual;

    function GetData: TObject; override;
    function GetDisplayName: string; override;
    function GetNodePath: string; override;
  end;

  PNodeData = ^TNodeData;
  TNodeData = record
    Node: TTreeNode;
    MissingPackageName: string;
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
    procedure actRemoveExecute(Sender: TObject);
    procedure actRemoveUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fPackageTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure fPackageTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
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
    fModel : TTreeModelBase<TTreeNode>;
    fNodes : TList<TTreeNode>;
    procedure PackageLoadCompleted(Sender: TObject);
    procedure ChangeState(Node: PVirtualNode; checkState: TCheckState);
    procedure VerifyDependencies;
    procedure SetView(const viewType:TPackageViewType);
  private
    class var criticalSection: TRTLCriticalSection;
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create(Owner: TComponent; const CompilationData: TCompilationData); override;
    procedure UpdateWizardState; override;
  end;

implementation
uses JclFileUtils, JclStrings, gnugettext, Utils, PackageInfoFactory, VirtualTreeHelper, TreeViewModel;

{$R *.dfm}

var
  threadWorking: Boolean;

type
  TPackageLoadThread = class(TThread)
  private
    fCompilationData: TCompilationData;
    fPackageInfoFactory: TPackageInfoFactory;
    fActive: Boolean;
    fList: TList<TTreeNode>;
    procedure LoadPackageInformations(const directory: string);
  protected
    procedure Execute; override;
    procedure Search(const folder: String);
  public
    constructor Create(data: TCompilationData; list: TList<TTreeNode>);
    destructor Destroy; override;
    property Active: Boolean read fActive write fActive;
  end;

  TVirtualTreeHelper = class helper for TBaseVirtualTree
  private
    procedure InternalTraverse(node: PVirtualNode; handler: TProc<PVirtualNode>);
  public
    procedure Traverse(node: PVirtualNode; handler: TProc<PVirtualNode>); overload;
    procedure Traverse(handler: TProc<PVirtualNode>); overload;
    procedure TraverseData(handler: TProc<PNodeData>);
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
begin
  inherited;
  if threadWorking then
  begin
    TPackageLoadThread(packageLoadThread).Active := false;
    packageLoadThread.WaitFor;
    packageLoadThread.Free;
    exit;
  end;

  fPackageTree.Traverse(fPackageTree.RootNode, procedure(Node: PVirtualNode)
  var
    data: PNodeData;
  begin
    if Node.checkState <> csCheckedNormal then
      exit;

    data := fPackageTree.GetNodeData(Node);
    if (data.Node.NodeType = ntPackage) then
      fCompilationData.PackageList.Add(data.Node.GetData as TPackageInfo);
  end);

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

procedure TShowPackageListPage.ChangeState(Node: PVirtualNode; checkState: TCheckState);
var
  child: PVirtualNode;
  data: PNodeData;
begin
  if Node = nil then
    exit;
  Node.checkState := checkState;
  data := fPackageTree.GetNodeData(Node);
  if data <> nil then
    data.Node.Selected := checkState = csCheckedNormal;

  child := Node.FirstChild;
  while child <> nil do
  begin
    ChangeState(child, checkState);
    child := child.NextSibling;
  end;
end;

procedure TShowPackageListPage.UpdateWizardState;
var
  button: TButton;
  selectedPackageCount: integer;
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
    fPackageTree.Traverse( procedure(Node: PVirtualNode)
    begin
      if Node.checkState = csCheckedNormal then
        inc(selectedPackageCount);
    end);
    button.Enabled := selectedPackageCount > 0;
  end;
end;

procedure TShowPackageListPage.VerifyDependencies;
var
  selectedPackages: TList<TPackageInfo>;
begin
  selectedPackages := TList<TPackageInfo>.Create;
  try
    fPackageTree.Traverse(procedure(Node: PVirtualNode)
    var
      data: PNodeData;
    begin
      data := fPackageTree.GetNodeData(Node);
      
      if Node.checkState <> csCheckedNormal then
        exit;

      if (data <> nil) and (data.Node.NodeType = ntPackage) then
         selectedPackages.Add(data.Node.GetData as TPackageInfo);
    end);

    fDependencyVerifier.Verify(selectedPackages, fInstalledPackageResolver);
  finally
    selectedPackages.Free;
  end;

  fPackageTree.BeginUpdate;
  try
    fPackageTree.TraverseData( procedure(data: PNodeData)
    var
      info: TPackageInfo;
    begin
      info := data.Node.GetData as TPackageInfo;
      if info = nil then
        exit;

      data.MissingPackageName := fDependencyVerifier.MissingPackages[info.PackageName];
    end);
  finally
    fPackageTree.EndUpdate;
  end;
end;

procedure TShowPackageListPage.packageTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  fPackageTree.BeginUpdate;
  try
    ChangeState(Node, Node.checkState);
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
      pvtTree: fModel := TTreeViewModel<TTreeNode>.Create(fNodes);
      pvtList: fModel := TListViewModel<TTreeNode>.Create(fNodes);
    else
        raise Exception.Create('Invalid View');
    end;

    fModel.OnCreateLogicalNode := function (name, path:string) : TTreeNode
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
     ntFolder: ImageIndex := 0;
     ntPackage: ImageIndex := 1;
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
  finally
    fPackageTree.EndUpdate;
    VerifyDependencies;
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
  finally
   fPackageTree.EndUpdate;
    VerifyDependencies;
  end;
end;

procedure TShowPackageListPage.actCollapseExecute(Sender: TObject);
begin
  inherited;
  fPackageTree.FullCollapse(fPackageTree.FocusedNode);
end;

procedure TShowPackageListPage.actExpandExecute(Sender: TObject);
begin
  inherited;
  fPackageTree.FullExpand(fPackageTree.FocusedNode);
end;

procedure TShowPackageListPage.actRemoveExecute(Sender: TObject);
begin
  inherited;
  if fPackageTree.SelectedCount > 0 then
  begin
    fPackageTree.DeleteSelectedNodes;
    VerifyDependencies;
  end;
end;

procedure TShowPackageListPage.actRemoveUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := fPackageTree.SelectedCount > 0;
end;

procedure TShowPackageListPage.actSelectAllExecute(Sender: TObject);
begin
  fPackageTree.BeginUpdate;
  try
    fPackageTree.Traverse( procedure(Node: PVirtualNode)
    begin
      Node.checkState := csCheckedNormal;
    end);
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
      fPackageTree.TraverseWithData( procedure(Node: PVirtualNode; data: PNodeData)
      var
        info:TPackageInfo;
      begin
        info := data.Node.GetData as TPackageInfo;
        if IsFileNameMatch(info.PackageName + '.dpk', fSelectMask) then
           Node.checkState := csCheckedNormal;
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
    fPackageTree.Traverse( procedure(Node: PVirtualNode)
    begin
      Node.checkState := csUncheckedNormal;
    end);
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
    if InputQuery(_('UnSelect Matching...'), _('File Mask'), value) then
    begin
      fSelectMask := value;
      fPackageTree.TraverseWithData( procedure(Node: PVirtualNode; data: PNodeData)
      var
        info: TPackageInfo;
      begin
        info := data.Node.GetData as TPackageInfo;
        if IsFileNameMatch(info.PackageName + '.dpk', fSelectMask) then
            Node.checkState := csUncheckedNormal;
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
begin
  data := Sender.GetNodeData(Node);
  if data.Node.NodeType <> ntPackage then
    Exit;

  info := data.Node.GetData as TPackageInfo;
  _type := _('Designtime Package');
  if (info.RunOnly) then
    _type := _('Runtime Package');

  builder := TStringBuilder.Create;
  try
    builder.Append(_('FullPath:') + info.FileName).AppendLine;
    builder.Append(_('Description:') + info.Description).AppendLine;
    builder.Append(_('Type:') + _type).AppendLine;
    builder.Append(_('Requires:')).AppendLine;
    builder.Append(info.RequiredPackageList.Text).AppendLine;
    if data.MissingPackageName <> '' then
      builder.Append(_('Missing Package:') + data.MissingPackageName);
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
    0:CellText := data.Node.GetDisplayName;
    1:if data.Node.NodeType = ntPackage then
        CellText := info.Description;
    2:if data.Node.NodeType = ntPackage then
      begin
        if info.RunOnly then
          CellText := _('runtime')
        else
          CellText := _('design');
      end;
  end;

end;

procedure TShowPackageListPage.fPackageTreeInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  data: PNodeData;
begin
  inherited;
  data := Sender.GetNodeData(Node);
  if data.Node.NodeType = ntFolder then
  begin
    ChildCount := fModel.GetChildCount(data.Node);
  end;
end;

procedure TShowPackageListPage.fPackageTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data: PNodeData;
  parentData: PNodeData;
  parentNodeData: TTreeNode;
begin
  inherited;
  Node.CheckType := ctCheckBox;

  parentData := Sender.GetNodeData(ParentNode);
  parentNodeData := nil;
  if parentData <> nil then
    parentNodeData := parentData.Node;

  data := Sender.GetNodeData(Node);
  data.Node := fModel.GetChild(parentNodeData, Node.Index);
  if data.Node.Selected then
    Node.CheckState := csCheckedNormal;

  if fModel.GetChildCount(data.Node) > 0 then
    InitialStates := [ivsHasChildren];
end;

procedure TShowPackageListPage.fPackageTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  inherited;
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
    if (data.Node.NodeType = ntPackage) and (data.MissingPackageName <> '') then
      TargetCanvas.Font.Color := clRed
    else
      TargetCanvas.Font.Color := clBlack;
  end;
end;

{ TPackageLoadThread }

constructor TPackageLoadThread.Create(data: TCompilationData; list: TList<TTreeNode>);
begin
  inherited Create(true);
  fCompilationData := data;
  fList := list;
  fPackageInfoFactory := TPackageInfoFactory.Create;
end;

destructor TPackageLoadThread.Destroy;
begin
  fPackageInfoFactory.Free;
  inherited;
end;

procedure TPackageLoadThread.Execute;
begin
  inherited;
  fActive := true;
  try
    try
      Search(fCompilationData.BaseFolder);
    except
      on e: Exception do
        ShowMessage(e.Message);
    end;
  finally
  end;
end;

procedure TPackageLoadThread.LoadPackageInformations(const directory: string);
var
  sr: TSearchRec;
begin
  if FindFirst(PathAppend(directory,fCompilationData.Pattern), faAnyFile, sr) = 0 then
  begin
    try
      repeat
        if ExtractFileExt(sr.Name) = '.dpk' then
          fList.Add( TPackageTreeNode.Create(fPackageInfoFactory.CreatePackageInfo(PathAppend(directory, sr.Name))));
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

procedure TPackageLoadThread.Search(const folder: String);
var
  directoryList: TStringList;
  directory: string;
begin
  if not fActive then
    exit;

  directoryList := TStringList.Create;
  try
    BuildFileList(PathAppend(folder,'*.*'), faDirectory, directoryList);
    for directory in directoryList do
    begin
      Search(PathAppend(folder, directory));
    end;
    LoadPackageInformations(folder);
  finally
    directoryList.Free;
  end;
end;
{ TTreeNode }

constructor TTreeNode.Create(const name, path: string);
begin
  fName := name;
  fPath := path;
  fNodeType := ntFolder;
  fSelected := true;
end;

function TTreeNode.GetData: TObject;
begin
  Result := nil;
end;

function TTreeNode.GetDisplayName: string;
begin
  Result := fName;
end;

function TTreeNode.GetNodePath: string;
begin
  Result := fPath;
end;

{ TPackageTreeNode }

constructor TPackageTreeNode.Create(const info: TPackageInfo);
begin
  fInfo := info;
  fNodeType := ntPackage;
  fSelected := true;
end;

function TPackageTreeNode.GetData: TObject;
begin
  Result:= fInfo;
end;

function TPackageTreeNode.GetDisplayName: string;
begin
  Result := fInfo.PackageName;
end;

function TPackageTreeNode.GetNodePath: string;
var
  i: integer;
begin
  Result := fInfo.FileName;
  i := Pos(':', Result);
  if i <> 0 then
    Result := StrRestOf(Result, i+2);
end;

{ TVirtualTreeHelper }

procedure TVirtualTreeHelper.InternalTraverse(node: PVirtualNode; handler: TProc<PVirtualNode>);
var
  queue : TQueue<PVirtualNode>;
  current, child : PVirtualNode;
begin
  queue := TQueue<PVirtualNode>.Create;
  queue.Enqueue(node);
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

procedure TVirtualTreeHelper.Traverse(node: PVirtualNode; handler: TProc<PVirtualNode>);
begin
  InternalTraverse(node, handler)
end;

procedure TVirtualTreeHelper.Traverse(handler: TProc<PVirtualNode>);
begin
  Traverse(RootNode, handler);
end;


procedure TVirtualTreeHelper.TraverseWithData(handler: TProc<PVirtualNode, PNodeData>);
begin
  InternalTraverse(Self.RootNode, procedure(node: PVirtualNode)
  var
    data: PNodeData;
  begin
    data := Self.GetNodeData(node);
    if (data <> nil) and (data.Node <> nil) and (data.Node.GetData <> nil) then
      handler(node, data);
  end);
end;

procedure TVirtualTreeHelper.TraverseData(handler: TProc<PNodeData>);
begin
  Traverse( procedure(node: PVirtualNode)
  var
    data: PNodeData;
  begin
    data := Self.GetNodeData(node);
    //Assert(data.Node <> nil, 'there should not be any node without model node');
    if (data <> nil) and (data.Node.GetData <> nil) then
      handler(data);
  end);
end;

end.
