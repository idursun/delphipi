{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageShowPackageList;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Controls, Forms,
  Dialogs, PageBase, ComCtrls, PackageInfo, ImgList, WizardIntfs, Menus, VirtualTrees;

type
  TShowPackageListPage = class(TWizardPage)
    SelectPopupMenu: TPopupMenu;
    miSelectAll: TMenuItem;
    miUnselectAll: TMenuItem;
    miSelectMatching: TMenuItem;
    N1: TMenuItem;
    ImageList: TImageList;
    miUnselectMatching: TMenuItem;
    fPackageTree: TVirtualStringTree;
    lblWait: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure packageTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure packageTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure packageTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure packageTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
    procedure miSelectAllClick(Sender: TObject);
    procedure miUnselectAllClick(Sender: TObject);
    procedure miSelectMatchingClick(Sender: TObject);
    procedure miUnselectMatchingClick(Sender: TObject);
    procedure packageTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
  private
    packageLoadThread: TThread;
    //fPackageTree : TVirtualStringTree;
    fProcessedPackageInfo: TPackageInfo;
    fSelectMask: string;
    procedure PackageLoadCompleted(Sender: TObject);
    procedure ChangeState(node: PVirtualNode; checkState: TCheckState);

    procedure PackageInfoCollectCallBack(const node: PVirtualNode);
    procedure CheckRequiredPackagesCallBack(const node: PVirtualNode);
    procedure UncheckDependentPackagesCallBack(const node: PVirtualNode);
    procedure SelectAllCallBack(const node: PVirtualNode);
    procedure UnselectAllCallBack(const node: PVirtualNode);
    procedure SelectMatchingCallBack(const node: PVirtualNode);
    procedure UnSelectMatchingCallBack(const node: PVirtualNode);
  public
    constructor Create(Owner: TComponent; const compilationData: TCompilationData); override;
    procedure UpdateWizardState; override;
  end;

implementation
uses JclFileUtils, gnugettext;

{$R *.dfm}
type
  PNodeData = ^TNodeData;
  TNodeData = record
    Name: string;
    Info: TPackageInfo;
  end;
var
  threadWorking : Boolean;
type

  TPackageLoadThread = class(TThread)
  private
    fCompilationData: TCompilationData;
    fTree: TBaseVirtualTree;
    procedure BuildFileNodes(parent: PVirtualNode; directory: string);
  protected
    procedure Execute; override;
    procedure Search(parent: PVirtualNode; folder: String);
    procedure CleanTree(node: PVirtualNode);
  public
    constructor Create(data: TCompilationData; tree: TBaseVirtualTree);
  end;

  TVirtualNodeHandler = procedure(const node: PVirtualNode) of object;
  TVirtualTreeHelper = class helper for TBaseVirtualTree
  public
    procedure Traverse(node: PVirtualNode; handler: TVirtualNodeHandler); overload;
    procedure Traverse(handler: TVirtualNodeHandler); overload;
  end;


{ TShowPackageListPage }

constructor TShowPackageListPage.Create(Owner: TComponent;
  const compilationData: TCompilationData);
begin
  inherited;
  fCompilationData := compilationData;
//  CreateVirtualTree;

  packageLoadThread := TPackageLoadThread.Create(FCompilationData, fPackageTree);
  with packageLoadThread do begin
    FreeOnTerminate := true;
    OnTerminate := packageLoadCompleted;
    lblWait.Visible := true;
    //packageListView.AddItem(_('Looking for packages in folders...'),nil);
    threadWorking := true;
    Resume;
  end;
end;

procedure TShowPackageListPage.ChangeState(node: PVirtualNode;
  checkState: TCheckState);
var
  child: PVirtualNode;
  //data: PNodeData;
begin
 if node = nil then exit;
  node.CheckState := checkState;
  // code below unchecks dependent packages when a required package is unchecked, vice versa
//  data := fPackageTree.GetNodeData(node);
//  if data.info <> nil then
//  begin
//    fProcessedPackageInfo := data.info;
//    if node.CheckState = csUncheckedNormal then
//      fPackageTree.Traverse(fPackageTree.RootNode, UncheckDependentPackagesCallBack);
//    if node.CheckState = csCheckedNormal then
//      fPackageTree.Traverse(fPackageTree.RootNode, CheckRequiredPackagesCallBack);
//  end;

  child := node.FirstChild;
  while child <> nil do
  begin
     ChangeState(child, checkState);
     child := child.NextSibling;
  end;
end;

procedure TShowPackageListPage.CheckRequiredPackagesCallBack(
  const node: PVirtualNode);
var
  data: PNodeData;  
begin
  if node.CheckState = csCheckedNormal then exit;
  if fProcessedPackageInfo = nil then exit;
  
  data := fPackageTree.GetNodeData(node);
  if (data = nil) or (data.Info = nil) then exit;
  if fProcessedPackageInfo.DependsOn(data.Info) then
    node.CheckState := csCheckedNormal;
end;

procedure TShowPackageListPage.UncheckDependentPackagesCallBack(
  const node: PVirtualNode);
var
  data: PNodeData;
begin
  if node.CheckState = csUncheckedNormal then exit;
  if fProcessedPackageInfo = nil then exit;

  data := fPackageTree.GetNodeData(node);
  if (data = nil) or (data.Info = nil) then exit;
  if data.Info.DependsOn(fProcessedPackageInfo) then
    node.CheckState := csUncheckedNormal;
end;

procedure TShowPackageListPage.UnselectAllCallBack(const node: PVirtualNode);
begin
  node.CheckState := csUncheckedNormal;
end;

procedure TShowPackageListPage.UnSelectMatchingCallBack(
  const node: PVirtualNode);
var
  data: PNodeData;
begin
  data := fPackageTree.GetNodeData(node);
  if (data <> nil) and (data.Info <> nil) then
  begin
    if IsFileNameMatch(data.Info.PackageName + '.dpk', fSelectMask) then
       node.CheckState := csUncheckedNormal;
  end;
end;

procedure TShowPackageListPage.SelectAllCallBack(const node: PVirtualNode);
begin
  node.CheckState := csCheckedNormal;
end;

procedure TShowPackageListPage.SelectMatchingCallBack(const node: PVirtualNode);
var
  data: PNodeData;
begin
  data := fPackageTree.GetNodeData(node);
  if (data <> nil) and (data.Info <> nil) then
  begin
    if IsFileNameMatch(data.Info.PackageName + '.dpk', fSelectMask) then
       node.CheckState := csCheckedNormal;
  end;
end;

procedure TShowPackageListPage.UpdateWizardState;
var
  button: TButton;
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
  end;
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
begin
  threadWorking := false;
  lblWait.Visible := false;
  fPackageTree.FullExpand;
  UpdateWizardState;
end;

procedure TShowPackageListPage.packageTreeChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  ChangeState(Node, Node.CheckState);
  fPackageTree.InvalidateChildren(Node,true);
end;

procedure TShowPackageListPage.packageTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: UnicodeString);
var
  data: PNodeData;
  _type: string;
  info : TPackageInfo;
begin
  data := Sender.GetNodeData(Node);
  if data.Info <> nil then
  begin
    info := data.Info;
    _type := _('Designtime Package');
    if (info.RunOnly) then
      _type := _('Runtime Package');
    HintText := _('FullPath:')+ info.FileName+#13#10+
                _('Description:')+ info.Description+#13#10+
                _('Type:')+ _type+#13#10+
                _('Requires:')+ #13#10 + info.RequiredPackageList.Text;
  end;
end;

procedure TShowPackageListPage.packageTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := sizeof(TNodeData);
end;

procedure TShowPackageListPage.packageTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  data : PNodeData;
begin
  CellText := '';
  data := Sender.GetNodeData(Node);
  case Column of
    0: CellText := data.Name;
    1: if data.Info <> nil then
         CellText := data.Info.Description;
    2: if data.Info <> nil then
         if data.Info.RunOnly then
           CellText := _('runtime')
         else
           CellText := _('design');
  end;
end;

procedure TShowPackageListPage.packageTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  data: PNodeData;  
begin
  if column <> 0 then exit;
  data := Sender.GetNodeData(Node);
  if data.Info = nil then
    ImageIndex := 0
  else
    ImageIndex := 1;  
end;

procedure TShowPackageListPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  if threadWorking then begin
    packageLoadThread.Suspend;
    packageLoadThread.Terminate;
    exit;
  end;
  fPackageTree.Traverse(fPackageTree.RootNode, packageInfoCollectCallBack);
  fCompilationData.PackageList.Pack;
end;

procedure TShowPackageListPage.PackageInfoCollectCallBack(
  const node: PVirtualNode);
var
  data: PNodeData;  
begin
  if node.CheckState <> csCheckedNormal then exit;
  data := fPackageTree.GetNodeData(node);
  if data.Info <> nil then
    fCompilationData.PackageList.Add(data.Info);
end;

procedure TShowPackageListPage.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
end;

procedure TShowPackageListPage.miSelectAllClick(Sender: TObject);
begin
  fPackageTree.Traverse(SelectAllCallBack);
  fPackageTree.Invalidate;
end;

procedure TShowPackageListPage.miSelectMatchingClick(Sender: TObject);
var
  value : string;
begin
  value := fCompilationData.Pattern;
  if InputQuery(_('Select Matching...'),_('File Mask'),value) then
  begin
    fSelectMask := value;
    fPackageTree.Traverse(SelectMatchingCallBack);
    fPackageTree.Invalidate;
  end;
end;

procedure TShowPackageListPage.miUnselectAllClick(Sender: TObject);
begin
  fPackageTree.Traverse(UnselectAllCallBack);
  fPackageTree.Invalidate;
end;

procedure TShowPackageListPage.miUnselectMatchingClick(Sender: TObject);
var
  value: string;
begin
  value := fCompilationData.Pattern;
  if InputQuery(_('UnSelect Matching...'),_('File Mask'),value) then
  begin
    fSelectMask := value;
    fPackageTree.Traverse(UnSelectMatchingCallBack);
    fPackageTree.Invalidate;
  end;
end;

{ TPackageLoadThread }

constructor TPackageLoadThread.Create(data: TCompilationData; tree: TBaseVirtualTree);
begin
  inherited Create(true);
  fCompilationData := data;
  fTree:= tree;
end;

procedure TPackageLoadThread.CleanTree(node: PVirtualNode);
var
 c: PVirtualNode;
 data: PNodeData;
begin
  if node = nil then exit;
  if node.ChildCount > 0 then
  begin
     c := node.FirstChild;
     while c <> nil do
     begin
         data := fTree.GetNodeData(c);
         if data.Info = nil then
           CleanTree(c);
         c := c.NextSibling;
     end;
  end;

  if node.ChildCount = 0 then
    fTree.DeleteNode(node);
end;

procedure TPackageLoadThread.Execute;
begin
  inherited;
  fTree.BeginUpdate;
  try
    Search(fTree.RootNode, fCompilationData.BaseFolder);
    CleanTree(fTree.RootNode);
  finally
    fTree.EndUpdate;
  end;
end;

procedure TPackageLoadThread.BuildFileNodes(parent: PVirtualNode; directory: string);
var
  sr: TSearchRec;
  child: PVirtualNode;
  data: PNodeData;
begin
  if FindFirst(directory + '\' + fCompilationData.Pattern, faAnyFile, sr) = 0 then
  begin
    try
      repeat
        if ExtractFileExt(sr.Name) = '.dpk' then
        begin
          child := fTree.AddChild(parent);
          child.CheckState := csCheckedNormal;
          child.CheckType := ctCheckBox;
          data := fTree.GetNodeData(child);
          data.Name := sr.Name;
          data.Info := TPackageInfo.Create(directory + sr.Name);
        end;
      until FindNext(Sr) <> 0;
    finally
       FindClose(sr);
    end;
  end;
end;

procedure TPackageLoadThread.Search(parent: PVirtualNode; folder: String);
var
  child: PVirtualNode;
  data: PNodeData;
  directoryList : TStringList;
  directory: string;
begin
  directoryList := TStringList.Create;
  try
    BuildFileList(folder + '\*.*', faDirectory, directoryList);
    for directory in directoryList do
    begin
      child := fTree.AddChild(parent);
      child.CheckState := csCheckedNormal;
      child.CheckType := ctCheckBox;
      child.States := child.States + [vsHasChildren];
      data := fTree.GetNodeData(child);
      data.Name := directory;
      Search(child, folder + '\' + directory);
    end;
    BuildFileNodes(parent, folder + '\' + directory);
  finally
    directoryList.Free;
  end;
end;

{ TVirtualTreeHelper }

procedure TVirtualTreeHelper.Traverse(node: PVirtualNode;
  handler: TVirtualNodeHandler);
begin
  if node = nil then exit;
  handler(node);
  
  if node.ChildCount > 0 then
    Traverse(node.FirstChild, handler);

  if (node.NextSibling <> nil) and (node <> node.NextSibling) then
    Traverse(node.NextSibling, handler);
end;

procedure TVirtualTreeHelper.Traverse(handler: TVirtualNodeHandler);
begin
  Traverse(RootNode, handler);
end;

end.
