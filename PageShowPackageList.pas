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
    procedure packageTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure miSelectAllClick(Sender: TObject);
    procedure miUnselectAllClick(Sender: TObject);
    procedure miSelectMatchingClick(Sender: TObject);
    procedure miUnselectMatchingClick(Sender: TObject);
    procedure packageTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure fPackageTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure fPackageTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure fPackageTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    packageLoadThread: TThread;
    fSelectMask: string;
    procedure PackageLoadCompleted(Sender: TObject);
    procedure ChangeState(node: PVirtualNode; checkState: TCheckState);

    procedure ByCollectingPackageInfo(node: PVirtualNode);
  public
    constructor Create(Owner: TComponent; const compilationData: TCompilationData); override;
    procedure UpdateWizardState; override;
  end;

implementation
uses JclFileUtils, gnugettext, Utils, PackageInfoFactory, PackageDependencyVerifier;

{$R *.dfm}
type
  PNodeData = ^TNodeData;
  TNodeData = record
    Name: string;
    Info: TPackageInfo;
    MissingPackageName: string;
  end;
var
  threadWorking : Boolean;
type

  TPackageLoadThread = class(TThread)
  private
    fCompilationData: TCompilationData;
    fPackageInfoFactory: TPackageInfoFactory;
    fTree: TBaseVirtualTree;
    fActive: Boolean;
    procedure BuildFileNodes(parent: PVirtualNode; directory: string);
  protected
    procedure Execute; override;
    procedure Search(parent: PVirtualNode; folder: String);
    procedure RemoveEmptyFolderNodes(node: PVirtualNode);
  public
    constructor Create(data: TCompilationData; tree: TBaseVirtualTree);
    destructor Destroy; override;
    property Active: boolean read fActive write fActive;
  end;

  TVirtualTreeHelper = class helper for TBaseVirtualTree
  public
    procedure Traverse(node: PVirtualNode; handler: TProc<PVirtualNode>); overload;
    procedure Traverse(handler: TProc<PVirtualNode>); overload;
  end;


{ TShowPackageListPage }

constructor TShowPackageListPage.Create(Owner: TComponent;
  const compilationData: TCompilationData);
begin
  inherited;
  fCompilationData := compilationData;
  packageLoadThread := TPackageLoadThread.Create(FCompilationData, fPackageTree);
  with packageLoadThread do begin
    OnTerminate := packageLoadCompleted;
    lblWait.Visible := true;
    threadWorking := true;
    fPackageTree.Visible := false;
    Resume;
  end;
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
var
  dependencyVerifier: TPackageDependencyVerifier;
begin
  threadWorking := false;
  if TPackageLoadThread(packageLoadThread).Active then
  begin
    lblWait.Visible := false;
    fPackageTree.Visible := True;
    fPackageTree.FullExpand;
    
    fCompilationData.PackageList.Clear;  //TODO: why?
    dependencyVerifier := TPackageDependencyVerifier.Create(fCompilationData);
    dependencyVerifier.Initialize;
    try
      fPackageTree.Traverse(ByCollectingPackageInfo);
      dependencyVerifier.Verify;
      
      fPackageTree.Traverse(procedure(node:PVirtualNode) 
      var
        data: PNodeData;
      begin
        data := fPackageTree.GetNodeData(node);
        if (data = nil) or (data.Info = nil) then exit;
        data.MissingPackageName := dependencyVerifier.MissingPackages[data.Info.PackageName];
      end);
      
    finally
      dependencyVerifier.Free;
    end;
    UpdateWizardState;
  end;
end;

procedure TShowPackageListPage.ChangeState(node: PVirtualNode;
  checkState: TCheckState);
var
  child: PVirtualNode;
begin
 if node = nil then exit;
  node.CheckState := checkState;
  child := node.FirstChild;
  while child <> nil do
  begin
     ChangeState(child, checkState);
     child := child.NextSibling;
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


procedure TShowPackageListPage.packageTreeChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  ChangeState(Node, Node.CheckState);
  fPackageTree.InvalidateChildren(Node,true);
end;

procedure TShowPackageListPage.packageTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := sizeof(TNodeData);
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
    TPackageLoadThread(packageLoadThread).Active := false;
    packageLoadThread.WaitFor;
    packageLoadThread.Free;
    exit;
  end;
  fCompilationData.PackageList.Clear;
  fPackageTree.Traverse(fPackageTree.RootNode, ByCollectingPackageInfo);
  fCompilationData.PackageList.Pack;
end;

procedure TShowPackageListPage.ByCollectingPackageInfo(
  node: PVirtualNode);
var
  data: PNodeData;  
begin
  if node.CheckState <> csCheckedNormal then exit;
  data := fPackageTree.GetNodeData(node);
  if (data <> nil) and (data.Info <> nil) then
    fCompilationData.PackageList.Add(data.Info);
end;

procedure TShowPackageListPage.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
end;

procedure TShowPackageListPage.fPackageTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

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
    if data.MissingPackageName <> '' then
      HintText := HintText +#13#10 + _('Missing Package:')  + data.MissingPackageName;
  end;
end;

procedure TShowPackageListPage.fPackageTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
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

procedure TShowPackageListPage.fPackageTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  data: PNodeData;  
begin
  inherited;
  data := Sender.GetNodeData(Node);
  if Column = 0 then
  begin
     if (data.Info <> nil) and (data.MissingPackageName <> '') then
       TargetCanvas.Font.Color := clRed
     else
       TargetCanvas.Font.Color := clBlack;
  end;
end;

procedure TShowPackageListPage.miSelectAllClick(Sender: TObject);
begin
  fPackageTree.Traverse(procedure (node:PVirtualNode) begin
     node.CheckState := csCheckedNormal;
  end);
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
    fPackageTree.Traverse(procedure (node:PVirtualNode)
    var
      data: PNodeData;
    begin
        data := fPackageTree.GetNodeData(node);
        if (data <> nil) and (data.Info <> nil) then
        begin
          if IsFileNameMatch(data.Info.PackageName + '.dpk', fSelectMask) then
          node.CheckState := csCheckedNormal;
        end;
    end);
    fPackageTree.Invalidate;
  end;
end;

procedure TShowPackageListPage.miUnselectAllClick(Sender: TObject);
begin
  fPackageTree.Traverse(procedure (node:PVirtualNode) begin
    node.CheckState := csUncheckedNormal;
  end);
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
    fPackageTree.Traverse(procedure (node:PVirtualNode)
    var
      data: PNodeData;
    begin
      data := fPackageTree.GetNodeData(node);
      if (data <> nil) and (data.Info <> nil) then
      begin
        if IsFileNameMatch(data.Info.PackageName + '.dpk', fSelectMask) then
        node.CheckState := csUncheckedNormal;
      end;
    end);
    fPackageTree.Invalidate;
  end;
end;

{ TPackageLoadThread }

constructor TPackageLoadThread.Create(data: TCompilationData; tree: TBaseVirtualTree);
begin
  inherited Create(true);
  fCompilationData := data;
  fTree:= tree;
  fPackageInfoFactory := TPackageInfoFactory.Create;
end;

destructor TPackageLoadThread.Destroy;
begin
  fPackageInfoFactory.Free;   
  inherited;
end;

procedure TPackageLoadThread.RemoveEmptyFolderNodes(node: PVirtualNode);
var
  c: PVirtualNode;
  data: PNodeData;
begin
  if not fActive then exit;
  if node = nil then exit;
  
  c := fTree.GetFirstChild(node);
  while c <> nil do
  begin
    data := fTree.GetNodeData(c);
    if data.Info = nil then
      RemoveEmptyFolderNodes(c);
    c := fTree.GetNextSibling(c);
  end;
  if node.FirstChild = nil then
    fTree.DeleteNode(node);
end;

procedure TPackageLoadThread.Execute;
begin
  inherited;
  fActive := true;
  fTree.BeginUpdate;
  try
    try
      Search(fTree.RootNode, fCompilationData.BaseFolder);
      RemoveEmptyFolderNodes(fTree.RootNode);
    except
      on e:Exception do ShowMessage(e.Message);
    end;
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
  if FindFirst(PathAddSeparator(directory) + fCompilationData.Pattern, faAnyFile, sr) = 0 then
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
          data.Info := fPackageInfoFactory.CreatePackageInfo(PathAddSeparator(directory) + sr.Name);
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
  if not fActive then exit;
  
  directoryList := TStringList.Create;
  try
    BuildFileList(PathAddSeparator(folder) + '*.*', faDirectory, directoryList);
    for directory in directoryList do
    begin
      child := fTree.AddChild(parent);
      child.CheckState := csCheckedNormal;
      child.CheckType := ctCheckBox;
      child.States := child.States + [vsHasChildren];
      data := fTree.GetNodeData(child);
      data.Name := directory;
      Search(child, PathAddSeparator(folder) + directory);
    end;
    BuildFileNodes(parent, PathAddSeparator(folder) + directory);
  finally
    directoryList.Free;
  end;
end;

{ TVirtualTreeHelper }

procedure TVirtualTreeHelper.Traverse(node: PVirtualNode;
  handler: TProc<PVirtualNode>);
begin
  if node = nil then exit;
  handler(node);
  
  if node.ChildCount > 0 then
    Traverse(node.FirstChild, handler);

  if (node.NextSibling <> nil) and (node <> node.NextSibling) then
    Traverse(node.NextSibling, handler);
end;

procedure TVirtualTreeHelper.Traverse(handler:TProc<PVirtualNode>);
begin
  Traverse(RootNode, handler);
end;

end.
