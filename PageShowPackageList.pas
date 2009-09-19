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
  ActnList;

type

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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure packageTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure packageTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure miSelectAllClick(Sender: TObject);
    procedure miUnselectAllClick(Sender: TObject);
    procedure miSelectMatchingClick(Sender: TObject);
    procedure miUnselectMatchingClick(Sender: TObject);
    procedure packageTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure fPackageTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure fPackageTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure fPackageTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure miCollapseAllClick(Sender: TObject);
    procedure miCollapseChildrenClick(Sender: TObject);
    procedure miExpandChildrenClick(Sender: TObject);
    procedure miExpandAllClick(Sender: TObject);
    procedure fPackageTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure actRemoveExecute(Sender: TObject);
  private
    packageLoadThread: TThread;
    fSelectMask: string;
    fDependencyVerifier: TPackageDependencyVerifier;
    procedure PackageLoadCompleted(Sender: TObject);
    procedure ChangeState(Node: PVirtualNode; checkState: TCheckState);

    procedure ByCollectingPackageInfo(Node: PVirtualNode);
    procedure VerifyDependencies;
  public
    constructor Create(Owner: TComponent; const CompilationData: TCompilationData); override;
    procedure UpdateWizardState; override;
  end;

implementation

uses JclFileUtils, gnugettext, Utils, PackageInfoFactory, VirtualTreeHelper;
{$R *.dfm}

var
  threadWorking: Boolean;

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
    procedure RemoveEmptyFolderNodes(Node: PVirtualNode);
  public
    constructor Create(data: TCompilationData; tree: TBaseVirtualTree);
    destructor Destroy; override;
    property Active: Boolean read fActive write fActive;
  end;

  { TShowPackageListPage }

constructor TShowPackageListPage.Create(Owner: TComponent; const CompilationData: TCompilationData);
begin
  inherited;
  fCompilationData := CompilationData;

  if not DirectoryExists(fCompilationData.BaseFolder) then
    exit;

  fDependencyVerifier := TPackageDependencyVerifier.Create(fCompilationData);
  fDependencyVerifier.Initialize;

  packageLoadThread := TPackageLoadThread.Create(fCompilationData, fPackageTree);
  with packageLoadThread do
  begin
    OnTerminate := PackageLoadCompleted;
    lblWait.Visible := true;
    threadWorking := true;
    fPackageTree.Visible := false;
    Resume;
  end;
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
  fCompilationData.PackageList.Clear;
  fPackageTree.Traverse(fPackageTree.RootNode, ByCollectingPackageInfo);
  fCompilationData.PackageList.Pack;
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
begin

  threadWorking := false;
  if TPackageLoadThread(packageLoadThread).Active then
  begin
    lblWait.Visible := false;
    fPackageTree.Visible := true;
    fPackageTree.FullExpand;

    VerifyDependencies;
    UpdateWizardState;
  end;
end;

procedure TShowPackageListPage.ChangeState(Node: PVirtualNode; checkState: TCheckState);
var
  child: PVirtualNode;
begin
  if Node = nil then
    exit;
  Node.checkState := checkState;
  child := Node.FirstChild;
  while child <> nil do
  begin
    ChangeState(child, checkState);
    child := child.NextSibling;
  end;
  VerifyDependencies;
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
    fPackageTree.Traverse( procedure(Node: PVirtualNode)begin if Node.checkState = csCheckedNormal then inc(selectedPackageCount); end);
    button.Enabled := selectedPackageCount > 0;
  end;
end;

procedure TShowPackageListPage.VerifyDependencies;
begin
  fCompilationData.PackageList.Clear;
  fPackageTree.Traverse(ByCollectingPackageInfo);
  fDependencyVerifier.Verify;
  fPackageTree.TraverseData( procedure(data: PNodeData)
  begin
    data.MissingPackageName := fDependencyVerifier.MissingPackages[data.Info.PackageName];
  end);
  fPackageTree.Invalidate;
end;

procedure TShowPackageListPage.packageTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  ChangeState(Node, Node.checkState);
  fPackageTree.InvalidateChildren(Node, true);
  UpdateWizardState;
end;

procedure TShowPackageListPage.packageTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := sizeof(TNodeData);
end;

procedure TShowPackageListPage.packageTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  data: PNodeData;
begin
  if Column <> 0 then
    exit;
  data := Sender.GetNodeData(Node);
  if data.Info = nil then
    ImageIndex := 0
  else
    ImageIndex := 1;
end;

procedure TShowPackageListPage.actRemoveExecute(Sender: TObject);
begin
  inherited;
  if fPackageTree.FocusedNode <> nil then
  begin
    fPackageTree.DeleteNode(fPackageTree.FocusedNode);
    VerifyDependencies;
  end;
end;

procedure TShowPackageListPage.ByCollectingPackageInfo(Node: PVirtualNode);
var
  data: PNodeData;
begin
  if Node.checkState <> csCheckedNormal then
    exit;
  data := fPackageTree.GetNodeData(Node);
  if (data <> nil) and (data.Info <> nil) then
    fCompilationData.PackageList.Add(data.Info);
end;

procedure TShowPackageListPage.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
end;

procedure TShowPackageListPage.fPackageTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

var
  data: PNodeData;
  _type: string;
  info: TPackageInfo;
  builder : TStringBuilder;
begin
  data := Sender.GetNodeData(Node);
  if data.Info <> nil then
  begin
    info := data.Info;
    _type := _('Designtime Package');
    if (info.RunOnly) then
      _type := _('Runtime Package');

    builder := TStringBuilder.Create;
    builder.Append(_('FullPath:') + info.FileName).AppendLine;
    builder.Append(_('Description:') + info.Description).AppendLine;
    builder.Append(_('Type:') + _type).AppendLine;
    builder.Append(_('Requires:')).AppendLine;
    builder.Append(info.RequiredPackageList.Text).AppendLine;
    if data.MissingPackageName <> '' then
      builder.Append(_('Missing Package:') + data.MissingPackageName);
    HintText := builder.ToString;
    builder.Free;
  end;
end;

procedure TShowPackageListPage.fPackageTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PNodeData;
begin
  CellText := '';
  data := Sender.GetNodeData(Node);

  case Column of
    0:
      begin
        CellText := data.Name;
      end;
    1:
      if data.Info <> nil then
        CellText := data.Info.Description;
    2:
      if data.Info <> nil then
        if data.Info.RunOnly then
          CellText := _('runtime')
        else
          CellText := _('design');
  end;
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
    if (data.Info <> nil) and (data.MissingPackageName <> '') then
      TargetCanvas.Font.Color := clRed
    else
      TargetCanvas.Font.Color := clBlack;
  end;
end;

procedure TShowPackageListPage.miCollapseAllClick(Sender: TObject);
begin
  inherited;
  fPackageTree.FullCollapse;
end;

procedure TShowPackageListPage.miCollapseChildrenClick(Sender: TObject);
begin
  inherited;
  fPackageTree.FullCollapse(fPackageTree.FocusedNode);
end;

procedure TShowPackageListPage.miExpandAllClick(Sender: TObject);
begin
  inherited;
  fPackageTree.FullExpand;
end;

procedure TShowPackageListPage.miExpandChildrenClick(Sender: TObject);
begin
  inherited;
  fPackageTree.FullExpand(fPackageTree.FocusedNode);
end;

procedure TShowPackageListPage.miSelectAllClick(Sender: TObject);
begin
  fPackageTree.Traverse( procedure(Node: PVirtualNode)begin Node.checkState := csCheckedNormal; end);
  fPackageTree.Invalidate;
  UpdateWizardState;
end;

procedure TShowPackageListPage.miSelectMatchingClick(Sender: TObject);
var
  value: string;
begin
  value := fCompilationData.Pattern;
  if InputQuery(_('Select Matching...'), _('File Mask'), value) then
  begin
    fSelectMask := value;
    fPackageTree.TraverseWithData( procedure(Node: PVirtualNode; data: PNodeData)
    begin
      if IsFileNameMatch(data.Info.PackageName + '.dpk', fSelectMask) then
        Node.checkState := csCheckedNormal;
    end);
    fPackageTree.Invalidate;
  end;
  UpdateWizardState;
end;

procedure TShowPackageListPage.miUnselectAllClick(Sender: TObject);
begin
  fPackageTree.Traverse( procedure(Node: PVirtualNode)begin Node.checkState := csUncheckedNormal; end);
  fPackageTree.Invalidate;
  UpdateWizardState;
end;

procedure TShowPackageListPage.miUnselectMatchingClick(Sender: TObject);
var
  value: string;
begin
  value := fCompilationData.Pattern;
  if InputQuery(_('UnSelect Matching...'), _('File Mask'), value) then
  begin
    fSelectMask := value;
    fPackageTree.TraverseWithData(
    procedure(Node: PVirtualNode; data: PNodeData)
    begin
      if IsFileNameMatch(data.Info.PackageName + '.dpk', fSelectMask) then
        Node.checkState := csUncheckedNormal;
    end);
    fPackageTree.Invalidate;
  end;
  UpdateWizardState;
end;

{ TPackageLoadThread }

constructor TPackageLoadThread.Create(data: TCompilationData; tree: TBaseVirtualTree);
begin
  inherited Create(true);
  fCompilationData := data;
  fTree := tree;
  fPackageInfoFactory := TPackageInfoFactory.Create;
end;

destructor TPackageLoadThread.Destroy;
begin
  fPackageInfoFactory.Free;
  inherited;
end;

procedure TPackageLoadThread.RemoveEmptyFolderNodes(Node: PVirtualNode);
var
  c: PVirtualNode;
  data: PNodeData;
begin
  if not fActive then
    exit;
  if Node = nil then
    exit;

  c := fTree.GetFirstChild(Node);
  while c <> nil do
  begin
    data := fTree.GetNodeData(c);
    if data.Info = nil then
      RemoveEmptyFolderNodes(c);
    c := fTree.GetNextSibling(c);
  end;
  if Node.FirstChild = nil then
    fTree.DeleteNode(Node);
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
      on e: Exception do
        ShowMessage(e.Message);
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
          child.checkState := csCheckedNormal;
          child.CheckType := ctCheckBox;
          data := fTree.GetNodeData(child);
          data.Name := sr.Name;
          data.Info := fPackageInfoFactory.CreatePackageInfo(PathAddSeparator(directory) + sr.Name);
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

procedure TPackageLoadThread.Search(parent: PVirtualNode; folder: String);
var
  child: PVirtualNode;
  data: PNodeData;
  directoryList: TStringList;
  directory: string;
begin
  if not fActive then
    exit;
  directoryList := TStringList.Create;
  try
    BuildFileList(PathAddSeparator(folder) + '*.*', faDirectory, directoryList);
    for directory in directoryList do
    begin
      child := fTree.AddChild(parent);
      child.checkState := csCheckedNormal;
      child.CheckType := ctCheckBox;
      child.States := child.States + [vsHasChildren];
      data := fTree.GetNodeData(child);
      data.Name := directory;
      Search(child, PathAddSeparator(folder) + directory);
    end;
    BuildFileNodes(parent, folder);
  finally
    directoryList.Free;
  end;
end;

end.
