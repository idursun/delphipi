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
    PopupMenu: TPopupMenu;
    miSelectAll: TMenuItem;
    miUnselectAll: TMenuItem;
    miSelectUsing: TMenuItem;
    N1: TMenuItem;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure packageTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure packageTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure packageTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure packageTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: UnicodeString);
  private
    packageLoadThread: TThread;
    packageTree : TVirtualStringTree;
    procedure PackageLoadCompleted(Sender: TObject);
    procedure ChangeState(node: PVirtualNode; checkState: TCheckState);
    procedure CreateVirtualTree;
    procedure packageTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
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
    name: string;
    info: TPackageInfo;
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
    procedure CleanTree(node: PVirtualNode; level: integer);
  public
    constructor Create(data: TCompilationData; tree: TBaseVirtualTree);
  end;

{ TShowPackageListPage }

procedure TShowPackageListPage.ChangeState(node: PVirtualNode;
  checkState: TCheckState);
var
  child: PVirtualNode;
begin
 if node = nil then  exit;
  node.CheckState := checkState;
  child := node.FirstChild;
  while child <> nil do
  begin
     ChangeState(child, checkState);
     child := child.NextSibling;
  end;
end;

constructor TShowPackageListPage.Create(Owner: TComponent;
  const compilationData: TCompilationData);
begin
  inherited;
  fCompilationData := compilationData;
  CreateVirtualTree;

  packageLoadThread := TPackageLoadThread.Create(FCompilationData, packageTree);
  with packageLoadThread do begin
    FreeOnTerminate := true;
    OnTerminate := packageLoadCompleted;
    //packageListView.AddItem(_('Looking for packages in folders...'),nil);
    threadWorking := true;
    Resume;
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
  button.Enabled := (not threadWorking) and (fCompilationData.PackageList <> nil) and (fCompilationData.PackageList.Count > 0);
  if not threadWorking then
  begin
    button := wizard.GetButton(wbtNext);
    button.Caption := _('Compile');
  end;
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
begin
  threadWorking := false;
  packageTree.FullExpand;
  wizard.UpdateInterface;
end;

procedure TShowPackageListPage.packageTreeChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  ChangeState(Node, Node.CheckState);
  packageTree.InvalidateChildren(Node,true);
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
  if data.info <> nil then
  begin
    info := data.info;
    _type := _('Designtime Package');
    if (info.RunOnly) then
      _type := _('Runtime Package');
    HintText := _('FullPath   :')+ info.FileName+#13#10+
                _('Description:')+ info.Description+#13#10+
                _('Type       :')+ _type+#13#10+
                _('Requires   :')+ #13#10 + info.RequiredPackageList.Text;
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
    0: CellText := data.name;
    1: if data.info <> nil then
         CellText := data.info.Description;
    2: if data.info <> nil then
         if data.info.RunOnly then
           CellText := _('runtime');
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
  if data.info = nil then
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
  fCompilationData.PackageList.Pack;
end;

procedure TShowPackageListPage.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
end;

procedure TShowPackageListPage.CreateVirtualTree;
begin
  packageTree := TVirtualStringTree.Create(Self);
  with packageTree do
  begin
    Name := 'packageTree';
    Parent := Self;
    Left := 12;
    Top := 7;
    Width := 470;
    Height := 220;
    Anchors := [akLeft, akTop, akRight, akBottom];
    Header.AutoSizeIndex := 0;
    Header.Font.Charset := DEFAULT_CHARSET;
    Header.Font.Color := clWindowText;
    Header.Font.Height := -11;
    Header.Font.Name := 'Tahoma';
    Header.Font.Style := [];
    Header.Options := [hoColumnResize, hoDrag, hoVisible];
    TabOrder := 0;
    TreeOptions.MiscOptions := [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    ShowHint := true;
    HintMode := hmHint;
    StateImages := ImageList;
    OnChecked := packageTreeChecked;
    OnGetText := packageTreeGetText;
    OnGetNodeDataSize := packageTreeGetNodeDataSize;
    OnGetHint := packageTreeGetHint;
    OnGetImageIndex := packageTreeGetImageIndex;
    with Header.Columns.Add do begin 
      Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus];
      Position := 0;
      Width := 220;
      Caption := _('Package');
    end;
    with Header.Columns.Add do begin
      Position := 1;
      Width := 220;
      Caption := _('Description');
    end;
    with Header.Columns.Add do begin
      Position := 2;
      Width := 70;
      Caption := _('Type');
    end;
  end;
end;

{ TPackageLoadThread }

constructor TPackageLoadThread.Create(data: TCompilationData; tree: TBaseVirtualTree);
begin
  inherited Create(true);
  fCompilationData := data;
  fTree:= tree;
end;

procedure TPackageLoadThread.CleanTree(node: PVirtualNode; level: integer);
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
         if data.info = nil then
           CleanTree(c, level+1);
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
    CleanTree(fTree.RootNode,0);
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
          data.name := sr.Name;
          data.info := TPackageInfo.Create(directory + '\' + sr.Name);
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
      data.name := directory;
      Search(child, folder + '\' + directory);
    end;
    BuildFileNodes(parent, folder + '\' + directory);
  finally
    directoryList.Free;
  end;
end;

end.
