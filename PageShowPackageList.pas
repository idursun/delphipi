{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageShowPackageList;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Controls, Forms,
  Dialogs, PageBase, ComCtrls, PackageInfo, ImgList, WizardIntfs, Menus;

type
  TShowPackageListPage = class(TWizardPage)
    packageListView: TListView;
    PopupMenu: TPopupMenu;
    miSelectAll: TMenuItem;
    miUnselectAll: TMenuItem;
    miSelectUsing: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure packageListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miSelectAllClick(Sender: TObject);
    procedure miUnselectAllClick(Sender: TObject);
    procedure miSelectUsingClick(Sender: TObject);
  private
    packageLoadThread: TThread;
    procedure DisplayPackageList(const PackageList: TPackageList);
    procedure PackageLoadCompleted(Sender: TObject);
  public
    constructor Create(Owner: TComponent; const compilationData: TCompilationData); override;
    procedure UpdateWizardState; override;
  end;

implementation

uses JclFileUtils, gnugettext;
{$R *.dfm}
var
  threadWorking : Boolean;
type

  TPackageLoadThread = class(TThread)
  private
    fCompilationData: TCompilationData;
  protected
    procedure Execute; override;
  public
   constructor Create(data: TCompilationData);
  end;

{ TShowPackageListPage }

constructor TShowPackageListPage.Create(Owner: TComponent;
  const compilationData: TCompilationData);
begin
  inherited;
  fCompilationData := compilationData;
    // If there is no source file path in the list then load
  if FCompilationData.SourceFilePaths.Count = 0 then
  begin
    packageLoadThread := TPackageLoadThread.Create(FCompilationData);
    with packageLoadThread do begin
      FreeOnTerminate := true;
      OnTerminate := packageLoadCompleted;
      packageListView.AddItem(_('Looking for packages in folders...'),nil);
      threadWorking := true;
      Resume;
    end;
  end else begin
    DisplayPackageList(fCompilationData.PackageList);
  end;
end;


procedure TShowPackageListPage.miSelectAllClick(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  for I := 0 to packageListView.Items.Count - 1 do
  begin
    packageListView.Items[I].Checked := true;
  end;
end;

procedure TShowPackageListPage.miSelectUsingClick(Sender: TObject);
begin
  inherited;
  ShowMessage('Not Implemented');
end;

procedure TShowPackageListPage.miUnselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  for I := 0 to packageListView.Items.Count - 1 do
  begin
    packageListView.Items[I].Checked := false;
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

procedure TShowPackageListPage.packageListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
var
  info : TPackageInfo;
  _type : string;
const
  CRLF = #13#10;
begin
  inherited;
  info := TPackageInfo(Item.Data);
  _type := _('Designtime Package');
  if (info.RunOnly) then
    _type := _('Runtime Package');
  InfoTip := _('FullPath:')+info.FileName+CRLF+
             _('Type    :')+ _type +CRLF+
             _('Requires:')+ CRLF + info.RequiredPackageList.Text;
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
begin
  threadWorking := false;
  DisplayPackageList(fCompilationData.PackageList);
  wizard.UpdateInterface;
end;

procedure TShowPackageListPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i : integer;
  info : TPackageInfo;
begin
  inherited;
  if threadWorking then begin
    packageLoadThread.Suspend;
    packageLoadThread.Terminate;
    exit;
  end;

  for i := 0 to packageListView.Items.Count - 1 do begin
    info := TPackageInfo(packageListView.Items[i].Data);
    if not packageListView.Items[i].Checked then
    begin
      fCompilationData.PackageList.Remove(info);
      FreeAndNil(info);
    end;
  end;
  fCompilationData.PackageList.Pack;
end;

procedure TShowPackageListPage.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
end;

procedure TShowPackageListPage.DisplayPackageList(const PackageList: TPackageList);
var
  info : TPackageInfo;
  I: Integer;
begin
  if PackageList = nil then exit;
  packageListView.Clear;
  packageListView.Items.BeginUpdate;
  try
    for I := 0 to PackageList.Count - 1 do begin
      info := PackageList[i];
      with packageListView.Items.Add do begin
        Caption := info.Description;
        if Caption = '' then
          Caption := _('<No Description>');

        SubItems.Add(info.PackageName);
        if info.RunOnly then
          SubItems.Add(_('runtime'))
        else
          SubItems.Add(_('design'));
        Checked := True;
        Data := info;
      end;
    end;
  finally
    packageListView.Items.EndUpdate;
  end;
end;

{ TPackageLoadThread }

constructor TPackageLoadThread.Create(data: TCompilationData);
begin
  inherited Create(true);
  fCompilationData := data;
end;

procedure TPackageLoadThread.Execute;
var
  searcher: IJclFileEnumerator;
  fileName: string;
  info: TPackageInfo;
  fileList :TStringList;
begin
  inherited;

  fCompilationData.PackageList.InitialFolder := fCompilationData.BaseFolder;
  searcher := TJclFileEnumerator.Create;
  fileList := TStringList.Create;
  try
    searcher.RootDirectory := fCompilationData.BaseFolder;
    searcher.FileMask := fCompilationData.Pattern + ';*.hlp';
    searcher.FillList(fileList);
    while searcher.RunningTasks > 0 do
      Sleep(100);
    for fileName in fileList do begin
      if ExtractFileExt(fileName) = '.dpk' then begin
        info := TPackageInfo.Create(fileName);
        fCompilationData.PackageList.Add(info);
      end;
      if ExtractFileExt(fileName) = '.hlp' then
        fCompilationData.HelpFiles.Add(fileName);
    end;
    fCompilationData.PackageList.SortList;
  finally
    searcher := nil;
    fileList.Free;
  end;
end;

end.
