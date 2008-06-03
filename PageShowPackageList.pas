{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageShowPackageList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Controls, Forms,
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
    procedure packageListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miSelectAllClick(Sender: TObject);
    procedure miUnselectAllClick(Sender: TObject);
    procedure miSelectUsingClick(Sender: TObject);
  private
    packageLoadThread: TThread;
    procedure DisplayPackageList(const PackageList: TPackageList);
    procedure PackageLoadCompleted(Sender: TObject);
  public
    procedure UpdateWizardState(const wizard: IWizard); override;
  end;

var
  ShowPackageListPage: TShowPackageListPage;
  threadWorking : Boolean;
implementation
uses WizardData, JclFileUtils, gnugettext;
{$R *.dfm}
type
  TPackageLoadThread = class(TThread)
  protected
    procedure Execute; override;
  end;
var
  //NOTE WizardData.pas'ý interface kýsmýnda tanýmlamak istemiyorum o yüzden
  //encapsulation'ý deliyorum burada.
  data : TWizardData;

{ TShowPackageListPage }

procedure TShowPackageListPage.FormCreate(Sender: TObject);
begin
  inherited;
  TranslateComponent(self);
  data := TWizardData(wizard.GetData);
  packageLoadThread := TPackageLoadThread.Create(true);
  with packageLoadThread do begin
    FreeOnTerminate := true;
    OnTerminate := packageLoadCompleted;
    packageListView.AddItem(_('Looking for packages in folders...'),nil);
    threadWorking := true;
    Resume;
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

procedure TShowPackageListPage.UpdateWizardState(const wizard: IWizard);
var
  button: TButton;
begin
  inherited;
  wizard.SetHeader(_('Select Packages'));
  wizard.SetDescription(_('Select packages that you want to compile.'));
  button := wizard.GetButton(wbtNext);
  button.Enabled := (not threadWorking) and ((data.PackageList <> nil) and (data.PackageList.Count > 0));
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
             _('Requires:')+ CRLF + info.Requires.Text;
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
begin
  threadWorking := false;
  DisplayPackageList(data.PackageList);
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
    if packageListView.Items[i].Checked then continue;
    info := TPackageInfo(packageListView.Items[i].Data);
    info.Free;
    data.PackageList[i] := nil;
  end;
  data.PackageList.Pack;
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

procedure TPackageLoadThread.Execute;
var
  packageList: TPackageList;
  searcher: IJclFileEnumerator;
  fileName: string;
  info: TPackageInfo;
  foundFiles : TStringList;
  helpFiles : TStringList;
begin
  inherited;
  foundFiles := TStringList.Create;
  helpFiles := TStringList.Create;
  packageList := TPackageList.Create;
  
  packageList.InitialFolder := data.BaseFolder;
  searcher := TJclFileEnumerator.Create;
  try
    searcher.RootDirectory := data.BaseFolder;
    searcher.FileMask := data.Pattern + ';*.hlp';
    searcher.FillList(foundFiles);
    while searcher.RunningTasks > 0 do
      Sleep(100);
    for fileName in foundFiles do begin
      if ExtractFileExt(fileName) = '.dpk' then begin
        info := TPackageInfo.Create(fileName);
        packageList.Add(info);
      end;
      if ExtractFileExt(fileName) = '.hlp' then
        helpFiles.Add(fileName);
    end;
    data.SetPackageList(packageList);
    data.PackageList.SortList;
    data.HelpFiles.Assign(helpFiles);
  finally
    foundFiles.Free;
    helpFiles.Free;
  end;
end;

end.
