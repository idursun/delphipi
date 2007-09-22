unit PageShowPackageList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Controls, Forms,
  Dialogs, PageBase, ComCtrls, PackageInfo, ImgList, WizardIntfs;

type
  TShowPackageListPage = class(TWizardPage)
    packageListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure packageListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure DisplayPackageList(const PackageList: TPackageList);
    procedure PackageLoadCompleted(Sender: TObject);
  public
    procedure UpdateWizardState(const wizard: IWizard); override;
  end;

var
  ShowPackageListPage: TShowPackageListPage;
implementation
uses WizardData;
{$R *.dfm}
type
  TPackageLoadThread = class(TThread)
  protected
    procedure Execute; override;
  end;
var
  //NOTE WizardData.pas'ý interface kýsmýnda tanýmlamak isemtiyorum o yüzden
  //encapsulation'ý deliyorum burada.
  data : TWizardData;

{ TShowPackageListPage }

procedure TShowPackageListPage.FormCreate(Sender: TObject);
var
  directory : string;
begin
  inherited;
  data := TWizardData(wizard.GetData);

  with TPackageLoadThread.Create(true) do begin
    FreeOnTerminate := true;
    OnTerminate := packageLoadCompleted;
    packageListView.AddItem('Parsing Packages...',nil);
    Resume;
  end;
end;

procedure TShowPackageListPage.UpdateWizardState(const wizard: IWizard);
var
  button: TButton;
begin
  inherited;
  wizard.SetHeader('Select Packages');
  wizard.SetDescription('Runtime packages will be compiled, runtime packages will be installed.');
  if data.PackageList = nil then
     exit;
     
  button := wizard.GetButton(wbtNext);
  button.Enabled := data.PackageList.Count > 0;
end;

procedure TShowPackageListPage.packageListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
var
  info : TPackageInfo;
  _type : string;
begin
  inherited;
  info := TPackageInfo(Item.Data);
  _type := 'Designtime Package';
  if (info.RunOnly) then
    _type := 'Runtime Package';
  InfoTip := 'FullPath:'+info.FileName+#13#10+
             'Type    :'+ _type +#13#10+
             'Requires:'#13#10 + info.Requires.Text;
end;

procedure TShowPackageListPage.PackageLoadCompleted(Sender: TObject);
begin
  DisplayPackageList(data.PackageList);
end;

procedure TShowPackageListPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i : integer;
  info : TPackageInfo;
begin
  inherited;
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
  packageListView.Clear;
  packageListView.Items.BeginUpdate;
  try
    for I := 0 to PackageList.Count - 1 do begin
      info := PackageList[i];
      with packageListView.Items.Add do begin
        Caption := info.Description;
        if Caption = '' then
          Caption := '<No Description>';

        SubItems.Add(info.PackageName);
        if info.RunOnly then
          SubItems.Add('runtime')
        else
          SubItems.Add('design');
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
  directory: string;
begin
  inherited;
  directory := data.BaseFolder +'\' + data.Pattern;
  data.SetPackageList(TPackageList.LoadFromFolder(directory));
  data.PackageList.SortList;
end;

end.
