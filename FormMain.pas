unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclBorlandTools, CheckLst, ComCtrls, ActnList, ImgList,
  ToolWin, PackageInfo, ExtCtrls;

type
  TfrmMain = class(TForm)
    ListView1: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    ToolButton2: TToolButton;
    ActionList1: TActionList;
    actSelectFolder: TAction;
    actCompile: TAction;
    actExit: TAction;
    StatusBar1: TStatusBar;
    ToolButton3: TToolButton;
    actAbout: TAction;
    ToolButton4: TToolButton;
    Memo: TMemo;
    Splitter1: TSplitter;
    procedure actSelectFolderExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure ListView1InfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure actCompileUpdate(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
  private
    FPackageList: PackageInfo.TPackageList;
    inst : TJclDelphiInstallation;
    procedure DisplayPackageList(const PackageList: TPackageList);
  protected
    procedure DoCreate; override;
    procedure handletext(const text:string);
  end;

var
  frmMain: TfrmMain;

implementation
{$R *.dfm}
uses  JclSysUtils, FileCtrl, FormAbout;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  Application.CreateForm(TfrmAbout,frmAbout);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TfrmMain.actCompileExecute(Sender: TObject);
var
  i: Integer;
  info : TPackageInfo;
  SourcePaths : TStringList;
  includes: String;
  j: Integer;
begin
  SourcePaths := TStringList.Create;
  try
    FPackageList.GetSourceList(SourcePaths);
    memo.Lines.Assign(SourcePaths);
    for I := 0 to SourcePaths.Count - 1 do begin
      //inst.AddToLibrarySearchPath(SourcePaths[i]);
    end;
  finally
    SourcePaths.Free;
  end;

  inst.OutputCallback := self.handletext;
  for i := 0 to ListView1.Items.Count - 1 do begin
    if not ListView1.Items[i].Checked then continue;
    info := TPackageInfo(ListView1.Items[i].Data);
    if info.RunOnly then begin
      inst.DCC32.MakePackage(info.filename, inst.BPLOutputPath,inst.DCPOutputPath);
    end
    else
      inst.InstallIDEPackage(info.filename, inst.BPLOutputPath,inst.DCPOutputPath);
  end;
end;

procedure TfrmMain.actCompileUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ListView1.Items.Count > 0;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actSelectFolderExecute(Sender: TObject);
var
  directory: string;
  mask : string;
begin
  if SelectDirectory('Select the folder where packages are','C:\',directory) then begin
    mask := '*D11.dpk';
    if InputQuery('Please specify package mask','Mask',mask) then begin
      FPackageList := TPackageList.LoadFromFolder(directory+'\'+mask);
      FPackageList.SortList;
      DisplayPackageList(FPackageList);
    end;
  end;

end;

procedure TfrmMain.DisplayPackageList(const PackageList: TPackageList);
var
  info : TPackageInfo;
  I: Integer;
begin
  ListView1.Clear;
  ListView1.Items.BeginUpdate;
  try
    for I := 0 to PackageList.Count - 1 do begin
      info := PackageList[i];
      with ListView1.Items.Add do begin
        Caption := info.Description;
        SubItems.Add(info.PackageName + ' ('+inttostr(info.weight)+')');
        if info.RunOnly then
          SubItems.Add('runtime')
        else
          SubItems.Add('design');
        Checked := True;
        Data := info;
      end;
    end;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TfrmMain.DoCreate;
var
  insts : TJclBorRADToolInstallations;
begin
  inherited;
  insts := TJclBorRADToolInstallations.Create;
  inst := TJclDelphiInstallation(insts.Installations[0]);
end;

procedure TfrmMain.handletext(const text: string);
begin
  memo.lines.add(text);
  Application.ProcessMessages;
end;

procedure TfrmMain.ListView1InfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: string);
var
  info : TPackageInfo;
begin
  info := TPackageInfo(Item.Data);
  InfoTip := 'Requires:'+ info.Requires.Text;
end;

end.