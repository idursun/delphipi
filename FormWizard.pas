{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit FormWizard;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, Vcl.Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, WizardIntfs, ActnList, System.Actions;

type
  TFrmWizard = class(TForm, IWizard)
    HeaderPanel: TPanel;
    LogoImage: TImage;
    lblHeader: TLabel;
    lblDescription: TLabel;
    DockPanel: TPanel;
    pBottom: TPanel;
    btnBack: TButton;
    btnNext: TButton;
    btnAbout: TButton;
    Bevel1: TBevel;
    actionList: TActionList;
    actNext: TAction;
    actBack: TAction;
    actAbout: TAction;

    procedure FormCreate(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actBackExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCompilationData : TCompilationData;
    FBaseFolder: String;
    FStates : TStringList;
    procedure SetBaseFolder(const Value: String);
  protected
    procedure SelectPage(const pageNo: Integer);
  public
    class var Wizard: IWizard;
    procedure UpdateInterface;
    function GetAction(buttonType: TWizardButtonType): TAction;
    procedure SetDescription(const desc: string);
    procedure SetHeader(const header: string);
    function GetState(const key: string): TObject;
    procedure SetState(const key: string; const value:TObject);

    property BaseFolder: String read FBaseFolder write SetBaseFolder;
  end;
var
  frmWizard: TfrmWizard;

implementation
uses FormAbout, PageBase, PageSelectFolders, PageCompilerOptions,
     PageProgress, PageShowPackageList, PageInstallHelpFiles, PageSummary,
     gnugettext;
var
  Pages: array of TPageClass;
  CurPage: Byte;
  ActivePage : TWizardPage;

{$R *.dfm}
{ TfrmWizard }


procedure TfrmWizard.FormCreate(Sender: TObject);
begin
  FCompilationData := TCompilationData.Create;
  FStates := TStringList.Create;

  TFrmWizard.Wizard := self as IWizard;
  self.Caption := Application.Title;
  if (ParamCount > 0) then
    FCompilationData.BaseFolder := ParamStr(1);
  SelectPage(0);
  TranslateComponent(self);
end;

procedure TFrmWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FCompilationData.Free;
  FStates.Free;
end;

procedure TFrmWizard.actAboutExecute(Sender: TObject);
begin
  Application.CreateForm(TfrmAbout,frmAbout);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TFrmWizard.actBackExecute(Sender: TObject);
begin
  SelectPage(CurPage-1);
end;

procedure TFrmWizard.actNextExecute(Sender: TObject);
begin
  if CurPage + 1 = Length(Pages) then
    Close
  else
    SelectPage(CurPage+1);
end;

function TFrmWizard.GetAction(buttonType: TWizardButtonType): TAction;
begin
  Result := nil;
  case buttonType of
    wbtNext: Result := actNext;
    wbtBack: Result := actBack;
  end;
end;

function TFrmWizard.GetState(const key: string): TObject;
var
  index: Integer;
begin
  Result := nil;
  index := FStates.IndexOf(key);
  if index <> -1 then
    Result := FStates.Objects[index];
end;

procedure TFrmWizard.SelectPage(const pageNo: Integer);
begin
  if (pageNo < 0) then exit;
  if (pageNo > Length(Pages)) then
    Close;
    
  if(Assigned(ActivePage)) then begin
    ActivePage.Close;
    FreeAndNil(ActivePage);
  end;

  CurPage := pageNo;
  ActivePage := Pages[pageNo].Create(self,FCompilationData);
  if not ActivePage.CanShowPage then 
  begin
     SelectPage(pageNo+1);
     exit;
  end;
  ActivePage.Wizard := self;
  UpdateInterface;

  ActivePage.ManualDock(DockPanel);
  ActivePage.Visible := true;
  ActivePage.SetFocus;
end;

procedure TFrmWizard.SetBaseFolder(const Value: String);
begin
  FBaseFolder := Value;
  UpdateInterface;
end;

procedure TFrmWizard.SetDescription(const desc: string);
begin
  lblDescription.Caption := desc;
end;

procedure TFrmWizard.SetHeader(const header: string);
begin
  lblHeader.Caption := header;
end;

procedure TFrmWizard.SetState(const key: string; const value:TObject);
var
  index: Integer;
begin
  index := FStates.IndexOf(key);
  if index <> -1 then
    FStates.Objects[index] := value
  else
    FStates.AddObject(key, value);
end;

procedure TFrmWizard.UpdateInterface;
begin
  if not assigned(ActivePage) then
     exit;
  actNext.Enabled := true;
  actNext.Visible := true;
  actNext.Caption := _('&Next >>');
  actNext.OnUpdate := nil;

  actBack.Enabled := true;
  actBack.Visible := true;
  actBack.Caption := _('<< &Back');
  actBack.OnUpdate := nil;

  ActivePage.UpdateWizardState;

  actBack.Enabled := actBack.Enabled and (CurPage > 0);
//  btnNext.Enabled := btnNext.Enabled; //and (CurPage < length(Pages)-1);
end;

initialization
   SetLength(Pages,5);
   Pages[0] := TSelectFoldersPage;
   Pages[1] := TSelectCompilerOptions;
   Pages[2] := TShowPackageListPage;
   Pages[3] := TProgressPage;
  // Pages[4] := TInstallHelpFilesPage;
   Pages[4] := TSummaryPage;
end.
