{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit FormWizard;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, WizardIntfs;

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

    procedure FormCreate(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    FCompilationData : TCompilationData;
    FBaseFolder: String;
    procedure SetBaseFolder(const Value: String);
  protected
    procedure SelectPage(const pageNo: Integer);
  public
    class var Wizard: IWizard;  
    procedure UpdateInterface;
    function GetButton(buttonType: TWizardButtonType): TButton;
    procedure SetDescription(const desc: string);
    procedure SetHeader(const header: string);
    property BaseFolder: String read FBaseFolder write SetBaseFolder;
  end;
var
  frmWizard: TfrmWizard;

implementation
uses FormAbout, PageBase, PageSelectFolders, PageSelectDelphiInstallation,
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
  TFrmWizard.Wizard := self as IWizard;

  if (ParamCount > 0) then
    FCompilationData.BaseFolder := ParamStr(1);
  SelectPage(0);
  TranslateComponent(self);
end;

procedure TFrmWizard.btnAboutClick(Sender: TObject);
begin
  Application.CreateForm(TfrmAbout,frmAbout);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TFrmWizard.btnNextClick(Sender: TObject);
begin
  if CurPage + 1 = Length(Pages) then
    Close
  else
    SelectPage(CurPage+1);
end;

procedure TFrmWizard.btnBackClick(Sender: TObject);
begin
  SelectPage(CurPage-1);
end;

function TFrmWizard.GetButton(buttonType: TWizardButtonType): TButton;
begin
  Result := nil;
  case buttonType of
    wbtNext: Result := btnNext;
    wbtBack: Result := btnBack;
  end;
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
  if not ActivePage.CanShowPage then begin
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

procedure TFrmWizard.UpdateInterface;
begin
  if not assigned(ActivePage) then
     exit;
  btnNext.Enabled := true;
  btnNext.Visible := true;
  btnNext.Caption := _('&Next >>');

  btnBack.Enabled := true;
  btnBack.Visible := true;
  btnBack.Caption := _('<< &Back');

  ActivePage.UpdateWizardState;

  btnBack.Enabled := btnBack.Enabled and (CurPage > 0);
//  btnNext.Enabled := btnNext.Enabled; //and (CurPage < length(Pages)-1);
end;

initialization
   SetLength(Pages,6);
   Pages[0] := TSelectFoldersPage;
   Pages[1] := TSelectDelphiInstallationPage;
   Pages[2] := TShowPackageListPage;
   Pages[3] := TProgressPage;
   Pages[4] := TInstallHelpFilesPage;
   Pages[5] := TSummaryPage;
end.
