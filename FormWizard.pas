{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit FormWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, WizardIntfs;

type
  TfrmWizard = class(TForm, IWizard)
    HeaderPanel: TPanel;
    Bevel: TBevel;
    btnNext: TButton;
    btnBack: TButton;
    Image1: TImage;
    lblHeader: TLabel;
    lblDescription: TLabel;
    DockPanel: TPanel;
    btnAbout: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    FWizardData :TInterfacedObject;
    FBaseFolder: String;
    procedure SetBaseFolder(const Value: String);
  protected
    procedure SelectPage(const pageNo: Integer);
  public
    procedure UpdateInterface;
    function GetData: TInterfacedObject;
    function GetButton(buttonType: TWizardButtonType): TButton;
    procedure SetDescription(const desc: string);
    procedure SetHeader(const header: string);
    property BaseFolder: String read FBaseFolder write SetBaseFolder;
  end;
var
  frmWizard: TfrmWizard;

implementation
uses FormAbout, PageBase, PageSelectFolders, PageSelectDelphiInstallation,
    PageProgress, PageShowPackageList, PageInstallHelpFiles, WizardData,
    gnugettext;
var
  Pages: array of TPageClass;
  CurPage: Byte;
  ActivePage : TWizardPage;

{$R *.dfm}
{ TfrmWizard }

procedure TfrmWizard.FormCreate(Sender: TObject);
begin
  FWizardData := TWizardData.Create;
  if (ParamCount > 0) then
    TWizardData(FWizardData).SetBaseFolder(ParamStr(1));
  SelectPage(0);
  TranslateComponent(self);
end;

procedure TfrmWizard.btnAboutClick(Sender: TObject);
begin
  Application.CreateForm(TfrmAbout,frmAbout);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TfrmWizard.btnNextClick(Sender: TObject);
begin
  if CurPage + 1 = Length(Pages) then
    Close
  else
    SelectPage(CurPage+1);
end;

procedure TfrmWizard.btnBackClick(Sender: TObject);
begin
  SelectPage(CurPage-1);
end;

function TfrmWizard.GetButton(buttonType: TWizardButtonType): TButton;
begin
  Result := nil;
  case buttonType of
    wbtNext: Result := btnNext;
    wbtBack: Result := btnBack;
  end;
end;

function TfrmWizard.GetData: TInterfacedObject;
begin
  Result := self.fwizardData;
end;

procedure TfrmWizard.SelectPage(const pageNo: Integer);
begin
  if (pageNo < 0) then exit;
  if (pageNo > Length(Pages)) then
    Close;
    
  if(Assigned(ActivePage)) then begin
    ActivePage.Close;
    FreeAndNil(ActivePage);
  end;

  CurPage := pageNo;
  ActivePage := Pages[pageNo].Create(self,self as IWizard);
  if not ActivePage.CanShowPage then begin
     SelectPage(pageNo+1);
     exit;
  end;
  
  UpdateInterface;

  ActivePage.ManualDock(DockPanel);
  ActivePage.Visible := true;
  ActivePage.SetFocus;
end;

procedure TfrmWizard.SetBaseFolder(const Value: String);
begin
  FBaseFolder := Value;
  UpdateInterface;
end;

procedure TfrmWizard.SetDescription(const desc: string);
begin
  lblDescription.Caption := desc;
end;

procedure TfrmWizard.SetHeader(const header: string);
begin
  lblHeader.Caption := header;
end;

procedure TfrmWizard.UpdateInterface;
begin
  if not assigned(ActivePage) then
     exit;
  btnNext.Enabled := true;
  btnBack.Enabled := true;
  btnNext.Caption := _('&Next >>');
  btnBack.Caption := _('<< &Back');

  ActivePage.UpdateWizardState(self as IWizard);

  btnBack.Enabled := btnBack.Enabled and (CurPage > 0);
//  btnNext.Enabled := btnNext.Enabled; //and (CurPage < length(Pages)-1);
end;

initialization
   SetLength(Pages,5);
   Pages[0] := TSelectFoldersPage;
   Pages[1] := TShowPackageListPage;
   Pages[2] := TSelectDelphiInstallationPage;
   Pages[3] := TProgressPage;
   Pages[4] := TInstallHelpFilesPage;
end.
