{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ComCtrls, WizardIntfs, CompileThread;

type
  TProgressPage = class(TWizardPage)
    GroupBox1: TGroupBox;
    ProgressBar: TProgressBar;
    Label1: TLabel;
    lblPackage: TLabel;
    lblCurrentPackageNo: TLabel;
    memo: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    compileThreadWorking: Boolean;
    compileThread: TCompileThread;
    fCurrPackageNo: Integer;

    procedure handleText(const text: string);
    procedure compileCompleted(sender: TObject);
    procedure SetCurrentPackageNo(const Value: Integer);
    property CurrectPackageNo: Integer read fCurrPackageNo write SetCurrentPackageNo;
  protected
    procedure WriteInfo(const color: TColor; const info: string);
  public
    procedure Compile;
    procedure UpdateWizardState; override;
  end;

var
  ProgressPage: TProgressPage;

implementation
uses PackageInfo, gnugettext, StrUtils;
type

  TPageProgressMonitor = class(TInterfacedObject, IProgressMonitor )
  private
    FStepNo: Integer;
    FPackageName: String;
    FPage: TProgressPage;
    procedure SetPackageName(const Value: String);
    procedure SetStepNo(const Value: Integer);
    function GetStepNo: Integer;
    function GetPackageName: String;
  public
    constructor Create(const page: TProgressPage);  
    procedure PackageCompiled(const packageInfo: TPackageInfo; status: TPackageStatus);
    property StepNo: Integer read FStepNo write SetStepNo;
    property PackageName: String read FPackageName write SetPackageName;

  end;
var
  pageProgressMonitor : TPageProgressMonitor;

{$R *.dfm}

{ TProgressPage }

procedure TProgressPage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if compileThreadWorking then begin
    compileThread.Cancel := true;
    compileThread.WaitFor;
    compileThread.Terminate;
  end;
  pageProgressMonitor := nil;
end;

procedure TProgressPage.FormCreate(Sender: TObject);
begin
  inherited;
  TranslateComponent(self);
  lblPackage.Caption := '';
  CurrectPackageNo := 0;
  compileThreadWorking := false;
  Compile;
end;

procedure TProgressPage.UpdateWizardState;
begin
  inherited;
  wizard.SetHeader(_('Compile and Install Packages'));
  wizard.SetDescription(_('Compiling packages that you have selected. Design time packages are going to be installed.'));
  with wizard.GetButton(wbtNext) do
    Enabled := not compileThreadWorking;

  with wizard.GetButton(wbtBack) do
    Enabled := not compileThreadWorking;
end;

procedure TProgressPage.WriteInfo(const color: TColor; const info: string);
var
  oldColor: TColor;
begin
  oldColor := memo.SelAttributes.Color;
  memo.SelAttributes.Color := color;
  memo.Lines.Add(info);
  memo.SelAttributes.Color := oldColor;
end;


procedure TProgressPage.Compile;
begin
  pageProgressMonitor := TPageProgressMonitor.Create(self);
  fCompilationData.Installation.OutputCallback := self.handletext;
  ProgressBar.Max := fCompilationData.PackageList.Count;
  compileThread := TCompileThread.Create(fCompilationData);
  compileThread.Monitor := pageProgressMonitor;
  with compileThread do begin
    OnTerminate := compileCompleted;
    FreeOnTerminate := true;
    compileThreadWorking := true;
    Resume;
  end;
end;

procedure TProgressPage.handleText(const text: string);
var
  S : String;
begin
  S := Trim(Text);

  if Pos('Fatal:', S) > 0 then
    WriteInfo(clRed, text);

  //memo.lines.add(text);
  memo.SelAttributes.Color := clBlack;
end;

procedure TProgressPage.SetCurrentPackageNo(const Value: Integer);
begin
  if fCurrPackageNo = Value then
    exit;

  fCurrPackageNo := Value;
  lblCurrentPackageNo.Caption := Format('%d/%d',[fCurrPackageNo,fCompilationData.PackageList.Count]);
end;

procedure TProgressPage.CompileCompleted(sender: TObject);
begin
  lblPackage.Caption :='';
  ProgressBar.Position := 0;
  compileThreadWorking := false;
  WriteInfo(clBlack, _('*** Completed'));
  Wizard.UpdateInterface;
end;

constructor TPageProgressMonitor.Create(const page: TProgressPage);
begin
  FPage := page;
end;

function TPageProgressMonitor.GetPackageName: String;
begin
  Result := FPackageName;
end;

function TPageProgressMonitor.GetStepNo: Integer;
begin
  Result := FStepNo;
end;

procedure TPageProgressMonitor.PackageCompiled(const packageInfo: TPackageInfo;
  status: TPackageStatus);
begin
  case status of
    psNone: ;
    psCompiling: FPage.WriteInfo(clBlack, _('Compiling:') + packageInfo.PackageName);
    psInstalling: FPage.WriteInfo(clBlue, _('Installing'));
    psSuccess: FPage.WriteInfo(clGreen, _('Successful'));
    psError: fPage.WriteInfo(clRed,_('Failed'));
  end;
end;

procedure TPageProgressMonitor.SetPackageName(const Value: String);
begin
  if FPackageName <> Value then
  begin
    FPackageName := Value;
    fPage.lblPackage.Caption := FPackageName;
  end;
end;

procedure TPageProgressMonitor.SetStepNo(const Value: Integer);
begin
  if FStepNo <> Value then
  begin
    FStepNo := Value;
    fPage.ProgressBar.Position := FStepNo;
    fPage.CurrectPackageNo := fPage.CurrectPackageNo + 1;
  end;
end;

end.
