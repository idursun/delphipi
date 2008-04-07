unit PageProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ComCtrls, WizardIntfs;

type
  TProgressPage = class(TWizardPage)
    GroupBox1: TGroupBox;
    ProgressBar: TProgressBar;
    Label1: TLabel;
    lblPackage: TLabel;
    Label2: TLabel;
    lblFileName: TLabel;
    GroupBox2: TGroupBox;
    Memo: TMemo;
    lblCurrentPackageNo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    compileThreadWorking: Boolean;
    compileThread: TThread;
    fCurrPackageNo: Integer;
    procedure handleText(const text: string);
    procedure compileCompleted(sender: TObject);
    procedure SetCurrentPackageNo(const Value: Integer);
    property CurrectPackageNo: Integer read fCurrPackageNo write SetCurrentPackageNo;
  public
    procedure Compile;
    procedure UpdateWizardState(const wizard: IWizard); override;
  end;

var
  ProgressPage: TProgressPage;

implementation
uses PackageInfo, WizardData, PackageCompiler;
type
  TCompileThread = class(TThread)
  private
    fStepNo: Integer;
    fPackageName: String;
    fPage : TProgressPage;
  protected
    procedure Execute; override;
    procedure UpdatePage;
  public
    constructor Create(page: TProgressPage);
  end;
  
var
  data: TWizardData;
{$R *.dfm}

{ TProgressPage }

procedure TProgressPage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if compileThreadWorking then begin
    compileThread.Suspend;
    compileThread.Terminate;
  end;
end;

procedure TProgressPage.FormCreate(Sender: TObject);
begin
  inherited;
  data := TWizardData(wizard.GetData);
  lblPackage.Caption := '';
  lblFileName.Caption := '';
  CurrectPackageNo := 0;
  compileThreadWorking := false;
  Compile;
end;

procedure TProgressPage.UpdateWizardState(const wizard: IWizard);
begin
  inherited;
  wizard.SetHeader('Compile and Install Packages');
  wizard.SetDescription('Compiling packages that you have selected. Design time packages are going to be installed.');
  with wizard.GetButton(wbtNext) do
    Enabled := not compileThreadWorking;

  with wizard.GetButton(wbtPrevious) do
    Enabled := not compileThreadWorking;
end;

procedure TProgressPage.Compile;
begin
  data.Installation.OutputCallback := self.handletext;
  ProgressBar.Max := data.PackageList.Count;
  compileThread := TCompileThread.Create(self);
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
  if S[Length(S)] =')' then begin
    //lblFileName.Caption := ExtractFileName(S);
    //Sleep(1);
  end else
    memo.lines.add(text);
end;

procedure TProgressPage.SetCurrentPackageNo(const Value: Integer);
begin
  if fCurrPackageNo = Value then
    exit;

  fCurrPackageNo := Value;
  lblCurrentPackageNo.Caption := Format('%d/%d',[fCurrPackageNo,data.PackageList.Count]);
end;

procedure TProgressPage.compileCompleted(sender: TObject);
begin
  lblPackage.Caption :='';
  lblFileName.Caption := '';
  ProgressBar.Position := 0;
  compileThreadWorking := false;
  wizard.UpdateInterface;
  memo.Lines.Add('*** Completed');
end;

{ TCompileThread }

constructor TCompileThread.Create(page: TProgressPage);
begin
  inherited Create(true);
  fPage := page;
end;

procedure TCompileThread.Execute;
var
  info : TPackageInfo;
  compiler: TPackageCompiler;
  sourceList: TStringList;
  i : integer;
  compileSuccessful : Boolean;
begin
  inherited;
  sourceList := TStringList.Create;
  compiler := TPackageCompiler.Create(data.Installation);
  try
    data.PackageList.GetSourcePaths(sourceList);
    compiler.AddSourcePathsToIDE(sourceList);
    for i := 0 to data.PackageList.Count - 1 do begin
      info := data.PackageList[i];
      fStepNo := fStepNo + 1;
      fPackageName := info.PackageName;
      Synchronize(UpdatePage);
      compileSuccessful := compiler.CompilePackage(info);
      if compileSuccessful and (not info.RunOnly) then
        compiler.InstallPackage(info);
      if not compileSuccessful then
      begin
      
      end; 
    end;
  finally
    compiler.Free;
    sourceList.Free;
  end;
end;

procedure TCompileThread.UpdatePage;
begin
  fPage.lblPackage.Caption := fPackageName;
  fPage.ProgressBar.Position := fStepNo;
  fPage.CurrectPackageNo := fPage.CurrectPackageNo + 1;
end;

end.
