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
    procedure FormCreate(Sender: TObject);
  private
    isCompiling: Boolean;
    procedure handleText(const text: string);
    procedure compileCompleted(sender: TObject);
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

procedure TProgressPage.FormCreate(Sender: TObject);
begin
  inherited;
  data := TWizardData(wizard.GetData);
  lblPackage.Caption := '';
  lblFileName.Caption := '';
  isCompiling := false;
  Compile;
end;

procedure TProgressPage.UpdateWizardState(const wizard: IWizard);
begin
  inherited;
  wizard.SetHeader('Compiling Packages');
  wizard.SetDescription('Compiling...');
  with wizard.GetButton(wbtNext) do
    Enabled := not isCompiling;

  with wizard.GetButton(wbtPrevious) do
    Enabled := not isCompiling;
end;

procedure TProgressPage.Compile;
begin
  data.Installation.OutputCallback := self.handletext;
  ProgressBar.Max := data.PackageList.Count;
  with TCompileThread.Create(self) do begin
    OnTerminate := compileCompleted;
    FreeOnTerminate := true;
    isCompiling := true;
    Resume;
  end;
end;

procedure TProgressPage.handleText(const text: string);
var
  S : String;
begin
 S := Trim(Text);
  if S[Length(S)] =')' then begin
     lblFileName.Caption := ExtractFileName(S);
  end else
    memo.lines.add(text);
end;

procedure TProgressPage.compileCompleted(sender: TObject);
begin
  lblPackage.Caption :='';
  lblFileName.Caption := '';
  ProgressBar.Position := 0;
  isCompiling := false;
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
      
      if compiler.CompilePackage(info) and (not info.RunOnly) then
        compiler.InstallPackage(info);
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
end;

end.
