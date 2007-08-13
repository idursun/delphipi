program DelphiPI;

uses
  Forms,
  FormMain in 'FormMain.pas' {frmMain},
  PackageInfo in 'PackageInfo.pas',
  FormAbout in 'FormAbout.pas' {frmAbout},
  FormOptions in 'FormOptions.pas' {frmOptions},
  PackageCompiler in 'PackageCompiler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi PI';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
