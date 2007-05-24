program AutoInstaller;

uses
  Forms,
  FormMain in 'FormMain.pas' {frmMain},
  PackageInfo in 'PackageInfo.pas',
  FormAbout in 'FormAbout.pas' {frmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
