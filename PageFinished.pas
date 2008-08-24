{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageFinished;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, gnugettext;

type
  TFinishedPage = class(TWizardPage)
    Label1: TLabel;
    btnSave: TButton;
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateWizardState; override;
    
  end;

var
  FinishedPage: TFinishedPage;

implementation
uses ScriptPersister,WizardIntfs;
{$R *.dfm}

{ TFinishedPage }

procedure TFinishedPage.btnSaveClick(Sender: TObject);
var
  dialog: TSaveDialog;
  scripter: TScriptPersister;
begin
  dialog := TSaveDialog.Create(self);
  try
    if dialog.Execute then begin
      scripter := TScriptPersister.Create;
      try
        scripter.Save(fCompilationData, dialog.FileName);
      finally
        scripter.Free;
      end;
    end;
  finally
    dialog.Free;
  end;
end;

procedure TFinishedPage.UpdateWizardState;
begin
  inherited;
  FWizard.SetHeader(_('Finished'));
  FWizard.SetHeader(_('Installations are completed'));
  with FWizard.GetButton(wbtNext) do
    Caption := _('&Exit');

  with FWizard.GetButton(wbtBack) do
    Visible := false;
end;

end.
