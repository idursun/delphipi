{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageInstallHelpFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ComCtrls, WizardIntfs;

type
  TInstallHelpFilesPage = class(TWizardPage)
    Label1: TLabel;
    helpFileList: TListView;
    btnInstallHelpFiles: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnInstallHelpFilesClick(Sender: TObject);
  private
  public
    procedure UpdateWizardState(const wizard: IWizard); override;
  end;

var
  InstallHelpFilesPage: TInstallHelpFilesPage;

implementation
{$R *.dfm}

uses WizardData, JclFileUtils, JclBorlandTools, gnugettext;
var
  Data: TWizardData;

{ TInstallHelpFilesPage }

procedure TInstallHelpFilesPage.FormCreate(Sender: TObject);
var
  I: Integer;
  item: TListItem;
begin
  inherited;
  TranslateComponent(self);
  data := TWizardData(wizard.GetData);
  if data.HelpFiles.Count = 0 then begin
    label1.Caption := _('No help files are found.');
    helpFileList.Enabled := false;
    btnInstallHelpFiles.Enabled := false;
    exit;
  end; 

  if (data.Installation.VersionNumber <= 7) then
    for I := 0 to data.HelpFiles.Count - 1 do begin
      item :=helpFileList.Items.Add;
      item.Caption := data.HelpFiles[i];
      item.Checked := true;
    end;
    TranslateComponent(self);
end;

procedure TInstallHelpFilesPage.UpdateWizardState(const wizard: IWizard);
begin
  inherited;
  wizard.SetHeader(_('Help Files'));
  wizard.SetDescription(_('Select the help files that you want to register, if there are any.'));
  with wizard.GetButton(wbtNext) do
    Caption := _('&Finish');
  with wizard.GetButton(wbtBack) do
    Visible := False;
end;

procedure TInstallHelpFilesPage.btnInstallHelpFilesClick(Sender: TObject);
var
  openHelp : TJclBorlandOpenHelp;
  helpFileName: string;
  successCount: integer;
begin
  inherited;
  successCount := 0;
  if data.Installation is TJclBorRADToolInstallation then begin
    openHelp := Data.Installation.OpenHelp;
    for helpFileName in data.HelpFiles do begin
      if openHelp.AddHelpFile(helpFileName,PathExtractFileNameNoExt(helpFileName)) then
        inc(successCount);
    end;
  end;
  ShowMessage(Format(_('%d help file(s) are registered successfully'),[successCount]));
end;

end.
