{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit PageInstallHelpFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Vcl.Controls, Forms,
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
    procedure UpdateWizardState; override;
    function CanShowPage: Boolean; override;
  end;

var
  InstallHelpFilesPage: TInstallHelpFilesPage;

implementation
{$R *.dfm}

uses JclFileUtils, JclIDEUtils, gnugettext;

{ TInstallHelpFilesPage }

function TInstallHelpFilesPage.CanShowPage: Boolean;
begin
  Result := fCompilationData.HelpFiles.Count > 0;
end;

procedure TInstallHelpFilesPage.FormCreate(Sender: TObject);
var
  I: Integer;
  item: TListItem;
begin
  inherited;
  if fCompilationData.HelpFiles.Count = 0 then begin
    label1.Caption := _('No help files are found.');
    helpFileList.Enabled := false;
    btnInstallHelpFiles.Enabled := false;
    exit;
  end;

  if (fCompilationData.Installation.VersionNumber <= 7) then
    for I := 0 to fCompilationData.HelpFiles.Count - 1 do begin
      item :=helpFileList.Items.Add;
      item.Caption := fCompilationData.HelpFiles[i];
      item.Checked := true;
    end;
end;

procedure TInstallHelpFilesPage.UpdateWizardState;
begin
  inherited;
  wizard.SetHeader(_('Help Files'));
  wizard.SetDescription(_('Select the help files that you want to register, if there are any.'));
  with wizard.GetAction(wbtBack) do
    Visible := False;
end;

procedure TInstallHelpFilesPage.btnInstallHelpFilesClick(Sender: TObject);
//var
//  openHelp : TJclBorlandOpenHelp;
//  helpFileName: string;
//  successCount: integer;
begin
  inherited;
//  successCount := 0;
//  if fCompilationData.Installation is TJclBorRADToolInstallation then begin
//    openHelp := fCompilationData.Installation.OpenHelp;
//    for helpFileName in fCompilationData.HelpFiles do begin
//      if openHelp.AddHelpFile(helpFileName,PathExtractFileNameNoExt(helpFileName)) then
//        inc(successCount);
//    end;
//  end;
//  ShowMessage(Format(_('%d help file(s) are registered successfully'),[successCount]));
end;

end.
