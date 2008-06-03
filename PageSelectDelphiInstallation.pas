{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageSelectDelphiInstallation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ExtCtrls, WizardIntfs;

type
  TSelectDelphiInstallationPage = class(TWizardPage)
    rgDelphiVersions: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    procedure UpdateWizardState(const wizard: IWizard); override;
    function CanShowPage: Boolean; override;
  end;

var
  SelectDelphiInstallationPage: TSelectDelphiInstallationPage;

implementation
uses WizardData,JclBorlandTools,gnugettext;

var
  installations : TJclBorRADToolInstallations;
  data : TWizardData;
{$R *.dfm}

{ TSelectDelphiInstallationPage }

procedure TSelectDelphiInstallationPage.FormCreate(Sender: TObject);
var
  i:integer;
begin
  inherited;
  TranslateComponent(self);
  installations := TJclBorRADToolInstallations.Create;

  for I := 0 to installations.Count - 1 do begin
    rgDelphiVersions.Items.Add(installations.Installations[i].Description);
  end;
  rgDelphiVersions.ItemIndex := 0;
  
  data := TWizardData(wizard.GetData);
  if data.Installation = nil then
    exit;
    
  for I := 0 to installations.Count - 1 do begin
    if (installations.Installations[i].VersionNumber = data.Installation.VersionNumber) then begin
      rgDelphiVersions.ItemIndex := i;
      break;
    end;
  end;
end;

function TSelectDelphiInstallationPage.CanShowPage: Boolean;
begin
  Result := installations.Count > 1;
  if installations.Count = 1 then begin
    data.SetInstallation(installations[0]);
  end;
end;

procedure TSelectDelphiInstallationPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  data.SetInstallation(installations.Installations[rgDelphiVersions.ItemIndex]);
end;

procedure TSelectDelphiInstallationPage.UpdateWizardState(
  const wizard: IWizard);
var
  button: TButton;
begin
  inherited;
  wizard.SetHeader(_('Select Delphi Installation'));
  wizard.SetDescription(_('Please select delphi installation that you want to compile with'));
  button := wizard.GetButton(wbtNext);
  button.Caption := _('Compile');
end;

end.
