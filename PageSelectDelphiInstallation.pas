unit PageSelectDelphiInstallation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ExtCtrls,  dxGDIPlusClasses, WizardIntfs;

type
  TSelectDelphiInstallationPage = class(TWizardPage)
    rgDelphiVersions: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    procedure UpdateWizardState(const wizard: IWizard); override;
  end;

var
  SelectDelphiInstallationPage: TSelectDelphiInstallationPage;

implementation
uses WizardData,JclBorlandTools;

var
    installations : TJclBorRADToolInstallations;
{$R *.dfm}

{ TSelectDelphiInstallationPage }

procedure TSelectDelphiInstallationPage.FormCreate(Sender: TObject);
var
  i:integer;
  data : TWizardData;
begin
  inherited;
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

procedure TSelectDelphiInstallationPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  data : TWizardData;
begin
  inherited;
  data := TWizardData(wizard.GetData);
  data.SetInstallation(installations.Installations[rgDelphiVersions.ItemIndex]);

end;

procedure TSelectDelphiInstallationPage.UpdateWizardState(
  const wizard: IWizard);
var
  button: TButton;
begin
  inherited;
  wizard.SetHeader('Select Delphi Installation');
  wizard.SetDescription('Please select delphi installation that you want to compile with');
  button := wizard.GetButton(wbtNext);
  button.Caption := 'Compile';
end;

end.
