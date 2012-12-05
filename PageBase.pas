{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit PageBase;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WizardIntfs;

type
  TWizardPage = class(TForm)
  private
    function GetWizardController: IWizard;
    procedure SetWizardController(const Value: IWizard);
  protected
    fWizardController: IWizard;
    fCompilationData: TCompilationData;
  public
    constructor Create(Owner: TComponent; const compilationData: TCompilationData); reintroduce; virtual;
    procedure UpdateWizardState; virtual; 
    function CanShowPage: Boolean; virtual;
    property Wizard: IWizard read GetWizardController write SetWizardController;
  end;

  TPageClass = class of TWizardPage;

implementation

uses FormWizard;
{$R *.dfm}

function TWizardPage.CanShowPage: Boolean;
begin
  Result := true;
end;

constructor TWizardPage.Create(Owner: TComponent; const compilationData: TCompilationData);
begin
  inherited Create(Owner);
  fCompilationData := compilationData;
end;

function TWizardPage.GetWizardController: IWizard;
begin
  Result := TFrmWizard.Wizard;
end;

procedure TWizardPage.SetWizardController(const Value: IWizard);
begin
  fWizardController := Value;
end;
procedure TWizardPage.UpdateWizardState;
begin
  Assert(Assigned(Wizard));
end;

end.

