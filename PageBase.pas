{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WizardIntfs;

type
  TWizardPage = class(TForm)
  private
  protected
    wizard: IWizard;
  public
    constructor Create(Owner: TComponent; const wizard: IWizard); reintroduce; virtual;
    procedure UpdateWizardState(const wizard: IWizard); virtual;
    function CanShowPage: Boolean; virtual;
  end;
  
  TPageClass = class of TWizardPage;

var
  WizardPage: TWizardPage;

implementation

{$R *.dfm}

function TWizardPage.CanShowPage: Boolean;
begin
  Result := true;
end;

constructor TWizardPage.Create(Owner: TComponent; const wizard: IWizard);
begin
  inherited Create(Owner);
  self.wizard := wizard;
end;

procedure TWizardPage.UpdateWizardState(const wizard: IWizard);
begin

end;

end.
