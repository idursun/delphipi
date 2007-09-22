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
    constructor Create(Owner: TComponent; const wizard: IWizard); virtual;
    procedure UpdateWizardState(const wizard: IWizard); virtual;
  end;
  
  TPageClass = class of TWizardPage;

var
  WizardPage: TWizardPage;

implementation

{$R *.dfm}

constructor TWizardPage.Create(Owner: TComponent; const wizard: IWizard);
begin
  inherited Create(Owner);
  self.wizard := wizard;
end;

procedure TWizardPage.UpdateWizardState(const wizard: IWizard);
begin

end;

end.
