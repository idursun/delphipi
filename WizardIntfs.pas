{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit WizardIntfs;

interface
uses Classes, StdCtrls, ActnList;
type
   TWizardButtonType = (wbtNone, wbtNext, wbtBack, wbtHelp);
   IWizard = interface
     ['{82A45FE8-CCA1-4DED-91DB-67F1D2989D56}']
     procedure UpdateInterface;
     function GetAction(buttonType: TWizardButtonType): TAction;
//     function GetData: TInterfacedObject;
     procedure SetHeader(const header:String);
     procedure SetDescription(const desc: String);
     function GetState(const key:string):TObject;
    procedure SetState(const key: string; const value:TObject);
   end;

implementation

end.
