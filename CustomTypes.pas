{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit CustomTypes;

interface
type

TString = class
private
  fStr: string;
public
  constructor Create(const str: string);
  property Value: string read fStr write fStr;
end;

implementation

{ TString }

constructor TString.Create(const str: string);
begin
  fStr := str;
end;

end.
