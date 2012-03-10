unit TestDelphiVersionGuess;

interface
uses TestFramework;

type
TestDelphiDelphiVersionGuess = class(TTestCase)
  published
    procedure Should_distinguish_Delphi6_and_DelphiXE2_package_files;
end;

implementation
uses Utils;
{ TestDelphiDelphiVersionGuess }

procedure TestDelphiDelphiVersionGuess.Should_distinguish_Delphi6_and_DelphiXE2_package_files;
var
  actual: integer;
begin
   CheckEquals(DELPHI_VERSION_6, Utils.GuessDelphiVersion('pack60'), 'Should guess version as delphi 6');
   CheckEquals(DELPHI_XE2, Utils.GuessDelphiVersion('pack160'), 'should guess version as delphi xe2');
end;

initialization
  RegisterTest(TestDelphiDelphiVersionGuess.Suite);
end.
