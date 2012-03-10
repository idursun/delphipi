unit TestDelphiVersionGuess;

interface
uses TestFramework;

type
TestDelphiDelphiVersionGuess = class(TTestCase)
  published
    procedure Should_guess_Delphi6;
    procedure Should_guess_DelphiXE2;
end;

implementation
uses Utils;
{ TestDelphiDelphiVersionGuess }

procedure TestDelphiDelphiVersionGuess.Should_guess_Delphi6;
var
  actual: integer;
begin
   CheckEquals(DELPHI_VERSION_6, Utils.GuessDelphiVersion('pack60'));
   CheckEquals(DELPHI_VERSION_6, Utils.GuessDelphiVersion('pack6'));
   CheckEquals(DELPHI_VERSION_6, Utils.GuessDelphiVersion('packD6'));
   CheckEquals(DELPHI_VERSION_6, Utils.GuessDelphiVersion('packD60'));
end;

procedure TestDelphiDelphiVersionGuess.Should_guess_DelphiXE2;
begin
   CheckEquals(DELPHI_XE2, Utils.GuessDelphiVersion('pack160'));
   CheckEquals(DELPHI_XE2, Utils.GuessDelphiVersion('pack16'));
   CheckEquals(DELPHI_XE2, Utils.GuessDelphiVersion('packD16'));
   CheckEquals(DELPHI_XE2, Utils.GuessDelphiVersion('packD160'));
end;

initialization
  RegisterTest(TestDelphiDelphiVersionGuess.Suite);
end.
