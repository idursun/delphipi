{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit Utils;

interface
uses classes, JclIDEUtils, Windows;
const
  VERSION = '0.71';
  CODE = 'Marauder';
  AUTHOR = 'Ýbrahim DURSUN';

  //delphi compiler versions
  DELPHI_VERSION_UNKNOWN = -1;
  DELPHI_VERSION_5 = 0;
  DELPHI_VERSION_6 = 1;
  DELPHI_VERSION_7 = 2;
  DELPHI_VERSION_8 = 3;
  DELPHI_VERSION_2005 = 4;
  DELPHI_VERSION_2006 = 5;
  DELPHI_VERSION_2007 = 6;
  DELPHI_VERSION_2009 = 7;
  DELPHI_VERSION_2010 = 8;
  DELPHI_XE = 9;
  DELPHI_XE2 = 10;
  DELPHI_XE3 = 11;
  DELPHI_XE4 = 12;
  DELPHI_XE5 = 13;
  DELPHI_XE6 = 14;
  DELPHI_LAST_VERSION = DELPHI_XE6;
  VersionNames: array[DELPHI_VERSION_UNKNOWN..DELPHI_LAST_VERSION] of string = ('Unknown', 'Delphi 5','Delphi 6','Delphi 7','Delphi 8','Delphi 2005', 'Delphi 2006', 'Delphi 2007','Delphi 2009','Delphi 2010', 'Delphi XE', 'Delphi XE2', 'Delphi XE3', 'Delphi XE4', 'Delphi XE5', 'Delphi XE6');
type
  TDelphiVersionArray = array[DELPHI_VERSION_5..DELPHI_LAST_VERSION] of TStringList;
function GuessDelphiVersion(name: string): integer;

implementation

uses JclFileUtils, SysUtils;
var
  i:Integer;
  patterns: TDelphiVersionArray;

procedure AddDelphiPattern(const version: integer; const pattern: string; weight: integer=1);
var
  I: Integer;
begin
  for I := 1 to weight do
    patterns[version].Add(pattern);
end;

function GuessDelphiVersion(name: string): integer;
var
  matches: array[DELPHI_VERSION_5..DELPHI_LAST_VERSION] of Integer;
  pattern : string;
  max, maxi, i, index : integer;
begin
  FillChar(matches, Length(matches)*sizeof(Integer),0 );
  name := JclFileUtils.PathExtractFileNameNoExt(name);
  for i := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
  begin
    for pattern in  patterns[i] do
    begin
       index := Pos(UpperCase(pattern), UpperCase(name));
       if index <> 0 then
         matches[i] := matches[i] + index + Length(pattern)*2;
    end;
  end;

  max := 0;
  maxi := -1;
  for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
    if matches[i] > max then
    begin
      max := matches[i];
      maxi := i;
    end;
  Result := maxi;
end;

initialization
  // delphi compiler versions: http://docwiki.embarcadero.com/RADStudio/XE5/en/Compiler_Versions
  for I := DELPHI_VERSION_5 to DELPHI_LAST_VERSION do
    patterns[I] := TStringList.Create;

  AddDelphiPattern(DELPHI_VERSION_5, '5', 2);
  AddDelphiPattern(DELPHI_VERSION_5, 'r5', 2);
  AddDelphiPattern(DELPHI_VERSION_5, 'd5', 2);
  AddDelphiPattern(DELPHI_VERSION_5, '50');
  AddDelphiPattern(DELPHI_VERSION_5, 'd50');
  AddDelphiPattern(DELPHI_VERSION_5, 'delphi5');
  AddDelphiPattern(DELPHI_VERSION_5, '_5');

  AddDelphiPattern(DELPHI_VERSION_6, '_6');
  AddDelphiPattern(DELPHI_VERSION_6, '6');
  AddDelphiPattern(DELPHI_VERSION_6, '60');
  AddDelphiPattern(DELPHI_VERSION_6, 'r6');
  AddDelphiPattern(DELPHI_VERSION_6, 'r60');
  AddDelphiPattern(DELPHI_VERSION_6, 'd6');
  AddDelphiPattern(DELPHI_VERSION_6, 'd60');
  AddDelphiPattern(DELPHI_VERSION_6, 'delphi6');

  AddDelphiPattern(DELPHI_VERSION_7, '7');
  AddDelphiPattern(DELPHI_VERSION_7, 'r7');
  AddDelphiPattern(DELPHI_VERSION_7, 'd7');
  AddDelphiPattern(DELPHI_VERSION_7, '70');
  AddDelphiPattern(DELPHI_VERSION_7, 'r70');
  AddDelphiPattern(DELPHI_VERSION_7, 'd70');
  AddDelphiPattern(DELPHI_VERSION_7, 'delphi7');
  AddDelphiPattern(DELPHI_VERSION_7, '_7');
  AddDelphiPattern(DELPHI_VERSION_7, '_70');

  AddDelphiPattern(DELPHI_VERSION_8, 'D8');
  AddDelphiPattern(DELPHI_VERSION_8, 'D80');
  AddDelphiPattern(DELPHI_VERSION_8, 'NET');
  AddDelphiPattern(DELPHI_VERSION_8, '8');
  AddDelphiPattern(DELPHI_VERSION_8, '80');

  AddDelphiPattern(DELPHI_VERSION_2005, '_9');
  AddDelphiPattern(DELPHI_VERSION_2005, '_90');
  AddDelphiPattern(DELPHI_VERSION_2005, '9');
  AddDelphiPattern(DELPHI_VERSION_2005, '90');
  AddDelphiPattern(DELPHI_VERSION_2005, 'r9');
  AddDelphiPattern(DELPHI_VERSION_2005, 'd9');
  AddDelphiPattern(DELPHI_VERSION_2005, 'r90');
  AddDelphiPattern(DELPHI_VERSION_2005, 'd90');
  AddDelphiPattern(DELPHI_VERSION_2005, 'delphi2005');
  AddDelphiPattern(DELPHI_VERSION_2005, '2005', 2);

  AddDelphiPattern(DELPHI_VERSION_2006, '_10');
  AddDelphiPattern(DELPHI_VERSION_2006, '_100');
  AddDelphiPattern(DELPHI_VERSION_2006, '10');
  AddDelphiPattern(DELPHI_VERSION_2006, 'r10');
  AddDelphiPattern(DELPHI_VERSION_2006, 'd10');
  AddDelphiPattern(DELPHI_VERSION_2006, '100');
  AddDelphiPattern(DELPHI_VERSION_2006, 'r100');
  AddDelphiPattern(DELPHI_VERSION_2006, 'd100');
  AddDelphiPattern(DELPHI_VERSION_2006, '2006',2);
  AddDelphiPattern(DELPHI_VERSION_2006, 'd2006');
  AddDelphiPattern(DELPHI_VERSION_2006, 'delphi2006');

  AddDelphiPattern(DELPHI_VERSION_2007, '11');
  AddDelphiPattern(DELPHI_VERSION_2007, 'r11');
  AddDelphiPattern(DELPHI_VERSION_2007, 'd11');
  AddDelphiPattern(DELPHI_VERSION_2007, '110');
  AddDelphiPattern(DELPHI_VERSION_2007, 'r110');
  AddDelphiPattern(DELPHI_VERSION_2007, 'd110');
  AddDelphiPattern(DELPHI_VERSION_2007, '_11');
  AddDelphiPattern(DELPHI_VERSION_2007, '_110');
  AddDelphiPattern(DELPHI_VERSION_2007, '2007', 2);
  AddDelphiPattern(DELPHI_VERSION_2007, 'd2007');
  AddDelphiPattern(DELPHI_VERSION_2007, 'delphi2007');

  AddDelphiPattern(DELPHI_VERSION_2009,'12');
  AddDelphiPattern(DELPHI_VERSION_2009,'r12');
  AddDelphiPattern(DELPHI_VERSION_2009,'d12');
  AddDelphiPattern(DELPHI_VERSION_2009,'120');
  AddDelphiPattern(DELPHI_VERSION_2009,'d2009');
  AddDelphiPattern(DELPHI_VERSION_2009,'2009');
  AddDelphiPattern(DELPHI_VERSION_2009,'delphi2009');
  AddDelphiPattern(DELPHI_VERSION_2009,'_12');

  AddDelphiPattern(DELPHI_VERSION_2010,'13');
  AddDelphiPattern(DELPHI_VERSION_2010,'14');
  AddDelphiPattern(DELPHI_VERSION_2010,'r13');
  AddDelphiPattern(DELPHI_VERSION_2010,'d13');
  AddDelphiPattern(DELPHI_VERSION_2010,'r14');
  AddDelphiPattern(DELPHI_VERSION_2010,'d14');
  AddDelphiPattern(DELPHI_VERSION_2010,'13');
  AddDelphiPattern(DELPHI_VERSION_2010,'_13');
  AddDelphiPattern(DELPHI_VERSION_2010,'d2010');
  AddDelphiPattern(DELPHI_VERSION_2010,'2010',2);
  AddDelphiPattern(DELPHI_VERSION_2010,'delphi2010');
  AddDelphiPattern(DELPHI_VERSION_2010,'_14');

  AddDelphiPattern(DELPHI_XE,'15',3);
  AddDelphiPattern(DELPHI_XE,'d15',3);
  AddDelphiPattern(DELPHI_XE,'r15',3);
  AddDelphiPattern(DELPHI_XE,'150',3);
  AddDelphiPattern(DELPHI_XE,'d15',3);
  AddDelphiPattern(DELPHI_XE,'_15',3);

  AddDelphiPattern(DELPHI_XE2,'16',3);
  AddDelphiPattern(DELPHI_XE2,'d16',3);
  AddDelphiPattern(DELPHI_XE2,'160',3);
  AddDelphiPattern(DELPHI_XE2,'d16',3);
  AddDelphiPattern(DELPHI_XE2,'_16',3);

  AddDelphiPattern(DELPHI_XE3,'17',3);
  AddDelphiPattern(DELPHI_XE3,'d17',3);
  AddDelphiPattern(DELPHI_XE3,'170',3);
  AddDelphiPattern(DELPHI_XE3,'d17',3);
  AddDelphiPattern(DELPHI_XE3,'_17',3);

  AddDelphiPattern(DELPHI_XE4,'18',3);
  AddDelphiPattern(DELPHI_XE4,'d18',3);
  AddDelphiPattern(DELPHI_XE4,'180',3);
  AddDelphiPattern(DELPHI_XE4,'d18',3);
  AddDelphiPattern(DELPHI_XE4,'_18',3);

  AddDelphiPattern(DELPHI_XE5,'19',3);
  AddDelphiPattern(DELPHI_XE5,'d19',3);
  AddDelphiPattern(DELPHI_XE5,'190',3);
  AddDelphiPattern(DELPHI_XE5,'d19',3);
  AddDelphiPattern(DELPHI_XE5,'_19',3);

  AddDelphiPattern(DELPHI_XE6,'20',3);
  AddDelphiPattern(DELPHI_XE6,'d20',3);
  AddDelphiPattern(DELPHI_XE6,'200',3);
  AddDelphiPattern(DELPHI_XE6,'d20',3);
  AddDelphiPattern(DELPHI_XE6,'_20',3);

end.
