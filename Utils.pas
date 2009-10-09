unit Utils;

interface
uses classes, JclBorlandTools,Windows;
const
  VERSION = '0.56';
  CODE = 'Josephine';
  AUTHOR = 'Ýbrahim DURSUN';

  DELPHI_VERSION_UNKNOWN = -1;
  DELPHI_VERSION_5 = 0;
  DELPHI_VERSION_6 = 1;
  DELPHI_VERSION_7 = 2;
  DELPHI_VERSION_8 = 3;
  DELPHI_VERSION_2006 = 4;
  DELPHI_VERSION_2007 = 5;
  DELPHI_VERSION_2009 = 6;
  DELPHI_VERSION_2010 = 7;
  VersionNames: array[DELPHI_VERSION_UNKNOWN..DELPHI_VERSION_2010] of string = ('Unknown', 'Delphi 5','Delphi 6','Delphi 7','Delphi 8','Delphi 2006', 'Delphi 2007','Delphi 2009','Delphi 2010');
type
  TDelphiVersionArray = array[DELPHI_VERSION_5..DELPHI_VERSION_2010] of TStringList;

implementation

uses JclFileUtils, SysUtils;

end.
