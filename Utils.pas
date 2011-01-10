unit Utils;

interface
uses classes, JclIDEUtils, Windows;
const
  VERSION = '0.57';
  CODE = 'Josephine';
  AUTHOR = '�brahim DURSUN';

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
  DELPHI_LAST_VERSION = DELPHI_XE;
  VersionNames: array[DELPHI_VERSION_UNKNOWN..DELPHI_LAST_VERSION] of string = ('Unknown', 'Delphi 5','Delphi 6','Delphi 7','Delphi 8','Delphi 2005', 'Delphi 2006', 'Delphi 2007','Delphi 2009','Delphi 2010', 'Delphi XE');
type
  TDelphiVersionArray = array[DELPHI_VERSION_5..DELPHI_LAST_VERSION] of TStringList;

implementation

uses JclFileUtils, SysUtils;

end.
