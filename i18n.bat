@ECHO OFF

ECHO Building GUI PO template file...
dxgettext -r -b . -b . --delphi --so .\po\delphipi.pot

ECHO .
ECHO Merging GUI translation files with template...

msgmerge -o .\po\tr.po -D .\po\tr.po .\po\delphipi.pot
msgmerge -o .\po\ru_RU.po -D .\po\ru_RU.po .\po\delphipi.pot
msgmerge -o .\po\default.po -D .\po\default.po .\po\delphipi.pot

ECHO .
ECHO Building MO file...
mkdir .\locale\TR\LC_MESSAGES
msgfmt .\po\tr.po -o .\locale\TR\LC_MESSAGES\default.mo
mkdir .\locale\RU\LC_MESSAGES
msgfmt .\po\ru_RU.po -o .\locale\RU\LC_MESSAGES\default.mo
mkdir .\locale\EN\LC_MESSAGES
msgfmt .\po\default.po -o .\locale\EN\LC_MESSAGES\default.mo
ECHO .

ECHO Embedding MO files into EXE
assemble --dxgettext .\delphipi.exe
assemble --dxgettext .\delphipi.x64.exe

ECHO .
ECHO Done.

PAUSE