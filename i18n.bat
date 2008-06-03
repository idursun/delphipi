@ECHO OFF

ECHO Building GUI PO template file...
dxgettext -r -b . -b . --delphi --so .\po\delphipi.pot

ECHO .
ECHO Merging GUI translation files with template...

msgmerge -o .\po\tr.po -D .\po tr.po .\po\delphipi.pot

ECHO .
ECHO Building MO file...
mkdir .\locale\TR\LC_MESSAGES
msgfmt .\po\tr.po -o .\locale\TR\LC_MESSAGES\default.mo
ECHO .

ECHO Embedding MO files into EXE
assemble --dxgettext .\delphipi.exe

ECHO .
ECHO Done.

PAUSE