unit Utils;

interface
uses classes, JclBorlandTools,Windows;

procedure GetDefaultPackageList(const list: TStrings; const versionNumberStr: string);

implementation

uses JclFileUtils, SysUtils;

procedure GetDefaultPackageList(const list: TStrings; const versionNumberStr: string);
var
  systemPath,path,searchPath, entry,versionSuffix: string;
  packageName: string;
  internalList: TStringList;
begin
   Assert(list <> nil, 'list cannot be null');
   systemPath := GetEnvironmentVariable('WINDIR') + '\System32\';
   versionSuffix :=  '0.bpl';
   searchPath := systemPath + '*' + versionSuffix;
   internalList := TStringList.Create;
   try
     BuildFileList(searchPath, faAnyFile, internalList);
     for entry in internalList   do
     begin
       packageName := ExtractFileName(entry);
       list.Add(packageName) ;
     end;
   finally
     internalList.Free;
   end;
   list.Add('designide');
end;
end.
