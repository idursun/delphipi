unit CompilationData;
interface
uses Classes, PackageInfo, PackageList, JclBorlandTools;

type
  TCompilationData = class
  private
    fBaseFolder: String;
    fInstallation: TJclBorRADToolInstallation;
    fPackageList: TPackageList;
    fHelpFiles: TStringList;
    fSourceFilePaths: TStringList;
    fPattern: String;
    fDCPOutputFolder: string;
    fBPLOutputFolder: string;

    procedure SetPackageList(const aPackageList: TPackageList);
    procedure SetSourceFilePaths(const aPathList: TStringList);
    procedure SetBaseFolder(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    //TODO: can be moved
    procedure ResolveSourcePaths;
    procedure ResolveHelpFiles;
    procedure AddSourcePathsToIDE;
    
    property Pattern: String read fPattern write fPattern;
    property Installation: TJclBorRADToolInstallation read fInstallation write fInstallation;
    property BaseFolder: String read fBaseFolder write SetBaseFolder;
    property HelpFiles: TStringList read fHelpFiles;
    property PackageList: TPackageList read fPackageList write SetPackageList;
    property SourceFilePaths : TStringList read fSourceFilePaths write SetSourceFilePaths;
    property DCPOutputFolder: string read fDCPOutputFolder write fDCPOutputFolder;
    property BPLOutputFolder: string read fBPLOutputFolder write fBPLOutputFolder;
  end;

implementation

uses JclFileUtils,SysUtils;

constructor TCompilationData.Create;
begin
  fPattern := '*.dpk';
  fPackageList := TPackageList.Create;
  fHelpFiles := TStringList.Create;
  fSourceFilePaths := TStringList.Create;
end;

destructor TCompilationData.Destroy;
begin
  fPackageList.Free;
  fHelpFiles.Free;
  fSourceFilePaths.Free;
  inherited;
end;

procedure TCompilationData.SetBaseFolder(const Value: String);
begin
  fBaseFolder := Value;
end;

procedure TCompilationData.SetPackageList(const aPackageList: TPackageList);
begin
  fPackageList.Free;
  fPackageList := aPackageList;
end;

procedure TCompilationData.SetSourceFilePaths(const aPathList: TStringList);
begin
  fSourceFilePaths := aPathList;
end;

procedure TCompilationData.ResolveHelpFiles;
var
  files: TStringList;
  filename: string;
begin
  if self.fInstallation.IDEVersionNumber >= 7 then exit; //delphi 2007 and later
  
  files := TStringList.Create;
  try
    AdvBuildFileList(fBaseFolder +'\*.hlp',
           faAnyFile,
           files,
           amAny,
           [flFullnames, flRecursive],
           '', nil);
     for filename in files do
       fHelpFiles.Add(filename);
  finally
    files.Free;
  end;
end;

//TODO: refactor -- finds paths of files have .pas extension and then matches pas files in packages, removes remaining paths 
procedure TCompilationData.ResolveSourcePaths;
var
  i,j: integer;
  files, containedFiles: TStringList;
begin
  Assert(assigned(SourceFilePaths));
  fSourceFilePaths.Clear;

  files := TStringList.Create;
  files.Sorted := true;
  files.Duplicates := dupIgnore;

  containedFiles := TStringList.Create;
  containedFiles.Sorted := true;
  containedFiles.Duplicates := dupIgnore;

  SourceFilePaths.Sorted := true;
  SourceFilePaths.Duplicates := dupIgnore;
     
  for i := 0 to fPackageList.Count - 1 do begin
    SourceFilePaths.Add(ExtractFileDir(fPackageList[i].FileName));
    for j := 0 to fPackageList[i].ContainedFileList.Count - 1 do
      containedFiles.Add(ExtractFileName(fPackageList[i].ContainedFileList[j]));
  end;

  AdvBuildFileList(fBaseFolder+'\*.pas',
           faAnyFile,
           files,
           amAny,
           [flFullnames, flRecursive],
           '', nil);

  for I := 0 to files.count - 1 do begin
    if containedFiles.IndexOf(ExtractFileName(files[i])) > 0 then
      SourceFilePaths.Add(ExtractFileDir(files[i]));
  end;
end;


procedure TCompilationData.AddSourcePathsToIDE;
var
  path : string;
begin
  Assert(assigned(SourceFilePaths));
  for path in SourceFilePaths do
    Installation.AddToLibrarySearchPath(path);
end;

end.
