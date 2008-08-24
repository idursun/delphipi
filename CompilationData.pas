unit CompilationData;
interface
uses Classes, PackageInfo, JclBorlandTools;

type
  TCompilationData = class(TInterfacedObject)
  private
    fBaseFolder: String;
    fInstallation: TJclBorRADToolInstallation;
    fPackageList: TPackageList;
    fHelpFiles: TStringList;
    fSourceFilePaths: TStringList;
    fPattern: String;
    fDCPOutputFolder: string;
    fBPLOutputFolder: string;

    function GetHelpFiles: TStringList;
    function GetSourceFilePaths: TStringList;
    procedure SetPackageList(const aPackageList: TPackageList);
    procedure SetSourceFilePaths(const aPathList: TStringList);
  public
    constructor Create;
    property SourceFilePaths : TStringList read GetSourceFilePaths write SetSourceFilePaths;
    property Pattern: String read fPattern write fPattern;
    property Installation: TJclBorRADToolInstallation read fInstallation write fInstallation;
    property BaseFolder: String read fBaseFolder write fBaseFolder;
    property HelpFiles: TStringList read GetHelpFiles;
    property PackageList: TPackageList read fPackageList write SetPackageList;
    property DCPOutputFolder: string read fDCPOutputFolder write fDCPOutputFolder;
    property BPLOutputFolder: string read fBPLOutputFolder write fBPLOutputFolder;
  end;
implementation

constructor TCompilationData.Create;
begin
  fPattern := '*.dpk';
  fPackageList := TPackageList.Create;
end;
function TCompilationData.GetHelpFiles: TStringList;
begin
  if fHelpFiles = nil then
    fHelpFiles := TStringList.Create;
  Result :=fHelpFiles;   
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
function TCompilationData.GetSourceFilePaths: TStringList;
begin
  if fSourceFilePaths = nil then
    fSourceFilePaths := TStringList.Create;
  Result := fSourceFilePaths;
end;
end.
