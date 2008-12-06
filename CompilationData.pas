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
  public
    constructor Create;
    destructor Destroy; override;
    
    property Pattern: String read fPattern write fPattern;
    property Installation: TJclBorRADToolInstallation read fInstallation write fInstallation;
    property BaseFolder: String read fBaseFolder write fBaseFolder;
    property HelpFiles: TStringList read fHelpFiles;
    property PackageList: TPackageList read fPackageList write SetPackageList;
    property SourceFilePaths : TStringList read fSourceFilePaths write fSourceFilePaths;
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

procedure TCompilationData.SetPackageList(const aPackageList: TPackageList);
begin
  fPackageList.Free;
  fPackageList := aPackageList;
end;

end.
