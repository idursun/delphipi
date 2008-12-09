{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
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
    fPattern: String;
    fDCPOutputFolder: string;
    fBPLOutputFolder: string;

    procedure SetPackageList(const aPackageList: TPackageList);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetIdePackages(const list: TStringList); virtual;  
    function GetIdeVersionSuffix: string; virtual;
    function SetDelphiVersion(const version:string):boolean; virtual;
    
    property Pattern: String read fPattern write fPattern;
    property Installation: TJclBorRADToolInstallation read fInstallation write fInstallation;
    property BaseFolder: String read fBaseFolder write fBaseFolder;
    property HelpFiles: TStringList read fHelpFiles;
    property PackageList: TPackageList read fPackageList write SetPackageList;
    property DCPOutputFolder: string read fDCPOutputFolder write fDCPOutputFolder;
    property BPLOutputFolder: string read fBPLOutputFolder write fBPLOutputFolder;
  end;

implementation

uses JclFileUtils,SysUtils;
var
  installations: TJclBorRADToolInstallations;

constructor TCompilationData.Create;
begin
  fPattern := '*.dpk';
  fPackageList := TPackageList.Create;
  fHelpFiles := TStringList.Create;
end;

destructor TCompilationData.Destroy;
begin
  fPackageList.Free;
  fHelpFiles.Free;
  inherited;
end;

procedure TCompilationData.GetIdePackages(const list: TStringList);
var
 i: integer;
begin
  assert(Assigned(Installation),'installation cannot be null');
 
  for i := 0 to Installation.IdePackages.Count - 1 do
    list.Add(Installation.IdePackages.PackageFileNames[i]);
end;

function TCompilationData.GetIdeVersionSuffix: string;
begin
  Result := Installation.VersionNumberStr;
end;

function TCompilationData.SetDelphiVersion(const version: string): boolean;
var
  installation: TJclBorRADToolInstallation;
  i : integer;
begin
  for i := 0 to installations.Count - 1 do begin
    installation := installations.Installations[i];
    if UpperCase(Trim(installation.VersionNumberStr)) = UpperCase(Trim(version)) then
    begin
      fInstallation := installation;
      break;
    end;
  end;
  if fInstallation = nil then
    raise Exception.Create('cannot find delphi version:' + version);
end;

procedure TCompilationData.SetPackageList(const aPackageList: TPackageList);
begin
  fPackageList.Free;
  fPackageList := aPackageList;
end;
initialization
  installations := TJclBorRADToolInstallations.Create;
finalization
  installations.Free;  

end.
