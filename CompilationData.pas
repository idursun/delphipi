{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (ibrahimdursun gmail)
 License : GNU General Public License 2.0
**}
unit CompilationData;
interface
uses Classes, PackageInfo, PackageList, JclIDEUtils;

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
    fDCUOutputFolder: string;
    FScripting: Boolean;
    fConditionals: string;

    procedure SetPackageList(const aPackageList: TPackageList);
    procedure SetInstallation(const Value: TJclBorRADToolInstallation);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetIdePackages(const list: TStringList); virtual;  
    function GetIdeVersionSuffix: string; virtual;
    function SetDelphiVersion(const version:string):boolean; virtual;
    
    property Pattern: String read fPattern write fPattern;
    property Installation: TJclBorRADToolInstallation read fInstallation write SetInstallation;
    property BaseFolder: String read fBaseFolder write fBaseFolder;
    property HelpFiles: TStringList read fHelpFiles;
    property PackageList: TPackageList read fPackageList write SetPackageList;
    property DCPOutputFolder: string read fDCPOutputFolder write fDCPOutputFolder;
    property BPLOutputFolder: string read fBPLOutputFolder write fBPLOutputFolder;
    property DCUOutputFolder: string read fDCUOutputFolder write fDCUOutputFolder;
    property Conditionals: string read fConditionals write fConditionals;

    property Scripting: Boolean read fScripting write fScripting;
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
  fScripting := False;
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
  if Result = 'd11' then //Delphi 2007 packages have version 10 extension
    Result := 'd10';
end;

function TCompilationData.SetDelphiVersion(const version: string): boolean;
var
  installation: TJclBorRADToolInstallation;
  i : integer;
begin
  Result := false;
  for i := 0 to installations.Count - 1 do
  begin
    installation := installations.Installations[i];
    if UpperCase(Trim(installation.VersionNumberStr)) = UpperCase(Trim(version)) then
    begin
      fInstallation := installation;
      Result := true;
      break;
    end;
  end;
  if fInstallation = nil then
    raise Exception.Create('cannot find delphi version:' + version);
end;

procedure TCompilationData.SetInstallation(
  const Value: TJclBorRADToolInstallation);
begin
  if fInstallation = Value then
    exit;
    
  fInstallation := Value;

  BPLOutputFolder := fInstallation.BPLOutputPath[bpWin32];
  DCPOutputFolder := fInstallation.DCPOutputPath[bpWin32];
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
