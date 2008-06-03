{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit WizardData;

interface
uses WizardIntfs, JclBorlandTools, PackageInfo, classes ;
type
  TWizardData = class(TInterfacedObject)
  private
    fBaseFolder: String;
    inst: TJclBorRADToolInstallation;
    fPackageList: TPackageList;
    fHelpFiles: TStringList;
    fSourceFilePaths: TStringList;
    fPattern: String;
  public
    constructor Create;
    function BaseFolder: string;
    function Installation: TJclBorRADToolInstallation;
    function PackageList: TPackageList;
    function Pattern: string;
    function HelpFiles: TStringList;
    function SourceFilePaths: TStringList;

    procedure SetBaseFolder(const folder: string);
    procedure SetInstallation(const anInstallation: TJclBorRADToolInstallation);
    procedure SetPattern(const pattern: string);
    procedure SetPackageList(const aPackageList: TPackageList);
    procedure SetSourceFilePaths(const aPathList: TStringList);
  end;
  
implementation

{ TWizardData }

function TWizardData.BaseFolder: string;
begin
  Result := fBaseFolder;
end;

constructor TWizardData.Create;
begin
  fPattern := '*.dpk';
end;

function TWizardData.HelpFiles: TStringList;
begin
  if fHelpFiles = nil then
    fHelpFiles := TStringList.Create;
  Result :=fHelpFiles;   
end;

function TWizardData.Installation: TJclBorRADToolInstallation;
begin
  Result := inst;
end;

function TWizardData.PackageList: TPackageList;
begin
  Result := fPackageList;
end;

function TWizardData.Pattern: string;
begin
  Result := fPattern;
end;

procedure TWizardData.SetBaseFolder(const folder: string);
begin
  fBaseFolder := folder;
end;

procedure TWizardData.SetInstallation(
  const anInstallation: TJclBorRADToolInstallation);
begin
  inst := anInstallation;
end;

procedure TWizardData.SetPackageList(const aPackageList: TPackageList);
begin
  fPackageList.Free;
  fPackageList := aPackageList;
end;

procedure TWizardData.SetPattern(const pattern: string);
begin
  fPattern := pattern;
end;

procedure TWizardData.SetSourceFilePaths(const aPathList: TStringList);
begin
  fSourceFilePaths := aPathList;
end;

function TWizardData.SourceFilePaths: TStringList;
begin
  Result := fSourceFilePaths;
end;

end.
