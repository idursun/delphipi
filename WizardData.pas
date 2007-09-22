unit WizardData;

interface
uses WizardIntfs, JclBorlandTools, PackageInfo ;
type
  TWizardData = class(TInterfacedObject)
  private
    fBaseFolder: String;
    inst: TJclBorRADToolInstallation;
    fPackageList: TPackageList;
    fPattern: String;
  public
    constructor Create;
    function BaseFolder: string;
    function Installation: TJclBorRADToolInstallation;
    function PackageList: TPackageList;
    function Pattern: string;
    procedure SetBaseFolder(const folder: string);
    procedure SetInstallation(const anInstallation: TJclBorRADToolInstallation);
    procedure SetPattern(const pattern: string);
    procedure SetPackageList(const aPackageList: TPackageList);
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
  fPackageList := aPackageList;
end;

procedure TWizardData.SetPattern(const pattern: string);
begin
  fPattern := pattern;
end;

end.
