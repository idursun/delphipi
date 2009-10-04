{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
  License : GNU General Public License 2.0
** }
unit TreeNodes;
interface
uses treemodel, packageinfo;
type
  TNodeType = (ntNode, ntFolder, ntPackage);
  TTreeNode = class(TInterfacedObject, INode)
  private
    fName: string;
    fPath: string;
    fSelected: boolean;
    fNodeType: TNodeType;
  public
    constructor Create(const name, path: string);
    function GetData: TObject; virtual;
    function GetDisplayName: string; virtual;
    function GetNodePath: string; virtual;
    property NodeType: TNodeType read fNodeType;
    property Selected: boolean read fSelected write fSelected;
  end;

  TPackageTreeNode = class(TTreeNode)
  private
    fInfo: TPackageInfo;
    fMissingPackageName: string;
  public
    constructor Create(const info: TPackageInfo); virtual;

    function GetData: TObject; override;
    function GetDisplayName: string; override;
    function GetNodePath: string; override;

    property MissingPackageName: string read fMissingPackageName write fMissingPackageName;
  end;

implementation
uses JclStrings;
constructor TTreeNode.Create(const name, path: string);
begin
  fName := name;
  fPath := path;
  fNodeType := ntFolder;
  fSelected := true;
end;

function TTreeNode.GetData: TObject;
begin
  Result := nil;
end;

function TTreeNode.GetDisplayName: string;
begin
  Result := fName;
end;

function TTreeNode.GetNodePath: string;
begin
  Result := fPath;
end;

constructor TPackageTreeNode.Create(const info: TPackageInfo);
begin
  fInfo := info;
  fNodeType := ntPackage;
  fSelected := true;
end;

function TPackageTreeNode.GetData: TObject;
begin
  Result := fInfo;
end;

function TPackageTreeNode.GetDisplayName: string;
begin
  Result := fInfo.PackageName;
end;

function TPackageTreeNode.GetNodePath: string;
var
  i: integer;
begin
  Result := fInfo.FileName;
  i := Pos(':', Result);
  if i <> 0 then
    Result := StrRestOf(Result, i + 2);
end;

end.
