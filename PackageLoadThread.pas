{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (ibrahimdursun gmail)
  License : GNU General Public License 2.0
  ** }
unit PackageLoadThread;

interface
uses Classes, SysUtils, Generics.Collections, PackageInfo, TreeNodes, PackageInfoFactory;
type
  TPackageLoadThread = class(TThread)
  private
    fPackageInfoFactory: TPackageInfoFactory;
    fActive: Boolean;
    fList: TList<TTreeNode>;
    fDirectory: string;
    fPattern: string;
    procedure LoadPackageInformations(const directory: string);
  protected
    procedure Execute; override;
    procedure Search(const folder: String);
  public
    constructor Create(directory: string; pattern: string; list: TList<TTreeNode>);
    destructor Destroy; override;
    property Active: Boolean read fActive write fActive;
  end;

implementation
uses Dialogs, JclFileUtils;
{ TPackageLoadThread }

constructor TPackageLoadThread.Create(directory: string; pattern: string; list: TList<TTreeNode>);
begin
  inherited Create(true);
  Assert(Assigned(list), 'List cannot be null');

  fList := list;
  fPackageInfoFactory := TPackageInfoFactory.Create;
  fDirectory := directory;
  fPattern := pattern;
end;

destructor TPackageLoadThread.Destroy;
begin
  fPackageInfoFactory.Free;
  inherited;
end;

procedure TPackageLoadThread.Execute;
begin
  inherited;
  fActive := true;
  try
    try
      Search(fDirectory);
    except
      on e: Exception do
        ShowMessage(e.Message);
    end;
  finally
    fActive := false;
  end;
end;

procedure TPackageLoadThread.LoadPackageInformations(const directory: string);
var
  sr: TSearchRec;
  packageNode: TPackageTreeNode;
begin
  if FindFirst(PathAppend(directory, fPattern), faAnyFile, sr) = 0 then
  begin
    try
      repeat
        if UpperCase(ExtractFileExt(sr.Name)) = '.DPK' then
        begin
          packageNode := TPackageTreeNode.Create(fPackageInfoFactory.CreatePackageInfo(PathAppend(directory, sr.Name)));
          if not fList.Contains(packageNode) then
            fList.Add(packageNode);
        end;
      until (FindNext(sr) <> 0) and Active;
    finally
      FindClose(sr);
    end;
  end;
end;

procedure TPackageLoadThread.Search(const folder: String);
var
  directoryList: TStringList;
  directory, dir: string;
begin
  directoryList := TStringList.Create;
  try
    BuildFileList(PathAppend(folder, '*.*'), faAnyFile, directoryList);
    for directory in directoryList do
    begin
      if not Active then
        Break;
      dir := PathAppend(folder, directory);
      if IsDirectory(dir) then
        Search(dir);
    end;
    LoadPackageInformations(folder);
  finally
    directoryList.Free;
  end;
end;


end.
