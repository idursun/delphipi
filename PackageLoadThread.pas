unit PackageLoadThread;

interface
uses Classes, SysUtils, Generics.Collections, PackageInfo, TreeNodes, CompilationData, PackageInfoFactory;
type
  TPackageLoadThread = class(TThread)
  private
    fCompilationData: TCompilationData;
    fPackageInfoFactory: TPackageInfoFactory;
    fActive: Boolean;
    fList: TList<TTreeNode>;
    procedure LoadPackageInformations(const directory: string);
  protected
    procedure Execute; override;
    procedure Search(const folder: String);
  public
    constructor Create(data: TCompilationData; list: TList<TTreeNode>);
    destructor Destroy; override;
    property Active: Boolean read fActive write fActive;
  end;


implementation
uses Dialogs, JclFileUtils;
{ TPackageLoadThread }

constructor TPackageLoadThread.Create(data: TCompilationData; list: TList<TTreeNode>);
begin
  inherited Create(true);
  fCompilationData := data;
  fList := list;
  fPackageInfoFactory := TPackageInfoFactory.Create;
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
      Search(fCompilationData.BaseFolder);
    except
      on e: Exception do
        ShowMessage(e.Message);
    end;
  finally
  end;
end;

procedure TPackageLoadThread.LoadPackageInformations(const directory: string);
var
  sr: TSearchRec;
begin
  if FindFirst(PathAppend(directory, fCompilationData.Pattern), faAnyFile, sr) = 0 then
  begin
    try
      repeat
        if UpperCase(ExtractFileExt(sr.Name)) = '.DPK' then
          fList.Add(TPackageTreeNode.Create(fPackageInfoFactory.CreatePackageInfo(PathAppend(directory, sr.Name))));
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

procedure TPackageLoadThread.Search(const folder: String);
var
  directoryList: TStringList;
  directory: string;
begin
  if not fActive then
    exit;

  directoryList := TStringList.Create;
  try
    BuildFileList(PathAppend(folder, '*.*'), faDirectory, directoryList);
    for directory in directoryList do
    begin
      Search(PathAppend(folder, directory));
    end;
    LoadPackageInformations(folder);
  finally
    directoryList.Free;
  end;
end;


end.
