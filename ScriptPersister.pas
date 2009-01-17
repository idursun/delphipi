{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit ScriptPersister;

interface
uses SysUtils, Classes, CompilationData, PackageInfoFactory;
type
  //TODO: refactor: this class has more than one responsibility = scanner + script persister
  TScriptPersister = class
  private
    fLines: TStringList;
    fLine: String;
    fCurrentLine :integer;
    fPackageInfoFactory: TPackageInfoFactory;
  protected
    function IsSectionHeader(line: string):boolean;
    function GetSectionHeader(line: string):string;
    function ReadNextLine: string;
    procedure SetPackageList(compilationData: TCompilationData);
    procedure SetDelphiVersion(compilationData: TCompilationData);
    function HasNextLine: boolean;
    { Added: Ronald Siekman - 24 12 2008 }
    procedure SetLibrarySearchPath(compilationData: TCompilationData);
  public
    constructor Create();
    destructor Destroy; override;
    
    function Load(const scriptFilePath: string):TCompilationData;
    procedure Save(const compilationData: TCompilationData; const scriptFilePath: string);
  private
   class var
     const Header_BaseFolder = 'base-folder';
     const Header_DelphiVersion = 'delphi-version';
     const Header_BPLOutputFolder = 'bpl-output-folder';
     const Header_DCPOutputFolder = 'dcp-output-folder';
     const Header_DCUOutputFolder = 'dcu-output-folder';
     const Header_Packages = 'packages';
     { Added: Ronald Siekman - 24 12 2008 }
     const Header_Library_Search_Paths = 'library-search-paths';
  end;

implementation
uses JclStrings, JclFileUtils, JclBorlandTools, PackageInfo;
type

  TScriptWriter = class(TStringList)
     procedure WriteHeader(header: string);
     procedure WriteDetail(line:string);
  end;

{ TScriptPersister }

constructor TScriptPersister.Create();
begin
  fLines := TStringList.Create;

  { Changed: Ronald Siekman - 24 12 2008 }
  fPackageInfoFactory := TPackageInfoFactory.Create;
end;

destructor TScriptPersister.Destroy;
begin
  fLines.Free;
  fPackageInfoFactory.Free;
  inherited;
end;

function TScriptPersister.GetSectionHeader(line: string): string;
begin
   Result := StrLower(Copy(line, 1, Length(line)-1));
end;

function TScriptPersister.HasNextLine: boolean;
begin
  Result := fCurrentLine < fLines.Count;
end;

function TScriptPersister.IsSectionHeader(line: string): boolean;
begin
   Result := (line[Length(line)] = ':') and (line[1] <> ' '); 
end;

function TScriptPersister.Load(
  const scriptFilePath: string): TCompilationData;
var
  header: String;
begin
  Result := TCompilationData.Create;
  if not FileExists(scriptFilePath) then exit;
  fCurrentLine := 0;
  fLine := '';
  { Changed: Ronald Siekman - 24 12 2008 }
  //fLines := TStringList.Create;
  fLines.LoadFromFile(scriptFilePath);
  try
    while HasNextLine do
    begin
      fLine := ReadNextLine;
      if IsSectionHeader(fLine) then
      begin
        Result.Scripting := True;
        header := GetSectionHeader(fLine);

        if header = Header_BaseFolder then
          Result.BaseFolder := ReadNextLine;
        if header = Header_DelphiVersion then
          SetDelphiVersion(Result);
        if header = Header_BPLOutputFolder then
          Result.BPLOutputFolder := ReadNextLine;
        if header = Header_DCPOutputFolder then
          Result.DCPOutputFolder := ReadNextLine;
        if header = Header_DCUOutputFolder then
          Result.DCUOutputFolder := ReadNextLine;
        if header = Header_Library_Search_Paths then
          SetLibrarySearchPath(Result);
        if header = Header_Packages then
          SetPackageList(Result);
      end;
    end;
  finally
    //FreeAndNil(fLines);
  end;
end;
function TScriptPersister.ReadNextLine: string;
begin
  if not HasNextLine then begin
    Result := '';
    exit;
  end;

  fLine :=  Trim(fLines[fCurrentLine]);
  inc(fCurrentLine);
  Result := fLine;
end;

procedure TScriptPersister.SetDelphiVersion(compilationData: TCompilationData);
begin
  ReadNextLine;
  compilationData.SetDelphiVersion(fLine);
end;

procedure TScriptPersister.SetLibrarySearchPath(
  compilationData: TCompilationData);

  procedure SemiColonTextToStringList( const line: string;
    var List: TStringList );
  var
    sValue: string;
    sLine: string;
    iPos: Integer;
  begin
    sValue := line;

    iPos := Pos(';', sValue);
    while not(iPos = 0) do
    begin
      sLine := Trim(Copy(sValue, 1, iPos-1));
      List.Add(sLine);
      Delete(sValue, 1, iPos);
      iPos := Pos(';', sValue);
    end;

    sValue := Trim(sValue);
    if not(sValue = '') then
    begin
      List.Add(sValue);
    end;
  end;

var
  line : string;
  slPaths: TStringList;
begin
  { Added: Ronald Siekman - 24 12 2008 }
  if Assigned(compilationData.Installation) then
  begin
    slPaths := TStringList.Create;
    try
      SemiColonTextToStringList(compilationData.Installation.LibrarySearchPath,
                                slPaths);

      while HasNextLine do
      begin
        line := ReadNextLine;
        if IsSectionHeader(line) then begin
          Dec(fCurrentLine);
          break;
        end;

        if (slPaths.IndexOf(line) = -1 ) then
        begin
          compilationData.Installation.AddToLibrarySearchPath(line);
        end;
      end;
    finally
      slPaths.Free;
    end;
  end;
end;

procedure TScriptPersister.SetPackageList(compilationData: TCompilationData);
var
  line : string;
  folder : string;
begin
   { Changed: Ronald Siekman - 24 12 2008 }
   folder := IncludeTrailingPathDelimiter(compilationData.BaseFolder);

   while HasNextLine do
   begin
     line := ReadNextLine;
     if IsSectionHeader(line) then begin
       Dec(fCurrentLine);
       break;
     end;

     if (ExtractFilePath(line) = '' ) then
     begin
       line := folder+line;
     end;

     if FileExists( line ) then
     begin
       compilationData.PackageList.Add(fPackageInfoFactory.CreatePackageInfo(line));
     end;
   end;
end;

procedure TScriptPersister.Save(const compilationData: TCompilationData; const scriptFilePath: string);
var
  script: TScriptWriter;
  i: Integer;
begin
  if compilationData = nil then exit;
  script := TScriptWriter.Create;
  try
    with compilationData, script do begin
      WriteHeader(Header_BaseFolder);
        WriteDetail(compilationData.BaseFolder);

      WriteHeader(Header_DelphiVersion);
        WriteDetail(compilationData.Installation.VersionNumberStr);

      WriteHeader(Header_BPLOutputFolder);
        WriteDetail(compilationData.BPLOutputFolder);

      WriteHeader(Header_DCPOutputFolder);
        WriteDetail(compilationData.DCPOutputFolder);

      WriteHeader(Header_Packages);
      for i := 0 to compilationData.PackageList.Count-1 do
      begin
        if PathIsChild(compilationData.PackageList[i].FileName, compilationData.BaseFolder) then
        begin
          WriteDetail(PathGetRelativePath(compilationData.BaseFolder,compilationData.PackageList[i].FileName));
        end else begin
          WriteDetail(compilationData.PackageList[i].FileName);
        end;
      end;
    end;
    script.SaveToFile(scriptFilePath);
  finally
    script.Free;
  end;
end;

{ TScriptWriter }

procedure TScriptWriter.WriteDetail(line: string);
begin
   self.Add('  ' + line);
end;

procedure TScriptWriter.WriteHeader(header: string);
begin
   self.Add(header+':');
end;

end.
