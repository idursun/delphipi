{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit ScriptPersister;

interface
uses SysUtils, Classes, CompilationData;
type
  //TODO: refactor: this class has more than one responsibility = scanner + script loader
  TScriptPersister = class
  private
    fLines: TStringList;
    fLine: String;
    fCurrentLine :integer;
  protected
    function IsSectionHeader(line: string):boolean;
    function GetSectionHeader(line: string):string;
    function ReadNextLine: string;
    procedure ReadPackageList(compilationData: TCompilationData);
    function HasNextLine: boolean;
  public
    constructor Create();
    destructor Destroy; override;
    
    function Load(const scriptFilePath: string):TCompilationData;
    procedure Save(const compilationData: TCompilationData; const scriptFilePath: string);
  end;

implementation
uses JclStrings, PackageInfo;
type

  TScriptWriter = class(TStringList)
     procedure WriteHeader(header: string);
     procedure WriteDetail(line:string);
  end;

{ TScriptPersister }

constructor TScriptPersister.Create();
begin
  fLines := TStringList.Create;
end;

destructor TScriptPersister.Destroy;
begin
  fLines.Free;
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
  fLines := TStringList.Create;
  fLines.LoadFromFile(scriptFilePath);
  try
    while HasNextLine do
    begin
      fLine := ReadNextLine;
      if IsSectionHeader(fLine) then
      begin
        header := GetSectionHeader(fLine);
        if header = 'basefolder' then Result.BaseFolder := ReadNextLine;
        if header = 'bpl-output-folder' then Result.BPLOutputFolder := ReadNextLine;
        if header = 'dcp-output-folder' then Result.DCPOutputFolder := ReadNextLine;
        if header = 'packages' then ReadPackageList(Result);
      end;
    end;
  finally
    FreeAndNil(fLines);
  end;
end;

function TScriptPersister.ReadNextLine: string;
begin
  if not HasNextLine then begin
    Result := '';
    exit;
  end;

  inc(fCurrentLine);

  fLine :=  fLines[fCurrentLine];
  Result := fLine;
end;

procedure TScriptPersister.ReadPackageList(compilationData: TCompilationData);
begin

end;

procedure TScriptPersister.Save(const compilationData: TCompilationData; const scriptFilePath: string);
var
  script: TScriptWriter;
  package: TPackageInfo;
  i: Integer;
begin
  if compilationData = nil then exit;
  script := TScriptWriter.Create;
  try
    with compilationData, script do begin
      //basefolder
      WriteHeader('basefolder');
        WriteDetail(compilationData.BaseFolder);
        
      WriteHeader('dcp-output-folder');
        WriteDetail(compilationData.DCPOutputFolder);

      WriteHeader('bpl-output-folder');
        WriteDetail(compilationData.BPLOutputFolder);

      WriteHeader('packages');
      for i := 0 to compilationData.PackageList.Count-1 do
          WriteDetail(compilationData.PackageList[i].FileName);
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
