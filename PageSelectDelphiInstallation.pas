{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit PageSelectDelphiInstallation;

interface

uses
  CompilationData, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls, ExtCtrls, WizardIntfs, JclBorlandTools;

type
  TSelectDelphiInstallationPage = class(TWizardPage)
    rgDelphiVersions: TRadioGroup;
    grpOutputFolders: TGroupBox;
    lblBPLOutputFolder: TLabel;
    edtBPL: TEdit;
    btnBPLBrowse: TButton;
    lblDCP: TLabel;
    edtDCP: TEdit;
    btnDCPBrowse: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rgDelphiVersionsClick(Sender: TObject);
    procedure btnBPLBrowseClick(Sender: TObject);
    procedure btnDCPBrowseClick(Sender: TObject);
  private
   installations : TJclBorRADToolInstallations;
    procedure DisposeCustomObjects;
    procedure AddDelphiInstallation(const installation: TJclBorRADToolInstallation);
    procedure SaveInstallationOutputFolders(versionIndex: integer);
    procedure ShowInstallationOutputFolders(versionIndex: integer);
    procedure BrowseFolder(var folder: string);
  public
    constructor Create(Owner: TComponent; const compilationData: TCompilationData); override; 
    procedure UpdateWizardState; override;
    function CanShowPage: Boolean; override;
  end;

var
  SelectDelphiInstallationPage: TSelectDelphiInstallationPage;

implementation
uses gnugettext, FileCtrl;
type
  TCustomOutputFolders = class
     BPL: string;
     DCP: string;
  end;
  
var
  lastSelectedIndex : Integer;
{$R *.dfm}

{ TSelectDelphiInstallationPage }

constructor TSelectDelphiInstallationPage.Create(Owner: TComponent;
  const compilationData: TCompilationData);
begin
  inherited;
  FCompilationData := compilationData;
end;

procedure TSelectDelphiInstallationPage.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  outputFolders: TCustomOutputFolders;  
begin
  inherited;
  SaveInstallationOutputFolders(rgDelphiVersions.ItemIndex);

  outputFolders := rgDelphiVersions.Items.Objects[rgDelphiVersions.ItemIndex] as TCustomOutputFolders;
  if outputFolders <> nil then
  begin
    fCompilationData.BPLOutputFolder := outputFolders.BPL;
    fCompilationData.DCPOutputFolder := outputFolders.DCP;
  end;

  DisposeCustomObjects;
  if installations.Count = 1 then
    FCompilationData.Installation := installations[0]
  else
    FCompilationData.Installation := installations.Installations[rgDelphiVersions.ItemIndex];
end;

procedure TSelectDelphiInstallationPage.FormCreate(Sender: TObject);
var
  i: integer;
begin
  inherited;
  TranslateComponent(self);
  installations := TJclBorRADToolInstallations.Create;

  for i := 0 to installations.Count - 1 do begin
    AddDelphiInstallation(installations.Installations[i]);
  end;
  lastSelectedIndex := 0;
  //TODO: does this work?
  if fCompilationData.Installation <> nil then
    for i := 0 to installations.Count - 1 do begin
      if (installations.Installations[i].VersionNumber <> FCompilationData.Installation.VersionNumber) then
        continue;
      lastSelectedIndex := i;
    end;

  if (fCompilationData.Scripting) then
  begin
    edtBPL.Text := fCompilationData.BPLOutputFolder;
    edtDCP.Text := fCompilationData.DCPOutputFolder;
  end else begin
    //TODO: Eliminate hidden behaviour, radio button click event handler affects  custom folder save and show process
    ShowInstallationOutputFolders(lastSelectedIndex);
  end;

  rgDelphiVersions.ItemIndex := lastSelectedIndex;
end;

procedure TSelectDelphiInstallationPage.rgDelphiVersionsClick(Sender: TObject);
begin
  inherited;
  SaveInstallationOutputFolders(lastSelectedIndex);
  ShowInstallationOutputFolders(rgDelphiVersions.ItemIndex);
  lastSelectedIndex := rgDelphiVersions.ItemIndex;
end;

procedure TSelectDelphiInstallationPage.BrowseFolder(var folder:string);
var
  directory: string;
begin
  directory := '';
  if SelectDirectory(_('Select output folder'),'',directory) then begin
     folder := directory;
  end;
end;

procedure TSelectDelphiInstallationPage.btnBPLBrowseClick(Sender: TObject);
var
  path: string;
begin
  inherited;
  path := edtBPL.Text;
  BrowseFolder(path);
  edtBPL.Text := path;
end;

procedure TSelectDelphiInstallationPage.btnDCPBrowseClick(Sender: TObject);
var
  path: string;
begin
  inherited;
  path := edtDCP.Text;
  BrowseFolder(path);
  edtDCP.Text := path;
end;

function TSelectDelphiInstallationPage.CanShowPage: Boolean;
begin
   Result := True;
  //Result := FCompilationData.Scripting;
end;

procedure TSelectDelphiInstallationPage.SaveInstallationOutputFolders(versionIndex: integer);
var
  outputFolders: TCustomOutputFolders;
begin
  if (versionIndex < 0) or (versionIndex > rgDelphiVersions.Items.Count) then exit;
  
  outputFolders := rgDelphiVersions.Items.Objects[versionIndex] as TCustomOutputFolders;
  if outputFolders <> nil then
  begin
    outputFolders.BPL := edtBPL.Text;
    outputFolders.DCP := edtDCP.Text;
  end;
end;

procedure TSelectDelphiInstallationPage.ShowInstallationOutputFolders(versionIndex: integer);
var
  outputFolders: TCustomOutputFolders;
begin
  if (versionIndex < 0) or (versionIndex > rgDelphiVersions.Items.Count) then versionIndex := 0;
  outputFolders := rgDelphiVersions.Items.Objects[versionIndex] as TCustomOutputFolders;
  if outputFolders <> nil then
  begin
    edtBPL.Text := outputFolders.BPL;
    edtDCP.Text := outputFolders.DCP;
  end;
end;

procedure TSelectDelphiInstallationPage.AddDelphiInstallation(const installation: TJclBorRADToolInstallation);
var
  outputFolders: TCustomOutputFolders;
  i: integer;
begin
  i := rgDelphiVersions.Items.Add(installation.Description);
  outputFolders := TCustomOutputFolders.Create;
  outputFolders.BPL := installation.BPLOutputPath;
  outputFolders.DCP := installation.DCPOutputPath;
  rgDelphiVersions.Items.Objects[i] := outputFolders;
end;

procedure TSelectDelphiInstallationPage.DisposeCustomObjects;
var
  I: Integer;
  outputFolder: TCustomOutputFolders;
begin
  for I := 0 to rgDelphiVersions.Items.Count - 1 do
  begin
    outputFolder := rgDelphiVersions.Items.Objects[i] as TCustomOutputFolders;
    if outputFolder <> nil then
      outputFolder.Free;
  end;
end;

procedure TSelectDelphiInstallationPage.UpdateWizardState;
begin
  inherited;
  wizard.SetHeader(_('Select Delphi Installation'));
  wizard.SetDescription(_('Please select delphi installation that you want to compile with'));
end;

end.
