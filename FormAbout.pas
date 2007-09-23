{**
 DelphiPI (Delphi Package Installer)
 Author  : ibrahim dursun (t-hex) thex [at] thexpot ((dot)) net
 License : GNU General Public License 2.0
**}
unit FormAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmAbout = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Memo1: TMemo;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Image2: TImage;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation
uses ShellApi;
{$R *.dfm}

procedure TfrmAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.Label13Click(Sender: TObject);
begin
  ShellExecute(0,'open',PAnsiChar(Label13.Caption), NIL, NIL, SW_SHOWNORMAL);
end;

procedure TfrmAbout.Label1Click(Sender: TObject);
begin
  ShellExecute(0,'open',PAnsiChar(Label1.Caption), NIL, NIL, SW_SHOWNORMAL);
end;

procedure TfrmAbout.Label2Click(Sender: TObject);
begin
//  ShowMessage('open it yourself');
end;

end.
