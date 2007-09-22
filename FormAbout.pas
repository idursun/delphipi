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
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.dfm}

procedure TfrmAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.Label2Click(Sender: TObject);
begin
  ShowMessage('open it yourself');
end;

end.
