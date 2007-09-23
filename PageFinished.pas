unit PageFinished;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PageBase, StdCtrls;

type
  TFinishedPage = class(TWizardPage)
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FinishedPage: TFinishedPage;

implementation

{$R *.dfm}

end.
