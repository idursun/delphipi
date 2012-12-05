{ **
  DelphiPI (Delphi Package Installer)
  Author  : ibrahim dursun (ibrahimdursun gmail)
  License : GNU General Public License 2.0
** }

unit TreeModel;
interface

uses Classes, StrUtils, Generics.Collections;
type
  TDynStrArray = array of string;

  INode = interface
     function GetDisplayName: string;
     function GetData: TObject;
     function GetNodePath: string;
  end;

  TCreateLogicalNodeHandler<T> = reference to function(name, path:string):T;
  TTreeModelBase<T: INode > = class
  private
    fOnCreateLogicalNode: TCreateLogicalNodeHandler<T>;
  protected
    function DoCreateLogicalNode(name, path:string):T; virtual;
  public
    function GetChild(const parent: T; index: Integer): T; virtual; abstract;
    function GetChildCount(const parent: T): integer; virtual; abstract;
    property OnCreateLogicalNode: TCreateLogicalNodeHandler<T> read fOnCreateLogicalNode write fOnCreateLogicalNode;
  end;

implementation

uses JclStrings;

function TTreeModelBase<T>.DoCreateLogicalNode(name, path: string): T;
begin
  if Assigned(fOnCreateLogicalNode) then
    Result := fOnCreateLogicalNode(name,path)
  else
    Result := default(T);
end;

end.
