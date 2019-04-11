unit FormDesigner1;

interface

uses
  System.SysUtils, System.Classes, FormDesigner.Designer;

type
  TFormDesigner1 = class(TFormDesigner)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Form Designer', [TFormDesigner1]);
end;

end.
