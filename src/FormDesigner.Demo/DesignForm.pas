unit DesignForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormDesigner.Designer, ComponentsForm,
  Vcl.StdCtrls;

type
  TfrmDesignForm = class(TForm)
    fdDesigner: TFormDesigner;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDesignForm: TfrmDesignForm;
  frmComponentsForm: TfrmComponentsForm;

implementation

{$R *.dfm}

procedure TfrmDesignForm.FormActivate(Sender: TObject);
begin
  if not Assigned(frmComponentsForm) then
  begin
    frmComponentsForm := TfrmComponentsForm.Create(Self);
    frmComponentsForm.Top := Top;
    frmComponentsForm.Left := Left - frmComponentsForm.Width - 10;
    frmComponentsForm.Show;
  end;
end;

end.
