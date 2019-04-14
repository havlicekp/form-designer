unit DesignForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormDesigner.Designer, ComponentsForm,
  Vcl.StdCtrls, TypInfo, Vcl.ToolWin, Vcl.ComCtrls;

type
  TfrmDesignForm = class(TForm)
    fdDesigner: TFormDesigner;
    procedure FormActivate(Sender: TObject);
    procedure fdDesignerControlAdded(Sender: TObject);
  end;

var
  frmDesignForm: TfrmDesignForm;
  frmComponentsForm: TfrmComponentsForm;

implementation

{$R *.dfm}

procedure TfrmDesignForm.fdDesignerControlAdded(Sender: TObject);
var
  i: Integer;
  Button: TToolButton;
begin
  for i := 0 to frmComponentsForm.tbControls.ButtonCount - 1 do
  begin
    Button := frmComponentsForm.tbControls.Buttons[i];
    if Button.Down then
      Button.Down := False;
  end;
end;

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
