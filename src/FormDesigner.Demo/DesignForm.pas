unit DesignForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormDesigner.Designer, ComponentsForm,
  Vcl.StdCtrls, TypInfo, Vcl.ToolWin, Vcl.ComCtrls, Vcl.ExtCtrls,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.Components;

type
  TfrmDesignForm = class(TForm)
    fdDesigner: TFormDesigner;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure fdDesignerControlAdded(Sender: TObject);
    procedure fdDesignerEvent(Sender: TObject);
    function UpdateCaption(Control: TControl): String;
    procedure FormCreate(Sender: TObject);
    procedure Button1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Label1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
  end;

var
  frmDesignForm: TfrmDesignForm;
  frmComponentsForm: TfrmComponentsForm;

implementation

{$R *.dfm}

procedure TfrmDesignForm.Button1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Caption :=  Format('Button1: %d, %d', [X, Y]);
end;

procedure TfrmDesignForm.CheckBox1Click(Sender: TObject);
begin
  fdDesigner.DrawGrid := CheckBox1.Checked;
end;

procedure TfrmDesignForm.CheckBox2Click(Sender: TObject);
begin
  fdDesigner.SnapToGrid := CheckBox2.Checked;
end;

procedure TfrmDesignForm.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
    fdDesigner.DragMode := dmImmediate
  else
    fdDesigner.DragMode := dmDeferred;
end;

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

procedure TfrmDesignForm.fdDesignerEvent(Sender: TObject);
begin
  UpdateCaption(TControl(Sender));
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

procedure TfrmDesignForm.FormCreate(Sender: TObject);
begin
  CheckBox1.Checked := fdDesigner.DrawGrid;
  CheckBox2.Checked := fdDesigner.SnapToGrid;
  CheckBox3.Checked := fdDesigner.DragMode = dmImmediate;
end;

procedure TfrmDesignForm.Label1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Caption :=  Format('Label1: %d, %d', [X, Y]);
end;

function TfrmDesignForm.UpdateCaption(Control: TControl): String;
begin
  if Assigned(Control) then
    Caption := Format('Form Designer - %s: %s; Rect (%d, %d, %d, %d); Width: %d, Height: %d',
    [Control.Name, Control.ClassName, Control.Left, Control.Top, Control.BoundsRect.Right,
    Control.BoundsRect.Bottom, COntrol.Width, Control.Height])
  else
    Caption := 'Form Designer - Nothing Selected';
end;


end.
