unit ComponentsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormDesigner.Designer, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.ToolWin,
  Vcl.Menus, Utils;

type
  TfrmComponentsForm = class(TForm)
    ilImages: TImageList;
    tbControls: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
  end;

var
  frmComponentsForm: TfrmComponentsForm;

implementation

{$R *.dfm}

uses
  DesignForm;

const
  ControlClasses: array[0..12] of TControlClass =
    (TButton, TLabel, TEdit, TCheckBox, TRadioButton, TMemo, TComboBox,
    TListBox, TProgressBar, TGroupBox, TPanel, TRadioGroup, TShape);

procedure TfrmComponentsForm.FormCreate(Sender: TObject);
var
  ControlClass: TControlClass;
  ImageIndex: Integer;
  i: Integer;
  Btn: TToolButton;
begin
  for i := 0 to Length(ControlClasses) - 1 do
  begin
    ControlClass := ControlClasses[i];
    ImageIndex := AddResourceToImageList(ilImages, ControlClass.ClassName);
    Btn := AddControlToToolbar(tbControls, ImageIndex, ControlClass.ClassName, ControlClass);
    Btn.OnClick := ButtonClick;
  end;
end;

procedure TfrmComponentsForm.ButtonClick(Sender: TObject);
var
  Button: TToolButton;
  ControlClass: TControlClass;
begin
  Button := TToolButton(Sender);
  ControlClass := TControlClass(Button.Tag);
  frmDesignForm.fdDesigner.AddControl(ControlClass);
end;

end.
