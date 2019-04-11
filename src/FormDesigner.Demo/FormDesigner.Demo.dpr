program FormDesigner.Demo;

{$R *.dres}

uses
  Vcl.Forms,
  ComponentsForm in 'ComponentsForm.pas' {frmComponentsForm},
  Vcl.Themes,
  Vcl.Styles,
  Utils in 'Utils.pas',
  DesignForm in 'DesignForm.pas' {frmDesignForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDesignForm, frmDesignForm);
  Application.Run;
end.
