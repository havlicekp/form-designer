object frmDesignForm: TfrmDesignForm
  Left = 100
  Top = 400
  Caption = 'Form Designer Demo'
  ClientHeight = 336
  ClientWidth = 584
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object fdDesigner: TFormDesigner
    Color = 15980210
    DragMode = dmDeferred
    OnControlAdded = fdDesignerControlAdded
    Left = 80
    Top = 208
  end
end
