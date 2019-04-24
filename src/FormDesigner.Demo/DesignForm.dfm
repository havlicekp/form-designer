object frmDesignForm: TfrmDesignForm
  Left = 0
  Top = 0
  Caption = 'Form Designer'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object fdDesigner: TFormDesigner
    ShowHints = True
    OnControlAdded = fdDesignerControlAdded
    OnControlSelected = fdDesignerEvent
    OnControlModified = fdDesignerEvent
    Left = 48
    Top = 48
  end
end
