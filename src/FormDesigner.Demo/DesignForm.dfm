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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CheckBox1: TCheckBox
    Left = 24
    Top = 296
    Width = 97
    Height = 17
    Caption = 'DrawGrid'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 128
    Top = 296
    Width = 97
    Height = 17
    Caption = 'SnapToGrid'
    TabOrder = 1
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 240
    Top = 296
    Width = 105
    Height = 17
    Caption = 'ImmediateMode'
    TabOrder = 2
    OnClick = CheckBox3Click
  end
  object fdDesigner: TFormDesigner
    SnapToGrid = False
    Color = 15980210
    DragMode = dmImmediate
    OnControlAdded = fdDesignerControlAdded
    OnControlSelected = fdDesignerEvent
    OnControlModified = fdDesignerEvent
    Left = 104
    Top = 80
  end
end
