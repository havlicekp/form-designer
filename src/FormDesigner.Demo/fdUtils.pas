unit fdUtils;

interface

uses Classes, ExtCtrls, Controls, Windows, Vcl.Imaging.PngImage, Vcl.ComCtrls,
  Vcl.Graphics, System.SysUtils;

function AddResourceToImageList(ImageList: TImageList; ResourceName: String): Integer;
function AddControlToToolbar(ToolBar: TToolBar; AImageIndex: Integer; ControlName: String; ControlClass: TControlClass) : TToolButton;
function FormatControlInfo(Control: TControl): String;

implementation

function AddControlToToolbar(ToolBar: TToolBar; AImageIndex: Integer; ControlName: String; ControlClass: TControlClass) : TToolButton;
var
  Button, LastBtn: TToolButton;
  LastBtnIdx: Integer;
begin
  Button := TToolButton.Create(ToolBar);
  with Button do
  begin
    ImageIndex := AImageIndex;
    Hint := ControlName;
    ShowHint := True;
    Style := tbsCheck;
    Grouped := True;
    Tag := Integer(ControlClass);
  end;
  LastBtnIdx := ToolBar.ButtonCount - 1;
  if LastBtnIdx > -1 then
  begin
    LastBtn := ToolBar.Buttons[LastBtnIdx];
    Button.Left := LastBtn.Left + LastBtn.Width;
    Button.Top := LastBtn.Top + LastBtn.Height;
  end;
  Button.Parent := ToolBar;
  Result := Button;
end;

function AddResourceToImageList(ImageList: TImageList;
  ResourceName: String): Integer;
var
  Png: TPngImage;
  Bitmap: TBitmap;
begin
  Png := TPngImage.Create;
  Bitmap := TBitmap.Create;
  try
    Png.LoadFromResourceName(HInstance, ResourceName);
    Bitmap.Assign(Png);
    Bitmap.AlphaFormat := afIgnored;
    Result := ImageList.Add(Bitmap, nil);
  finally
    Png.Free;
    Bitmap.Free;
  end;
end;

function FormatControlInfo(Control: TControl): String;
begin
  if Assigned(Control) then
    Result :=
      Format('%s: %s; Rect (%d, %d, %d, %d); Width: %d, Height: %d',
      [Control.Name, Control.ClassName, Control.Left, Control.Top,
      Control.BoundsRect.Right, Control.BoundsRect.Bottom, Control.Width,
      Control.Height])
  else
    Result := 'Nothing Selected';
end;


end.
