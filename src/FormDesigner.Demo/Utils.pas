unit Utils;

interface

uses Classes, ExtCtrls, Controls, Windows, Vcl.Imaging.PngImage, Vcl.ComCtrls,
  Vcl.Graphics, System.SysUtils;

function AddResourceToImageList(ImageList: TImageList; ResourceName: String): Integer;
function AddControlToToolbar(ToolBar: TToolBar; ImageIndex: Integer; ControlName: String; ControlClass: TControlClass) : TToolButton;
function FormatControlInfo(Control: TControl): String;

implementation

function AddControlToToolbar(ToolBar: TToolBar; ImageIndex: Integer; ControlName: String; ControlClass: TControlClass) : TToolButton;
var
  Button, LastBtn: TToolButton;
  LastBtnIdx: Integer;
begin
  Button := TToolButton.Create(ToolBar);
  Button.ImageIndex := ImageIndex;
  Button.Hint := ControlName;
  Button.ShowHint := True;
  Button.Style := tbsCheck;
  Button.Grouped := True;
  Button.Tag := Integer(ControlClass);
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
