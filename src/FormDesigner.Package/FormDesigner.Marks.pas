unit FormDesigner.Marks;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, FormDesigner.Interfaces, FormDesigner.Utils;

type

  { TMARK }
  TMark = class(TCustomControl)
  private
    FFormDesigner: IFormDesigner;
  public
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); virtual; abstract;
    procedure Update(Control: TControl); virtual; abstract;
    procedure SetProps(ASize: byte; AColor: TColor; AFormDesigner: IFormDesigner);
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TUPMARK }
  TUpMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TDOWNMARK }
  TDownMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TLEFTMARK }
  TLeftMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TRIGHTMARK }
  TRightMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TUPLEFTMARK }
  TUpLeftMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TUPRIGHTMARK }
  TUpRightMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TDOWNLEFTMARK }
  TDownLeftMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TDOWNRIGHTMARK }
  TDownRightMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

// ----------------------------------------------------------
// TMarks
// ----------------------------------------------------------

constructor TMark.Create;
begin
  inherited Create(AOwner);
  Visible := False;
end;

constructor TUpMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNS;
end;

constructor TDownMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNS;
end;

constructor TLeftMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeWE;
end;

constructor TRightMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeWE;
end;

constructor TUpLeftMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNWSE;
end;

constructor TUpRightMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNESW;
end;

constructor TDownLeftMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNESW;
end;

constructor TDownRightMark.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNWSE;
end;

procedure TUpMark.Update(Control: TControl);
begin
  Left := Control.Left + ((Control.Width - Width) div 2);
  Top := Control.Top - (Height div 2);
end;

procedure TDownMark.Update(Control: TControl);
begin
  Left := Control.Left + ((Control.Width - Width) div 2);
  Top := Control.Top + Control.Height - (Height div 2);
end;

procedure TLeftMark.Update(Control: TControl);
begin
  Left := Control.Left - (Width div 2);
  Top := Control.Top + ((Control.Height - Height) div 2);
end;

procedure TRightMark.Update(Control: TControl);
begin
  Left := Control.Left + Control.Width - (Width div 2) ;
  Top := Control.Top + ((Control.Height - Height) div 2);
end;

procedure TUpLeftMark.Update(Control: TControl);
begin
  Left := Control.Left - (Width div 2);
  Top := Control.Top - (Height div 2);
end;

procedure TDownLeftMark.Update(Control: TControl);
begin
  Left := Control.Left - (Width div 2);
  Top := Control.Top + Control.Height - (Height div 2);
end;

procedure TUpRightMark.Update(Control: TControl);
begin
  Left := Control.BoundsRect.Right - (Width div 2);
  Top := Control.Top - (Height div 2);
end;

procedure TDownRightMark.Update(Control: TControl);
begin
  Left := Control.BoundsRect.Right - (Width div 2);
  Top := Control.Top + Control.Height - (Height div 2);
end;

procedure TMark.Paint;
begin
  inherited;
  Canvas.Pen.Color := RGB(0, 120, 215);
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Color := Color;
  Canvas.Rectangle(0, 0, BoundsRect.Width, BoundsRect.Height);
end;

procedure TMark.SetProps(ASize: byte; AColor: TColor; AFormDesigner: IFormDesigner);
begin
  Width := ASize;
  Height := ASize;
  FFormDesigner := AFormDesigner;
  Color := AColor;
end;

procedure TUpMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do
  begin
    if (Y <> Top) and (Y <> Bottom) then
    begin
      if Y > ChildRect.Top + ChildRect.Height then
      begin
        Top := ChildRect.Top + ChildRect.Height;
        Bottom := Y;
      end
      else
      begin
        Top := Y;
        Bottom := ChildRect.Top + ChildRect.Height;
      end;
      FFormDesigner.UpdateRect(Rect, dUp, Rect.Top, Rect.Bottom);
    end;
  end;
end;

procedure TDownMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
    Log('TDownMark', 'ChildRect (%d, %d, %d, %d), Rect (%d, %d, %d, %d)', [ChildRect.Left, ChildRect.Top, ChildRect.Right, ChildRect.Bottom, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]);
  with Rect do
  begin
    if (Y <> Rect.Bottom) and (Y <> Rect.Top) then
    begin
      if Y < ChildRect.Top then
      begin
        Rect.Top := Y;
        Rect.Bottom := ChildRect.Top
      end
      else
      begin
        Rect.Bottom := Y;
        Rect.Top := ChildRect.Top
      end;
      FFormDesigner.UpdateRect(Rect, dDown, Rect.Bottom, Rect.Top);
    end;
  end;
      Log('TDownMark', 'ChildRect (%d, %d, %d, %d), Rect (%d, %d, %d, %d)', [ChildRect.Left, ChildRect.Top, ChildRect.Right, ChildRect.Bottom, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]);
end;

procedure TRightMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do
  begin
    if (X <> Rect.Right) and (X <> Rect.Left) then
    begin
      if X > ChildRect.Left then
      begin
        Rect.Left := ChildRect.Left;
        Rect.Right := X;
      end
      else
      begin
        Rect.Left := X;
        Rect.Right := ChildRect.Left;
      end;
      FFormDesigner.UpdateRect(Rect, dRight, Rect.Right, Rect.Top);
    end;
  end;
end;

procedure TLeftMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do
  begin
    if (X <> Rect.Left) and (X <> Rect.Right) then
    begin
      if X > ChildRect.Left + ChildRect.Width then
      begin
        Rect.Left := ChildRect.Left + ChildRect.Width;
        Rect.Right := X;
      end
      else
      begin
        Rect.Left := X;
        Rect.Right := ChildRect.Left + ChildRect.Width;
      end;
      FFormDesigner.UpdateRect(Rect, dLeft, Rect.Left, Rect.Bottom);
    end;
  end;
end;

procedure TUpLeftMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do
  begin
    if (X < ChildRect.Left + ChildRect.Width) and (Y < ChildRect.Top + ChildRect.Height)
    then
    begin
      Rect.Left := X;
      Rect.Bottom := ChildRect.Top + ChildRect.Height;
      Rect.Top := Y;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y < ChildRect.Top + ChildRect.Height)
    then
    begin
      Rect.Top := Y;
      Rect.Bottom := ChildRect.Top + ChildRect.Height;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X;
    end;
    if (X < ChildRect.Left + ChildRect.Width) and (Y > ChildRect.Top + ChildRect.Height)
    then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y;
      Rect.Left := X;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y > ChildRect.Top + ChildRect.Height)
    then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X;
    end;
    FFormDesigner.UpdateRect(Rect, dUpLeft, Rect.Top, Rect.Left);
  end;
end;

procedure TUpRightMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do
  begin
    if (X > ChildRect.Left) and (Y < ChildRect.Top + ChildRect.Height) then
    begin
      Rect.Left := ChildRect.Left;
      Rect.Bottom := ChildRect.Top + ChildRect.Height;
      Rect.Top := Y;
      Rect.Right := X;
    end;
    if (X < ChildRect.Left) and (Y < ChildRect.Top + ChildRect.Height) then
    begin
      Rect.Top := Y;
      Rect.Bottom := ChildRect.Top + ChildRect.Height;
      Rect.Left := X;
      Rect.Right := ChildRect.Left;
    end;
    if (X > ChildRect.Left) and (Y > ChildRect.Top + ChildRect.Height) then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y;
      Rect.Left := ChildRect.Left;
      Rect.Right := X;
    end;
    if (X < ChildRect.Left) and (Y > ChildRect.Top + ChildRect.Height) then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y;
      Rect.Left := X;
      Rect.Right := ChildRect.Left;
    end;
    FFormDesigner.UpdateRect(Rect, dUpRight, Rect.Top, Rect.Left);
  end;
end;

procedure TDownLeftMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do
  begin
    if (X < ChildRect.Left + ChildRect.Width) and (Y > ChildRect.Top) then
    begin
      Rect.Left := X;
      Rect.Bottom := Y;
      Rect.Top := ChildRect.Top;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X;
    end;
    if (X < ChildRect.Left + ChildRect.Width) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := X;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y > ChildRect.Top) then
    begin
      Rect.Top := ChildRect.Top;
      Rect.Bottom := Y;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X;
    end;
    FFormDesigner.UpdateRect(Rect, dDownLeft, Rect.Left, Rect.Bottom);
  end;
end;

procedure TDownRightMark.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do  begin
    if (X > ChildRect.Left) and (Y > ChildRect.Top) then
    begin
      Rect.Left := ChildRect.Left;
      Rect.Bottom := Y;
      Rect.Top := ChildRect.Top;
      Rect.Right := X;
    end;
    if (X < ChildRect.Left) and (Y > ChildRect.Top) then
    begin
      Rect.Top := ChildRect.Top;
      Rect.Bottom := Y;
      Rect.Left := X;
      Rect.Right := ChildRect.Left;
    end;
    if (X < ChildRect.Left) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := X;
      Rect.Right := ChildRect.Left;
    end;
    if (X > ChildRect.Left) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := ChildRect.Left;
      Rect.Right := X;
    end;
    FFormDesigner.UpdateRect(Rect, dDownRight, Rect.Left, Rect.Bottom);
  end;
end;

end.
