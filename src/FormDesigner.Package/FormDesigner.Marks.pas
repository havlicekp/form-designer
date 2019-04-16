unit FormDesigner.Marks;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, FormDesigner.Interfaces, FormDesigner.Utils;

type

  { TMARK }
  TMark = class(TCustomControl)
  private
    FClickOrigin: TPoint;
    FFormDesigner: IFormDesigner;
  public
    procedure SetSizingOrigin(const X, Y: Integer);
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); virtual; abstract;
    procedure Update(Control: TControl); virtual; abstract;
    procedure SetProps(ASize: byte; AColor: TColor; AFormDesigner: IFormDesigner);
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TUPMARK }
  TUpMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TDOWNMARK }
  TDownMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TLEFTMARK }
  TLeftMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TRIGHTMARK }
  TRightMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TUPLEFTMARK }
  TUpLeftMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TUPRIGHTMARK }
  TUpRightMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TDOWNLEFTMARK }
  TDownLeftMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  { TDOWNRIGHTMARK }
  TDownRightMark = class(TMark)
  public
    procedure Update(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
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
  FClickOrigin := TPoint.Zero;
end;

procedure TMark.SetSizingOrigin(const X, Y: Integer);
var
  HalfWidth: Integer;
begin
  inherited;
  HalfWidth := Width div 2;
  if (X <> HalfWidth) then
  begin
    if (X > HalfWidth) then
      FClickOrigin.X := -(X mod HalfWidth)
    else
      FClickOrigin.X := HalfWidth - X;
  end;

  if (Y <> HalfWidth) then
  begin
    if (Y > HalfWidth) then
      FClickOrigin.Y := -(Y mod HalfWidth)
    else
      FClickOrigin.Y := HalfWidth - Y;
  end;

  Log('TMark', 'X: %d, Y: %d, FClickOrigin.X: %d, FClickOrigin.Y: %d', [X, Y, FClickOrigin.X, FClickOrigin.Y]);
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

procedure TUpMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  Log('TUpMark', 'Orig Rect', Rect);
  with Rect do
  begin
    if (Y <> Top) and (Y <> Bottom) then
    begin
      if Y > ChildRect.Top + ChildRect.Height then
      begin
        Top := ChildRect.Top + ChildRect.Height;
        Bottom := Y + FClickOrigin.Y;
      end
      else
      begin
        Top := Y + FClickOrigin.Y;
        Bottom := ChildRect.Top + ChildRect.Height;
      end;
      FFormDesigner.UpdateRect(Rect, dUp);
    end;
  end;
end;

procedure TDownMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  with Rect do
  begin
    if (Y <> Rect.Bottom) and (Y <> Rect.Top) then
    begin
      if Y < ChildRect.Top then
      begin
        Rect.Top := Y + FClickOrigin.Y;
        Rect.Bottom := ChildRect.Top
      end
      else
      begin
        Rect.Bottom := Y + FClickOrigin.Y;
        Rect.Top := ChildRect.Top
      end;
      FFormDesigner.UpdateRect(Rect, dDown);
    end;
  end;
end;

procedure TRightMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
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
        Rect.Right := X + FClickOrigin.X;
      end
      else
      begin
        Rect.Left := X + FClickOrigin.X;
        Rect.Right := ChildRect.Left;
      end;
      FFormDesigner.UpdateRect(Rect, dRight);
    end;
  end;
end;

procedure TLeftMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
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
        Rect.Right := X + FClickOrigin.X;
      end
      else
      begin
        Rect.Left := X + FClickOrigin.X;
        Rect.Right := ChildRect.Left + ChildRect.Width;
      end;
      FFormDesigner.UpdateRect(Rect, dLeft);
    end;
  end;
end;

procedure TUpLeftMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
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
      Rect.Left := X + FClickOrigin.X;
      Rect.Bottom := ChildRect.Top + ChildRect.Height;
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y < ChildRect.Top + ChildRect.Height)
    then
    begin
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Bottom := ChildRect.Top + ChildRect.Height;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X + FClickOrigin.X;
    end;
    if (X < ChildRect.Left + ChildRect.Width) and (Y > ChildRect.Top + ChildRect.Height)
    then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Left := X + FClickOrigin.X;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y > ChildRect.Top + ChildRect.Height)
    then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X + FClickOrigin.X;
    end;
    FFormDesigner.UpdateRect(Rect, dUpLeft);
  end;
end;

procedure TUpRightMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
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
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Right := X + FClickOrigin.X;
    end;
    if (X < ChildRect.Left) and (Y < ChildRect.Top + ChildRect.Height) then
    begin
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Bottom := ChildRect.Top + ChildRect.Height;
      Rect.Left := X + FClickOrigin.X;
      Rect.Right := ChildRect.Left;
    end;
    if (X > ChildRect.Left) and (Y > ChildRect.Top + ChildRect.Height) then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Left := ChildRect.Left;
      Rect.Right := X + FClickOrigin.X;
    end;
    if (X < ChildRect.Left) and (Y > ChildRect.Top + ChildRect.Height) then
    begin
      Rect.Top := ChildRect.Top + ChildRect.Height;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Left := X + FClickOrigin.X;
      Rect.Right := ChildRect.Left;
    end;
    FFormDesigner.UpdateRect(Rect, dUpRight);
  end;
end;

procedure TDownLeftMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
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
      Rect.Left := X + FClickOrigin.X;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Top := ChildRect.Top;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X + FClickOrigin.X;
    end;
    if (X < ChildRect.Left + ChildRect.Width) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := X + FClickOrigin.X;
      Rect.Right := ChildRect.Left + ChildRect.Width;
    end;
    if (X > ChildRect.Left + ChildRect.Width) and (Y > ChildRect.Top) then
    begin
      Rect.Top := ChildRect.Top;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Left := ChildRect.Left + ChildRect.Width;
      Rect.Right := X + FClickOrigin.X;
    end;
    FFormDesigner.UpdateRect(Rect, dDownLeft);
  end;
end;

procedure TDownRightMark.MouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  Log('DonwnRightMark', 'MouseMoveHandler Rect', Rect);
  ChildRect := FFormDesigner.GetChildRect();
  Log('DonwnRightMark', 'MouseMoveHandler Child', ChildRect);
  with Rect do  begin
    if (X > ChildRect.Left) and (Y > ChildRect.Top) then
    begin
      Rect.Left := ChildRect.Left;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Top := ChildRect.Top;
      Rect.Right := X + FClickOrigin.X;
    end;
    if (X < ChildRect.Left) and (Y > ChildRect.Top) then
    begin
      Rect.Top := ChildRect.Top;
      Rect.Bottom := Y + FClickOrigin.Y;
      Rect.Left := X + FClickOrigin.X;
      Rect.Right := ChildRect.Left;
    end;
    if (X < ChildRect.Left) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := X + FClickOrigin.X;
      Rect.Right := ChildRect.Left;
    end;
    if (X > ChildRect.Left) and (Y < ChildRect.Top) then
    begin
      Rect.Top := Y + FClickOrigin.Y;
      Rect.Bottom := ChildRect.Top;
      Rect.Left := ChildRect.Left;
      Rect.Right := X + FClickOrigin.X;
    end;
    FFormDesigner.UpdateRect(Rect, dDownRight);
  end;
end;

end.
