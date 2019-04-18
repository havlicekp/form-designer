unit FormDesigner.DragHandles;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, FormDesigner.Interfaces, FormDesigner.Utils;

type

  /// Base class for drag handles
  TDragHandle = class(TCustomControl)
  private
    FClickOrigin: TPoint;
    FFormDesigner: IFormDesigner;
  public
    procedure SetSizingOrigin(const X, Y: Integer);
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); virtual; abstract;
    procedure UpdatePosition(Control: TControl); virtual; abstract;
    procedure SetProps(ASize: byte; AColor: TColor; AFormDesigner: IFormDesigner);
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
  end;

  TUpDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TDownDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TLeftDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TRightDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TUpLeftDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TUpRightDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TDownLeftDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TDownRightDragHandle = class(TDragHandle)
  public
    procedure UpdatePosition(Control: TControl); override;
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

// ----------------------------------------------------------
// TDragHandles
// ----------------------------------------------------------

constructor TDragHandle.Create;
begin
  inherited Create(AOwner);
  Visible := False;
  FClickOrigin := TPoint.Zero;
end;

procedure TDragHandle.SetSizingOrigin(const X, Y: Integer);
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

  Log('TDragHandle', 'X: %d, Y: %d, FClickOrigin.X: %d, FClickOrigin.Y: %d', [X, Y, FClickOrigin.X, FClickOrigin.Y]);
end;

constructor TUpDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNS;
end;

constructor TDownDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNS;
end;

constructor TLeftDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeWE;
end;

constructor TRightDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeWE;
end;

constructor TUpLeftDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNWSE;
end;

constructor TUpRightDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNESW;
end;

constructor TDownLeftDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNESW;
end;

constructor TDownRightDragHandle.Create;
begin
  inherited Create(AOwner);
  Cursor := crSizeNWSE;
end;

procedure TUpDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.Left + ((Control.Width - Width) div 2);
  Top := Control.Top - (Height div 2);
end;

procedure TDownDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.Left + ((Control.Width - Width) div 2);
  Top := Control.Top + Control.Height - (Height div 2);
end;

procedure TLeftDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.Left - (Width div 2);
  Top := Control.Top + ((Control.Height - Height) div 2);
end;

procedure TRightDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.Left + Control.Width - (Width div 2) ;
  Top := Control.Top + ((Control.Height - Height) div 2);
end;

procedure TUpLeftDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.Left - (Width div 2);
  Top := Control.Top - (Height div 2);
end;

procedure TDownLeftDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.Left - (Width div 2);
  Top := Control.Top + Control.Height - (Height div 2);
end;

procedure TUpRightDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.BoundsRect.Right - (Width div 2);
  Top := Control.Top - (Height div 2);
end;

procedure TDownRightDragHandle.UpdatePosition(Control: TControl);
begin
  Left := Control.BoundsRect.Right - (Width div 2);
  Top := Control.Top + Control.Height - (Height div 2);
end;

procedure TDragHandle.Paint;
begin
  inherited;
  Canvas.Pen.Color := RGB(0, 120, 215);
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Color := Color;
  Canvas.Rectangle(0, 0, BoundsRect.Width, BoundsRect.Height);
end;

procedure TDragHandle.SetProps(ASize: byte; AColor: TColor; AFormDesigner: IFormDesigner);
begin
  Width := ASize;
  Height := ASize;
  Color := AColor;
  FFormDesigner := AFormDesigner;
end;

procedure TUpDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
var
  Rect: TRect;
  ChildRect: TRect;
begin
  Rect := FFormDesigner.GetRect();
  ChildRect := FFormDesigner.GetChildRect();
  Log('TUpDragHandle', 'Orig Rect', Rect);
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

procedure TDownDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
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

procedure TRightDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
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

procedure TLeftDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
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

procedure TUpLeftDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
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

procedure TUpRightDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
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

procedure TDownLeftDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
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

procedure TDownRightDragHandle.MouseMoveHandler(Sender: TControl; X, Y: integer);
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
