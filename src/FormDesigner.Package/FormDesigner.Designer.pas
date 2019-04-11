unit FormDesigner.Designer;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, Vcl.AppEvnts, FormDesigner.Interfaces,
  FormDesigner.Utils, FormDesigner.Marks, ExtCtrls, System.DateUtils;

type

  TDragMode = (dmImmediate, dmDeferred);
  TFormDesignerException = class(Exception);
  TChildChange = procedure(OldChild, NewChild: TControl) of object;
  TSizeChanging = procedure(Control: TControl; Direction: TDirections;
    X1, X2: integer) of object;
  TBeginChange = procedure(Control: TControl) of object;
  TEndChange = TBeginChange;
  TRectDrawed = procedure(Control: TControl; Rect: TRect) of object;
  TMessageHandler = procedure(Sender: TControl; X, Y: integer) of object;
  TChildClick = TMessageHandler;
  TKeyDown = procedure(Sender: TControl; Key: Word; Shift: TShiftState)
    of object;
  TMarkProc = reference to procedure(Mark: TMark);
  TControlAdded = reference to procedure;
  TMarkClass = class of TMark;

  TWinProcedure = function(Wnd: HWnd; msg: DWORD; wParam: wParam;
    lParam: lParam): LResult; stdcall;
  TWindowProcedure = class;

  { TFormDesigner }
  TFormDesigner = class(TComponent, IFormDesigner)
  private
    FParent: TWinControl;
    FChild: TControl;
    FMarkSize: byte;
    FRect: TRect;
    FControlRect: TRect;
    FVisible: Boolean;
    FClickX: integer;
    FClickY: integer;
    FOldProcs: TObjectList<TWindowProcedure>;
    FColor: TColor;
    FState: TFormDesignerState;
    FOnChildChange: TChildChange;
    FOnSizeChanging: TSizeChanging;
    FOnBeginChange: TBeginChange;
    FOnEndChange: TEndChange;
    FOnChildClick: TChildClick;
    FOnRectDrawed: TRectDrawed;
    FApplicationEvents: TApplicationEvents;
    FForm: TForm;
    FCurrentMark: TMark;
    FDrawGrid: Boolean;
    FFormOnPaint: TNotifyEvent;
    FGridGap: Integer;
    FSnapToGrid: Boolean;
    FToolTip: TBalloonHint;
    FDragMode: TDragMode;
    FLastMouseMove: TDateTime;
    FTimer: TTimer;
    FLastMousePos: TPoint;
    FLastHintedControl: TControl;
    FOldRect: TRect;
    FControlToAdd: TControl;
    FControlToAddRect: TRect;
    procedure SetChild(Value: TControl);
    procedure SetMarkSize(Value: byte);
    procedure SetVisible(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetGridGap(Value: integer);
    procedure SetDrawGrid(Value: Boolean);
    procedure ClipCursor;
    procedure UpdateMarks;
    procedure OnMessageReceived(var msg: tagMSG; var Handled: Boolean);
    procedure CallMessageHandler(msg: UINT; Control: TWinControl;
      var pt: TPoint; Handler: TMessageHandler);
    procedure ProcessMessage(var msg: tagMSG; Handler: TMessageHandler);
    procedure OnFormPaintHandler(Sender: TObject);
    procedure ForEachMark(Proc: TMarkProc);
    procedure OnTimerHandler(Sender: TObject);
    procedure OnFormMouseEnterHandler(Sender: TObject);
    procedure ShowHint(CursorPos: TPoint; Format: String; const Args: array of const);
    function MarkOfType(MarkClass: TMarkClass) : TMark;
    property Child: TControl read FChild write SetChild;
  public
    procedure Draw;
    function GetRect: TRect;
    function GetChildRect: TRect;
    procedure SetState(State: TFormDesignerState);
    property Visible: Boolean read FVisible write SetVisible;
    procedure AddControl(ControlClass: TControlClass); overload;
    procedure AddControl(Control: TControl); overload;
    procedure FocusControl(Control: TControl);
    procedure RemoveControl(Control: TControl);
    procedure UpdateRect(Rect: TRect; Direction: TDirections;
      UpperBound: integer; LowerBound: integer);
    procedure OnLButtonUpHandler(Update: Boolean);
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer);
    procedure OnLButtonDownHandler(Sender: TControl; X, Y: integer);
    procedure OnKeyDownHandler(Sender: TControl; Key: Word; Shift: TShiftState);
    procedure StartSizing(Mark: TMark);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MarkSize: byte read FMarkSize write SetMarkSize default 6;
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid default True;
    property Color: TColor read FColor write SetColor default clBlack;
    property OnChildChange: TChildChange read FOnChildChange
      write FOnChildChange;
    property OnBeginChange: TBeginChange read FOnBeginChange
      write FOnBeginChange;
    property OnSizeChanging: TSizeChanging read FOnSizeChanging
      write FOnSizeChanging;
    property OnEndChange: TEndChange read FOnEndChange write FOnEndChange;
    property OnChildClick: TChildClick read FOnChildClick write FOnChildClick;
    property OnRectDrawed: TRectDrawed read FOnRectDrawed write FOnRectDrawed;
    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default True;
    property GridGap: integer read FGridGap write SetGridGap default 8;
    property DragMode: TDragMode read FDragMode write FDragMode default dmImmediate;
  end;

  TWindowProcedure = class
  private
    FSizer: TFormDesigner;
    FControl: TControl;
    FOldProcedure: TWndMethod;
    FOldWinProcedure: TWinProcedure;
    procedure NewProcedure(var msg: TMessage);
  end;

  procedure Register;

const
  MarkClasses: array [0 .. 7] of TMarkClass = (TUpMark, TDownMark, TLeftMark,
    TRightMark, TUpLeftMark, TUpRightMark, TDownLeftMark, TDownRightMark);

implementation

// ----------------------------------------------------------
// TWindowProcedury
// ----------------------------------------------------------

// ----------------------------------------------------------
// Tato okenni procedura je prirazena do
// WndProc prvku predanem do TFormDesigner.AddControl
//
procedure TWindowProcedure.NewProcedure(var msg: TMessage);
begin
  if (msg.msg <> WM_SETFOCUS) and (msg.msg <> WM_LBUTTONDOWN) and
    (msg.msg <> WM_CHAR) and (msg.msg <> WM_KEYDOWN) and
    (msg.msg <> WM_LBUTTONDBLCLK) and (msg.Msg <> WM_MOUSEMOVE) then
    FOldProcedure(msg);
end;

// ----------------------------------------------------------
// Spec okenni procedura jen pro Edit prvek TComboBoxu
// (protoze nahradit jeho ok. proc. jde jen pres
// SetWindowLong, ktera neprijma metody - cili
// NewProcedure TWindowProcedure)
//
function WinProcedure(Wnd: HWnd; msg: DWORD; wParam: wParam; lParam: lParam)
  : LResult; stdcall;
var
  OldProc: TWindowProcedure;
begin
  OldProc := TWindowProcedure(GetProp(Wnd, 'TWindowProcedure'));
  if (msg <> WM_SETFOCUS) and (msg <> WM_LBUTTONDOWN) and (msg <> WM_CHAR) and
    (msg <> WM_KEYDOWN) and (msg <> WM_LBUTTONDBLCLK) and (msg <> WM_MOUSEMOVE) then
    Result := CallWindowProc(Addr(OldProc.FOldWinProcedure), Wnd, msg,
      wParam, lParam);
end;

// ----------------------------------------------------------
// TFormDesigner
// ----------------------------------------------------------

procedure TFormDesigner.ForEachMark(Proc: TMarkProc);
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    Proc(TMark(Components[i]));
end;

constructor TFormDesigner.Create(AOwner: TComponent);
var
  MarkClass: TMarkClass;
  Mark: TMark;
begin
  inherited Create(AOwner);
  if not(AOwner is TForm) then
    FForm := TForm(GetParentForm(TControl(AOwner)))
  else
    FForm := (AOwner as TForm);

  FOldProcs := TObjectList<TWindowProcedure>.Create;

  FForm.OnMouseEnter := OnFormMouseEnterHandler;
  FColor := RGB(178, 214, 243);
  FMarkSize := 8;
  DrawGrid := True;
  FGridGap := 8;
  FSnapToGrid := True;
  FDragMode := dmDeferred;

  for MarkClass in MarkClasses do
  begin
    Mark := MarkClass.Create(Self);
    Mark.SetProps(FMarkSize, FColor, Self);
    InsertComponent(Mark);
  end;

  if not (csDesigning in ComponentState) then
  begin
    FApplicationEvents := TApplicationEvents.Create(nil);
    FApplicationEvents.OnMessage := OnMessageReceived;

    FTimer := TTimer.Create(nil);
    FTimer.Interval := 200;
    FTimer.Enabled := True;
    FTimer.OnTimer := OnTimerHandler;
  end;

  FToolTip := TBalloonHint.Create(nil);
  //FToolTip.HideAfter := 100;
  FToolTip.Style := TBalloonHintStyle.bhsStandard;

end;

destructor TFormDesigner.Destroy;
begin
  FApplicationEvents.Free;
  FTimer.Free;
  FToolTip.Free;
  FOldProcs.Free;
  inherited Destroy;
end;

procedure TFormDesigner.SetState(State: TFormDesignerState);
begin
  FState := State;
end;

procedure TFormDesigner.ClipCursor;
var
  Handle: THandle;
begin
  if FParent is TForm then
    Handle := FParent.Handle
  else
    Handle := FParent.Parent.Handle;
  GetClientRect(Handle, FRect);
  ClientToScreen(Handle, TPoint(FRect.TopLeft));
  ClientToScreen(Handle, TPoint(FRect.BottomRight));
  Windows.ClipCursor(@FRect);
  FRect := FChild.BoundsRect;
end;

procedure TFormDesigner.OnLButtonUpHandler(Update: Boolean);
begin
  if (FState = ssReady) then
    Exit;

  if Assigned(FChild) then
  begin
    Log('Sizer', 'DrawFocusRect: FOldRect (%d, %d, %d, %d)', [FOldRect.Left, FOldRect.Top, FOldRect.Right, FOldRect.Bottom]);
    DrawFocusRect(GetDC(FChild.Parent.Handle), FOldRect);
  end;

  FState := ssReady;
  Windows.ClipCursor(nil);
  FCurrentMark := nil;
  if Update then
  begin
    if not(FChild is TForm) then
    begin
      FChild.BoundsRect := FRect;
      UpdateMarks;
    end;
  end;
  if not(FChild is TForm) then
    Visible := True;
  if Assigned(FOnEndChange) then
    FOnEndChange(FChild);
  FToolTip.HideHint;
  if Assigned(FControlToAdd) then
  begin
    if (FControlToAdd.Width = 0) and (FControlToAdd.Height = 0) then
    begin
      FControlToAdd.Width := FControlToAddRect.Width;
      FControlToAdd.Height := FControlToAddRect.Height;
      Child := FControlToAdd;
    end;
    FControlToAdd.Visible := True;
    AddControl(FControlToAdd);
    FControlToAdd := nil;
  end;
end;

procedure TFormDesigner.Draw;
var
  DC: HDC;
begin
  if not(FChild is TForm) and (FDragMode = dmDeferred) then
  begin
    DC := GetDC(FParent.Handle);
    Log('Sizer', 'Draw FOldRect (%d, %d, %d, %d)', [FOldRect.Left, FOldRect.Top, FOldRect.Right, FOldRect.Bottom]);
    DrawFocusRect(DC, FOldRect);
    Log('Sizer', 'Draw FRect (%d, %d, %d, %d)', [FRect.Left, FRect.Top, FRect.Right, FRect.Bottom]);
    DrawFocusRect(DC, FRect);
    FOldRect := FRect;
  end;
end;

// ----------------------------------------------------------
// Volana pres HOnMouseDown z GetMsgProc
//
procedure TFormDesigner.OnLButtonDownHandler(Sender: TControl; X, Y: integer);
begin

  Log('Sizer', 'OnLButtonDownHandler X: %d, Y: %d', [X, Y]);

  FToolTip.HideHint;

  // Pokud jsou TMarks viditelne, schovejme je
  if Visible then
    Visible := False;

  // Pokud nebylo kliknuto na nynejsi dite ...
  if FChild <> Sender then
  begin
    if Assigned(FOnChildChange) then
      FOnChildChange(Child, Sender);

    if FChild <> nil then
        Draw;

    if (Sender is TForm) then
    begin
      Child := nil;
      Exit;
    end
    else
    begin
      Child := TControl(Sender);
    end;
  end;

  if Assigned(FOnBeginChange) then
    FOnBeginChange(FChild);

  Application.ProcessMessages;

  FClickX := X - FChild.Left;
  FClickY := Y - FChild.Top;
  FState := ssMoving;

  ClipCursor;
end;

procedure TFormDesigner.OnKeyDownHandler(Sender: TControl; Key: Word;
  Shift: TShiftState);
begin
  case FState of
    ssMoving, ssSizing:
      if (Key = VK_ESCAPE) then
        OnLButtonUpHandler(False);
    ssReady:
      case Key of

        VK_DELETE:
          begin
            if not(FChild is TForm) and (FChild <> nil) then
            begin
              RemoveControl(FChild);
              if (FChild is TWinControl) then
                if TWinControl(FChild).ControlCount <> 0 then
                  EnumChilds(TWinControl(FChild), RemoveControl);
              if (FParent is TForm) then
                TForm(FParent).ActiveControl := nil;
              FChild.Free;
              if FOldProcs.Count <> 0 then
                FocusControl(TWindowProcedure(FOldProcs[0]).FControl)
              else
              begin
                Child := nil;
                Visible := False;
              end;
            end;
          end;

        VK_UP:
          begin
            if (ssCtrl in Shift) then
              Child.Top := Child.Top - 1
            else if (ssShift in Shift) then
              Child.Height := Child.Height - 1;
            UpdateMarks;
          end;

        VK_DOWN:
          begin
            if (ssCtrl in Shift) then
              Child.Top := Child.Top + 1
            else if (ssShift in Shift) then
              Child.Height := Child.Height + 1;
            UpdateMarks;
          end;

        VK_LEFT:
          begin
            if (ssCtrl in Shift) then
              Child.Left := Child.Left - 1
            else if (ssShift in Shift) then
              Child.Width := Child.Width - 1;
            UpdateMarks;
          end;

        VK_RIGHT:
          begin
            if (ssCtrl in Shift) then
              Child.Left := Child.Left + 1
            else if (ssShift in Shift) then
              Child.Width := Child.Width + 1;
            UpdateMarks;
          end;
      end;
  end;
end;

procedure TFormDesigner.StartSizing(Mark: TMark);
begin
  FCurrentMark := Mark;
  SetVisible(False);
  Application.ProcessMessages;
  SetState(ssSizing);
  ClipCursor;
  FRect := FChild.BoundsRect;
  FOldRect := TRect.Empty;
  Draw;
  Log('StartSizing', 'Mark: %s', [Mark.ClassName]);
end;

procedure TFormDesigner.AddControl(ControlClass: TControlClass);
begin
  FControlToAdd := ControlClass.Create(FForm);
  FControlToAddRect := FControlToAdd.BoundsRect;
end;

procedure TFormDesigner.AddControl(Control: TControl);
var
  OldProc: TWindowProcedure;
  editWnd: HWnd;
begin
  OldProc := TWindowProcedure.Create;
  with OldProc do
  begin
    FControl := Control;
    FSizer := Self;
    FOldProcedure := Control.WindowProc;
    Control.WindowProc := NewProcedure;
    if (Control is TComboBox) then
    begin
      editWnd := GetWindow(TComboBox(Control).Handle, GW_CHILD);
      SetClassLong(editWnd, GCL_HCURSOR, LoadCursor(0, IDC_ARROW));
      SetProp(editWnd, 'TWindowProcedure', DWORD(OldProc));
      @FOldWinProcedure := Pointer(SetWindowLong(editWnd, GWL_WNDPROC,
        Longint(@WinProcedure)));
    end;
  end;
  FOldProcs.Add(OldProc);
  Control.Cursor := crArrow;
end;

procedure TFormDesigner.FocusControl(Control: TControl);
begin
  if FChild <> Control then
  begin
    if Assigned(FOnChildChange) then
      FOnChildChange(Child, Control);
    Child := Control;
  end;
  if not Visible then
    Visible := True;
end;

procedure TFormDesigner.RemoveControl(Control: TControl);
var
  i: byte;
begin
  if FOldProcs.Count <> 0 then
  begin
    for i := 0 to FOldProcs.Count - 1 do
      if TWindowProcedure(FOldProcs[i]).FControl = Control then
      begin
        Control.WindowProc := TWindowProcedure(FOldProcs[i]).FOldProcedure;
        FOldProcs.Remove(FOldProcs[i]);
        Exit;
      end;
  end;
  raise TFormDesignerException.Create('Unable to remove a control');
end;

procedure TFormDesigner.ShowHint(CursorPos: TPoint; Format: String; const Args: array of const);
begin
  CursorPos.Offset(28, 28);
  FToolTip.Description := SysUtils.Format(Format, Args);
  //FToolTip.ShowHint(CursorPos);
  //Log('Sizer', 'Showing hint ... ');
end;

procedure TFormDesigner.OnMouseMoveHandler(Sender: TControl; X, Y: integer);
var
  CursorPos: TPoint;
  FormPos: TPoint;
begin

  CursorPos := Mouse.CursorPos;
  if (PointsEqual(CursorPos, FLastMousePos)) then
    Exit;
  FLastMousePos := CursorPos;

  Log('Sizer', 'OnMouseMoveHandler: X: %d, Y: %d, FClickX: %d, FClickY: %d', [X, Y, FClickX, FClickY]);

  FLastMouseMove := Now;
  Draw;

  if (FSnapToGrid) then
  begin
    FormPos := FForm.ScreenToClient(TPoint.Create(CursorPos.X, CursorPos.Y));
    with FRect do
    begin
      if ((X - FClickX) mod FGridGap) = 0 then
      begin
        FRect.Left := X - FClickX;
        FRect.Bottom := Top + FChild.Height;
        FRect.Right := Left + FChild.Width;
        if FDragMode = dmImmediate then
        begin
          FChild.SetBounds(FRect.Left, FRect.Top, FRect.Width, FRect.Height);
          Log('Sizer', 'Setting X: %d, Y: %d, FormPos.X: %d, FormPos.Y: %d', [Left, Top, FormPos.X, FormPos.Y]);
        end
        else
          Draw;
         if FState = ssMoving then
          ShowHint(CursorPos, '%d, %d', [FChild.Left, FChild.Top]);
      end;
      if ((Y - FClickY) mod FGridGap) = 0 then
      begin
        FRect.Top := Y - FClickY;
        FRect.Bottom := Top + FChild.Height;
        FRect.Right := Left + FChild.Width;
        if FDragMode = dmImmediate then
        begin
          FChild.SetBounds(FRect.Left, FRect.Top, FRect.Width, FRect.Height);
          Log('Sizer', 'Setting X: %d, Y: %d, FormPos.X: %d, FormPos.Y: %d', [Left, Top, FormPos.X, FormPos.Y]);
        end
        else
          Draw;
        if FState = ssMoving then
          ShowHint(CursorPos, '%d, %d', [FChild.Left, FChild.Top]);
      end;
      if Assigned(FOnSizeChanging) then
        FOnSizeChanging(Child, dPosChange, Left, Top);
    end;
  end
  else
  begin
    with FRect do
    begin
      Left := X - FClickX;
      Top := Y - FClickY;
      Bottom := Top + FChild.Height;
      Right := Left + FChild.Width;
      if Assigned(FOnSizeChanging) then
        FOnSizeChanging(Child, dPosChange, Left, Top);
    end;
    if FDragMode = dmImmediate then
      FChild.SetBounds(FRect.Left, FRect.Top, FRect.Width, FRect.Height)
    else
      Draw;
  end;

end;

procedure TFormDesigner.OnTimerHandler(Sender: TObject);
var
  ms: Int64;
  Control, Child: TControl;
  CursorPos: TPoint;
begin
  if (FState = ssSizing) or (FState = ssMoving) then
      Exit;
  ms := System.DateUtils.MilliSecondsBetween(Now, FLastMouseMove);
  if ms > 50 then
  begin
    CursorPos := Mouse.CursorPos;
    Control := Controls.FindVCLWindow(CursorPos);
    if (Control <> nil) and not (Control is TMark) then
    begin
      Child := TWinControl(Control).ControlAtPos(Control.ScreenToClient(CursorPos), True);
      if Assigned(Child) then
        Control := Child;
      if (Control = FLastHintedControl) or (Control is TForm) or (Control is TMark) then
        Exit;
      FLastHintedControl := Control;
      FToolTip.Description := Format('%s: %s%sBounds: %d, %d, %d, %d; Size: %d, %d', [Control.Name, Control.ClassName, sLineBreak, Control.Left, Control.Top, Control.BoundsRect.Right, Control.BoundsRect.Bottom, Control.Width, Control.Height, sLineBreak]);
      CursorPos.Offset(34, 24);
      FToolTip.ShowHint(CursorPos);
    end
    else
      FToolTip.HideHint;
  end;
end;

procedure TFormDesigner.OnFormMouseEnterHandler(Sender: TObject);
begin
  FToolTip.HideHint;
  FLastHintedControl := nil;
end;

procedure TFormDesigner.OnFormPaintHandler;
var
  i, j: integer;
begin
  for i := 0 to FForm.Height - 1 do
  begin
    if (i mod FGridGap) = 0 then
      for j := 0 to FForm.Width - 1 do
        if (j mod FGridGap) = 0 then
          FForm.Canvas.Pixels[j, i] := clBlack;
  end;
  if Assigned(FFormOnPaint) then
    FFormOnPaint(Self);
end;

procedure TFormDesigner.SetChild(Value: TControl);
var
  Wnd: HWnd;
  i: integer;
begin
  FOldRect := TRect.Empty;
  FChild := Value;
  if not(FChild = nil) then
  begin
    if not(FChild is TForm) then
      FParent := FChild.Parent
    else
      FParent := TWinControl(FChild);
    Wnd := FParent.Handle;
    SetWindowLong(Wnd, GWL_STYLE, GetWindowLong(Wnd, GWL_STYLE) and
      not WS_CLIPCHILDREN);
    FChild.BringToFront;
    ForEachMark(procedure (Mark: TMark)
      begin
        Mark.Parent := FParent;
        //BringWindowToTop(Mark.Handle);
        Mark.BringToFront;
      end);
    UpdateMarks;
  end;
end;

procedure TFormDesigner.UpdateMarks();
begin
  ForEachMark(
    procedure(Mark: TMark)
    begin
      Mark.Update(FChild);
    end);
end;

procedure TFormDesigner.UpdateRect(Rect: TRect; Direction: TDirections;
UpperBound: integer; LowerBound: integer);
begin
  //Log('Sizer', 'Top: %d, Left: %d, Right: %d, Bottom: %d', [Rect.Top, Rect.Left, Rect.Right, Rect.Bottom]);

  if (not FSnapToGrid) or (((Direction = dRight) or (Direction = dUpRight) or (Direction = dDownRight)) and (Rect.Right mod FGridGap = 0)) or
      (((Direction = dLeft) or (Direction = dUpLeft) or (Direction = dDownLeft)) and (Rect.Left mod FGridGap = 0)) or
      (((Direction = dUp) or (Direction = dUpLeft) or (Direction = dUpRight)) and (Rect.Top mod FGridGap = 0)) or
      (((Direction = dDown) or (Direction = dDownLeft) or (Direction = dDownRight)) and (Rect.Bottom mod FGridGap = 0))
      {((Direction = dUpLeft) and ((Rect.Left mod FGridGap = 0) and (Rect.Top mod FGridGap = 0))) or
      ((Direction = dUpRight) and ((Rect.Right mod FGridGap = 0) or (Rect.Top mod FGridGap = 0))) or
      ((Direction = dDownLeft) and ((Rect.Left mod FGridGap = 0) and (Rect.Bottom mod FGridGap = 0))) or
      ((Direction = dDownRight) and ((Rect.Right mod FGridGap = 0) and (Rect.Bottom mod FGridGap = 0)))}
   then
  begin
    //Draw;
    FRect := Rect;
    Log('Sizer', 'Setting FChild.BoundsRect (%d, %d, %d, %d)', [FRect.Left, FRect.Top, FRect.Right, FRect.Bottom]);
    if (FDragMode = dmImmediate) then
      FChild.BoundsRect := FRect
    else
      Draw;
    if Assigned(FOnSizeChanging) then
      FOnSizeChanging(FChild, Direction, UpperBound, LowerBound);
  end

end;

procedure TFormDesigner.CallMessageHandler(msg: UINT; Control: TWinControl;
var pt: TPoint; Handler: TMessageHandler);
begin
  case msg of

    WM_MOUSEMOVE:
      begin
        pt := FChild.Parent.ScreenToClient(Control.ClientToScreen(pt));
        Handler(Control.Parent, pt.X, pt.Y);
      end;

    WM_LBUTTONDOWN:
      begin
        Child := Control.ControlAtPos(pt, True);
        if Child <> nil then
          Handler(Child, pt.X, pt.Y)
        else
        begin
          if Control.Parent <> nil then
            pt := Control.Parent.ScreenToClient(Control.ClientToScreen(pt));
          Handler(Control, pt.X, pt.Y);
        end;
      end;

  end;
end;

procedure TFormDesigner.ProcessMessage(var msg: tagMSG; Handler: TMessageHandler);
var
  Control: TControl;
  ClsName: array [0 .. 5] of char;
  pt: TPoint;
  Parent: TWinControl;
begin
  pt.SetLocation(GET_X_LPARAM(msg.lParam), GET_Y_LPARAM(msg.lParam));

  Control := FindControl(msg.HWnd);

  if Assigned(FControlToAdd) and (FState = ssReady) then
  begin
    if Control is TForm then
      Parent := TWinControl(Control)
    else
      Parent := Control.Parent;
    with FControlToAdd do
    begin
      Left := pt.X;
      Top := pt.Y;
      Width := 0;
      Height := 0;
      Name := GetControlName(FForm, FControlToAdd.ClassType);
      Perform(WM_SETTEXT, NativeInt(0), NativeInt(PChar(Name)));
      Visible := False;
    end;
    Log('Sizer', 'FControlToAdd - Left: %d, Top: %d', [FControlToAdd.Left, FControlToAdd.Top]);
    FControlToAdd.Parent := Parent;
    Control := FControlToAdd;
    Child := Control;
    StartSizing(MarkOfType(TDownRightMark));
    Exit;
  end;

  if (Control <> nil) and (GetParentForm(Control) = FForm) then
  begin
    CallMessageHandler(msg.message, TWinControl(Control), pt, Handler);
  end
  else
  begin
    // No Delphi control found, is it EDIT in a Combobox?
    GetClassName(msg.HWnd, ClsName, 5);
    if UpperCase(ClsName) = 'EDIT' then
    begin
      // Probably TComboBox, find EDIT's parent
      Control := FindControl(GetParent(msg.HWnd));
      if (Control <> nil) and (GetParentForm(Control) = FForm) then
      begin
        Log('Sizer', 'WM_MOUSEMOVE: Control.Name: %s, Control.Parent.Name: %s',
          [Control.Name, Control.Parent.Name]);
        pt := Control.Parent.ScreenToClient(Control.ClientToScreen(pt));
        Handler(Control, pt.X, pt.Y);
      end;
    end;
  end;
end;

procedure TFormDesigner.OnMessageReceived(var msg: tagMSG; var Handled: Boolean);
var
  Control: TControl;
  Parent: TWinControl;
  Shift: TShiftState;
begin

  case msg.message of

    WM_MOUSEMOVE:
      begin
        if FState = ssMoving then
          ProcessMessage(msg, OnMouseMoveHandler)
        else if FState = ssSizing then
          ProcessMessage(msg, FCurrentMark.OnMouseMoveHandler);
      end;

    WM_LBUTTONDOWN:
      begin
        Control := FindControl(msg.HWnd);
        if ((msg.HWnd = FForm.Handle) or (GetParentForm(Control) = FForm)) then
        begin
          if (Control is TMark) then
            StartSizing(TMark(Control))
          else
            ProcessMessage(msg, OnLButtonDownHandler);
        end;
      end;

    WM_LBUTTONUP:
    begin
      Control := FindControl(msg.HWnd);
      if ((msg.HWnd = FForm.Handle) or (GetParentForm(Control) = FForm)) then
        OnLButtonUpHandler(True);
    end;

    WM_KEYDOWN:
    begin
      Control := FindControl(msg.HWnd);
      if ((msg.HWnd = FForm.Handle) or (GetParentForm(Control) = FForm)) then
      begin
        Shift := MsgGetShiftState(msg.wParam, msg.lParam);
        OnKeyDownHandler(FChild, msg.wParam, Shift);
      end;

    end;


  end;
end;

procedure TFormDesigner.SetColor(Value: TColor);
begin
  FColor := Value;
  ForEachMark(
    procedure(Mark: TMark)
    begin
      Mark.SetProps(FMarkSize, FColor, Self);
    end);
end;

procedure TFormDesigner.SetDrawGrid(Value: Boolean);
begin
  FDrawGrid := Value;
  if FDrawGrid then
  begin
    if Assigned(FForm.OnPaint) then
      FFormOnPaint := FForm.OnPaint;
    FForm.OnPaint := OnFormPaintHandler;
    FForm.Refresh;
  end
  else if Assigned(FFormOnPaint) then
  begin
    FForm.OnPaint := FFormOnPaint;
    FFormOnPaint := nil;
  end;
end;

procedure TFormDesigner.SetMarkSize(Value: byte);
begin
  FMarkSize := Value;
  ForEachMark(
    procedure(Mark: TMark)
    begin
      Mark.Width := Value;
      Mark.Height := Value;
    end);
end;

procedure TFormDesigner.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  ForEachMark(
    procedure(Mark: TMark)
    begin
      Mark.Visible := Value;
    end);
end;

function TFormDesigner.GetChildRect: TRect;
begin
  {if (Assigned(FControlToAdd)) then
    Result := TRect.Empty
  else}
    Result := FChild.BoundsRect;
end;

function TFormDesigner.GetRect: TRect;
begin
  Result := FRect;
end;

function TFormDesigner.MarkOfType(MarkClass: TMarkClass): TMark;
var
  i: integer;
  Mark: TMark;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Mark := TMark(Components[i]);
    if Mark.ClassNameIs(MarkClass.ClassName) then
    begin
      Result := Mark;
      Exit;
    end;
  end;
  raise TFormDesignerException.Create('Unknow Mark Type');
end;

procedure TFormDesigner.SetGridGap(Value: integer);
begin
  FGridGap := Value;
  FForm.Refresh;
end;

procedure Register;
begin
  RegisterComponents( 'Form Designer', [ TFormDesigner ] );
end;

end.
