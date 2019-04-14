unit FormDesigner.Designer;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, Vcl.AppEvnts, FormDesigner.Interfaces,
  FormDesigner.Utils, FormDesigner.Marks, ExtCtrls, System.DateUtils, TypInfo,
  RTTI;

type

  TDragMode = (dmImmediate, dmDeferred);
  TMessageHandler = procedure(Sender: TControl; X, Y: integer) of object;
  TMarkProc = reference to procedure(Mark: TMark);
  TMarkClass = class of TMark;
  TFormDesignerException = class(Exception);
  TWindowProc = function(Wnd: HWnd; msg: DWORD; wParam: wParam; lParam: lParam)
    : LResult; stdcall;
  TControlInfo = class;

  { TFormDesigner }
  TFormDesigner = class(TComponent, IFormDesigner)
  private
    FParent: TWinControl;
    FChild: TControl;
    FMarkSize: byte;
    FRect: TRect;
    FMarksVisible: Boolean;
    FClickX: integer;
    FClickY: integer;
    FControls: TObjectList<TControlInfo>;
    FColor: TColor;
    FState: TFormDesignerState;
    FApplicationEvents: TApplicationEvents;
    FForm: TForm;
    FCurrentMark: TMark;
    FDrawGrid: Boolean;
    FFormOnPaint: TNotifyEvent;
    FGridGap: integer;
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
    FControlToAddAutoSize: Boolean;
    FOnControlAdded: TNotifyEvent;
    FSizingOrigin: TPoint;
    procedure CancelSizeMove(WindowHandle: HWnd);
    procedure SetChild(Value: TControl);
    procedure SetMarkSize(Value: byte);
    procedure SetMarksVisible(Value: Boolean);
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
    procedure ShowHint(CursorPos: TPoint; Format: String;
      const Args: array of const);
    function MarkOfType(MarkClass: TMarkClass): TMark;
    function IsOwnedControl(Control: TControl): Boolean;
    function AlignToGrid(Num: Integer) : Integer;
    property Child: TControl read FChild write SetChild;
  public
    procedure Draw;
    function GetRect: TRect;
    function GetChildRect: TRect;
    property MarksVisible: Boolean read FMarksVisible write SetMarksVisible;
    procedure AddControl(ControlClass: TControlClass); overload;
    procedure AddControl(AControl: TControl); overload;
    procedure FocusControl(Control: TControl);
    procedure RemoveControl(Control: TControl);
    procedure UpdateRect(Rect: TRect; Direction: TDirections;
      UpperBound: integer; LowerBound: integer);
    procedure OnLButtonUpHandler(Update: Boolean; var Msg: tagMSG);
    procedure OnMouseMoveHandler(Sender: TControl; X, Y: integer);
    procedure OnLButtonDownHandler(Sender: TControl; X, Y: integer);
    procedure OnKeyDownHandler(WindowHandle: HWnd; wParam: wParam;
      lParam: lParam);
    procedure StartSizing(Mark: TMark);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MarkSize: byte read FMarkSize write SetMarkSize default 8;
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid
      default True;
    property Color: TColor read FColor write SetColor default clBlack;
    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default True;
    property GridGap: integer read FGridGap write SetGridGap default 8;
    property DragMode: TDragMode read FDragMode write FDragMode
      default dmImmediate;
    property OnControlAdded: TNotifyEvent read FOnControlAdded
      write FOnControlAdded;
  end;

  TControlInfo = class
  public
    Sizer: TFormDesigner;
    Control: TControl;
    // Used to replace Window procedure for Delphi controls
    PrevWndMethod: TWndMethod;
    // Used to replace Window procedure for controls not managed by Delphi
    // (like Edit inside a ComboBox)
    PrevWindowProc: TWindowProc;
    procedure ControlWindowProc(var msg: TMessage);
  end;

procedure Register;

const
  MarkClasses: array [0 .. 7] of TMarkClass = (TUpMark, TDownMark, TLeftMark,
    TRightMark, TUpLeftMark, TUpRightMark, TDownLeftMark, TDownRightMark);

implementation

// ----------------------------------------------------------
// TWindowProcedury
// ----------------------------------------------------------

function IsAllowedMessage(const msg: Cardinal): Boolean;
begin
  Result := (msg <> WM_SETFOCUS) and (msg <> WM_LBUTTONDOWN) and
    (msg <> WM_CHAR) and (msg <> WM_KEYDOWN) and (msg <> WM_LBUTTONDBLCLK) and
    (msg <> WM_MOUSEMOVE);
end;

// ----------------------------------------------------------
// Tato okenni procedura je prirazena do
// WndProc prvku predanem do TFormDesigner.AddControl
//
procedure TControlInfo.ControlWindowProc(var msg: TMessage);
begin
  if IsAllowedMessage(msg.msg) then
    PrevWndMethod(msg);
end;

// ----------------------------------------------------------
// Spec okenni procedura jen pro Edit prvek TComboBoxu
// (protoze nahradit jeho ok. proc. jde jen pres
// SetWindowLong, ktera neprijma metody - cili
// NewProcedure TWindowProcedure)
//
function ComboEditWindowProcedure(Wnd: HWnd; msg: DWORD; wParam: wParam;
  lParam: lParam): LResult; stdcall;
var
  ControlInfo: TControlInfo;
begin
  if IsAllowedMessage(msg) then
  begin
    ControlInfo := TControlInfo(GetProp(Wnd, 'TControlInfo'));
    Result := CallWindowProc(Addr(ControlInfo.PrevWindowProc), Wnd, msg,
      wParam, lParam);
  end;
end;

// ----------------------------------------------------------
// TFormDesigner
// ----------------------------------------------------------

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

  FControls := TObjectList<TControlInfo>.Create;

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

  if not(csDesigning in ComponentState) then
  begin
    FApplicationEvents := TApplicationEvents.Create(nil);
    FApplicationEvents.OnMessage := OnMessageReceived;

    FTimer := TTimer.Create(nil);
    FTimer.Interval := 200;
    FTimer.Enabled := True;
    FTimer.OnTimer := OnTimerHandler;
  end;

  FToolTip := TBalloonHint.Create(nil);
  // FToolTip.HideAfter := 100;
  FToolTip.Style := TBalloonHintStyle.bhsStandard;

end;

destructor TFormDesigner.Destroy;
begin
  FApplicationEvents.Free;
  FTimer.Free;
  FToolTip.Free;
  FControls.Free;
  inherited Destroy;
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

procedure TFormDesigner.OnLButtonUpHandler(Update: Boolean; var Msg: tagMSG);
var
  MousePos: TPoint;
begin
  if (FState <> ssReady) and IsMessageForWindow(msg.hwnd, FForm.Handle) then
  begin
    Log('Sizer', 'OnLButtonUpHandler: State: %s', [TRttiEnumerationType.GetName(FState)]);

    if Assigned(FChild) then
    begin
      Log('Sizer', 'DrawFocusRect: FOldRect (%d, %d, %d, %d)',
        [FOldRect.Left, FOldRect.Top, FOldRect.Right, FOldRect.Bottom]);
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
      MarksVisible := True;

    FToolTip.HideHint;

    if Assigned(FControlToAdd) then
    begin
      MousePos.SetLocation(GET_X_LPARAM(msg.lParam), GET_Y_LPARAM(msg.lParam));
      if FControlToAdd.BoundsRect.BottomRight.IsZero or PointsEqual(FSizingOrigin, MousePos) then
      begin
        FControlToAdd.Width := FControlToAddRect.Width;
        FControlToAdd.Height := FControlToAddRect.Height;
        Child := FControlToAdd;
      end;
      if FControlToAddAutoSize then
        SetOrdProp(FControlToAdd, 'AutoSize', NativeInt(True));
      FControlToAdd.Visible := True;
      AddControl(FControlToAdd);
      if Assigned(FOnControlAdded) then
        FOnControlAdded(FControlToAdd);
      FControlToAdd := nil;
      UpdateMarks;
    end;
  end;
end;

procedure TFormDesigner.Draw;
var
  DC: HDC;
begin
  if not(FChild is TForm) and
    (Assigned(FControlToAdd) or (FDragMode = dmDeferred)) then
  begin
    DC := GetDC(FParent.Handle);
    Log('Sizer', 'Draw: FOldRect (%d, %d, %d, %d)',
      [FOldRect.Left, FOldRect.Top, FOldRect.Right, FOldRect.Bottom]);
    DrawFocusRect(DC, FOldRect);
    Log('Sizer', 'Draw: FRect (%d, %d, %d, %d)',
      [FRect.Left, FRect.Top, FRect.Right, FRect.Bottom]);
    DrawFocusRect(DC, FRect);
    FOldRect := FRect;
  end;
end;

function TFormDesigner.AlignToGrid(Num: Integer) : Integer;
var
  Remainder: Integer;
begin
  Result := Num;
  if FSnapToGrid then
  begin
    Remainder := Num mod FGridGap;
    if (Remainder > (FGridGap div 2)) then
    begin
      Result := Num + (FGridGap - Remainder);
    end
    else
    begin
      Result := Num - Remainder;
    end;
  end;
  Log('Sizer', 'Aligned %d to %d', [Num, Result]);
end;

procedure TFormDesigner.OnLButtonDownHandler(Sender: TControl; X, Y: integer);
begin

  Log('Sizer', 'OnLButtonDownHandler X: %d, Y: %d', [X, Y]);
  Log('Sizer', 'FRect', FRect);

  FToolTip.HideHint;

  if MarksVisible then
    MarksVisible := False;

  if Sender is TForm then
  begin
    Child := nil;
    Exit;
  end;

  if Child <> Sender then
    Child := TControl(Sender);

  Draw;

  FClickX := X - FChild.Left;
  FClickY := Y - FChild.Top;

  FState := ssMoving;
  ClipCursor;

  // Process any waiting messages. This needs to be called
  // last in this proc to prevent premature processing of
  // WM_LBUTTONUP
  Application.ProcessMessages;
end;

procedure TFormDesigner.CancelSizeMove(WindowHandle: HWnd);
var
  Msg: tagMSG;
begin
  Msg.hwnd := WindowHandle;
  OnLButtonUpHandler(False, Msg);
end;

procedure TFormDesigner.OnKeyDownHandler(WindowHandle: HWnd; wParam: wParam;
  lParam: lParam);
var
  Shift: TShiftState;
  Key: Word;
begin
  if IsMessageForWindow(WindowHandle, FForm.Handle) then
  begin
    Shift := KeyDataToShiftState(lParam);
    Key := Word(wParam);
    case FState of
      ssMoving, ssSizing:
        if (Key = VK_ESCAPE) then
          CancelSizeMove(WindowHandle);
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
                if FControls.Count <> 0 then
                  FocusControl(FControls[0].Control)
                else
                begin
                  Child := nil;
                  MarksVisible := False;
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
end;

procedure TFormDesigner.StartSizing(Mark: TMark);
begin
  FCurrentMark := Mark;
  FState := ssSizing;
  MarksVisible := False;
  ClipCursor;
  FRect := FChild.BoundsRect;
  FOldRect := TRect.Empty;
  Application.ProcessMessages;
  Draw;
  Log('StartSizing', 'Mark: %s', [Mark.ClassName]);
end;

procedure TFormDesigner.AddControl(ControlClass: TControlClass);
begin
  FControlToAdd := ControlClass.Create(FForm);
  FControlToAddRect := FControlToAdd.BoundsRect;
  if IsPublishedProp(FControlToAdd, 'AutoSize') then
  begin
    FControlToAddAutoSize := Boolean(GetObjectProp(FControlToAdd, 'AutoSize'));
    if (FControlToAddAutoSize) then
      SetObjectProp(FControlToAdd, 'AutoSize', TObject(False));
  end
  else
    FControlToAddAutoSize := False;
end;

procedure TFormDesigner.AddControl(AControl: TControl);
var
  ControlInfo: TControlInfo;
  editWnd: HWnd;
begin
  ControlInfo := TControlInfo.Create;
  with ControlInfo do
  begin
    Control := AControl;
    Sizer := Self;
    PrevWndMethod := Control.WindowProc;
    Control.WindowProc := ControlWindowProc;
    if (AControl is TComboBox) then
    begin
      editWnd := GetWindow(TComboBox(Control).Handle, GW_CHILD);
      SetClassLong(editWnd, GCL_HCURSOR, LoadCursor(0, IDC_ARROW));
      SetProp(editWnd, 'TControlInfo', DWORD(ControlInfo));
      @PrevWindowProc := Pointer(SetWindowLong(editWnd, GWL_WNDPROC,
        Longint(@ComboEditWindowProcedure)));
    end;
  end;
  FControls.Add(ControlInfo);
  AControl.Cursor := crArrow;
end;

procedure TFormDesigner.FocusControl(Control: TControl);
begin
  if FChild <> Control then
  begin
    Child := Control;
  end;

  if not MarksVisible then
    MarksVisible := True;
end;

procedure TFormDesigner.RemoveControl(Control: TControl);
var
  i: byte;
begin
  if FControls.Count <> 0 then
  begin
    for i := 0 to FControls.Count - 1 do
      if FControls[i].Control = Control then
      begin
        Control.WindowProc := FControls[i].PrevWndMethod;
        FControls.Remove(FControls[i]);
        Exit;
      end;
  end;
  raise TFormDesignerException.Create('Unable to remove a control');
end;

procedure TFormDesigner.ShowHint(CursorPos: TPoint; Format: String;
  const Args: array of const);
begin
  CursorPos.Offset(28, 28);
  FToolTip.Description := SysUtils.Format(Format, Args);
  FToolTip.ShowHint(CursorPos);
  // Log('Sizer', 'Showing hint ... ');
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

  Log('Sizer', 'OnMouseMoveHandler: X: %d, Y: %d, FClickX: %d, FClickY: %d',
    [X, Y, FClickX, FClickY]);

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
          Log('Sizer', 'Setting X: %d, Y: %d, FormPos.X: %d, FormPos.Y: %d',
            [Left, Top, FormPos.X, FormPos.Y]);
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
          Log('Sizer', 'Setting X: %d, Y: %d, FormPos.X: %d, FormPos.Y: %d',
            [Left, Top, FormPos.X, FormPos.Y]);
        end
        else
          Draw;
        if FState = ssMoving then
          ShowHint(CursorPos, '%d, %d', [FChild.Left, FChild.Top]);
      end;
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
    if (Control <> nil) and not(Control is TMark) then
    begin
      Child := TWinControl(Control)
        .ControlAtPos(Control.ScreenToClient(CursorPos), True);
      if Assigned(Child) then
        Control := Child;
      if (Control = FLastHintedControl) or (Control is TForm) or
        (Control is TMark) then
        Exit;
      FLastHintedControl := Control;
      FToolTip.Description :=
        Format('%s: %s%sBounds: %d, %d, %d, %d; Size: %d, %d',
        [Control.Name, Control.ClassName, sLineBreak, Control.Left, Control.Top,
        Control.BoundsRect.Right, Control.BoundsRect.Bottom, Control.Width,
        Control.Height, sLineBreak]);
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
    ForEachMark(
      procedure(Mark: TMark)
      begin
        Mark.Parent := FParent;
        Mark.BringToFront;
        Mark.Update(FChild);
      end);
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
  Log('Sizer', 'UpdateRect (%d, %d, %d, %d)', [Rect.Left, Rect.Top, Rect.Right,
    Rect.Bottom]);
  if FSnapToGrid then
  begin
    if (((Direction = dRight) or (Direction = dUpRight) or
      (Direction = dDownRight)) and (Rect.Right mod FGridGap = 0)) or
      (((Direction = dLeft) or (Direction = dUpLeft) or (Direction = dDownLeft))
      and (Rect.Left mod FGridGap = 0)) or
      (((Direction = dUp) or (Direction = dUpLeft) or (Direction = dUpRight))
      and (Rect.Top mod FGridGap = 0)) or
      (((Direction = dDown) or (Direction = dDownLeft) or
      (Direction = dDownRight)) and (Rect.Bottom mod FGridGap = 0))
    { ((Direction = dUpLeft) and ((Rect.Left mod FGridGap = 0) and (Rect.Top mod FGridGap = 0))) or
      ((Direction = dUpRight) and ((Rect.Right mod FGridGap = 0) or (Rect.Top mod FGridGap = 0))) or
      ((Direction = dDownLeft) and ((Rect.Left mod FGridGap = 0) and (Rect.Bottom mod FGridGap = 0))) or
      ((Direction = dDownRight) and ((Rect.Right mod FGridGap = 0) and (Rect.Bottom mod FGridGap = 0))) }
    then
    begin
      // Draw;
      FRect := Rect;
      Log('Sizer', 'Setting FChild.BoundsRect (%d, %d, %d, %d)',
        [FRect.Left, FRect.Top, FRect.Right, FRect.Bottom]);
      if (FDragMode = dmImmediate) then
        FChild.BoundsRect := FRect
      else
        Draw;
    end
  end;
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
        begin
          if IsOwnedControl(Child) then
            Handler(Child, pt.X, pt.Y)
        end
        else
        begin
          if IsOwnedControl(Control) or (Control = FForm) then
          begin
            if Control.Parent <> nil then
              pt := Control.Parent.ScreenToClient(Control.ClientToScreen(pt));
            Handler(Control, pt.X, pt.Y);
          end;
        end;
      end;

  end;
end;

procedure TFormDesigner.ProcessMessage(var msg: tagMSG;
Handler: TMessageHandler);
var
  Control: TControl;
  ClsName: array [0 .. 5] of char;
  pt: TPoint;
  Parent: TWinControl;
begin
  if not IsMessageForWindow(msg.HWnd, FForm.Handle) then
    Exit;

  Control := FindControl(msg.HWnd);
  if (Control is TMark) then
  begin
    if FState = ssReady then
    begin
      StartSizing(TMark(Control));
      Exit;
    end;
  end;
  Log('Sizer', 'ProcessMessage: state: %s',
    [TRttiEnumerationType.GetName(FState)]);

  pt.SetLocation(GET_X_LPARAM(msg.lParam), GET_Y_LPARAM(msg.lParam));

  if Assigned(FControlToAdd) and (FState = ssReady) then
  begin
    if (csAcceptsControls in Control.ControlStyle) then
      Parent := TWinControl(Control)
    else
    begin
      Parent := Control.Parent;
      pt := Parent.ScreenToClient(Control.ClientToScreen(pt));
    end;
    with FControlToAdd do
    begin
      Visible := False;
      Name := GetControlName(FForm, FControlToAdd.ClassType);
      SetControlText(FControlToAdd, Name);
      Left := AlignToGrid(pt.X);
      Top := AlignToGrid(pt.Y);
      Width := 0;
      Height := 0;
    end;
    Log('Sizer', 'FControlToAdd (%d, %d, %d, %d)',
      [FControlToAdd.BoundsRect.Left, FControlToAdd.BoundsRect.Top,
      FControlToAdd.BoundsRect.Width, FControlToAdd.BoundsRect.Height]);
    FControlToAdd.Parent := Parent;
    Child := FControlToAdd;
    FSizingOrigin := TPoint.Create(pt.X, pt.Y);
    StartSizing(MarkOfType(TDownRightMark));
  end
  else if (Control <> nil) then
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
      if (Control <> nil) then
      begin
        Log('Sizer', 'EDIT event, Orig pt: pt.X: %d, pt.Y: %d',
          [pt.X, pt.Y]);

        ClientToScreen(msg.hwnd, pt);
        pt := Control.ScreenToClient(pt);
        Log('Sizer', 'EDIT event: Control.Name: %s, pt.X: %d, pt.Y: %d',
          [Control.Name, pt.X, pt.Y]);
        //FRect := Control.BoundsRect;
        //Handler(Control, pt.X, pt.Y);
        CallMessageHandler(msg.message, TWinControl(Control), pt, Handler);
      end;
    end;
  end;
end;

procedure TFormDesigner.OnMessageReceived(var msg: tagMSG;
var Handled: Boolean);
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
      ProcessMessage(msg, OnLButtonDownHandler);

    WM_LBUTTONUP:
      OnLButtonUpHandler(True, msg);

    WM_KEYDOWN:
      OnKeyDownHandler(msg.HWnd, msg.wParam, msg.lParam);

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

procedure TFormDesigner.SetMarksVisible(Value: Boolean);
begin
  Log('Sizer', 'MarksVisible: From: %s to %s', [BoolToStr(FMarksVisible), BoolToStr(Value)]);
  FMarksVisible := Value;
  ForEachMark(
    procedure(Mark: TMark)
    begin
      Mark.Visible := Value;
    end);
  Application.ProcessMessages;
end;

function TFormDesigner.GetChildRect: TRect;
begin
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

function TFormDesigner.IsOwnedControl(Control: TControl): Boolean;
var
  ControlInfo: TControlInfo;
begin
  Result := False;
  for ControlInfo in FControls do
    if ControlInfo.Control = Control then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TFormDesigner.ForEachMark(Proc: TMarkProc);
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    Proc(TMark(Components[i]));
end;

procedure Register;
begin
  RegisterComponents('Form Designer', [TFormDesigner]);
end;

end.
