unit FormDesigner.Designer;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, Vcl.AppEvnts, TypInfo, ExtCtrls,
  System.DateUtils,
  RTTI, System.SyncObjs, FormDesigner.Interfaces,
  FormDesigner.Utils, FormDesigner.DragHandles;

type

  /// Conrols behavior of TFormDesigner during moving/sizing.
  TDragMode = (
    /// Changes to control's position/size are visible immediatelly
    /// while dragging the mouse.
    dmImmediate,

    /// Changes to control's position/size are projected with a focus
    /// rectangle and are applied after the mouse is released.
    dmDeferred);

  /// Callback for TFormDesigner.ForEachDragHandle
  TDragHandleProc = reference to procedure(DragHandle: TDragHandle);

  TMessageHandler = procedure(Sender: TControl; X, Y: integer) of object;
  TDragHandleClass = class of TDragHandle;
  TFormDesignerException = class(Exception);
  TWindowProc = function(Wnd: HWnd; msg: Cardinal; wParam: wParam;
    lParam: lParam): LResult; stdcall;
  TControlInfo = class;
  TRectModifiers = class;

  TFormDesigner = class(TComponent, IFormDesigner)
  private
    FParent: TWinControl;
    FChild: TControl;
    FDragHandleSize: byte;
    FRect: TRect;
    FDragHandlesVisible: Boolean;
    FClickOrigin: TPoint;
    FControls: TObjectList<TControlInfo>;
    FDragHandleColor: TColor;
    FState: TFormDesignerState;
    FApplicationEvents: TApplicationEvents;
    FForm: TForm;
    FShowHints: Boolean;
    FCurrentDragHandle: TDragHandle;
    FDrawGrid: Boolean;
    FFormOnPaint: TNotifyEvent;
    FGridGap: integer;
    FSnapToGrid: Boolean;
    FToolTip: TBalloonHint;
    FDragMode: TDragMode;
    FLastMouseMove: TDateTime;
    FHintTimer: TTimer;
    FLastHintedControl: TControl;
    FOldRect: TRect;
    FControlToAdd: TControl;
    FControlToAddRect: TRect;
    FControlToAddAutoSize: Boolean;
    FSizingOrigin: TPoint;
    FButtonDownOrigin: TPoint;
    FRectModifiers: TRectModifiers;
    FOnControlSelected: TNotifyEvent;
    FOnControlModified: TNotifyEvent;
    FOnControlAdded: TNotifyEvent;
    procedure ControlSelected;
    procedure ControlModified;
    procedure ControlAdded;
    procedure SetShowHints(Value: Boolean);
    procedure UpdateChildProp(CtrlPropName, ShiftPropName: String;
      Value: integer; Shift: TShiftState);
    function TryGetParent(HWnd: HWnd; pt: TPoint;
      var Control: TControl): Boolean;
    procedure CancelSizeMove(WindowHandle: HWnd);
    procedure SetChild(Value: TControl);
    procedure SetDragHandleSize(Value: byte);
    procedure SetDragHandlesVisible(Value: Boolean);
    procedure SetDragHandleColor(Value: TColor);
    procedure SetGridGap(Value: integer);
    procedure SetDrawGrid(Value: Boolean);
    procedure ClipCursor;
    procedure UpdateDragHandles;
    procedure MessageReceivedHandler(var msg: tagMSG; var Handled: Boolean);
    procedure CallMessageHandler(msg: UINT; Control: TWinControl;
      var pt: TPoint; Handler: TMessageHandler);
    procedure PreProcessMessage(var msg: tagMSG; Handler: TMessageHandler);
    procedure FormPaintHandler(Sender: TObject);
    procedure ForEachDragHandle(Proc: TDragHandleProc);
    procedure HintTimerHandler(Sender: TObject);
    procedure FormMouseEnterHandler(Sender: TObject);
    function DragHandleOfType(DragHandleClass: TDragHandleClass): TDragHandle;
    function IsOwnedControl(Control: TControl): Boolean;
    function AlignToGrid(Num: integer; Offset: integer = 0): integer;
    procedure LButtonUpHandler(var msg: tagMSG; ApplyChanges: Boolean);
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer);
    procedure LButtonDownHandler(Sender: TControl; X, Y: integer);
    procedure KeyDownHandler(var msg: tagMSG);
    procedure DrawRect(OnlyCleanUp: Boolean = False);
    procedure StartSizing(DragHandle: TDragHandle; MousePos: TPoint);
    property DragHandlesVisible: Boolean read FDragHandlesVisible
      write SetDragHandlesVisible;
    property Child: TControl read FChild write SetChild;
    procedure SetupControlToAdd(var pt: TPoint; Control: TControl);
  public
    function GetRect: TRect;
    function GetChildRect: TRect;
    procedure UpdateRect(Rect: TRect; Direction: TDirections);
    procedure AddControl(ControlClass: TControlClass); overload;
    procedure AddControl(AControl: TControl); overload;
    procedure RemoveControl(Control: TControl);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DragHandleSize: byte read FDragHandleSize write SetDragHandleSize
      default 8;
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid
      default True;
    property DragHandleColor: TColor read FDragHandleColor
      write SetDragHandleColor default TColor(15980210); // RGB(178, 214, 243);
    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default True;
    property GridGap: integer read FGridGap write SetGridGap default 8;
    property ShowHints: Boolean read FShowHints write SetShowHints
      default False;
    property DragMode: TDragMode read FDragMode write FDragMode
      default dmDeferred;
    property OnControlAdded: TNotifyEvent read FOnControlAdded
      write FOnControlAdded;
    property OnControlSelected: TNotifyEvent read FOnControlSelected
      write FOnControlSelected;
    property OnControlModified: TNotifyEvent read FOnControlModified
      write FOnControlModified;
  end;

  /// Holds information about a control managed by TFormDesigner
  TControlInfo = class
  public
    FormDesigner: TFormDesigner;
    Control: TControl;

    /// Used to replace Window procedure for Delphi controls
    PrevWndMethod: TWndMethod;

    /// Used to replace Window procedure for controls not managed by Delphi
    /// (like Edit inside a ComboBox)
    PrevWindowProc: TWindowProc;

    procedure ControlWindowProc(var msg: TMessage);
  end;

  /// There are slight differences among TControl.BoundsRect
  /// For TButton, BoundsRect includes 1px gap around the control, which accomodes focus rectangle
  /// For TComboBox or TEdit, there is no such gap so the focus rect has to be expanded
  /// These differences are handled by TRectModifier
  TRectModifier = class abstract
  public
    function Modify(var Rect: TRect): TRect; virtual; abstract;
  end;

  /// Base class for TRectModifier. Its <see cref="Modify"> method returns
  /// TRect without any changes
  TRectModifierBase = class(TRectModifier)
    function Modify(var Rect: TRect): TRect; override;
  end;

  /// Inflates TRect by a specified number of pixels
  TInflatingRectModifier = class(TRectModifierBase)
  private
    FInflateBy: integer;
  public
    constructor Create(InflateBy: integer);
    function Modify(var Rect: TRect): TRect; override;
  end;

  TRectModifiers = class
  private
    FRectModifiers: TObjectDictionary<TControlClass, TRectModifier>;
    FDefaultRectModifier: TRectModifier;
  public
    constructor Create;
    destructor Destroy; override;
    function GetForControl(Control: TControl): TRectModifier;
  end;

procedure Register;

const
  DragHandleClasses: array [0 .. 7] of TDragHandleClass = (TUpDragHandle,
    TDownDragHandle, TLeftDragHandle, TRightDragHandle, TUpLeftDragHandle,
    TUpRightDragHandle, TDownLeftDragHandle, TDownRightDragHandle);

implementation

var
  LockMouse: TCriticalSection;

  // -----------------------------------------------------------------
  // Window Procedures
  // -----------------------------------------------------------------

  // Filters Window messages and skips those related to keyboard or mouse
  // interaction. Aim is to hide any control interaction while they are managed by
  // TFormDesigner
function IsAllowedMessage(const msg: Cardinal): Boolean;
begin
  Result := (msg <> WM_SETFOCUS) and (msg <> WM_LBUTTONDOWN) and
    (msg <> WM_CHAR) and (msg <> WM_KEYDOWN) and (msg <> WM_LBUTTONDBLCLK) and
    (msg <> WM_MOUSEMOVE);
end;

// Window procedure for controls managed by Delphi
procedure TControlInfo.ControlWindowProc(var msg: TMessage);
begin
  if IsAllowedMessage(msg.msg) then
    PrevWndMethod(msg);
end;

// Windows procedure for controls not managed by Delphi, like EDIT inside a
// TComboBox. It is set by using SetWindowLong. In this case instance
// methods can't be used.
function ComboEditWindowProcedure(Wnd: HWnd; msg: Cardinal; wParam: wParam;
  lParam: lParam): LResult; stdcall;
var
  ControlInfo: TControlInfo;
begin
  if IsAllowedMessage(msg) then
  begin
    ControlInfo := TControlInfo(GetProp(Wnd, 'TControlInfo'));
    Result := CallWindowProc(Addr(ControlInfo.PrevWindowProc), Wnd, msg,
      wParam, lParam);
  end
  else
    Result := 0;
end;


// -----------------------------------------------------------------
// TFormDesigner
// -----------------------------------------------------------------

constructor TFormDesigner.Create(AOwner: TComponent);
var
  DragHandleClass: TDragHandleClass;
  DragHandle: TDragHandle;
begin
  inherited Create(AOwner);
  if AOwner is TForm then
    FForm := TForm(AOwner)
  else
    FForm := TForm(GetParentForm(TControl(AOwner)));

  FForm.RemoveWindowStyle(WS_CLIPCHILDREN);
  FRectModifiers := TRectModifiers.Create;
  FControls := TObjectList<TControlInfo>.Create;

  FForm.OnMouseEnter := FormMouseEnterHandler;
  FDragHandleColor := RGB(178, 214, 243);
  FDragHandleSize := 8;
  FGridGap := 8;
  FSnapToGrid := True;
  FDragMode := dmDeferred;
  DrawGrid := True;

  for DragHandleClass in DragHandleClasses do
  begin
    DragHandle := DragHandleClass.Create(Self);
    DragHandle.SetProps(FDragHandleSize, FDragHandleColor, Self);
    InsertComponent(DragHandle);
  end;

  if not(csDesigning in ComponentState) then
  begin
    FApplicationEvents := TApplicationEvents.Create(nil);
    FApplicationEvents.OnMessage := MessageReceivedHandler;
    FHintTimer := TTimer.Create(nil);
    with FHintTimer do
    begin
      Interval := 200;
      Enabled := FShowHints;
      OnTimer := HintTimerHandler;
    end;
  end;

  FToolTip := TBalloonHint.Create(nil);
  // FToolTip.HideAfter := 100;
  FToolTip.Style := TBalloonHintStyle.bhsStandard;

end;

destructor TFormDesigner.Destroy;
begin
  FApplicationEvents.Free;
  FRectModifiers.Free;
  FHintTimer.Free;
  FToolTip.Free;
  FControls.Free;
  inherited Destroy;
end;

procedure TFormDesigner.SetupControlToAdd(var pt: TPoint; Control: TControl);
var
  Parent: TWinControl;
begin
  FSizingOrigin := TPoint.Create(pt.X, pt.Y);
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
  FControlToAdd.Parent := Parent;
  Child := FControlToAdd;
end;

procedure TFormDesigner.ClipCursor;
var
  Rect: TRect;
begin
  Rect := FParent.ClientRect.ClientToScreen(FParent);
  Windows.ClipCursor(@Rect);
end;

// ApplyChanges instructs whether changes to FChild's size/position should
// be applied on the button relase. When False, FChild's size/position
// is not changed.
procedure TFormDesigner.LButtonUpHandler(var msg: tagMSG;
  ApplyChanges: Boolean);
var
  MousePos: TPoint;
begin
  if (FState <> ssReady) and IsMessageForWindow(msg.HWnd, FForm.Handle) then
  begin
    FState := ssReady;
    FCurrentDragHandle := nil;
    FToolTip.HideHint;
    Windows.ClipCursor(nil);

    if Assigned(FChild) then
    begin
      // Clean up the last focus rect drawn
      DrawRect(True);
      // Should we apply changes to size/position?
      if ApplyChanges then
      begin
        FChild.BoundsRect := FRect;
        UpdateDragHandles;
      end;
      DragHandlesVisible := True;
    end;

    if Assigned(FControlToAdd) then
    begin
      MousePos.SetLocation(GET_X_LPARAM(msg.lParam), GET_Y_LPARAM(msg.lParam));
      if FControlToAdd.BoundsRect.BottomRight.IsZero or
        PointsEqual(FSizingOrigin, MousePos) then
      begin
        if (FSnapToGrid) then
        begin
          FControlToAdd.Left := AlignToGrid(FControlToAdd.Left);
          FControlToAdd.Top := AlignToGrid(FControlToAdd.Top);
        end;
        FControlToAdd.Width := FControlToAddRect.Width;
        FControlToAdd.Height := FControlToAddRect.Height;
        Child := FControlToAdd;
      end;
      if FControlToAddAutoSize then
        SetOrdProp(FControlToAdd, 'AutoSize', NativeInt(True));
      FControlToAdd.Visible := True;
      AddControl(FControlToAdd);
      ControlAdded;
      FControlToAdd := nil;
      UpdateDragHandles;
    end;
    ControlModified;
    LockMouse.Release;
  end;
end;

procedure TFormDesigner.DrawRect(OnlyCleanUp: Boolean = False);
var
  DC: HDC;
  RectModifier: TRectModifier;
begin
  if (FDragMode = dmDeferred) or Assigned(FControlToAdd) then
  begin
    DC := GetDC(FParent.Handle);
    try
      RectModifier := FRectModifiers.GetForControl(FChild);
      Log('Designer', 'Draw: FOldRect', FOldRect);
      DrawFocusRect(DC, RectModifier.Modify(FOldRect));
      if not OnlyCleanUp then
      begin
        Log('Designer', 'Draw: FRect', FRect);
        DrawFocusRect(DC, RectModifier.Modify(FRect));
        FOldRect := FRect;
      end;
    finally
      ReleaseDC(FParent.Handle, DC);
    end;
  end;
end;

function TFormDesigner.AlignToGrid(Num: integer; Offset: integer = 0): integer;
var
  Remainder: integer;
begin
  Result := Num;
  if FSnapToGrid then
  begin
    Remainder := Num mod FGridGap;
    if (Remainder > (FGridGap div 2)) then
    begin
      Result := Num + Offset + (FGridGap - Remainder);
    end
    else
    begin
      Result := Num + Offset - Remainder;
    end;
  end;
  Log('Designer', 'Aligned %d to %d', [Num, Result]);
end;

procedure TFormDesigner.LButtonDownHandler(Sender: TControl; X, Y: integer);
begin
  Log('Designer', 'OnLButtonDownHandler X: %d, Y: %d', [X, Y]);
  LockMouse.Acquire;
  FButtonDownOrigin := TPoint.Create(X, Y);
  FToolTip.HideHint;
  DragHandlesVisible := False;

  if not(Sender is TForm) then
  begin
    if Child <> Sender then
      Child := TControl(Sender);
    FClickOrigin.X := X - FChild.Left;
    FClickOrigin.Y := Y - FChild.Top;
    FState := ssMoving;
    DrawRect;
    ClipCursor;
  end
  else
  begin
    Child := nil;
  end;
end;

procedure TFormDesigner.CancelSizeMove(WindowHandle: HWnd);
var
  msg: tagMSG;
begin
  msg.HWnd := WindowHandle;
  LButtonUpHandler(msg, False);
end;

procedure TFormDesigner.UpdateChildProp(CtrlPropName, ShiftPropName: String;
  Value: integer; Shift: TShiftState);
var
  PropValue: integer;
begin
  if (ssCtrl in Shift) then
  begin
    PropValue := GetOrdProp(FChild, CtrlPropName);
    PropValue := PropValue + Value;
    SetOrdProp(FChild, CtrlPropName, PropValue);
  end
  else if (ssShift in Shift) then
  begin
    PropValue := GetOrdProp(FChild, ShiftPropName);
    PropValue := PropValue + Value;
    SetOrdProp(FChild, ShiftPropName, PropValue);
  end;
  FRect := FChild.BoundsRect;
  UpdateDragHandles;
  ControlModified;
end;

procedure TFormDesigner.KeyDownHandler(var msg: tagMSG);
var
  Shift: TShiftState;
  Key: Word;
begin
  if IsMessageForWindow(msg.HWnd, FForm.Handle) then
  begin
    Shift := KeyDataToShiftState(msg.lParam);
    Key := Word(msg.wParam);
    case FState of

      ssReady:
        case Key of

          VK_UP:
            UpdateChildProp('Top', 'Height', -1, Shift);

          VK_DOWN:
            UpdateChildProp('Top', 'Height', 1, Shift);

          VK_LEFT:
            UpdateChildProp('Left', 'Width', -1, Shift);

          VK_RIGHT:
            UpdateChildProp('Left', 'Width', 1, Shift);

          VK_DELETE:
            begin
              if FChild <> nil then
              begin
                RemoveControl(FChild);
                if (FChild is TWinControl) then
                  if TWinControl(FChild).ControlCount <> 0 then
                    EnumChilds(TWinControl(FChild), RemoveControl);
                if (FParent is TForm) then
                  TForm(FParent).ActiveControl := nil;
                FChild.Free;
                if FControls.Count <> 0 then
                  Child := FControls[0].Control
                else
                begin
                  Child := nil;
                  DragHandlesVisible := False;
                end;
              end;
            end;
        end;

      ssMoving, ssSizing:
        if (Key = VK_ESCAPE) then
          CancelSizeMove(msg.HWnd);

    end;
  end;
end;

procedure TFormDesigner.StartSizing(DragHandle: TDragHandle; MousePos: TPoint);
begin
  Log('Sizer', 'StartSizing');
  LockMouse.Acquire;
  FCurrentDragHandle := DragHandle;
  FCurrentDragHandle.SetSizingOrigin(MousePos.X, MousePos.Y);
  FState := ssSizing;
  DragHandlesVisible := False;
  ClipCursor;
  FRect := FChild.BoundsRect;
  FOldRect := TRect.Empty;
  DrawRect;
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
  EditWnd: HWnd;
begin
  ControlInfo := TControlInfo.Create;
  with ControlInfo do
  begin
    Control := AControl;
    FormDesigner := Self;
    PrevWndMethod := Control.WindowProc;
    Control.WindowProc := ControlWindowProc;
    if (AControl is TComboBox) then
    begin
      EditWnd := GetWindow(TComboBox(Control).Handle, GW_CHILD);
      SetClassLong(EditWnd, GCL_HCURSOR, LoadCursor(0, IDC_ARROW));
      SetProp(EditWnd, 'TControlInfo', DWORD(ControlInfo));
      @PrevWindowProc := Pointer(SetWindowLong(EditWnd, GWL_WNDPROC,
        Longint(@ComboEditWindowProcedure)));
    end;
  end;
  if AControl is TWinControl then
    TWinControl(AControl).RemoveWindowStyle(WS_CLIPCHILDREN);
  FControls.Add(ControlInfo);
  AControl.Cursor := crArrow;
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

procedure TFormDesigner.MouseMoveHandler(Sender: TControl; X, Y: integer);
begin
  Log('Designer', 'MouseMoveHandler: X: %d, Y: %d', [X, Y]);
  if PointsEqual(FButtonDownOrigin, TPoint.Create(X, Y)) then
    Exit;

  FLastMouseMove := Now;
  DrawRect;

  if (FSnapToGrid) then
  begin
    FRect.Left := AlignToGrid(X - FClickOrigin.X, FChild.Left mod FGridGap);
    FRect.Top := AlignToGrid(Y - FClickOrigin.Y, FChild.Top mod FGridGap);
  end
  else
  begin
    FRect.Left := X - FClickOrigin.X;
    FRect.Top := Y - FClickOrigin.Y;
  end;
  FRect.Bottom := FRect.Top + FChild.Height;
  FRect.Right := FRect.Left + FChild.Width;

  if FState = ssMoving then
  begin
    if FDragMode = dmImmediate then
      FChild.SetBounds(FRect.Left, FRect.Top, FRect.Width, FRect.Height)
    else
      DrawRect;
  end;
end;

procedure TFormDesigner.HintTimerHandler(Sender: TObject);
var
  ms: Int64;
  Control, Child: TControl;
  CursorPos: TPoint;
begin
  if FState = ssReady then
  begin
    ms := System.DateUtils.MilliSecondsBetween(Now, FLastMouseMove);
    if ms > 50 then
    begin
      CursorPos := Mouse.CursorPos;
      Control := Controls.FindVCLWindow(CursorPos);
      if (Control <> nil) and IsOwnedControl(Control) then
      begin
        Child := TWinControl(Control)
          .ControlAtPos(Control.ScreenToClient(CursorPos), True);
        // Is there a pure Delphi control? (TLabel, TShape, ... )
        if Assigned(Child) then
          Control := Child;
        if (Control <> FLastHintedControl) then
        begin
          FLastHintedControl := Control;
          FToolTip.Description :=
            Format('%s: %s%sBounds: %d, %d, %d, %d; Size: %d, %d',
            [Control.Name, Control.ClassName, sLineBreak, Control.Left,
            Control.Top, Control.BoundsRect.Right, Control.BoundsRect.Bottom,
            Control.Width, Control.Height, sLineBreak]);
          CursorPos.Offset(34, 24);
          FToolTip.ShowHint(CursorPos);
        end;
      end
      else
        FToolTip.HideHint;
    end;
  end;
end;

procedure TFormDesigner.FormMouseEnterHandler(Sender: TObject);
begin
  FToolTip.HideHint;
  FLastHintedControl := nil;
end;

procedure TFormDesigner.FormPaintHandler;
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
begin
  FOldRect := TRect.Empty;
  FChild := Value;
  if Assigned(FChild) then
  begin
    FParent := FChild.Parent;
    FRect := Child.BoundsRect;
    FChild.BringToFront;
    ForEachDragHandle(
      procedure(DragHandle: TDragHandle)
      begin
        DragHandle.Parent := FParent;
        DragHandle.BringToFront;
        DragHandle.UpdatePosition(FChild);
      end);
  end
  else
  begin
    FRect := TRect.Empty;
  end;
  ControlSelected;
end;

procedure TFormDesigner.UpdateDragHandles();
begin
  ForEachDragHandle(
    procedure(DragHandle: TDragHandle)
    begin
      DragHandle.UpdatePosition(FChild);
    end);
end;

procedure TFormDesigner.UpdateRect(Rect: TRect; Direction: TDirections);
begin
  Log('Designer', 'UpdateRect', Rect);
  if (not FSnapToGrid) or
    ((((Direction = dRight) or (Direction = dUpRight) or
    (Direction = dDownRight)) and (Rect.Right mod FGridGap = 0)) or
    (((Direction = dLeft) or (Direction = dUpLeft) or (Direction = dDownLeft))
    and (Rect.Left mod FGridGap = 0)) or
    (((Direction = dUp) or (Direction = dUpLeft) or (Direction = dUpRight)) and
    (Rect.Top mod FGridGap = 0)) or
    (((Direction = dDown) or (Direction = dDownLeft) or (Direction = dDownRight)
    ) and (Rect.Bottom mod FGridGap = 0))) then
  begin
    FRect := Rect;
    if (FDragMode = dmImmediate) then
      FChild.BoundsRect := FRect
    else
      DrawRect;
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

procedure TFormDesigner.PreProcessMessage(var msg: tagMSG;
Handler: TMessageHandler);
var
  Control: TControl;
  pt: TPoint;
begin
  if IsMessageForWindow(msg.HWnd, FForm.Handle) then
  begin
    pt := MAKEPOINT(msg.lParam);
    Control := FindControl(msg.HWnd);
    if Assigned(FControlToAdd) and (FState = ssReady) then
    begin
      SetupControlToAdd(pt, Control);
      StartSizing(DragHandleOfType(TDownRightDragHandle), TPoint.Zero);
    end
    else if Assigned(Control) then
    begin
      CallMessageHandler(msg.message, TWinControl(Control), pt, Handler);
    end
    else
    begin
      // No Delphi control found, is it EDIT in a Combobox?
      if TryGetParent(msg.HWnd, pt, Control) then
        CallMessageHandler(msg.message, TWinControl(Control), pt, Handler);
    end;
  end;
end;

procedure TFormDesigner.MessageReceivedHandler(var msg: tagMSG;
var Handled: Boolean);
var
  Control: TControl;
begin
  case msg.message of
    WM_MOUSEMOVE:
      begin
        if FState = ssMoving then
        begin
          Log('Designer', 'WM_MOUSEMOVE (%d)', [msg.HWnd]);
          PreProcessMessage(msg, MouseMoveHandler)
        end
        else if FState = ssSizing then
        begin
          Log('Designer', 'WM_MOUSEMOVE (%d)', [msg.HWnd]);
          PreProcessMessage(msg, FCurrentDragHandle.MouseMoveHandler);
        end;
      end;

    WM_LBUTTONDOWN:
      begin
        Log('Designer', 'WM_LBUTTONDOWN (%d)', [msg.HWnd]);
        Control := FindControl(msg.HWnd);
        if (Control is TDragHandle) then
          StartSizing(TDragHandle(Control), MAKEPOINT(msg.lParam))
        else
          PreProcessMessage(msg, LButtonDownHandler);
      end;

    WM_LBUTTONUP:
      begin
        Log('Designer', 'WM_LBUTTONUP (%d)', [msg.HWnd]);
        LButtonUpHandler(msg, True);
      end;

    WM_KEYDOWN:
      KeyDownHandler(msg);

  end;
end;

function TFormDesigner.TryGetParent(HWnd: HWnd; pt: TPoint;
var Control: TControl): Boolean;
var
  ClsName: array [0 .. 5] of char;
begin
  Result := False;
  GetClassName(HWnd, ClsName, 5);
  if UpperCase(ClsName) = 'EDIT' then
  begin
    // Probably TComboBox, find EDIT's parent
    Control := FindControl(GetParent(HWnd));
    if (Control <> nil) then
    begin
      Log('Designer', 'EDIT event, Orig pt: pt.X: %d, pt.Y: %d', [pt.X, pt.Y]);
      ClientToScreen(HWnd, pt);
      pt := Control.ScreenToClient(pt);
      Log('Designer', 'EDIT event: Control.Name: %s, pt.X: %d, pt.Y: %d',
        [Control.Name, pt.X, pt.Y]);
      // CallMessageHandler(msg.message, TWinControl(Control), pt, Handler);
      Result := True;
    end;
  end;
end;

procedure TFormDesigner.SetDragHandleColor(Value: TColor);
begin
  FDragHandleColor := Value;
  ForEachDragHandle(
    procedure(DragHandle: TDragHandle)
    begin
      DragHandle.SetProps(FDragHandleSize, FDragHandleColor, Self);
    end);
end;

procedure TFormDesigner.SetDrawGrid(Value: Boolean);
begin
  if FDrawGrid <> Value then
  begin
    FDrawGrid := Value;
    if FDrawGrid then
    begin
      if Assigned(FForm.OnPaint) then
        FFormOnPaint := FForm.OnPaint;
      FForm.OnPaint := FormPaintHandler;
    end
    else
    begin
      if Assigned(FFormOnPaint) then
      begin
        FForm.OnPaint := FFormOnPaint;
        FFormOnPaint := nil;
      end
      else
        FForm.OnPaint := nil;
    end;
    FForm.Refresh;
  end;
end;

procedure TFormDesigner.SetDragHandleSize(Value: byte);
begin
  FDragHandleSize := Value;
  ForEachDragHandle(
    procedure(DragHandle: TDragHandle)
    begin
      DragHandle.Width := Value;
      DragHandle.Height := Value;
    end);
end;

procedure TFormDesigner.SetDragHandlesVisible(Value: Boolean);
begin
  Log('Designer', 'DragHandlesVisible: From: %s to %s',
    [BoolToStr(FDragHandlesVisible), BoolToStr(Value)]);
  FDragHandlesVisible := Value;
  ForEachDragHandle(
    procedure(DragHandle: TDragHandle)
    begin
      DragHandle.Visible := Value;
    end);
  // Application.ProcessMessages;
  FForm.Update;
end;

function TFormDesigner.GetChildRect: TRect;
begin
  Result := FChild.BoundsRect;
end;

function TFormDesigner.GetRect: TRect;
begin
  Result := FRect;
end;

function TFormDesigner.DragHandleOfType(DragHandleClass: TDragHandleClass)
  : TDragHandle;
var
  i: integer;
  DragHandle: TDragHandle;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    DragHandle := TDragHandle(Components[i]);
    if DragHandle.ClassNameIs(DragHandleClass.ClassName) then
    begin
      Result := DragHandle;
      Exit;
    end;
  end;
  raise TFormDesignerException.Create('Unknow DragHandle Type');
end;

procedure TFormDesigner.SetGridGap(Value: integer);
begin
  FGridGap := Value;
  FForm.Refresh;
end;

procedure TFormDesigner.SetShowHints(Value: Boolean);
begin
  FHintTimer.Enabled := Value;
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

procedure TFormDesigner.ForEachDragHandle(Proc: TDragHandleProc);
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    Proc(TDragHandle(Components[i]));
end;

procedure TFormDesigner.ControlModified;
begin
  if (Assigned(FOnControlModified)) then
    FOnControlModified(FChild);
end;

procedure TFormDesigner.ControlSelected;
begin
  if (Assigned(FOnControlSelected)) then
    FOnControlSelected(FChild);
end;

procedure TFormDesigner.ControlAdded;
begin
  if (Assigned(FOnControlAdded)) then
    FOnControlAdded(FChild);
end;


// -----------------------------------------------------------------
// TInflatingRectModifier
// -----------------------------------------------------------------

constructor TInflatingRectModifier.Create(InflateBy: integer);
begin
  FInflateBy := InflateBy;
end;

function TInflatingRectModifier.Modify(var Rect: TRect): TRect;
var
  InflatedRect: TRect;
begin
  InflatedRect := Rect;
  InflatedRect.Inflate(FInflateBy, FInflateBy, FInflateBy, FInflateBy);
  Result := InflatedRect;
end;


// -----------------------------------------------------------------
// TRectModifierBase
// -----------------------------------------------------------------

function TRectModifierBase.Modify(var Rect: TRect): TRect;
begin
  Result := Rect;
end;


// -----------------------------------------------------------------
// TRectModifiers
// -----------------------------------------------------------------

function TRectModifiers.GetForControl(Control: TControl): TRectModifier;
var
  BaseClass: TControlClass;
begin
  for BaseClass in FRectModifiers.Keys do
  begin
    if Control.InheritsFrom(BaseClass) then
    begin
      Result := FRectModifiers.Items[BaseClass];
      Exit;
    end;
  end;
  Result := FDefaultRectModifier;
end;

constructor TRectModifiers.Create;
var
  InflatingRectModifier: TRectModifier;
begin
  FDefaultRectModifier := TRectModifierBase.Create;
  InflatingRectModifier := TInflatingRectModifier.Create(1);

  FRectModifiers := TObjectDictionary<TControlClass, TRectModifier>.Create
    ([doOwnsValues]);
  with FRectModifiers do
  begin
    Add(TCustomEdit, InflatingRectModifier);
    Add(TCustomListControl, InflatingRectModifier);
  end;
end;

destructor TRectModifiers.Destroy;
begin
  // No need to free FRectModifiers entries since
  // it has set dsOwnValues
  FDefaultRectModifier.Free;
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('Form Designer', [TFormDesigner]);
end;

initialization

LockMouse := TCriticalSection.Create;

finalization

LockMouse.Free;

end.
