unit FormDesigner.Designer;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, Vcl.AppEvnts, TypInfo, ExtCtrls,
  System.DateUtils, RTTI, System.SyncObjs, FormDesigner.Interfaces,
  FormDesigner.Utils, FormDesigner.DragHandles;

type

  /// Conrols behavior of TFormDesigner during moving/sizing.
  TDragMode = (
    /// Changes to control's position/size are projected with a focus
    /// rectangle and are applied after the mouse is released.
    dmDeferred,

    /// Changes to control's position/size are visible immediatelly
    /// while dragging the mouse. EXPERIMENTAL, needs more work.
    dmImmediate);

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
    FDragHandleSize: Byte;
    FDragRect: TRect;
    FDragHandlesVisible: Boolean;
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
    // Where a control was clicked in client (control)
    // coordinates. Used while dragging the control
    FClickOrigin: TPoint;
    // Initial mouse position when setting dimensions
    // for a new control (by dragging a focus rect)
    FSizingOrigin: TPoint;
    // Mouse position in parent coordinages
    // when a button was pressed
    FButtonDownOrigin: TPoint;
    FRectModifiers: TRectModifiers;
    FOnControlSelected: TNotifyEvent;
    FOnControlModified: TNotifyEvent;
    FOnControlAdded: TNotifyEvent;
    FDragHandleBorderColor: TColor;
    FEnabled: Boolean;
    procedure ControlSelected;
    procedure ControlModified;
    procedure ControlAdded;
    procedure SetEnabled(const Value: Boolean);
    procedure SetShowHints(const Value: Boolean);
    function TryGetParent(const HWnd: HWnd; pt: TPoint;
      var Control: TControl): Boolean;
    function FindControl(Handle: HWnd): TControl;
    procedure CancelSizeMove(WindowHandle: HWnd);
    procedure SetChild(Value: TControl);
    procedure SetDragHandleSize(Value: Byte);
    procedure SetDragHandlesVisible(Value: Boolean);
    procedure SetDragHandleColor(Value: TColor);
    procedure SetGridGap(Value: integer);
    procedure SetDrawGrid(Value: Boolean);
    procedure ClipCursor;
    procedure UpdateDragHandles;
    procedure MessageReceivedHandler(var msg: tagMSG; var Handled: Boolean);
    procedure FormPaintHandler(Sender: TObject);
    procedure ForEachDragHandle(Proc: TDragHandleProc);
    procedure HintTimerHandler(Sender: TObject);
    function DragHandleOfType(DragHandleClass: TDragHandleClass): TDragHandle;
    function IsOwnedControl(Control: TControl): Boolean;
    function AlignToGrid(Num: integer; Offset: integer = 0): integer;
    procedure MouseUpHandler(var msg: tagMSG; ApplyChanges: Boolean);
    procedure UpdateChildPos(Sender: TControl; X, Y: integer);
    procedure MouseMoveHandler(var msg: tagMSG);
    procedure StartMoving(Sender: TControl; X, Y: integer);
    procedure KeyDownHandler(var msg: tagMSG);
    procedure DrawRect(OnlyCleanUp: Boolean = False);
    procedure StartSizing(DragHandle: TDragHandle; MousePos: TPoint);
    property DragHandlesVisible: Boolean read FDragHandlesVisible
      write SetDragHandlesVisible;
    property Child: TControl read FChild write SetChild;
    procedure SetupControlToAdd(var pt: TPoint; Control: TControl);
    procedure SetDragHandleBorderColor(const Value: TColor);
    procedure MouseDownHandler(var msg: tagMSG);
    procedure SetCursor(CtrlInfo: TControlInfo);
    procedure RestoreCursor(CtrlInfo: TControlInfo);
  public
    function GetDragRect: TRect;
    function GetChildRect: TRect;
    procedure UpdateRect(Rect: TRect; Directions: TDirections);
    procedure AddControl(ControlClass: TControlClass); overload;
    procedure AddControl(AControl: TControl); overload;
    procedure RemoveControl(Control: TControl);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property DragHandleSize: Byte read FDragHandleSize write SetDragHandleSize
      default 8;
    property DragHandleBorderColor: TColor read FDragHandleBorderColor
      write SetDragHandleBorderColor default TColor($D77800);
    // RGB(0, 120, 215);
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid
      default True;
    property DragHandleColor: TColor read FDragHandleColor
      write SetDragHandleColor default TColor($F3D6B2); // RGB(178, 214, 243);
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
    PrevCursor: TCursor;

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

// Filters Window messages and skips those related to keyboard or mouse
// interaction. The aim is to hide any control interaction while they are managed by
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
  if not FormDesigner.Enabled or IsAllowedMessage(msg.msg) then
    PrevWndMethod(msg);
end;

// Windows procedure for controls not managed by Delphi, like EDIT inside a
// TComboBox. It is set by using SetWindowLong. In this case instance
// methods can't be used.
// If Form Designer is enabled, set arrow as a cursor.
function ComboEditWindowProcedure(Wnd: HWnd; msg: Cardinal; wParam: wParam;
  lParam: lParam): LResult; stdcall;
var
  ControlInfo: TControlInfo;
begin
  Result := 0;
  ControlInfo := TControlInfo(GetProp(Wnd, 'TControlInfo'));
  if (msg = WM_SETCURSOR) and ControlInfo.FormDesigner.Enabled then
    SetCursor(LoadCursor(0, IDC_ARROW))
  else if not ControlInfo.FormDesigner.Enabled or IsAllowedMessage(msg) then
    Result := CallWindowProc(Addr(ControlInfo.PrevWindowProc), Wnd, msg,
      wParam, lParam);
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

  FDragHandleColor := RGB(178, 214, 243);
  FDragHandleBorderColor := RGB(0, 120, 215);
  FDragHandleSize := 8;
  FGridGap := 8;
  FSnapToGrid := True;
  FDragMode := dmDeferred;
  FEnabled := True;
  DrawGrid := True;

  for DragHandleClass in DragHandleClasses do
  begin
    DragHandle := DragHandleClass.Create(Self);
    with DragHandle do
    begin
      Size := FDragHandleSize;
      Color := FDragHandleColor;
      BorderColor := FDragHandleBorderColor;
      FormDesigner := Self;
    end;
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
  FToolTip.HideAfter := 2000;
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

procedure TFormDesigner.MessageReceivedHandler(var msg: tagMSG;
  var Handled: Boolean);
begin
  if (FEnabled) then
  begin
    case msg.message of
      WM_MOUSEMOVE:
        MouseMoveHandler(msg);

      WM_LBUTTONDOWN:
        MouseDownHandler(msg);

      WM_LBUTTONUP:
        MouseUpHandler(msg, True);

      WM_KEYDOWN:
        KeyDownHandler(msg);
    end;
  end;
end;

procedure TFormDesigner.MouseDownHandler(var msg: tagMSG);
var
  GraphicCtrl, Control: TControl;
  pt: TPoint;
begin
  if IsMessageForWindow(msg.HWnd, FForm.Handle) then
  begin
    pt := MAKEPOINT(msg.lParam);
    Control := FindControl(msg.HWnd);
    if (Control is TDragHandle) then
    begin
      StartSizing(TDragHandle(Control), MAKEPOINT(msg.lParam))
    end
    else if Assigned(FControlToAdd) and (FState = fdsReady) then
    begin
      SetupControlToAdd(pt, Control);
      StartSizing(DragHandleOfType(TDownRightDragHandle), TPoint.Zero);
    end
    else if Assigned(Control) or TryGetParent(msg.HWnd, pt, Control) then
    begin
      // Is there a TGraphicControl (TLabel, .. ) under the cursor?
      GraphicCtrl := TWinControl(Control).ControlAtPos(pt, True);
      if GraphicCtrl <> nil then
      begin
        if IsOwnedControl(GraphicCtrl) then
          StartMoving(GraphicCtrl, pt.X, pt.Y)
      end
      else
      begin
        // Handle clicks on an owned control or parent form
        if IsOwnedControl(Control) or (Control = FForm) then
        begin
          if Control.Parent <> nil then
            pt := Control.Parent.ScreenToClient(Control.ClientToScreen(pt));
          StartMoving(Control, pt.X, pt.Y);
        end;
      end;
    end
  end;
end;

procedure TFormDesigner.MouseMoveHandler(var msg: tagMSG);
var
  pt: TPoint;
  Control: TControl;
begin
  if not(FState = fdsReady) then
  begin
    if IsMessageForWindow(msg.HWnd, FForm.Handle) then
    begin
      pt := MAKEPOINT(msg.lParam);
      Control := FindControl(msg.HWnd);
      if Assigned(Control) or TryGetParent(msg.HWnd, pt, Control) then
      begin
        pt := FChild.Parent.ScreenToClient(Control.ClientToScreen(pt));
        if FState = fdsMoving then
          UpdateChildPos(Control.Parent, pt.X, pt.Y)
        else
          FCurrentDragHandle.UpdateChildSize(Control.Parent, pt.X, pt.Y);
      end;
    end
  end
  else if FShowHints and (msg.HWnd = FForm.Handle) and
    not Assigned(FForm.ControlAtPos(MAKEPOINT(msg.lParam), True, True, True))
  then
  begin
    // Hide hint when mouse is over the FForm
    FToolTip.HideHint;
    FLastHintedControl := nil;
  end;

end;

procedure TFormDesigner.KeyDownHandler(var msg: tagMSG);
var
  Shift: TShiftState;
  Key: Word;

  procedure UpdateChildProp(CtrlPropName, ShiftPropName: String;
    Value: integer);
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
    FDragRect := FChild.BoundsRect;
    UpdateDragHandles;
    ControlModified;
  end;

begin
  if IsMessageForWindow(msg.HWnd, FForm.Handle) then
  begin
    Shift := KeyDataToShiftState(msg.lParam);
    Key := Word(msg.wParam);
    case FState of

      fdsReady:
        case Key of
          VK_UP:
            UpdateChildProp('Top', 'Height', -1);
          VK_DOWN:
            UpdateChildProp('Top', 'Height', 1);
          VK_LEFT:
            UpdateChildProp('Left', 'Width', -1);
          VK_RIGHT:
            UpdateChildProp('Left', 'Width', 1);
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

      fdsMoving, fdsSizing:
        if (Key = VK_ESCAPE) then
          CancelSizeMove(msg.HWnd);

    end;
  end;
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
  Parent.RemoveWindowStyle(WS_CLIPCHILDREN);
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

// ApplyChanges tells whether changes to FChild's size/position should
// be applied on the button relase. When False, FChild's size/position
// is not changed.
procedure TFormDesigner.MouseUpHandler(var msg: tagMSG; ApplyChanges: Boolean);
var
  MousePos: TPoint;
begin
  if (FState <> fdsReady) and IsMessageForWindow(msg.HWnd, FForm.Handle) then
  begin
    FState := fdsReady;
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
        FChild.BoundsRect := FDragRect;
        UpdateDragHandles;
      end;
      DragHandlesVisible := True;
    end;

    if Assigned(FControlToAdd) then
    begin
      MousePos.SetLocation(MAKEPOINT(msg.lParam));
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
      DrawFocusRect(DC, RectModifier.Modify(FOldRect));
      if not OnlyCleanUp then
      begin
        DrawFocusRect(DC, RectModifier.Modify(FDragRect));
        FOldRect := FDragRect;
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
end;

procedure TFormDesigner.StartMoving(Sender: TControl; X, Y: integer);
begin
  FButtonDownOrigin := TPoint.Create(X, Y);
  FToolTip.HideHint;
  DragHandlesVisible := False;
  if Assigned(FChild) then
    FDragRect := Child.BoundsRect;
  FOldRect := TRect.Empty;

  if not(Sender is TForm) then
  begin
    if Child <> Sender then
      Child := Sender;
    FClickOrigin.X := X - FChild.Left;
    FClickOrigin.Y := Y - FChild.Top;
    FState := fdsMoving;
    ClipCursor;
    DrawRect;
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
  MouseUpHandler(msg, False);
end;

procedure TFormDesigner.StartSizing(DragHandle: TDragHandle; MousePos: TPoint);
begin
  FCurrentDragHandle := DragHandle;
  FCurrentDragHandle.SetSizingOrigin(MousePos.X, MousePos.Y);
  FState := fdsSizing;
  DragHandlesVisible := False;
  ClipCursor;
  FDragRect := FChild.BoundsRect;
  FOldRect := TRect.Empty;
  FToolTip.HideHint;
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
    SetCursor(ControlInfo);
    if (AControl is TComboBox) then
    begin
      EditWnd := GetWindow(TComboBox(Control).Handle, GW_CHILD);
      SetProp(EditWnd, 'TControlInfo', DWORD(ControlInfo));
      @PrevWindowProc := Pointer(SetWindowLong(EditWnd, GWL_WNDPROC,
        Longint(@ComboEditWindowProcedure)));
    end;
    if Control is TWinControl then
    begin
      TWinControl(AControl).RemoveWindowStyle(WS_CLIPCHILDREN);
    end;
  end;
  FControls.Add(ControlInfo);
end;

procedure TFormDesigner.RemoveControl(Control: TControl);
var
  i: Byte;
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

procedure TFormDesigner.UpdateChildPos(Sender: TControl; X, Y: integer);
begin
  if PointsEqual(FButtonDownOrigin, TPoint.Create(X, Y)) then
    Exit;

  FLastMouseMove := Now;
  DrawRect;

  if (FSnapToGrid) then
  begin
    FDragRect.Left := AlignToGrid(X - FClickOrigin.X, FChild.Left mod FGridGap);
    FDragRect.Top := AlignToGrid(Y - FClickOrigin.Y, FChild.Top mod FGridGap);
  end
  else
  begin
    FDragRect.Left := X - FClickOrigin.X;
    FDragRect.Top := Y - FClickOrigin.Y;
  end;

  FDragRect.Bottom := FDragRect.Top + FChild.Height;
  FDragRect.Right := FDragRect.Left + FChild.Width;

  if FDragMode = dmImmediate then
    FChild.BoundsRect := FDragRect
  else
    DrawRect;

end;

procedure TFormDesigner.HintTimerHandler(Sender: TObject);
var
  ms: Int64;
  Control, Child: TControl;
  CursorPos: TPoint;
begin
  if FEnabled and (FState = fdsReady) then
  begin
    ms := System.DateUtils.MilliSecondsBetween(Now, FLastMouseMove);
    if ms > 50 then
    begin
      CursorPos := Mouse.CursorPos;
      Control := Controls.FindVCLWindow(CursorPos);
      if Assigned(Control) then
      begin
        Child := TWinControl(Control)
          .ControlAtPos(Control.ScreenToClient(CursorPos), True, True, True);
        // Is there a pure Delphi control? (TLabel, TShape, ... )
        if Assigned(Child) then
          Control := Child;
        if (Control <> FLastHintedControl) and IsOwnedControl(Control) then
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

procedure TFormDesigner.FormPaintHandler;
var
  i, j: integer;
begin
  if FEnabled then
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
end;

procedure TFormDesigner.SetChild(Value: TControl);
begin
  FChild := Value;
  if Assigned(FChild) then
  begin
    FParent := FChild.Parent;
    FDragRect := Child.BoundsRect;
    // Internally, Delphi manipulates windows during BringToFront
    // which brings troubles with painting focus rect
    // --> FChild.BringToFront;
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
    FDragRect := TRect.Empty;
  end;
  FOldRect := TRect.Empty;
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

procedure TFormDesigner.UpdateRect(Rect: TRect; Directions: TDirections);
begin
  FDragRect := Rect;
  if FSnapToGrid then
  begin
    if dLeft in Directions then
      FDragRect.Left := AlignToGrid(FDragRect.Left);

    if dRight in Directions then
      FDragRect.Right := AlignToGrid(FDragRect.Right);

    if dTop in Directions then
      FDragRect.Top := AlignToGrid(FDragRect.Top);

    if dBottom in Directions then
      FDragRect.Bottom := AlignToGrid(FDragRect.Bottom);
  end;

  if (FDragMode = dmImmediate) then
    FChild.BoundsRect := FDragRect
  else
    DrawRect;
end;

function TFormDesigner.TryGetParent(const HWnd: HWnd; pt: TPoint;
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
      ClientToScreen(HWnd, pt);
      pt := Control.ScreenToClient(pt);
      Result := True;
    end;
  end;
end;

procedure TFormDesigner.SetDragHandleBorderColor(const Value: TColor);
begin
  FDragHandleBorderColor := Value;
  ForEachDragHandle(
    procedure(DragHandle: TDragHandle)
    begin
      DragHandle.BorderColor := FDragHandleBorderColor;
    end);
end;

procedure TFormDesigner.SetDragHandleColor(Value: TColor);
begin
  FDragHandleColor := Value;
  ForEachDragHandle(
    procedure(DragHandle: TDragHandle)
    begin
      DragHandle.Color := FDragHandleColor;
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

procedure TFormDesigner.SetCursor(CtrlInfo: TControlInfo);
var
  EditWnd: HWnd;
begin
  with CtrlInfo do
  begin
    PrevCursor := Control.Cursor;
    Control.Cursor := crArrow;
  end;
end;

procedure TFormDesigner.RestoreCursor(CtrlInfo: TControlInfo);
begin
  CtrlInfo.Control.Cursor := CtrlInfo.PrevCursor;
end;

procedure TFormDesigner.SetEnabled(const Value: Boolean);
var
  CtrlInfo: TControlInfo;
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not FEnabled then
    begin
      Child := nil;
      DragHandlesVisible := False;
      for CtrlInfo in FControls do
        RestoreCursor(CtrlInfo);
    end
    else
    begin
      for CtrlInfo in FControls do
        SetCursor(CtrlInfo);
    end;
    FForm.Refresh;
  end;
end;

procedure TFormDesigner.SetDragHandleSize(Value: Byte);
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
  FDragHandlesVisible := Value;
  ForEachDragHandle(
    procedure(DragHandle: TDragHandle)
    begin
      DragHandle.Visible := Value;
    end);
  FForm.Update;
end;

function TFormDesigner.GetChildRect: TRect;
begin
  Result := FChild.BoundsRect;
end;

function TFormDesigner.GetDragRect: TRect;
begin
  Result := FDragRect;
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

procedure TFormDesigner.SetShowHints(const Value: Boolean);
begin
  FShowHints := Value;
  if not(csDesigning in ComponentState) then
  begin
    FHintTimer.Enabled := Value;
  end;
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

function TFormDesigner.FindControl(Handle: HWnd): TControl;
begin
  Result := Vcl.Controls.FindControl(Handle);
  if not Assigned(Result) then
    TryGetParent(Handle, TPoint.Zero, Result);
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

end.
