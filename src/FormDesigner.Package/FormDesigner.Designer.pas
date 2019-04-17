unit FormDesigner.Designer;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections, Vcl.AppEvnts, FormDesigner.Interfaces, TypInfo,
  FormDesigner.Utils, FormDesigner.Marks, ExtCtrls, System.DateUtils,
  RTTI, System.Diagnostics, System.SyncObjs;

type

  /// <summary>
  /// Conrols behavior of TFormDesigner during moving/sizing.
  /// </summary
  TDragMode = (
    /// <summary>
    /// Changes to control'l position/size are visible immediatelly
    /// while dragging the mouse.
    /// </summary>
    dmImmediate,

    /// <summary>
    /// Changes to controol's position/size are projected with a focus
    /// rectangle and are applied after the mouse is released.
    /// </summary>
    dmDeferred);

  TMessageHandler = procedure(Sender: TControl; X, Y: integer) of object;
  TMarkProc = reference to procedure(Mark: TMark);
  TMarkClass = class of TMark;
  TFormDesignerException = class(Exception);
  TWindowProc = function(Wnd: HWnd; msg: DWORD; wParam: wParam; lParam: lParam)
    : LResult; stdcall;
  TControlInfo = class;
  TRectModifiers = class;

  /// <summary>
  /// The TFormDesigner class.
  /// Since not all Delphi controls have Windows handle (TLabel, TShape, ..)
  /// SetCapture/ReleaseCaputre is not used for sizing/moving.
  /// </summary>
  TFormDesigner = class(TComponent, IFormDesigner)
  private
    FParent: TWinControl;
    FChild: TControl;
    FMarkSize: byte;
    FRect: TRect;
    FMarksVisible: Boolean;

    /// <summary>
    /// Where in the control was clicked in Client coords
    /// </summary>
    FClickOrigin: TPoint;

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
    FSizingOrigin: TPoint;
    FButtonDownOrigin: TPoint;
    FRectModifiers: TRectModifiers;
    FOnControlSelected: TNotifyEvent;
    FOnControlModified: TNotifyEvent;
    FOnControlAdded: TNotifyEvent;
    procedure ControlSelected;
    procedure ControlModified;
    procedure ControlAdded;
    procedure UpdateChildProp(CtrlPropName, ShiftPropName: String;
      Value: integer; Shift: TShiftState);
    function TryGetParent(HWnd: HWnd; pt: TPoint; var Control: TControl) : Boolean;
    procedure CancelSizeMove(WindowHandle: HWnd);
    procedure SetChild(Value: TControl);
    procedure SetMarkSize(Value: byte);
    procedure SetMarksVisible(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetGridGap(Value: integer);
    procedure SetDrawGrid(Value: Boolean);
    procedure ClipCursor;
    procedure UpdateMarks;
    procedure MessageReceivedHandler(var msg: tagMSG; var Handled: Boolean);
    procedure CallMessageHandler(msg: UINT; Control: TWinControl;
      var pt: TPoint; Handler: TMessageHandler);
    procedure PreProcessMessage(var msg: tagMSG; Handler: TMessageHandler);
    procedure FormPaintHandler(Sender: TObject);
    procedure ForEachMark(Proc: TMarkProc);
    procedure TimerHandler(Sender: TObject);
    procedure FormMouseEnterHandler(Sender: TObject);
    procedure ShowHint(CursorPos: TPoint; Format: String;
      const Args: array of const);
    function MarkOfType(MarkClass: TMarkClass): TMark;
    function IsOwnedControl(Control: TControl): Boolean;
    function AlignToGrid(Num: integer; Offset: integer = 0): integer;
    procedure LButtonUpHandler(var msg: tagMSG; UpdateChildRect: Boolean);
    /// <summary>
    /// Handles mouse move. X, Y are in client coords of Sender's parent
    /// </summary>
    procedure MouseMoveHandler(Sender: TControl; X, Y: integer);
    procedure LButtonDownHandler(Sender: TControl; X, Y: integer);
    procedure KeyDownHandler(var msg: tagMSG);
    procedure FocusControl(Control: TControl);
    procedure DrawRect(OnlyCleanUp: Boolean = False);
    procedure StartSizing(Mark: TMark; MousePos: TPoint);
    property MarksVisible: Boolean read FMarksVisible write SetMarksVisible;
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
    property MarkSize: byte read FMarkSize write SetMarkSize default 8;
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid
      default True;
    property Color: TColor read FColor write SetColor default clBlack;
    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default True;
    property GridGap: integer read FGridGap write SetGridGap default 8;
    property DragMode: TDragMode read FDragMode write FDragMode
      default dmDeferred;
    property OnControlAdded: TNotifyEvent read FOnControlAdded
      write FOnControlAdded;
    property OnControlSelected: TNotifyEvent read FOnControlSelected
      write FOnControlSelected;
    property OnControlModified: TNotifyEvent read FOnControlModified
      write FOnControlModified;
  end;

  /// <summary>
  /// Holds information about a control managed by TFormDesigner
  /// </summary>
  TControlInfo = class
  public
    Sizer: TFormDesigner;
    Control: TControl;

    /// <summary>
    /// Used to replace Window procedure for Delphi controls
    /// </summary>
    PrevWndMethod: TWndMethod;

    /// <summary>
    /// Used to replace Window procedure for controls not managed by Delphi
    /// (like Edit inside a ComboBox)
    /// </summary>
    PrevWindowProc: TWindowProc;
    procedure ControlWindowProc(var msg: TMessage);
  end;

  /// <summary>
  /// There are slight differences among TControl.BoundsRect
  /// For TButton, BoundsRect includes 1px gap around the control, which accomodes focus rectangle
  /// For TComboBox or TEdit, there is no such gap so the focus rect has to be expanded
  /// These differences are handled by TRectModifier's</summary>
  /// </summary>
  TRectModifier = class abstract
  public
    function Modify(var Rect: TRect): TRect; virtual; abstract;
  end;

  /// <summary>
  /// Base class for TRectModifier. Its <see cref="Modify"> method returns
  /// TRect without any changes
  /// </summary>
  TRectModifierBase = class(TRectModifier)
    function Modify(var Rect: TRect): TRect; override;
  end;

  /// <summary>
  /// Inflates TRect by a specified number of pixels
  /// </summary>
  TInflatingRectModifier = class(TRectModifierBase)
  private
    FInflateBy: integer;
  public
    constructor Create(InflateBy: integer);
    function Modify(var Rect: TRect): TRect; override;
  end;

  /// <summary>
  /// Holds collection of TRectModifier.
  /// </summary>
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
  MarkClasses: array [0 .. 7] of TMarkClass = (TUpMark, TDownMark, TLeftMark,
    TRightMark, TUpLeftMark, TUpRightMark, TDownLeftMark, TDownRightMark);

implementation

var
  LockMouse: TCriticalSection;

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
  if AOwner is TForm then
    FForm := TForm(AOwner)
  else
    FForm := TForm(GetParentForm(TControl(AOwner)));

  FRectModifiers := TRectModifiers.Create;
  FControls := TObjectList<TControlInfo>.Create;

  FForm.OnMouseEnter := FormMouseEnterHandler;
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
    FApplicationEvents.OnMessage := MessageReceivedHandler;

    FTimer := TTimer.Create(nil);
    FTimer.Interval := 200;
    FTimer.Enabled := True;
    FTimer.OnTimer := TimerHandler;
  end;

  FToolTip := TBalloonHint.Create(nil);
  // FToolTip.HideAfter := 100;
  FToolTip.Style := TBalloonHintStyle.bhsStandard;

end;

destructor TFormDesigner.Destroy;
begin
  FApplicationEvents.Free;
  FRectModifiers.Free;
  FTimer.Free;
  FToolTip.Free;
  FControls.Free;
  inherited Destroy;
end;

procedure TFormDesigner.SetupControlToAdd(var pt: TPoint; Control: TControl);
var
  Parent: TWinControl;
begin
  Log('Designer', 'SetupControToAdd');
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
  Log('Designer', 'FControlToAdd', FControlToAdd.BoundsRect);
  FControlToAdd.Parent := Parent;
  Child := FControlToAdd;
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
  // FRect.Inflate(1, 1, 1, 1);
end;

procedure TFormDesigner.LButtonUpHandler(var msg: tagMSG;
  UpdateChildRect: Boolean);
var
  MousePos: TPoint;
  InvalidRect: TRect;
  DC: HDC;
begin
  if (FState <> ssReady) and IsMessageForWindow(msg.HWnd, FForm.Handle) then
  begin
    Log('Designer', 'OnLButtonUpHandler: State: %s',
      [TRttiEnumerationType.GetName(FState)]);

    FState := ssReady;
    Windows.ClipCursor(nil);
    FCurrentMark := nil;

    FToolTip.HideHint;

    if Assigned(FChild) then
    begin
      DrawRect(True);
      if UpdateChildRect then
      begin
        FChild.BoundsRect := FRect;
        UpdateMarks;
      end;
      MarksVisible := True;
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
      UpdateMarks;
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
  LockMouse.Acquire;

  FButtonDownOrigin := TPoint.Create(X, Y);

  Log('Designer', 'OnLButtonDownHandler X: %d, Y: %d', [X, Y]);
  Log('Designer', 'FRect', FRect);

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

  DrawRect;

  FClickOrigin.X := X - FChild.Left;
  FClickOrigin.Y := Y - FChild.Top;

  FState := ssMoving;
  ClipCursor;

  // Process any waiting messages. This needs to be called
  // last in this proc to prevent premature processing of
  // WM_LBUTTONUP
  //Application.ProcessMessages;
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
  UpdateMarks;
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
                  MarksVisible := False;
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

procedure TFormDesigner.StartSizing(Mark: TMark; MousePos: TPoint);
begin
  Log('StartSizing', 'Mark: %s; Before Acquire', [Mark.ClassName]);
  LockMouse.Acquire;
    Log('StartSizing', 'Mark: %s; After Acquire', [Mark.ClassName]);
  FCurrentMark := Mark;
  FCurrentMark.SetSizingOrigin(MousePos.X, MousePos.Y);
  FState := ssSizing;
  MarksVisible := False;
  ClipCursor;
  FRect := FChild.BoundsRect;
  FOldRect := TRect.Empty;
  //Application.ProcessMessages;
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
  // FToolTip.ShowHint(CursorPos);
  // Log('Designer', 'Showing hint ... ');
end;

procedure TFormDesigner.MouseMoveHandler(Sender: TControl; X, Y: integer);
var
  CursorPos: TPoint;
begin

  Log('Designer',
    'OnMouseMoveHandler: X: %d, Y: %d, FButtonDownOrigin.X: %d, FButtonDownOrigin.kY: %d',
    [X, Y, FButtonDownOrigin.X, FButtonDownOrigin.Y]);

  CursorPos := Mouse.CursorPos;
  if (PointsEqual(CursorPos, FLastMousePos)) or PointsEqual(FButtonDownOrigin,
    TPoint.Create(X, Y)) then
    Exit;

  FLastMousePos := CursorPos;

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

  if (FState = ssMoving) then
  begin
    if FDragMode = dmImmediate then
      FChild.SetBounds(FRect.Left, FRect.Top, FRect.Width, FRect.Height)
    else
      DrawRect;
    // if FState = ssMoving then
    // ShowHint(CursorPos, '%d, %d', [FChild.Left, FChild.Top]);
  end;
end;

procedure TFormDesigner.TimerHandler(Sender: TObject);
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
      // FToolTip.ShowHint(CursorPos);
    end
    else
      FToolTip.HideHint;
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
    // MarksVisible := False;
    ForEachMark(
      procedure(Mark: TMark)
      begin
        Mark.Parent := FParent;
        Mark.BringToFront;
        Mark.Update(FChild);
      end);
    // MarksVisible := True;
    FRect := Child.BoundsRect;
  end
  else
    FRect := TRect.Empty;
  ControlSelected;
end;

procedure TFormDesigner.UpdateMarks();
begin
  ForEachMark(
    procedure(Mark: TMark)
    begin
      Mark.Update(FChild);
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
      StartSizing(MarkOfType(TDownRightMark), TPoint.Zero);
    end
    else if Assigned(Control) then
    begin
      CallMessageHandler(msg.message, TWinControl(Control), pt, Handler);
    end
    else
    begin
      // No Delphi control found, is it EDIT in a Combobox?
      if TryGetParent(msg.hwnd, pt, Control) then
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
          Log('Designer', 'WM_MOUSEMOVE (%d)', [msg.hwnd]);
          PreProcessMessage(msg, MouseMoveHandler)
        end
        else if FState = ssSizing then
        begin
          Log('Designer', 'WM_MOUSEMOVE (%d)', [msg.hwnd]);
          PreProcessMessage(msg, FCurrentMark.MouseMoveHandler);
        end;
      end;

    WM_LBUTTONDOWN:
      begin
        Log('Designer', 'WM_LBUTTONDOWN (%d)', [msg.hwnd]);
        Control := FindControl(msg.HWnd);
        if (Control is TMark) then
          StartSizing(TMark(Control), MAKEPOINT(msg.lParam))
        else
          PreProcessMessage(msg, LButtonDownHandler);
      end;

    WM_LBUTTONUP:
      begin
        Log('Designer', 'WM_LBUTTONUP (%d)', [msg.hwnd]);
        LButtonUpHandler(msg, True);
      end;

    WM_KEYDOWN:
      KeyDownHandler(msg);

  end;
end;

function TFormDesigner.TryGetParent(HWnd: HWnd; pt: TPoint; var Control: TControl) : Boolean;
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
      //CallMessageHandler(msg.message, TWinControl(Control), pt, Handler);
      Result := True;
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
  Log('Designer', 'MarksVisible: From: %s to %s', [BoolToStr(FMarksVisible),
    BoolToStr(Value)]);
  FMarksVisible := Value;
  ForEachMark(
    procedure(Mark: TMark)
    begin
      Mark.Visible := Value;
    end);
  //Application.ProcessMessages;
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

{ TInflatingRectModifier }

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

{ TRectModifierBase }

function TRectModifierBase.Modify(var Rect: TRect): TRect;
begin
  Result := Rect;
end;

{ TRectModifiers }

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
  FDefaultRectModifier.Free;
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
