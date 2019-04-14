unit FormDesigner.Utils;

interface

uses Classes, Controls, Windows, SysUtils, IOUtils, WinApi.Messages;


type

TEnumChildsProc = reference to procedure(Control: TControl);

{ TQueue }
TQueue = class
private
  FList: TList;
  FMax: Longint;
  function GetCount: Longint;
public
  function Get: Longint;
  procedure Add(Value: Longint);
  constructor Create(Max: Longint);
  destructor Destroy; override;
  property Count: Longint read GetCount;
 end;

function GET_X_LPARAM(lParam: lParam): Integer;
function GET_Y_LPARAM(lParam: lParam): Integer;
function GetControlName(Parent: TWinControl; Cls: TClass): String;
procedure Log(msg: string); overload;
procedure Log(const source, msg: string; const Rect: TRect); overload;
procedure Log(const source: string; const message: string); overload;
procedure Log(const Format: string; const Args: array of const); overload;
procedure Log(const source: string; const Format: string; const Args: array of const); overload;

procedure EnumChilds(RootCtrl: TWinControl; Proc: TEnumChildsProc);
procedure SetControlText(Control: TControl; Text: String);
function IsMessageForWindow(MsgHandle: HWnd; WindowHandle: HWnd) : Boolean;

implementation

// -----------------------------------------------------------------
// TQueue
// -----------------------------------------------------------------

constructor TQueue.Create;
begin
  inherited Create;
  FList := TList.Create;
  FMax := Max;
end;

destructor TQueue.Destroy;
begin
  inherited;
  FList.Free;
end;

procedure TQueue.Add;
begin
  if FList.Count < FMax then
    FList.Add(Pointer(Value));
end;

function TQueue.Get: Longint;
begin
  Result := Longint(FList.Last);
  FList.Remove(FList.Last);
end;

function TQueue.GetCount: Longint;
begin
  Result := FList.Count;
end;


// -----------------------------------------------------------------
// Routines
// -----------------------------------------------------------------

procedure EnumChilds(RootCtrl: TWinControl; Proc: TEnumChildsProc);
var
  i: byte;
begin
  if RootCtrl.ControlCount <> 0 then
    for i := 0 to RootCtrl.ControlCount - 1 do
    begin
      Proc(RootCtrl.Controls[i]);
      if RootCtrl.Controls[i] is TWinControl then
        if (RootCtrl.Controls[i] as TWinControl).ControlCount <> 0 then
          EnumChilds(RootCtrl.Controls[i] as TWinControl, Proc);
    end;
end;

function GetControlName(Parent: TWinControl; Cls: TClass): String;
var
  CtrlCount: Integer;
begin
  CtrlCount := 1;
  EnumChilds(Parent, procedure (Ctrl: TControl)
    begin
      if (Ctrl is Cls) then
        Inc(CtrlCount);
    end);
  // i.e. Button1
  Result := Format('%s%d', [Copy(Cls.ClassName, 2, Length(Cls.ClassName) - 1), CtrlCount]);
end;

function GET_X_LPARAM(lParam: lParam): Integer;
begin
  Result := Smallint(LoWord(lParam));
end;

function GET_Y_LPARAM(lParam: lParam): Integer;
begin
  Result := Smallint(HiWord(lParam));
end;


// -----------------------------------------------------------------
// Logging
// -----------------------------------------------------------------

function GetLogFilePath : String;
var
  logPath: string;
  appDataPath: string;
begin
  appDataPath := SysUtils.GetEnvironmentVariable('APPDATA');
  logPath := TPath.Combine(appDataPath, 'FormDesigner.Package');
  ForceDirectories(logPath);
  Result := TPath.Combine(logPath, 'log.txt');
end;

procedure Log(msg: string);
begin
  {$IFDEF DEBUG}
  TFile.AppendAllText(GetLogFilePath, Format('%s %d %s', [FormatDateTime('c', Now),
    GetCurrentThreadId, msg + sLineBreak]));
  {$ENDIF}
end;

procedure Log(const source, msg: string; const Rect: TRect); overload;
begin
  Log(source, msg + ' (%d, %d, %d, %d)', [Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]);
end;

procedure Log(const source: string; const message: string);
begin
  Log(source + ' ' + message);
end;

procedure Log(const Format: string; const Args: array of const);
var
  msg: string;
begin
  msg := System.SysUtils.Format(Format, Args);
  Log(msg);
end;

procedure Log(const source: string; const Format: string; const Args: array of const);
var
  msg: string;
begin
  msg := source + ' ' + System.SysUtils.Format(Format, Args);
  Log(msg);
end;

procedure SetControlText(Control: TControl; Text: String);
begin
  Control.Perform(WM_SETTEXT, NativeInt(0), NativeInt(PChar(Text)));
end;

function IsMessageForWindow(MsgHandle: HWnd; WindowHandle: HWnd) : Boolean;
var
  Parent: HWnd;
begin
  Result := False;
  if MsgHandle = WindowHandle then
  begin
    Result := True;
  end
  else
  begin
    Parent := GetAncestor(MsgHandle, GA_ROOT);
    Result := (Parent <> 0) and (Parent = WindowHandle);
  end;
end;

end.
