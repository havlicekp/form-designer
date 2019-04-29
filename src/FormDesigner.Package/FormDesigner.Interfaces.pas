unit FormDesigner.Interfaces;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections;

type

  TFormDesignerState = (fdsReady, fdsMoving, fdsSizing);
  TDirection = (dBottom, dTop, dLeft, dRight);
  TDirections = set of TDirection;

  IFormDesigner = interface
    function GetDragRect : TRect;
    function GetChildRect : TRect;
    procedure UpdateDragRect(Rect: TRect; Direction: TDirections);
  end;

implementation

end.
