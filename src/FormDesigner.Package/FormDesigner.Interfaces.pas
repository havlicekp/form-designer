unit FormDesigner.Interfaces;

interface

uses Classes, Controls, Graphics, Windows, Messages, Forms, SysUtils, StdCtrls,
  System.Generics.Collections;

type

  TFormDesignerState = (ssReady, ssMoving, ssSizing);
  TDirection = (dBottom, dTop, dLeft, dRight, dUpLeft, dUpRight, dDownLeft,
    dDownRight, dPosChange);
  TDirections = set of TDirection;

  IFormDesigner = interface
    function GetRect : TRect;
    function GetChildRect : TRect;
    procedure UpdateRect(Rect: TRect; Direction: TDirections);
  end;

implementation

end.
