(*
  (c) Vitaly Smirnov (VSdev)
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSPoints;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
//{$I-}
//{$B-}

interface

const
  PiDiv180 = Pi/180;

  RectSizeAdd = 1; // (1 or 0) Do we need this? Move it to implementation?

type
  TDistance = Extended;

  TAngle = Extended;

  TIntCoord = LongInt;//Integer;

  TFloatCoord = Extended;//Double

  generic TPoint<_TypeName> = record
  public
    X, Y: _TypeName;

    class function Zero: TPoint; static; inline;
    class function OneOne: TPoint; static; inline;
    class function FromCoords(AX, AY: _TypeName): TPoint; static; inline;

    class operator = (constref ALeft, ARight: TPoint): Boolean;
    class operator <> (constref ALeft, ARight: TPoint): Boolean;
    class operator + (constref ALeft, ARight: TPoint): TPoint;
    //class operator - (constref ALeft, ARight: TPoint): TPoint;

    procedure Setup(AX, AY: _TypeName);
    procedure SetZero;
    function IsZero: Boolean;
    function IsXInRange(ALeft, ARight: TFloatCoord): Boolean;
    function IsYInRange(ATop, ABottom: TFloatCoord): Boolean;
    function IsItInRectangle(ALeft, ATop, ARight, ABottom: TFloatCoord):
      Boolean;
    //function IsItInCircle(X0, Y0, ARadius: TFloatCoord): Boolean;
    //function IsItInCircle(ACenter: TFloatPoint; ARadius: TFloatCoord): Boolean;
    function Shifted(ADeltaX, ADeltaY: _TypeName): TPoint;
    function Shifted(constref ADeltaPosition: TPoint): TPoint;
    procedure Shift(ADeltaX, ADeltaY: _TypeName);
    procedure Shift(constref ADeltaPosition: TPoint);
    function Rotated(constref ACenter: TPoint; AAngle: TAngle): TPoint;
    function Rotate(constref ACenter: TPoint; AAngle: TAngle): TPoint;
    function DistanceTo(AX, AY: TFloatCoord): TDistance;
    function DistanceTo(constref APoint: TPoint): TDistance;

    function ToString: AnsiString;
  end;

  TIntPoint = specialize TPoint<TIntCoord>;
  TFloatPoint = specialize TPoint<TFloatCoord>;

  TIntPointHelper = type helper for TIntPoint
  public
    function ToString(Format1: Byte): AnsiString; overload;
  end;

  TFloatPointHelper = type helper for TFloatPoint
  public
    function GetRoundX: TIntCoord;
    function GetRoundY: TIntCoord;
    function GetTruncX: TIntCoord;
    function GetTruncY: TIntCoord;

    // Should they be Getters (and properties)?
    function Rounded: TIntPoint;
    function Round: TIntPoint;
    function Truncated: TIntPoint;
    function Trunc: TIntPoint;
    function Assigned: TIntPoint;

    function ToString(Format1, Format2: Byte): AnsiString; overload;

    property RoundX: TIntCoord read GetRoundX;
    property RoundY: TIntCoord read GetRoundY;
    property TruncX: TIntCoord read GetTruncX;
    property TruncY: TIntCoord read GetTruncY;
  end;

// These ones should be checked!
operator := (constref ARight: TIntPoint): TFloatPoint;
// It's trunceted! Or not.. Check it (if you want to be shure)!
operator := (constref ARight: TFloatPoint): TIntPoint;
operator + (constref ALeft: TFloatPoint;
  constref ARight: TIntPoint): TFloatPoint;
operator + (constref ALeft: TIntPoint;
  constref ARight: TFloatPoint): TFloatPoint;
{operator + (const AIntPoint1, AIntPoint2: TIntPoint): TFloatPoint;// It isn't working..
operator + (const AFloatPoint1, AFloatPoint2: TFloatPoint): TIntPoint;
operator + (const AFloatPoint1: TFloatPoint;
  const AIntPoint2: TIntPoint): TIntPoint;
operator + (const AIntPoint1: TIntPoint;
  const AFloatPoint2: TFloatPoint): TIntPoint;}
operator = (constref ALeft: TIntPoint; constref ARight: TFloatPoint): Boolean;
operator = (constref ALeft: TFloatPoint; constref ARight: TIntPoint): Boolean;
operator <> (constref ALeft: TIntPoint; constref ARight: TFloatPoint): Boolean;
operator <> (constref ALeft: TFloatPoint; constref ARight: TIntPoint): Boolean;
//*)

type
  TIntPoints = array of TIntPoint;
  TFloatPoints = array of TFloatPoint;

  PIntPoint = ^TIntPoint;
  PFloatPoint = ^TFloatPoint;
  PIntPoints = ^TIntPoints;
  PFloatPoints = ^TFloatPoints;


type
  TSize = LongWord;

  (*TSize2D = record
    Width: TSize;
    Height: TSize;
  end;

  TRectangle1 = record
    TopLeft: TIntPoint;
    BottomRight: TIntPoint;
  end;

  TRectangle2 = record
    Position: TIntPoint;
    Width: TSize;
    Height: TSize;
  end;

  TRectangle3Variations = 1..3;

  TRectangle3 = record
    //procedure GetWidth: Byte;
    case {RV:} TRectangle3Variations of
      1: (Position: TIntPoint; Width, Height: Byte);
      2: (TopLeft, BottomRight: TIntPoint);
      3: (Top, Left, Bottom, Right: TIntCoord{; R2: TRectangle2});
  end;*)

  TRect = record
  public
    Top: TIntCoord;
    Left: TIntCoord;
    Bottom: TIntCoord;
    Right: TIntCoord;

    function GetTopLeft: TIntPoint;
    procedure SetTopLeft(const AValue: TIntPoint);
    function GetBottomRight: TIntPoint;
    procedure SetBottomRight(const AValue: TIntPoint);
    function GetWidth: TSize;
    procedure SetWidth(AValue: TSize);
    function GetHeight: TSize;
    procedure SetHeight(AValue: TSize);

    procedure Setup(const ALeft, ATop, ARight, ABottom: TIntCoord);
    procedure Setup(const ATopLeft, ABottomRight: TIntPoint);
    procedure Setup(const ATopLeft: TIntPoint; const AWidth, AHeight: TSize);

    property TopLeft: TIntPoint read GetTopLeft write SetTopLeft;
    property BottomRight: TIntPoint read GetBottomRight write SetBottomRight;

    property Position: TIntPoint read GetTopLeft write SetTopLeft;
    property Width: TSize read GetWidth write SetWidth;
    property Height: TSize read GetHeight write SetHeight;
  end;


function IntCoordsToPoint(AX, AY: TIntCoord): TIntPoint;
function FloatCoordsToPoint(AX, AY: TFloatCoord): TFloatPoint;
function RotatePoint(constref APoint, ARotationPoint: TFloatPoint;
  AAngle: TAngle): TFloatPoint;
function LineDistance(constref APoint1, APoint2: TFloatPoint): TDistance;
function LineDistance(X1, Y1, X2, Y2: TFloatCoord): TDistance;
function IsPointInCircle(AX, AY, ACenterX, ACenterY, ARadius: Integer): Boolean;
function IsPointInCircle(constref APoint: TIntPoint;
  constref ACenter: TIntPoint; ARadius: Integer): Boolean;
function IsPointInCircle(AX, AY, ACenterX, ACenterY, ARadius: Extended): Boolean;
function IsPointInCircle(constref APoint: TFloatPoint;
  constref ACenter: TFloatPoint; ARadius: Extended): Boolean;

procedure DebugPrint(constref AMsg: AnsiString);


implementation

procedure DebugPrint(constref AMsg: AnsiString);
begin
  //WriteLn(AMsg);
end;


// generic TPoint<_TypeName> = record

class function TPoint.Zero: TPoint;
begin
  Result.Setup(0, 0);
end;

class function TPoint.OneOne: TPoint;
begin
  Result.Setup(1, 1);
end;

class function TPoint.FromCoords(AX, AY: _TypeName): TPoint;
begin
  Result.Setup(AX, AY);
end;

class operator TPoint.= (constref ALeft, ARight: TPoint): Boolean;
begin
  Result:=(ALeft.X = ARight.X) and (ALeft.Y = ARight.Y);
end;

class operator TPoint.<> (constref ALeft, ARight: TPoint): Boolean;
begin
  Result:=(ALeft.X <> ARight.X) or (ALeft.Y <> ARight.Y);
end;

class operator TPoint.+ (constref ALeft, ARight: TPoint): TPoint;
begin
  Result.Setup(ALeft.X+ARight.X, ALeft.Y+ARight.Y);
  DebugPrint('TPoint + TPoint');
end;

procedure TPoint.Setup(AX, AY: _TypeName);
begin
  X:=AX;
  Y:=AY;
end;

procedure TPoint.SetZero;
begin
  Setup(0, 0);
end;

function TPoint.IsZero: Boolean;
begin
  Result:=Self = Zero;
end;

function TPoint.IsXInRange(ALeft, ARight: TFloatCoord): Boolean;
begin
  Result:=(X >= ALeft) and (X <= ARight);
end;

function TPoint.IsYInRange(ATop, ABottom: TFloatCoord): Boolean;
begin
  Result:=(Y >= ATop) and (Y <= ABottom);
end;

function TPoint.IsItInRectangle(ALeft, ATop, ARight, ABottom: TFloatCoord):
  Boolean;
begin
  Result:=IsXInRange(ALeft, ARight) and IsYInRange(ATop, ABottom);
end;

//function TPoint.IsItInCircle(X0, Y0, ARadius: TFloatCoord): Boolean;
//function TPoint.IsItInCircle(ACenter: TFloatPoint; ARadius: TFloatCoord): Boolean;

function TPoint.Shifted(ADeltaX, ADeltaY: _TypeName): TPoint;
begin
  Result.Setup(X+ADeltaX, Y+ADeltaY);
end;

function TPoint.Shifted(constref ADeltaPosition: TPoint): TPoint;
begin
  //Result:=Self+ADeltaPosition;
  Result:=Shifted(ADeltaPosition.X, ADeltaPosition.Y);
end;

procedure TPoint.Shift(ADeltaX, ADeltaY: _TypeName);
begin
  Setup(X+ADeltaX, Y+ADeltaY);
end;

procedure TPoint.Shift(constref ADeltaPosition: TPoint);
begin
  Shift(ADeltaPosition.X, ADeltaPosition.Y);
end;

function TPoint.Rotated(constref ACenter: TPoint; AAngle: TAngle): TPoint;
begin
  Result:=RotatePoint(Self, ACenter, AAngle);
  {function RotatePoint(constref APoint, ARotationPoint: TFloatPoint;
    AAngle: TAngle): TFloatPoint;}
end;

function TPoint.Rotate(constref ACenter: TPoint; AAngle: TAngle): TPoint;
begin
  Self:=Rotated(ACenter, AAngle);
  Result:=Self;
end;

function TPoint.DistanceTo(AX, AY: TFloatCoord): TDistance;
begin
  Result:=LineDistance(X, Y, AX, AY);
end;

function TPoint.DistanceTo(constref APoint: TPoint): TDistance;
begin
  Result:=LineDistance(X, Y, APoint.X, APoint.Y);
end;

function TPoint.ToString: AnsiString;
var
  LX, LY: AnsiString;
begin
  Str(X, LX);
  Str(Y, LY);
  Result:='('+LX+', '+LY+')';
  //Result:='(X = '+LX+', Y = '+LY+')';
end;


// TIntPointHelper = type helper for TIntPoint

function TIntPointHelper.ToString(Format1: Byte): AnsiString;
var
  LX, LY: AnsiString;
begin
  Str(X:Format1, LX);
  Str(Y:Format1, LY);
  Result:='('+LX+', '+LY+')';
  //Result:='(X = '+LX+', Y = '+LY+')';
end;


// TFloatPointHelper = type helper for TFloatPoint

function TFloatPointHelper.GetRoundX: TIntCoord;
begin
  Result:=System.Round(X);
end;

function TFloatPointHelper.GetRoundY: TIntCoord;
begin
  Result:=System.Round(Y);
end;

function VSTrunc(AValue: Extended): LongInt;
begin
  Result:={System.}Trunc(AValue);
  if AValue < 0 then
    Result-=1;
end;

function TFloatPointHelper.GetTruncX: TIntCoord;
begin
  Result:=VSTrunc(X);
end;

function TFloatPointHelper.GetTruncY: TIntCoord;
begin
  Result:=VSTrunc(Y);
end;

function TFloatPointHelper.Rounded: TIntPoint;
begin
  Result.Setup(RoundX, RoundY);
end;

function TFloatPointHelper.Round: TIntPoint;
begin
  Self:=Rounded;
  Result:=Self;
end;

function TFloatPointHelper.Truncated: TIntPoint;
begin
  Result.Setup(TruncX, TruncY);
end;

function TFloatPointHelper.Trunc: TIntPoint;
begin
  Self:=Truncated;
  Result:=Self;
end;

function TFloatPointHelper.Assigned: TIntPoint;
begin
  Result:=Self;
end;

function TFloatPointHelper.ToString(Format1, Format2: Byte): AnsiString;
var
  LX, LY: AnsiString;
begin
  Str(X:Format1:Format2, LX);
  Str(Y:Format1:Format2, LY);
  Result:='('+LX+', '+LY+')';
  //Result:='(X = '+LX+', Y = '+LY+')';
end;


operator := (constref ARight: TIntPoint): TFloatPoint;
begin
  Result.Setup(ARight.X, ARight.Y);
  DebugPrint('f := i');
end;

operator := (constref ARight: TFloatPoint): TIntPoint;
begin
  Result.Setup(ARight.{TruncX}RoundX, ARight.{TruncY}RoundY);
  DebugPrint('i := f');
end;

operator + (constref ALeft: TFloatPoint;
  constref ARight: TIntPoint): TFloatPoint;
begin
  Result.Setup(ALeft.X+ARight.X, ALeft.Y+ARight.Y);
  DebugPrint('f + i = f');
end;

operator + (constref ALeft: TIntPoint;
  constref ARight: TFloatPoint): TFloatPoint;
begin
  Result.Setup(ALeft.X+ARight.X, ALeft.Y+ARight.Y);
  DebugPrint('i + f = f');
end;

{operator + (const AIntPoint1, AIntPoint2: TIntPoint): TFloatPoint;// It isn't working..
operator + (const AFloatPoint1, AFloatPoint2: TFloatPoint): TIntPoint;
operator + (const AFloatPoint1: TFloatPoint;
  const AIntPoint2: TIntPoint): TIntPoint;
operator + (const AIntPoint1: TIntPoint;
  const AFloatPoint2: TFloatPoint): TIntPoint;}

operator = (constref ALeft: TIntPoint; constref ARight: TFloatPoint): Boolean;
begin
  Result:=ALeft = ARight.Assigned;
end;

operator = (constref ALeft: TFloatPoint; constref ARight: TIntPoint): Boolean;
begin
  Result:=ALeft.Assigned = ARight;
end;

operator <> (constref ALeft: TIntPoint; constref ARight: TFloatPoint): Boolean;
begin
  Result:=ALeft <> ARight.Assigned;
end;

operator <> (constref ALeft: TFloatPoint; constref ARight: TIntPoint): Boolean;
begin
  Result:=ALeft.Assigned <> ARight;
end;


// Global functions

function IntCoordsToPoint(AX, AY: TIntCoord): TIntPoint;
begin
  Result.Setup(AX, AY);
end;

function FloatCoordsToPoint(AX, AY: TFloatCoord): TFloatPoint;
begin
  Result.Setup(AX, AY);
end;

function RotatePoint(constref APoint, ARotationPoint: TFloatPoint;
  AAngle: TAngle): TFloatPoint;
var
  SinAng, CosAng, DeltaX, DeltaY: Extended;
begin
  SinAng:=Sin(AAngle*PiDiv180);
  CosAng:=Cos(AAngle*PiDiv180);
  DeltaX:=APoint.X-ARotationPoint.X;
  DeltaY:=APoint.Y-ARotationPoint.Y;
  Result.X:=ARotationPoint.X+DeltaX*CosAng-DeltaY*SinAng;
  Result.Y:=ARotationPoint.Y+DeltaX*SinAng+DeltaY*CosAng;
end;

function LineDistance(constref APoint1, APoint2: TFloatPoint): TDistance;
begin
  Result:=LineDistance(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y);
end;

function LineDistance(X1, Y1, X2, Y2: TFloatCoord): TDistance;
begin
  Result:=Sqrt(Sqr(X2-X1)+Sqr(Y2-Y1));
end;

function IsPointInCircle(AX, AY, ACenterX, ACenterY, ARadius: Integer): Boolean;
begin
  Result:=Sqrt(Sqr(AX-ACenterX)+Sqr(AY-ACenterY)) <= Abs(ARadius); //Abs??
end;

function IsPointInCircle(constref APoint: TIntPoint;
  constref ACenter: TIntPoint; ARadius: Integer): Boolean;
begin
  Result:=IsPointInCircle(APoint.X, APoint.Y, ACenter.X, ACenter.Y, ARadius);
end;

function IsPointInCircle(AX, AY, ACenterX, ACenterY, ARadius: Extended): Boolean;
begin
  Result:=Sqrt(Sqr(AX-ACenterX)+Sqr(AY-ACenterY)) <= ARadius; //Abs??
end;

function IsPointInCircle(constref APoint: TFloatPoint;
  constref ACenter: TFloatPoint; ARadius: Extended): Boolean;
begin
  Result:=IsPointInCircle(APoint.X, APoint.Y, ACenter.X, ACenter.Y, ARadius);
end;


// TRect

procedure TRect.Setup(const ALeft, ATop, ARight, ABottom: TIntCoord);
begin
  Left:=ALeft;
  Top:=ATop;
  Right:=ARight;
  Bottom:=ABottom;
end;

procedure TRect.Setup(const ATopLeft, ABottomRight: TIntPoint);
begin
  Setup(ATopLeft.X, ATopLeft.Y, ABottomRight.X, ABottomRight.Y);
end;

procedure TRect.Setup(const ATopLeft: TIntPoint; const AWidth, AHeight: TSize);
begin
  Setup(ATopLeft, ATopLeft.Shifted(AWidth-RectSizeAdd, AHeight-RectSizeAdd));
end;

function TRect.GetTopLeft: TIntPoint;
begin
  Result:=IntCoordsToPoint(Top, Left);
end;

procedure TRect.SetTopLeft(const AValue: TIntPoint);
begin
  Top:=AValue.Y;
  Left:=AValue.X;
end;

function TRect.GetBottomRight: TIntPoint;
begin
  Result:=IntCoordsToPoint(Bottom, Right);
end;

procedure TRect.SetBottomRight(const AValue: TIntPoint);
begin
  Bottom:=AValue.Y;
  Right:=AValue.X;
end;

//function GetPosition: TIntPoint;

function TRect.GetWidth: TSize;
begin
  Result:=Right-Left+RectSizeAdd;
end;

procedure TRect.SetWidth(AValue: TSize);
begin
  Right:=Left+AValue-RectSizeAdd;
end;

function TRect.GetHeight: TSize;
begin
  Result:=Bottom-Top+RectSizeAdd;
end;

procedure TRect.SetHeight(AValue: TSize);
begin
  Bottom:=Top+AValue-RectSizeAdd;
end;


end.
