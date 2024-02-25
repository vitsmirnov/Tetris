(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSEngineComponents;

{$mode objfpc}
//{$modeswitch advancedrecords}
{$modeswitch typehelpers}
//{$J-}
//{$B-}
//{$I-}

interface

uses
  VSPoints;

type
  TElement = class;
  TFigure = class;
  TGameField = class;

  TVSEngineObject = TObject;

  TTime = LongInt;//LongWord;

  {TElementCore = class(TVSEngineObject)
  private
    FPosition: TFloatPoint;
  end;
  //TElements = array ...;
  TFigureCore = class(TVSEngineObject)
  private
    FElements: array of TElementCore;
    FRotationPoint: TFloatPoint;
  end;}

  TElement = class(TVSEngineObject)
  private
    FCount: Integer; static;
  private
    FOwner: TFigure;
    FPosition: TFloatPoint;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(AOwner: TFigure = nil; AX: TFloatCoord = 0;
      AY: TFloatCoord = 0);
    destructor Destroy; override;

    function Setup(AOwner: TFigure = nil; AX: TFloatCoord = 0;
      AY: TFloatCoord = 0): TElement;
    function Setup(AOwner: TFigure; APosition: TFloatPoint): TElement;

    function GetCopy(AOwner: TFigure): TElement; virtual;

    procedure Update(ADeltaTime: TTime); virtual; //abstract;

    property Owner: TFigure read FOwner write FOwner;
    property Position: TFloatPoint read FPosition write FPosition;
    property X: TFloatCoord read FPosition.X write FPosition.X;
    property Y: TFloatCoord read FPosition.Y write FPosition.Y;
    //property RoundX: TIntCoord read FPosition.RoundX write FPosition.RoundX;
    //property RoundY: TIntCoord read FPosition.RoundY write FPosition.RoundY;
  end;

  TElements = array of TElement;

  // TFigureArr/TFigureList/TControl/TContainer??
  TFigure = class(TVSEngineObject)
  private
    FGameField: TGameField;
    FElements: TElements;
    FRotationPoint: TFloatPoint;
    //FMovePoint: TFloatPoint;
    FSpeed: Extended;// Char per second

    function GetElement(Index: Integer): TElement;
    function GetElementsNumber: TSize;
  public
    constructor Create(AGameField: TGameField);
    destructor Destroy; override;

    procedure Update(const ADeltaTime: {LongInt}TTime; var HasLanded:
      Boolean); virtual;

    {procedure Setup(constref AElements: TElements; constref ARotationPoint:
      TIntPoint; ASpeed: Extended; AGameField: TGameField; PutOnGameField:
      Boolean = False);}
    procedure Setup(AFigure: TFigure; PutOnGameField: Boolean); //virtual;

    function AddElement(AElement: TElement): TElement;
    procedure AddElements(constref AElements: TElements);
    procedure DestroyElements;

    //function LoadFromFile(var AFile: TextFile): Boolean; virtual;

    function PutAwayFromField: Boolean;
    procedure LeaveElementsOnFieldOrDestroy;
    function DoesElementExist(AIndex: Integer): Boolean;

    // These ones return true if the move/shift completed successfully
    function Move(ADeltaX, ADeltaY: TFloatCoord): Boolean; virtual;
    function Move(ADeltaPosition: TFloatPoint): Boolean;
    function Shift(ADeltaX, ADeltaY: TFloatCoord; AMaxMoveStep:
      Extended = 1): Boolean;
    function Shift(ADeltaPosition: TFloatPoint; AMaxMoveStep:
      Extended = 1): Boolean;
    function MoveTo(AX, AY: TFloatCoord;
      AMoveFromRotationPoint: Boolean = False): Boolean;
    function MoveTo(APoint: TFloatPoint;
      AMoveFromRotationPoint: Boolean = False): Boolean;
    function Rotate(AAngle: TAngle): Boolean; virtual;
  public
    function CanMoveElementTo(AX, AY: TIntCoord): Boolean;
    function CanMoveElementTo(APosition: TIntPoint): Boolean;
    function CanShiftFigureOn(ADeltaX, ADeltaY: TFloatCoord): Boolean;
    function CanShiftFigureOn(ADeltaPosition: TFloatPoint): Boolean;

    function MinX: TFloatCoord;
    function MinY: TFloatCoord;
    function MaxX: TFloatCoord;
    function MaxY: TFloatCoord;
    function Center: TFloatPoint;

    procedure Refresh; virtual;
  public
    property GameField: TGameField read FGameField{;//} write FGameField;
    property Elements[Index: Integer]: TElement read GetElement; default;
    property RotationPoint: TFloatPoint read FRotationPoint write FRotationPoint;
    property Speed: Extended read FSpeed write FSpeed;// default 3.5;//nil;
    property ElementsNumber: TSize read GetElementsNumber;
  end;

  TFigures = array of TFigure;

  TFiguresHelper = type helper for TFigures
    procedure Free;
    //function LoadFromFile(var AFile: TextFile): Boolean;
    //function LoadFromFile(constref AFileName: AnsiString): Boolean;
  end;

  TGameFieldElements = array of array of TElement;

  TChangeElementEvent = procedure (AX, AY: TFloatCoord{TIntCoord};
    AElement: TElement) of object;

  TGameField = class(TVSEngineObject)
  public const
    DefaultWidth = 10;
    DefaultHeight = 25;
  private
    FElements: TGameFieldElements;
    FOnChangeElement: TChangeElementEvent;

    function GetElement(AX, AY: TIntCoord): TElement;
    procedure SetElement(AX, AY: TIntCoord; AElement: TElement);
    function GetElement(APosition: TIntPoint): TElement;
    procedure SetElement(APosition: TIntPoint; AElement: TElement);
    function GetWidth: Word;
    procedure SetWidth(AValue: Word);
    function GetHeight: Word;
    procedure SetHeight(AValue: Word);
  public
    constructor Create(AWidth: Word = DefaultWidth; AHeight: Word =
      DefaultHeight);
    destructor Destroy; override;

    procedure Setup(AWidth, AHeight: Word);

    procedure DestroyElements;
    procedure ResetElements;
    function DoesElementExist(AX, AY: TIntCoord): Boolean;
    //function IsPointInBorders(AX, AY: TIntCoord): Boolean;// Same as DoesElementExist

    //function DestroyElement(AX, AY: TIntCoord): Boolean;
    //function PutFigure(AFigure: TFigure): Boolean;

    property Elements[AX, AY: TIntCoord]: TElement read
      GetElement write SetElement; default;
    property ElementsOnPos[APosition: TIntPoint]: TElement read
      GetElement write SetElement; //default;
    property Width: Word read GetWidth write SetWidth;
    property Height: Word read GetHeight write SetHeight;
    property OnChangeElement: TChangeElementEvent read FOnChangeElement write
      FOnChangeElement;// default nil;
  end;


//procedure AddLog(constref AMessage: AnsiString);

var
  DebugMode: Boolean = False;//True;

const
  LogFileName{: AnsiString} = '..\resource\'+'Log.txt';


implementation

uses
  SysUtils, //Dialogs,
  //VSCRTadd, VSStrings,//; // Temp! Only for debug!
  {VSStrings,} VSUtils;

procedure AddLog(constref AMessage: AnsiString);
begin
  LogMessage(LogFileName, AMessage);
end;


// TElement = class(TVSEngineObject)

class constructor TElement.Create;
begin
  FCount:=0;
end;

class destructor TElement.Destroy;
begin
  if FCount <> 0 then
  begin
    AddLog('TElement.Destroy(class): Count = '+IntToStr(FCount)+'!');
    if DebugMode then
    begin
      WriteLn;
      WriteLn('Error in TElement.Destroy(class): Count = '+
        IntToStr(FCount)+'!');
      ReadLn;
    end;
  end;
end;

constructor TElement.Create(AOwner: TFigure = nil;
  AX: TFloatCoord = 0; AY: TFloatCoord = 0);
begin
  inherited Create;
  FCount:=FCount+1;
  Setup(AOwner, AX, AY);
end;

destructor TElement.Destroy;
begin
  FCount:=FCount-1;
  inherited Destroy;
end;

function TElement.Setup(AOwner: TFigure = nil;
  AX: TFloatCoord = 0; AY: TFloatCoord = 0): TElement;
begin
  FOwner:=AOwner;
  FPosition.X:=AX;
  FPosition.Y:=AY;
  Result:=Self;//??
end;

function TElement.Setup(AOwner: TFigure; APosition: TFloatPoint): TElement;
begin
  Result:=Setup(AOwner, APosition.X, APosition.Y);
end;

function TElement.GetCopy(AOwner: TFigure): TElement;
begin
  Result:=TElement.Create(AOwner, FPosition.X, FPosition.Y);
  //TObject(Result):=Self.ClassType.Create;
end;

procedure TElement.Update(ADeltaTime: TTime);
begin
end;


// TFigure = class(TVSEngineObject);

constructor TFigure.Create(AGameField: TGameField);
begin
  inherited Create;
  FGameField:=AGameField;
  FRotationPoint.SetZero;
  SetLength(FElements, 0);
  FSpeed:=0;//TTetrisGameEngine.DefaultSpeed; //??
end;

destructor TFigure.Destroy;
begin
  // This should be checked!!
  // We don't need to destroy elements, game field will do it.
  if FGameField = nil then
    DestroyElements
  else
    SetLength(FElements, 0);
  inherited Destroy;
end;

function TFigure.GetElement(Index: Integer): TElement;
begin
  if DoesElementExist(Index) then
    Result:=FElements[Index]
  else
    Result:=nil;
end;

function TFigure.GetElementsNumber: TSize;
begin
  Result:=Length(FElements);
end;

procedure TFigure.Setup(AFigure: TFigure; PutOnGameField: Boolean);
var
  i: Integer;
begin
  if AFigure = nil then
    Exit; //??

  PutAwayFromField;
  DestroyElements;

  SetLength(FElements, Length(AFigure.FElements));
  for i:=Low(FElements) to High(FElements) do
  begin
    //FElements[i]:=TElementClass(AFigure.FElements[i].ClassType).Create(nil, 0, 0);
    //TObject(FElements[i]):=AFigure.FElements[i].ClassType.Create;
    //FElements[i].Setup(Self, AFigure.FElements[i].X, AFigure.FElements[i].Y);

    FElements[i]:=AFigure.FElements[i].GetCopy(Self);
    if PutOnGameField and (FGameField <> nil) then
      FGameField.ElementsOnPos[FElements[i].Position]:=FElements[i];
  end;
  //FGameField:=AFigure.FGameField; //!!
  FRotationPoint:=AFigure.FRotationPoint;
  FSpeed:=AFigure.FSpeed;// ?
end;

{function TFigure.CurrentDistanceToMove(const ADeltaTime: LongInt): Extended;
begin
  Result:=FSpeed*(ADeltaTime/OneSecond));
end;}

procedure TFigure.Update(const ADeltaTime: LongInt; var HasLanded: Boolean);
const
  OneSecond = 1000;
begin
  // Move it!
  HasLanded:=not Shift(0, -FSpeed*(ADeltaTime/OneSecond), 1);
end;

function TFigure.AddElement(AElement: TElement): TElement;
begin
  SetLength(FElements, Length(FElements)+1);
  FElements[High(FElements)]:=AElement;
  Result:=AElement;//??
end;

procedure TFigure.AddElements(constref AElements: TElements);
var
  i, n: Integer;
begin
  if Length(AElements) = 0 then
    Exit;
  n:=Length(FElements);
  SetLength(FElements, n+Length(AElements));
  for i:=n to High(FElements) do
    FElements[i]:=AElements[i-n];
end;

procedure TFigure.DestroyElements;
var
  i: Integer;
begin
  if Length(FElements) = 0 then
    Exit;
  //PutAwayFromField;
  //ShowMessage(IntToStr(Low(FElements))+', '+IntToStr(High(FElements)));
  for i:=Low(FElements) to High(FElements) do
  begin
    if (FGameField <> nil) and (FGameField.ElementsOnPos[
      FElements[i].Position]=FElements[i]) then
      FGameField.ElementsOnPos[FElements[i].Position]:=nil;
    FElements[i].Free;
    //FElements[i]:=nil;
  end;
  SetLength(FElements, 0);
end;

function TFigure.PutAwayFromField: Boolean;
var
  i: Integer;
begin
  Result:=FGameField <> nil;
  if Result then
    for i:=0 to High(FElements) do
      if FGameField.ElementsOnPos[FElements[i].Position]=FElements[i] then
        FGameField.ElementsOnPos[FElements[i].Position]:=nil;
end;

procedure TFigure.LeaveElementsOnFieldOrDestroy;
var
  i: Integer;
begin
  for i:=0 to High(FElements) do
  begin
    if (FGameField<>nil) and (FGameField.ElementsOnPos[FElements[i].Position]=
      FElements[i]) then
    begin
      FElements[i].Owner:=nil;
      FElements[i]:=nil;
    end
    else
      FElements[i].Free;
  end;
  SetLength(FElements, 0);
end;

function TFigure.DoesElementExist(AIndex: Integer): Boolean;
begin
  Result:=IsIntInRange(AIndex, 0, High(FElements));
end;

function TFigure.CanMoveElementTo(AX, AY: TIntCoord): Boolean;
begin
  Result:=(FGameField = nil) or (FGameField.DoesElementExist(AX, AY) and
    ((FGameField[AX, AY] = nil) or (FGameField[AX, AY].Owner = Self)));
end;

function TFigure.CanMoveElementTo(APosition: TIntPoint): Boolean;
begin
  Result:=CanMoveElementTo(APosition.X, APosition.Y);
end;

function TFigure.Move(ADeltaX, ADeltaY: TFloatCoord): Boolean;
var
  i: Integer;
begin
  if Length(FElements)=0 then
    Exit({False}True);//???

  for i:=Low(FElements) to High(FElements) do
    if not CanMoveElementTo(FElements[i].Position.
      Shifted(ADeltaX, ADeltaY){.Trunced}) then
      Exit(False);

  for i:=0 to High(FElements) do
    with FElements[i] do
    begin
      if (FGameField<>nil) and
         //(FGameField.ElementsOnPos[Position]=FElements[i]) and
         (FGameField.ElementsOnPos[Position]<>nil) and
         (FGameField.ElementsOnPos[Position].Owner=Self) then
        FGameField.ElementsOnPos[Position]:=nil;
      Position.Shift(ADeltaX, ADeltaY);
      //FGameField.ElementsOnPos[Position]:=FElements[i];
    end;

  if FGameField<>nil then
    for i:=0 to High(FElements) do
      FGameField.ElementsOnPos[FElements[i].Position]:=FElements[i];

  FRotationPoint.Shift(ADeltaX, ADeltaY);

  Result:=True;
end;

function TFigure.Move(ADeltaPosition: TFloatPoint): Boolean;
begin
  Result:=Move(ADeltaPosition.X, ADeltaPosition.Y);
end;

function TFigure.Shift(ADeltaX, ADeltaY: TFloatCoord; AMaxMoveStep:
  Extended = 1): Boolean;
var
  TempPoint: TFloatPoint;
  L: Extended;
begin
  if Length(FElements)=0 then
    Exit({False}True);//???

  AMaxMoveStep:=Abs(AMaxMoveStep);// ???!!! It should be positive

  TempPoint.SetZero;
  if (Abs(ADeltaX) > AMaxMoveStep) or (Abs(ADeltaY) > AMaxMoveStep) then
  begin
    TempPoint.Setup(ADeltaX, ADeltaY);

    if Abs(ADeltaX) > Abs(ADeltaY) then
      L:=Abs(ADeltaX)
    else
      L:=Abs(ADeltaY);
    if L<>0 then
    begin
      ADeltaX:=(ADeltaX/L)*AMaxMoveStep;
      ADeltaY:=(ADeltaY/L)*AMaxMoveStep;
    end
    else
    begin
      ADeltaX:=0;
      ADeltaY:=0;
    end;

    TempPoint.Shift(-ADeltaX, -ADeltaY);
  end;

  Result:=Move(ADeltaX, ADeltaY);

  if not TempPoint.IsZero and Result then
    Result:=Shift(TempPoint, AMaxMoveStep);
end;

function TFigure.Shift(ADeltaPosition: TFloatPoint; AMaxMoveStep:
  Extended = 1): Boolean;
begin
  Result:=Shift(ADeltaPosition.X, ADeltaPosition.Y, AMaxMoveStep);
end;

function TFigure.MoveTo(AX, AY: TFloatCoord;
  AMoveFromRotationPoint: Boolean = False): Boolean;
begin
  if AMoveFromRotationPoint then
    Result:=Move(AX-FRotationPoint.X, AY-FRotationPoint.Y)
  else
    Result:=Move(AX-MinX, AY-MinY);//Move(AX-FRotationPoint.X, AY-FRotationPoint.Y);
end;

function TFigure.MoveTo(APoint: TFloatPoint;
  AMoveFromRotationPoint: Boolean = False): Boolean;
begin
  Result:=MoveTo(APoint.X, APoint.Y, AMoveFromRotationPoint);
end;

//function TFugure.Rotate(AAngle: TAngle): Boolean;
function TFigure.Rotate(AAngle: TAngle): Boolean;
var
  i: Integer;
begin
  if Length(FElements) = 0 then
    Exit(True);

  for i:=0 to High(FElements) do
    if not CanMoveElementTo(FElements[i].Position.Rotated(
      FRotationPoint, AAngle){.Rounded}) then
      Exit(False);

  for i:=0 to High(FElements) do
  begin
    if (FGameField<>nil) and
       (FGameField.ElementsOnPos[FElements[i].Position]<>nil) and
       (FGameField.ElementsOnPos[FElements[i].Position].Owner=Self) then
      FGameField.ElementsOnPos[FElements[i].Position]:=nil;
    FElements[i].Position.Rotate(FRotationPoint, AAngle);
    {FElements[i].Position:=FElements[i].Position.Rotated(FRotationPoint,
      AAngle).Rounded;}
  end;

  if FGameField<>nil then
    for i:=0 to High(FElements) do
      FGameField.ElementsOnPos[FElements[i].Position]:=FElements[i];

  Result:=True;
  //??
  //FRotationPoint.Round;
end;

(*procedure TFigure.MoveLeft;
begin
  Shift(-MoveStep, 0);
end;

procedure TFigure.MoveRight;
begin
  Shift(MoveStep, 0);
end;

procedure TFigure.MoveUp;
begin
  Shift(0, MoveStep);
end;

procedure TFigure.MoveDown;
begin
  Shift(0, -MoveStep);
end;

procedure TFigure.RotateLeft;
begin
  Rotate(90);
end;

procedure TFigure.RotateRight;
begin
  Rotate(-90);
end;

procedure TFigure.DropDown;
var
  Distance: Extended = 100;//??
begin
  if FGameField <> nil then
    Distance:=MinY+1+1;//FGameField.Height;
  Shift(0, -Distance);
end;*)

function TFigure.CanShiftFigureOn(ADeltaX, ADeltaY: TFloatCoord): Boolean;
var
  i: Integer;
begin
  for i:=0 to High(FElements) do
    if not CanMoveElementTo(FElements[i].Position.Shifted(ADeltaX, ADeltaY)) then
      Exit(False);
  Result:=True;
end;

function TFigure.CanShiftFigureOn(ADeltaPosition: TFloatPoint): Boolean;
begin
  Result:=CanShiftFigureOn(ADeltaPosition.X, ADeltaPosition.Y);
end;

function TFigure.MinX: TFloatCoord;
var
  i: Integer;
begin
  if Length(FElements)=0 then Exit(0);
  Result:=FElements[0].Position.X;
  for i:=1 to High(FElements) do
    if FElements[i].Position.X<Result then
      Result:=FElements[i].Position.X;
end;

function TFigure.MinY: TFloatCoord;
var
  i: Integer;
begin
  if Length(FElements)=0 then Exit(0);
  Result:=FElements[0].Position.Y;
  for i:=1 to High(FElements) do
    if FElements[i].Position.Y<Result then
      Result:=FElements[i].Position.Y;
end;

function TFigure.MaxX: TFloatCoord;
var
  i: Integer;
begin
  if Length(FElements)=0 then Exit(0);
  Result:=FElements[0].Position.X;
  for i:=1 to High(FElements) do
    if FElements[i].Position.X>Result then
      Result:=FElements[i].Position.X;
end;

function TFigure.MaxY: TFloatCoord;
var
  i: Integer;
begin
  if Length(FElements)=0 then Exit(0);
  Result:=FElements[0].Position.Y;
  for i:=1 to High(FElements) do
    if FElements[i].Position.Y>Result then
      Result:=FElements[i].Position.Y;
end;

function TFigure.Center: TFloatPoint;
var
  i: Integer;
  ALeft, ATop, ARight, ABottom: TFloatCoord;
begin
  if Length(FElements) = 0 then
  begin
    Result.Setup(0, 0);
    Exit;
  end;
  with FElements[Low(FElements)] do
  begin
    ALeft:=X;//0;
    ATop:=Y;//0;
    ARight:=X;//0;
    ABottom:=Y;//0;
  end;
  for i:=Low(FElements)+1 to High(FElements) do
    with FElements[i] do
    begin
      if X < ALeft then
        ALeft:=X
      else
      if X > ARight then
        ARight:=X;

      if Y < ABottom then
        ABottom:=Y
      else
      if Y > ATop then
        ATop:=Y;
    end;
  Result.Setup(ALeft+(ARight-ALeft)/2, ABottom+(ATop-ABottom)/2);
end;

procedure TFigure.Refresh;
var
  i: Integer;
begin
  if (FGameField = nil) or (FGameField.OnChangeElement = nil) then
    Exit;
  for i:=Low(FElements) to High(FElements) do
    FGameField.OnChangeElement(
      FElements[i].Position.X,
      FElements[i].Position.Y, FElements[i]);
end;


// TFiguresHelper = type helper for TFigures

procedure TFiguresHelper.Free;
var
  i: Integer;
begin
  for i:=Low(Self) to High(Self) do
    FreeAndNil(Self[i]);
  SetLength(Self, 0);
end;

//function LoadFromFile(var AFile: TextFile): Boolean;
//function LoadFromFile(constref AFileName: AnsiString): Boolean;


// TGameField = class(TVSEngineObject);

constructor TGameField.Create(AWidth: Word = DefaultWidth;
  AHeight: Word = DefaultHeight);
begin
  inherited Create;
  SetLength(FElements, AWidth, AHeight);
  ResetElements;
  FOnChangeElement:=nil;
end;

destructor TGameField.Destroy;
begin
  DestroyElements;
  SetLength(FElements, 0, 0);
  inherited Destroy;
end;

procedure TGameField.ResetElements{(FromX, ToX, FromY, ToY: )};
var
  i, j: Integer;
begin
  for i:=0 to High(FElements) do
    for j:=0 to High(FElements[i]) do
      FElements[i, j]:=nil;
end;

{procedure TGameField.ResetElements;
begin
  ResetElements(0, High(FElements), 0, );
end;}

procedure TGameField.DestroyElements;
var
  i, j: Integer;
begin
  for i:=0 to High(FElements) do
    for j:=0 to High(FElements[i]) do
    begin
      FElements[i, j].Free;
      {F}Elements[i, j]:=nil; // Need to use SetElement to do FOnChangeElement!
      //FreeAndNil(FElements[i, j]);
    end;
  //SetLength(FElements, 0, 0);
end;

function TGameField.GetElement(AX, AY: TIntCoord): TElement;
begin
  if DoesElementExist(AX, AY) then
    Result:=FElements[AX, AY]
  else
    Result:=nil;
end;

procedure TGameField.SetElement(AX, AY: TIntCoord; AElement: TElement);
begin
  if not DoesElementExist(AX, AY) then
    Exit;
  {if FElements[AX, AY] <> nil then
    then //what?? Do we need to change it's coords?}
  //
  if FOnChangeElement <> nil then
    FOnChangeElement(AX, AY, AElement);
  //
  FElements[AX, AY]:=AElement;

  {if FOnChangeElement <> nil then
    FOnChangeElement(AX, AY, AElement);}
end;

function TGameField.GetElement(APosition: TIntPoint): TElement;
begin
  Result:=GetElement(APosition.X, APosition.Y);
end;

procedure TGameField.SetElement(APosition: TIntPoint; AElement: TElement);
begin
  SetElement(APosition.X, APosition.Y, AElement);
end;

function TGameField.GetWidth: Word;
begin
  Result:=Length(FElements);
end;

procedure TGameField.SetWidth(AValue: Word);
var
  PredWidth, i, j: Integer;
begin
  PredWidth:=GetWidth;//Length(FElements);
  if AValue > PredWidth then
  begin
    SetLength(FElements, AValue, GetHeight);
    for i:=PredWidth to AValue-1 do
      for j:=0 to High(FElements[i]) do
        FElements[i, j]:=nil;
  end
  else
  if AValue < PredWidth then
  begin
    for i:=AValue to PredWidth-1 do
      for j:=0 to High(FElements[i]) do
      begin
        FElements[i, j].Free;
        {F}Elements[i, j]:=nil;
      end;
    SetLength(FElements, AValue, GetHeight);
  end;
end;

function TGameField.GetHeight: Word;
begin
  if Length(FElements)>0 then
    Result:=Length(FElements[0])
  else
    Result:=0;
end;

procedure TGameField.SetHeight(AValue: Word);
var
  PredHeight, i, j: Integer;
begin
  PredHeight:=GetHeight;//Length(FElements);
  if AValue > PredHeight then
  begin
    SetLength(FElements, GetWidth, AValue);
    for i:=0 to High(FElements) do
      for j:=PredHeight to AValue-1 do
        FElements[i, j]:=nil;
  end
  else
  if AValue < PredHeight then
  begin
    for i:=0 to High(FElements) do
      for j:=AValue to PredHeight-1 do
      begin
        FElements[i, j].Free;
        {F}Elements[i, j]:=nil;
      end;
    SetLength(FElements, GetWidth, AValue);
  end;
end;

procedure TGameField.Setup(AWidth, AHeight: Word);
begin
  // It's not optimal solution!
  SetWidth(AWidth);
  SetHeight(AHeight);
end;

//function TGameField.IsPointInBorders(AX, AY: TIntCoord): Boolean;
function TGameField.DoesElementExist(AX, AY: TIntCoord): Boolean;
begin
  Result:=IsIntInRange(AX, 0, High(FElements)) and
          IsIntinRange(AY, 0, High(FElements[AX]));
end;


//initialization
//begin
//finalization
end.
