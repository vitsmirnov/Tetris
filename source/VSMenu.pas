(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSMenu;

{$mode objfpc}
//{$modeswitch advancedrecords}

interface

uses
  VSPoints, VSStrings, VSCRT;

{const
  DefaultScreenWidth = 80;
  DefaultScreenHeight = 25;}

type
  TMenuButton = class;
  TMenu = class;

  TOnClick = procedure of object; //TOnButtonClick

  TMenuClass = TObject;

  TMenuButton = class(TMenuClass)
  private
    //FName: TString;
    //FOwnerMenu: TMenu;
    FCaption: TString;
    FX, FY: TIntCoord;//Byte;
    FOnClick: TOnClick;//TMenuEvent;

    procedure SetX(AValue: TIntCoord);
    procedure SetY(AValue: TIntCoord);
    function GetPosition: TIntPoint;
    procedure SetPosition(AValue: TIntPoint);
  public
    constructor Create(constref ACaption: TString = 'Menu Button';
      AX: TIntCoord = 1; AY: TIntCoord = 1; AOnClick: TOnClick = nil);

    procedure Setup(constref ACaption: TString = 'Menu Button';
      AX: TIntCoord = 1; AY: TIntCoord = 1; AOnClick: TOnClick = nil);
    procedure Click;

    property Caption: TString read FCaption write FCaption;
    property X: TIntCoord read FX write SetX default 1;
    property Y: TIntCoord read FY write SetY default 1;
    property Position: TIntPoint read GetPosition write SetPosition;
    property OnClick: TOnClick read FOnClick write FOnClick;
  end;

  TMenuButtons = array of TMenuButton;

  TMenuSize = SmallInt;//Word;

  TCursorSkin = String[10];//Char;

  //TOnCloseQuery = procedure ({var}out CanClose: Boolean) of object;
  TOnExitMenuQuery = procedure ({out}var CanExit: Boolean) of object;

  TMenuEventType = ({metNone, }metMoveCursorToPrev, metMoveCursorToNext,
    metButtonClick, metExitMenu);

  TMenuEvent = procedure of object;

  //All possible events/commands. Do we need this?
  TMenuEvents = array[TMenuEventType] of TMenuEvent;

  // Each key is assigned to menu event or takes nil
  TMenuCommands = array[TKeyCode] of TMenuEvent;

  TMenuControlKeys = array[TMenuEventType] of TKeys;

  TDrawMethod = procedure of object;

  TMenu = class(TMenuClass)
  private
    FCaption: TStrings;//TString;
    FCaptionPosition: TIntPoint;
    FTop: TIntCoord;
    FLeft: TIntCoord;
    FWidth: TSize;//??
    FHeight: TSize;//??
    //FBackground: TStrings;//TTextCanvas;

    FOnExitQuery: TOnExitMenuQuery;
    FEvents: TMenuEvents;
    FCommands: TMenuCommands;

    FButtons: TMenuButtons;
    FCurPos: TMenuSize;
    FCursorDelta: TIntPoint;
    FCursorSkin: TCursorSkin;
    FCursorGoThrough: Boolean;
    FExitMenu: Boolean;

    FDrawMethod: TDrawMethod;

    procedure SetCurPos(AValue: TMenuSize);
    function GetButtonsNumber: TMenuSize;//Byte;
    function GetButtons(AIndex: TMenuSize): TMenuButton;
    function GetFocusButton: TMenuButton;
    function GetCursorPositionOnScreen: TIntPoint;
  protected
    procedure DoTheCommand(AKey: TKeyCode);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;//Reset
    procedure Run(ADoClrScr: Boolean = True);
    procedure ExitMenu;
    procedure MoveCursorTo(AValue: TMenuSize);
    procedure MoveCursorToPrev;
    procedure MoveCursorToNext;
    procedure DrawStandartMethod(ADoClrScr: Boolean = False);
    procedure Draw(ADoClrScr: Boolean = False); virtual;
    procedure DrawOnStrings(var AStrings: TStrings;
      ADeltaX: TIntCoord = 0; ADeltaY: TIntCoord = 0);

    procedure FocusButtonClick;

    function AddButton(constref ACaption: TString = 'Menu Button';
      AX: TIntCoord = 1; AY: TIntCoord = 1;
      AOnClick: TOnClick = nil): TMenuButton;//Integer
    procedure DeleteButton(AIndex: TMenuSize);
    function GetButtonIndex(AButton: TMenuButton): TMenuSize;

    procedure ResetControlKeys;
    procedure SetControlKey(AEventType: TMenuEventType; AKey: TKeyCode);
    procedure SetControlKey(AMenuEvent: TMenuEvent; AKey: TKeyCode);
    procedure SetControlKeys(AEventType: TMenuEventType;
      constref AKeys: TKeys; DeletePrevious: Boolean = True);
    procedure SetControlKeys(AMenuEvent: TMenuEvent;
      constref AKeys: TKeys; DeletePrevious: Boolean = True);
    function GetControlKeys(AEventType: TMenuEventType): TKeys;
    function GetControlKeys: TMenuControlKeys;

    procedure AlignXYToCenter(
      ALeft: TIntCoord = 1; ARight: TIntCoord = DefaultScreenWidth;
      ATop: TIntCoord = 1; ABottom: TIntCoord = DefaultScreenHeight;
      ALineInterval: Byte = 1);
    procedure AlignXToCenter(
      ALeft: TIntCoord = 1; ARight: TIntCoord = DefaultScreenWidth);
    procedure AlignYToCenter(
      ATop: TIntCoord = 1; ABottom: TIntCoord = DefaultScreenHeight;
      ALineInterval: Byte = 1; AShiftIfOdd: Boolean = False);
    procedure AlignToLeft(ALeft: TIntCoord = 3); // 3 because we need to draw cursor
    procedure AlignToRight(ARight: TIntCoord = DefaultScreenWidth);
    procedure AlignToTop(ATop: TIntCoord = 1; ALineInterval: Byte = 1);
    procedure AlignToBottom(ABottom: TIntCoord = DefaultScreenHeight;
      ALineInterval: Byte = 1);
    procedure ShiftX(AX: TIntCoord);
    procedure ShiftY(AY: TIntCoord);
    procedure ShiftXY(AX, AY: TIntCoord);//ShiftButtons

    procedure ImportCaptions(constref ACaptions: TStrings);
    function LoadCaptionsFromFile(var AFile: TextFile): Boolean;

    property Caption: TStrings read FCaption write FCaption;
    property CaptionPosition: TIntPoint read FCaptionPosition write FCaptionPosition;
    property CurPos: TMenuSize read FCurPos write SetCurPos default -1;//0
    property CursorDelta: TIntPoint read FCursorDelta write FCursorDelta;
    property CursorDeltaX: TIntCoord read FCursorDelta.X write FCursorDelta.X default -2;
    property CursorDeltaY: TIntCoord read FCursorDelta.Y write FCursorDelta.Y default 0;
    property CursorSkin: TCursorSkin read FCursorSkin write FCursorSkin;// default '>';
    property CursorGoThrough: Boolean read FCursorGoThrough write FCursorGoThrough default True;//False;
    property ButtonsNumber: TMenuSize read GetButtonsNumber default 0;
    property Buttons[AIndex: TMenuSize]: TMenuButton read GetButtons; default;
    property FocusButton: TMenuButton read GetFocusButton;//Buttons[FCurPos];
    property CursorPositionOnScreen: TIntPoint read GetCursorPositionOnScreen;
    property OnExitQuery: TOnExitMenuQuery read FOnExitQuery write FOnExitQuery;// default nil;

    property DrawMethod: TDrawMethod read FDrawMethod write FDrawMethod;
  end;


  TYesNoDialog = class(TMenu)
  private
    FResult: Boolean;
    procedure Yes;
    procedure No;
  public
    constructor Create(constref AMsg: TString);
    property Res: Boolean read FResult default False;
  end;

  TMessageDialog = class(TMenu)
  public
    constructor Create(constref AMsg: TString);
  end;

// It returns false if No or Escape and it returns true if Yes
function ShowYesNoDialog(constref AMsg: TString; FocusOn: TMenuSize = 0): Boolean;
function ShowYesNoDialog2(constref AMsg: TString; FocusOn: TMenuSize = 0): Boolean;
// It just shows message with 'Ok' button
procedure ShowMessage(constref AMsg: TString);

// Temp??
function ReadIntFromRange(ABegin, AEnd: Integer;
  constref AMessages: TStrings; DoClrScr: Boolean = False;
  ShowRange: Boolean = False; ShowErrorMsg: Boolean = False;
  {constref RangeMsg: TString = 'It should be %d..%d:';}
  GoToNextLine: Boolean = True): Integer;
function ReadIntFromRange(ABegin, AEnd: Integer;
  constref AMessage: TString = ''; DoClrScr: Boolean = False;
  ShowRange: Boolean = False; ShowErrorMsg: Boolean = False{;
  const RangeMsg: TString = 'It should be %d..%d:'}): Integer;
function ReadIntFromMenu(constref AHeader: TStrings;
  constref AMenu: TStrings; CurPos: SmallInt = 0;
  DoClrScr: Boolean = True; CurGoThrough: Boolean = False): SmallInt;//Byte;
function ReadIntFromMenuXY(constref AHeader: TStrings;
  constref AMenu: TStrings; X0: Byte = 1; Y0: Byte = 1;
  LineInterval: Byte = 0; Centered: Boolean = False; CurPos: SmallInt = 0;
  CurGoThrough: Boolean = False; DoClrScr: Boolean = True): SmallInt;//Byte;
{function ReadIntFromMenu(constref AHeader: TStrings;
  constref AMenu: TStrings; var CurPos: Byte;
  DoClrScr: Boolean = True): SmallInt;//Byte;}
function ReadString(constref ACaption: AnsiString; DoClrScr:
  Boolean = False): AnsiString;

const
  EmptyChar = ' ';


implementation

uses
  VSUtils;

function ShowYesNoDialog2(constref AMsg: TString; FocusOn: TMenuSize = 0): Boolean;
var
  Choise: Integer;
begin
  Choise:=ReadIntFromMenuXY([AMsg, ''], [' Yes', ' No'],
    (ScreenWidth{-Length(AMsg)}) div 2, (ScreenHeight-4) div 2,
    0, True, FocusOn, True, True);
  Result:=Choise=0;
end;

function ShowYesNoDialog(constref AMsg: TString; FocusOn: TMenuSize = 0): Boolean;
var
  YNDlg: TYesNoDialog;
begin
  YNDlg:=TYesNoDialog.Create(AMsg);
  YNDlg.CurPos:=FocusOn;
  YNDlg.Run;
  Result:=YNDlg.Res;
  YNDlg.Free;
end;

procedure ShowMessage(constref AMsg: TString);
var
  MsgDlg: TMessageDialog;
begin
  MsgDlg:=TMessageDialog.Create(AMsg);
  //WriteStringXY((ScreenWidth-Length(AMsg)) div 2, ScreenHeight div 2 - 2, AMsg);
  MsgDlg.Run;
  MsgDlg.Free;
end;


//

function ReadIntFromRange(ABegin, AEnd: Integer;
  constref AMessages: TStrings; DoClrScr: Boolean = False;
  ShowRange: Boolean = False; ShowErrorMsg: Boolean = False;
  {constref RangeMsg: TString = 'It should be %d..%d:';}
  GoToNextLine: Boolean = True): Integer;
var
  TempStr: TString;
  WrongSymbolNumber: Integer;
begin
  repeat
    if DoClrScr then ClrScr;
    //WriteStringsXY(WhereX, WhereY, AMessages);
    WriteStrings(AMessages);
    if ShowRange then
      WriteLn('It should be ', ABegin, '..', AEnd, ':');
      //WriteLn(Format(RangeMsg, [ABegin, AEnd]));
    //WriteLn;
    ReadLn(TempStr);
    Val(TempStr, Result, WrongSymbolNumber);
    if ShowErrorMsg and ((WrongSymbolNumber<>0)
      or not IsIntInRange(Result, ABegin, AEnd)) then
      WriteLn('"', TempStr, '" is not expected. It should be ',
        ABegin, '..', AEnd, '. Try again:');
    if GoToNextLine then WriteLn;
  until (WrongSymbolNumber=0) and IsIntInRange(Result, ABegin, AEnd);
end;

function ReadIntFromRange(ABegin, AEnd: Integer;
  constref AMessage: TString = ''; DoClrScr: Boolean = False;
  ShowRange: Boolean = False; ShowErrorMsg: Boolean = False{;
  constref RangeMsg: TString = 'It should be %d..%d:'}): Integer;
begin
  Result:=ReadIntFromRange(ABegin, AEnd, [AMessage], DoClrScr,
    ShowRange, {RangeMsg,} ShowErrorMsg, False);
end;

function ReadIntFromMenu(constref AHeader: TStrings;
  constref AMenu: TStrings; CurPos: SmallInt = 0;
  DoClrScr: Boolean = True; CurGoThrough: Boolean = False): SmallInt;//Byte;
var
  MenuHigh: SmallInt;//Byte;
  //Key: TKeyCode;
begin
  MenuHigh:=High(AMenu);
  if MenuHigh<0 then Exit(-1);
  //CursorOn;
  if DoClrScr then ClrScr;
  WriteStrings(AHeader);
  WriteStrings(AMenu);
  if not (CurPos in [0..MenuHigh]) then
    CurPos:=0;
  GoToXY(1, 1+Length(AHeader)+CurPos);
  repeat
    //Key:=GetKey;
    case GetKey of
      KeyUp, KeyLeft: begin
        if CurPos>0 then
          CurPos:=CurPos-1
        else if CurGoThrough {and (CurPos=0)} then
          CurPos:=MenuHigh;
        GoToXY(WhereX, 1+Length(AHeader)+CurPos);
      end;
      KeyDown, KeyRight, KeyTab: begin
        if CurPos<MenuHigh then
          CurPos:=CurPos+1
        else if CurGoThrough then
          CurPos:=0;
        GoToXY(WhereX, 1+Length(AHeader)+CurPos);
      end;
      KeyEnter, KeySpace: Exit(CurPos);
      KeyEscape: Exit(-1);
    end;
    {if Key in [KeyUp, KeyDown, KeyRight, KeyLeft, KeyTab] then
      GoToXY(WhereX, 1+Length(AHeader)+CurPos);}
  until False;
end;

{function ReadIntFromMenu(constref AHeader: TStrings;
  constref AMenu: TStrings; var CurPos: Byte;
  DoClrScr: Boolean = True): SmallInt;//Byte;
begin

end;}

function ReadIntFromMenuXY(constref AHeader: TStrings;
  constref AMenu: TStrings; X0: Byte = 1; Y0: Byte = 1;
  LineInterval: Byte = 0; Centered: Boolean = False; CurPos: SmallInt = 0;
  CurGoThrough: Boolean = False; DoClrScr: Boolean = True): SmallInt;//Byte;
var
  MenuHigh, MenuWidth: SmallInt;//Byte;
  //Key: TKeyCode;
begin
  MenuHigh:=High(AMenu);
  if MenuHigh < 0 then
    Exit(-1);
  MenuWidth:=StringsMaxLength(AMenu);
  // ???
  {if MenuWidth < StringsMaxLength(AHeader) then
    MenuWidth:=StringsMaxLength(AHeader);}
  //CursorOn;
  if DoClrScr then
    ClrScr;
  {X, Y: TIntCoord; constref AStrings: TStrings;
  ColCopyNumber: SmallInt = 1; RowCopyNumber: SmallInt = 1;
  LineInterval: Byte = 0; ShowError: Boolean = False}
  if Centered then
    WriteStringsXY(X0+(MenuWidth-StringsMaxLength(AHeader)) div 2, Y0,
      AHeader, 1, 1, LineInterval, Centered{, False})
  else
    WriteStringsXY(X0{+(MenuWidth-StringsMaxLength(AHeader)) div 2}, Y0,
      AHeader, 1, 1, LineInterval, Centered{, False});
  WriteStringsXY(X0, Y0+Length(AHeader)*(LineInterval+1),
    AMenu, 1, 1, LineInterval, Centered{, False});
  if not (CurPos in [0..MenuHigh]) then
    CurPos:=0;
  GoToXY(X0+Round((MenuWidth-Length(AMenu[CurPos]))/2-0)*Byte(Centered),
    Y0+Length(AHeader)*(LineInterval+1)+CurPos*(LineInterval+1));
  CursorOn;
  repeat
    //Key:=GetKey;
    case GetKey of
      KeyUp, KeyLeft: begin
        if CurPos>0 then
          CurPos:=CurPos-1
        else
        if CurGoThrough {and (CurPos=0)} then
          CurPos:=MenuHigh;
        GoToXY({WhereX}X0+Round((MenuWidth-Length(AMenu[CurPos]))/2-0)*
          Byte(Centered),
          Y0+Length(AHeader)*(LineInterval+1)+CurPos*(LineInterval+1));
      end;
      KeyDown, KeyRight, KeyTab: begin
        if CurPos<MenuHigh then
          CurPos:=CurPos+1
        else
        if CurGoThrough then
          CurPos:=0;
        GoToXY({WhereX}X0+Round((MenuWidth-Length(AMenu[CurPos]))/2-0)*
          Byte(Centered),
          Y0+Length(AHeader)*(LineInterval+1)+CurPos*(LineInterval+1));
      end;
      KeyEnter, KeySpace: Exit(CurPos);
      KeyEscape: Exit(-1);
    end;
    {if Key in [KeyUp, KeyDown, KeyRight, KeyLeft, KeyTab] then
      GoToXY(WhereX, 1+Length(AHeader)+CurPos);}
  until False;
end;

//procedure WriteStringInScrCtr(constref AString: AnsiString);
function MsgScreenCentreX(constref AString: AnsiString): Integer;
begin
  Result:=(ScreenWidth-Length(AString)) div 2;
end;

{function MsgScreenCentreY(constref AString: AnsiString): Integer;
begin
  Result:=(ScreenHeight-Length(AString)) div 2;
end;}

function ReadString{Dlg}(constref ACaption: AnsiString; DoClrScr:
  Boolean = False): AnsiString;
var
  X, Y: Integer;
begin
  if DoClrScr then
    ClrScr;
  X:=MsgScreenCentreX(ACaption);
  Y:=ScreenHeight div 2;
  WriteStringXYInRect(X, Y-2, ACaption);
  GoToXY(X, Y);
  ReadLn(Result);
end;


// TYesNoDialog

constructor TYesNoDialog.Create(constref AMsg: TString);
begin
  inherited Create;
  FResult:=False;
  Caption:=[AMsg{, ''}];
  CaptionPosition:=IntCoordsToPoint(
    (ScreenWidth-Length(AMsg)) div 2, ScreenHeight div 2 - 2);
  AddButton('Yes', (ScreenWidth-10) div 2, ScreenHeight div 2, @Yes);
  AddButton('No', (ScreenWidth-10) div 2+8, ScreenHeight div 2, @No);
  //AlignToCenter(True, True, 1, ScreenWidth, 1, ScreenHeight);
  {AlignToTop(ScreenHeight div 2 - 1, 0);
  AlignXToCenter(1, ScreenWidth);}
end;

procedure TYesNoDialog.Yes;
begin
  FResult:=True;
  ExitMenu;
end;

procedure TYesNoDialog.No;
begin
  FResult:=False;
  ExitMenu;
end;


// TMessageDialog
constructor TMessageDialog.Create(constref AMsg: TString);
begin
  inherited Create;
  Caption:=[AMsg{, ''}];
  {CaptionPosition:=IntCoordsToPoint(
    (ScreenWidth-Length(AMsg)) div 2, ScreenHeight div 2 - 2);}
  AddButton('Ok', (ScreenWidth-Length('Ok')) div 2, ScreenHeight div 2,
    @EXitMenu);
  //AlignXYToCenter(True, True, 1, ScreenWidth, 1, ScreenHeight);
  AlignToTop(ScreenHeight div 2 - 2, 1);
  AlignXToCenter(1, ScreenWidth);
end;

function GetKeyIfPressed(var AKey: TKeyCode): Boolean;
begin
  Result:=KeyPressed;
  if Result then
    AKey:=GetKey;
end;


// TMenuButton

constructor TMenuButton.Create(constref ACaption: TString = 'Menu Button';
  AX: TIntCoord = 1; AY: TIntCoord = 1; AOnClick: TOnClick = nil);
begin
  //FCaption:='MenuButton';
  FX:=1;
  FY:=1;
  //FOnClick:=nil;

  //FOwnerMenu: TMenu;
  Setup(ACaption, AX, AY, AOnClick);
end;

procedure TMenuButton.SetX(AValue: TIntCoord);
begin
  // Do we need this check?
  //if IsXOnScreen(AValue) then
    FX:=AValue;
end;

procedure TMenuButton.SetY(AValue: TIntCoord);
begin
  //if IsXOnScreen(AValue) then
    FY:=AValue;
end;

function TMenuButton.GetPosition: TIntPoint;
begin
  Result.X:=FX;
  Result.Y:=FY;
end;

procedure TMenuButton.SetPosition(AValue: TIntPoint);
begin
  X:=AValue.X;
  Y:=AValue.Y;
end;

procedure TMenuButton.Setup(constref ACaption: TString = 'Menu Button';
  AX: TIntCoord = 1; AY: TIntCoord = 1; AOnClick: TOnClick = nil);
begin
  FCaption:=ACaption;
  X:=AX;
  Y:=AY;
  FOnClick:=AOnClick;
end;

procedure TMenuButton.Click;
begin
  if FOnClick<>nil then
    FOnClick;
end;


// TMenu

constructor TMenu.Create;
begin
  FCaption:=['Menu'];//TStrings;
  FCaptionPosition:=IntCoordsToPoint(1, 1);
  FTop:=1;
  FLeft:=1;
  FWidth:=30;// TSize; ??
  FHeight:=20;//??
  //FBackground: TStrings;//TTextCanvas;
  FOnExitQuery:=nil;

  FEvents[metMoveCursorToPrev]:=@MoveCursorToPrev;
  FEvents[metMoveCursorToNext]:=@MoveCursorToNext;
  FEvents[metButtonClick]:=@FocusButtonClick;
  FEvents[metExitMenu]:=@ExitMenu;

  ResetControlKeys;
  SetControlKeys(metMoveCursorToPrev,
    [KeyUp, KeyLeft, KeyW, KeyA, KeyBigW, KeyBigA], False);
  SetControlKeys(metMoveCursorToNext,
    [KeyDown, KeyRight, KeyTab, KeyS, KeyD, KeyBigS, KeyBigD], False);
  SetControlKeys(metButtonClick, [KeyEnter, KeySpace], False);
  SetControlKeys(metExitMenu, [KeyEscape, KeyBackspace{, KeyQ, KeyBigQ}], False);

  SetLength(FButtons, 0);
  FCurPos:=0;//-1;//0 ??!!
  FCursorDelta.X:=-2;
  FCursorDelta.Y:=0;
  FCursorSkin:='>';
  FCursorGoThrough:=True;//False;
  FExitMenu:=False;

  FDrawMethod:=nil;
end;

destructor TMenu.Destroy;
begin
  Clear;
end;

procedure TMenu.Clear;
var i: Integer;
begin
  for i:=0 to High(FButtons) do
    FButtons[i].Free;
  SetLength(FButtons, 0);
end;

procedure TMenu.Run(ADoClrScr: Boolean = True);
begin
  FExitMenu:=False;
  Draw(ADoClrScr);
  repeat
    //if KeyPressed then
    //if FNeedToRefresh then
    begin
      //Draw;
      //FNeedToRefresh:=False;
    end;
    DoTheCommand(GetKey);
  until FExitMenu;//False
end;

procedure TMenu.ExitMenu;
begin
  if FOnExitQuery<>nil then
    FOnExitQuery(FExitMenu)
  else
    FExitMenu:=True;
end;

procedure TMenu.ResetControlKeys;
var
  i: Integer;
begin
  for i:=Low(FCommands) to High(FCommands) do
    FCommands[i]:=nil;
end;

procedure TMenu.SetControlKey(AEventType: TMenuEventType; AKey: TKeyCode);
begin
  FCommands[AKey]:=FEvents[AEventType];
end;

procedure TMenu.SetControlKey(AMenuEvent: TMenuEvent; AKey: TKeyCode);
begin
  FCommands[AKey]:=AMenuEvent;
end;

procedure TMenu.SetControlKeys(AEventType: TMenuEventType;
  constref AKeys: TKeys; DeletePrevious: Boolean = True);
var
  i: Integer;
begin
  if DeletePrevious then
    for i:=Low(FCommands) to High(FCommands) do
      if FCommands[i]=FEvents[AEventType] then
        FCommands[i]:=nil;
  for i:=0 to High(AKeys) do
    FCommands[AKeys[i]]:=FEvents[AEventType];
end;

procedure TMenu.SetControlKeys(AMenuEvent: TMenuEvent;
  constref AKeys: TKeys; DeletePrevious: Boolean = True);
var
  i: Integer;
begin
  if DeletePrevious then
    for i:=Low(FCommands) to High(FCommands) do
      if FCommands[i]=AMenuEvent{FEvents[AEventType]} then
        FCommands[i]:=nil;
  for i:=0 to High(AKeys) do
    FCommands[AKeys[i]]:=AMenuEvent;//FEvents[AEventType];
end;

function TMenu.GetControlKeys(AEventType: TMenuEventType): TKeys;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i:=Low(FCommands) to High(FCommands) do
    if FCommands[i]=FEvents[AEventType] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)]:=i;
    end;
end;

function TMenu.GetControlKeys: TMenuControlKeys;
var
  i: Integer;
  AEventType: TMenuEventType;
begin
  for i:=Low(FCommands) to High(FCommands) do
    for AEventType:=Low(AEventType) to High(AEventType) do
      if FCommands[i]=FEvents[AEventType] then
      begin
        SetLength(Result[AEventType], Length(Result[AEventType])+1);
        Result[AEventType, High(Result[AEventType])]:=i;
      end;
end;

function TMenu.GetButtonIndex(AButton: TMenuButton): TMenuSize;
var
  i: Integer;
begin
  for i:=0 to High(FButtons) do
    if FButtons[i]=AButton then
      Exit(i);
  Result:=-1;
end;

procedure TMenu.DoTheCommand(AKey: TKeyCode);
begin
  if FCommands[AKey]<>nil then
    FCommands[AKey];
end;

procedure TMenu.FocusButtonClick;
begin
  if FocusButton<>nil then
    FocusButton.Click;
end;

function TMenu.AddButton(constref ACaption: TString = 'Menu Button';
  AX: TIntCoord = 1; AY: TIntCoord = 1; AOnClick: TOnClick = nil): TMenuButton;
begin
  SetLength(FButtons, Length(FButtons)+1);
  FButtons[High(FButtons)]:=TMenuButton.Create(ACaption, AX, AY, AOnClick);
  Result:=FButtons[High(FButtons)];
end;

procedure TMenu.DeleteButton(AIndex: TMenuSize);
var
  i: Integer;
begin
  if not IsIntInRange(AIndex, 0, High(FButtons)) then Exit;
  FButtons[AIndex].Free;
  for i:=AIndex to High(FButtons)-1 do
    FButtons[i]:=FButtons[i+1];
  SetLength(FButtons, Length(FButtons)-1);

  // It should be ckecked out!
  if (FCurPos>AIndex) or (FCurPos>High(FButtons)) then
    FCurPos:=FCurPos-1;
end;

procedure TMenu.SetCurPos(AValue: TMenuSize);
begin
  if IsIntInRange(AValue, 0, High(FButtons)) then
    FCurPos:=AValue;
end;

function TMenu.GetButtonsNumber: TMenuSize;//Byte;
begin
  Result:=Length(FButtons);
end;

function TMenu.GetButtons(AIndex: TMenuSize): TMenuButton;
begin
  if IsIntInRange(AIndex, 0, High(FButtons)) then
    Result:=FButtons[AIndex]
  else
    Result:=nil;
end;

function TMenu.GetFocusButton: TMenuButton;
begin
  Result:=Buttons[FCurPos];
end;

function TMenu.GetCursorPositionOnScreen: TIntPoint;
begin
  if (FocusButton<>nil) {and IsPointOnScreen(FocusButton^.X+FCursorDelta.X,
    FocusButton^.Y+FCursorDelta.Y)} then
    Result:=IntCoordsToPoint(FocusButton.X+FCursorDelta.X,
      FocusButton.Y+FCursorDelta.Y)
  else
    Result:=IntCoordsToPoint(0, 0);
end;

procedure TMenu.DrawStandartMethod(ADoClrScr: Boolean = False);
var
  i: Integer;
begin
  if ADoClrScr then
    ClrScr;
  //WriteStringsXY(FCaptionPosition, FCaption, 1, 1, 0, True);
  if Length(FCaption) > 0 then
    WriteStringXYInRect(FCaptionPosition, FCaption[0]);
  for i:=Low(FButtons) to High(FButtons) do
    with FButtons[i] do
      WriteStringXYInRect(X, Y, Caption);
  if FocusButton <> nil then
  begin
    WriteStringXYInRect(CursorPositionOnScreen, FCursorSkin);
    GoToXY(CursorPositionOnScreen);
  end;
end;

procedure TMenu.Draw(ADoClrScr: Boolean = False);
begin
  {if Length(FButtons) = 0 then
    Exit;}

  if FDrawMethod <> nil then
    FDrawMethod()
  else
    DrawStandartMethod(ADoClrScr);
end;

procedure TMenu.DrawOnStrings(var AStrings: TStrings;
  ADeltaX: TIntCoord = 0; ADeltaY: TIntCoord = 0);
var
  i: Integer;
begin
  {AStrings.PutStrings(
    FCaptionPosition.X+ADeltaX, FCaptionPosition.Y-1+ADeltaY, FCaption);}
  if Length(FCaption) > 0 then
    AStrings.PutString(
      FCaptionPosition.X+ADeltaX, FCaptionPosition.Y-1+ADeltaY, FCaption[0]);
  for i:=Low(FButtons) to High(FButtons) do
    with FButtons[i] do
      AStrings.PutString(X+ADeltaX, Y-1+ADeltaY, Caption);
  if FocusButton <> nil then
    with CursorPositionOnScreen.Shifted(ADeltaX, -1+ADeltaY) do
      AStrings.PutString(X, Y, FCursorSkin);
end;

procedure TMenu.MoveCursorTo(AValue: TMenuSize);
{var
  CurPosOnScr: TIntPoint;}
begin
  {if (Length(FButtons)=0) or not IsIntInRange(FCurPos, 0, High(FButtons)) or
    not IsIntInRange(AValue, 0, High(FButtons)) then
    Exit;}
  if (FocusButton=nil) or (Buttons[AValue]=nil) then
    Exit;
  //CurPosOnScr:=CursorPositionOnScreen;
  //if IsPointOnScreen(CurPosOnScr) then
  WriteStringXY(CursorPositionOnScreen,
    FillString(EmptyChar, Length(FCursorSkin)));
  // We've checked AValue's position
  FCurPos:=AValue;
  WriteStringXY(CursorPositionOnScreen, FCursorSkin);
  if IsPointOnScreen(CursorPositionOnScreen) then
    GoToXY(CursorPositionOnScreen.X, CursorPositionOnScreen.Y)
  else
    GoToXY(ScreenWidth, ScreenHeight);//CursorOff;
end;

procedure TMenu.MoveCursorToPrev;
begin
  if FCurPos > 0 then
    MoveCursorTo(FCurPos-1)
  else
  if FCursorGoThrough then
    MoveCursorTo(High(FButtons));
end;

procedure TMenu.MoveCursorToNext;
begin
  if FCurPos < High(FButtons) then
    MoveCursorTo(FCurPos+1)
  else
  if FCursorGoThrough then
    MoveCursorTo(0);
end;

procedure TMenu.AlignXYToCenter(
  ALeft: TIntCoord = 1; ARight: TIntCoord = DefaultScreenWidth;
  ATop: TIntCoord = 1; ABottom: TIntCoord = DefaultScreenHeight;
  ALineInterval: Byte = 1);
begin
  AlignXToCenter(ALeft, ARight);
  AlignYToCenter(ATop, ABottom, ALineInterval{, False});
end;

procedure TMenu.AlignXToCenter(ALeft: TIntCoord = 1; ARight: TIntCoord = DefaultScreenWidth);
var
  i, LWidth: Integer;
begin
  LWidth:=ARight-ALeft+1;
  FCaptionPosition.X:=(LWidth-FCaption.MaxLength) div 2+1;
  for i:=Low(FButtons) to High(FButtons) do
    with FButtons[i] do
      X:=(LWidth-Caption.Length) div 2+1;
end;

procedure TMenu.AlignYToCenter(ATop: TIntCoord = 1; ABottom: TIntCoord = DefaultScreenHeight;
  ALineInterval: Byte = 1; AShiftIfOdd: Boolean = False);
var
  i, LStrCount, LHeight, LTop: Integer;
begin
  LHeight:=ABottom-ATop+1;
  LStrCount:=(Length(FCaption)+ButtonsNumber)*(ALineInterval+1)-ALineInterval;//12-3
  LTop:=(LHeight-LStrCount) div 2 + 1;
  if AShiftIfOdd and Odd(LTop) then
    LTop+=1;
  FCaptionPosition.Y:=LTop;
  LTop:=LTop+Length(FCaption)*(ALineInterval+1);
  for i:=0 to ButtonsNumber-1 do
    FButtons[i].Y:=LTop+i*(ALineInterval+1);
end;

procedure TMenu.AlignToLeft(ALeft: TIntCoord = 3);
var
  i: Integer;
begin
  {if not IsXOnScreen(ALeft) then
    Exit;}
  FCaptionPosition.X:=ALeft;
  for i:=0 to High(FButtons) do
    FButtons[i].X:=ALeft;
end;

procedure TMenu.AlignToRight(ARight: TIntCoord = DefaultScreenWidth);
var
  i: Integer;
begin
  {if not IsXOnScreen(ARight) then
    Exit;}
  FCaptionPosition.X:=ARight-StringsMaxLength(FCaption)+1;
  for i:=0 to High(FButtons) do
    FButtons[i].X:=ARight-Length(FButtons[i].Caption)+1;
end;

procedure TMenu.AlignToTop(ATop: TIntCoord = 1; ALineInterval: Byte = 1);
var
  i: Integer;
begin
  {if not IsYOnScreen(ATop) then
    Exit;}
  FCaptionPosition.Y:=ATop;
  for i:=0 to High(FButtons) do
    FButtons[i].Y:=ATop+Length(FCaption)*(ALineInterval+1)+i*(ALineInterval+1);
end;

procedure TMenu.AlignToBottom(ABottom: TIntCoord = DefaultScreenHeight;
  ALineInterval: Byte = 1);
var
  i: Integer;
begin
  {if not IsYOnScreen(ABottom) then
    Exit;}
  FCaptionPosition.Y:=ABottom-(Length(FButtons)+Length(FCaption)-1)*
    (ALineInterval+1);
  for i:=0 to High(FButtons) do
    FButtons[i].Y:=ABottom-(High(FButtons)-i)*(ALineInterval+1);
end;

procedure TMenu.ShiftX(AX: TIntCoord);
begin
  ShiftXY(AX, 0);
end;

procedure TMenu.ShiftY(AY: TIntCoord);
begin
  ShiftXY(0, AY);
end;

procedure TMenu.ShiftXY(AX, AY: TIntCoord);//ShiftButtons
var
  i: Integer;
begin
  if (AX = 0) and (AY = 0) then
    Exit;
  // We need to check coords
  //if not Is...
  with FCaptionPosition do
  begin
    //if AX<>0 then
      X:=X+AX;
    //if AY<>0 then
      Y:=Y+AY;
  end;
  for i:=0 to High(FButtons) do
    with FButtons[i] do
    begin
      X:=X+AX;
      Y:=Y+AY;
    end;
end;

procedure TMenu.ImportCaptions(constref ACaptions: TStrings);
var
  i: Integer;
begin
  if Length(ACaptions) = 0 then
    Exit;

  if ACaptions[Low(ACaptions)] <> '' then
    Caption:=[ACaptions[Low(ACaptions)], '']
  else
    Caption:=[];
  //for i:=Low(ACaptions)+1 to
  for i:=0 to ButtonsNumber-1 do
    if IsIntInRange(i+1, Low(ACaptions), High(ACaptions)) then
      Buttons[i].Caption:=ACaptions[i+1]
    {else
      Exit};
end;

function TMenu.LoadCaptionsFromFile(var AFile: TextFile): Boolean;
var
  LCaptions: TStrings;
begin
  Result:=LCaptions.LoadFromFile(AFile, ButtonsNumber+1);
  if Result then
    ImportCaptions(LCaptions);
end;


end.
