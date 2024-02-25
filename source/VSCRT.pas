(*
  (c) Vitaly Smirnov (VSdev)
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSCRT;

{$mode objfpc}
//{$modeswitch advancedrecords}
{$modeswitch typehelpers}
//{$I-}
//{$B-}

//{$DEFINE WINDOWSMODE}

interface

uses
  {$IFDEF WINDOWSMODE}
  Windows, Graphics,// Forms,
  {$ENDIF}
  VSPoints, VSStrings, VSIntList;

const
  DefaultScreenWidth = 80;
  DefaultScreenHeight = 25;

  OneSecond = 1000; //aSecond

  OneByte = 256;

  {$Region 'original CRT colors'}
  { Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

  { Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;
  {$EndRegion}

  // KeyCodes // Move it to *.inc files?
  {$IFDEF WINDOWSMODE}
  {$Region 'Windows'}
  // Key codes
  // Arrow controls
  KeyUp = VK_UP;//OneByte+72;
  KeyDown = VK_DOWN;//OneByte+80;
  KeyRight = VK_RIGHT;//OneByte+77;
  KeyLeft = VK_LEFT;//OneByte+75;
  // "Pop" keys
  KeySpace = 32;
  KeyEnter = 13;
  KeyDelete = VK_DELETE;//OneByte+83;
  KeyPlus = VK_OEM_PLUS;//43;
  KeyMinus = VK_OEM_MINUS;//95;
  KeyEscape = 27;
  KeyBackspace = 8;
  KeyTab = 9;
  KeyHome = VK_HOME;//OneByte+71;
  // Numbers
  Key0 = 48;
  Key1 = 49;
  Key2 = 50;
  Key3 = 51;
  Key4 = 52;
  Key5 = 53;
  Key6 = 54;
  Key7 = 55;
  Key8 = 56;
  Key9 = 57;
  // Letters small
  KeyA = VK_A;//97;
  KeyW = VK_W;//119;
  KeyS = VK_S;//115;
  KeyD = VK_D;//100;
  KeyQ = VK_Q;//113;
  KeyY = VK_Y;//121;
  KeyN = VK_N;//110;
  KeyP = VK_P;//112;
  // Letters big (with Shift or Caps Lock)
  KeyBigA = VK_A;//65;
  KeyBigW = VK_W;//87;
  KeyBigS = VK_S;//83;
  KeyBigD = VK_D;//68;
  KeyBigQ = VK_Q;//81;
  KeyBigY = VK_Y;//89;
  KeyBigN = VK_N;//78;
  KeyBigP = VK_P;//80;
  // Shift + number
  {KeyShift1 = 33; // !
  KeyShift2 = 64; // @
  KeyShift3 = 35; // #
  KeyShift4 = 36; // $
  KeyShift5 = 37; // %
  KeyShift6 = 94; // ^
  KeyShift7 = 38; // &
  KeyShift8 = 42; // * KeyAsterisk = 42 //Shift+8( * )
  KeyShift9 = 40; // (
  KeyShift0 = 41; // )
  // Ctrl + number
  KeyCtrl1 = OneByte+2; //
  KeyCtrl2 = OneByte+3; //
  KeyCtrl3 = OneByte+4; //
  KeyCtrl4 = OneByte+5; //
  KeyCtrl5 = OneByte+6; //
  KeyCtrl6 = OneByte+7; //
  KeyCtrl7 = OneByte+8; //
  KeyCtrl8 = OneByte+9; //
  KeyCtrl9 = OneByte+10; //
  KeyCtrl0 = OneByte+11; //
  // "F" keys
  KeyF1 = OneByte+78;
  KeyF2 = OneByte+9;
  KeyF3 = OneByte+8;
  KeyF4 = OneByte+2;
  KeyF5 = OneByte+121;//OneByte+119
  KeyF6 = OneByte+32;
  KeyF7 = OneByte+46;
  KeyF8 = OneByte+48;
  KeyF9 = OneByte+67;
  KeyF10 = OneByte+68;
  KeyF11 = OneByte+51;
  KeyF12 = OneByte+85;}
  {$EndRegion}
  {$ELSE}
  {$Region 'Console'}
  // Key codes
  // Arrow controls
  KeyUp = OneByte+72;
  KeyDown = OneByte+80;
  KeyRight = OneByte+77;
  KeyLeft = OneByte+75;
  // "Pop" keys
  KeySpace = 32;
  KeyEnter = 13;
  KeyDelete = OneByte+83;
  KeyPlus = 43;
  KeyMinus = 95;
  KeyEscape = 27;
  KeyBackspace = 8;
  KeyTab = 9;
  KeyHome = OneByte+71;
  // Numbers
  Key0 = 48;
  Key1 = 49;
  Key2 = 50;
  Key3 = 51;
  Key4 = 52;
  Key5 = 53;
  Key6 = 54;
  Key7 = 55;
  Key8 = 56;
  Key9 = 57;
  // Letters small
  KeyA = 97;
  KeyW = 119;
  KeyS = 115;
  KeyD = 100;
  KeyQ = 113;
  KeyY = 121;
  KeyN = 110;
  KeyP = 112;
  // Letters big (with Shift or Caps Lock)
  KeyBigA = 65;
  KeyBigW = 87;
  KeyBigS = 83;
  KeyBigD = 68;
  KeyBigQ = 81;
  KeyBigY = 89;
  KeyBigN = 78;
  KeyBigP = 80;
  // Shift + number
  KeyShift1 = 33; // !
  KeyShift2 = 64; // @
  KeyShift3 = 35; // #
  KeyShift4 = 36; // $
  KeyShift5 = 37; // %
  KeyShift6 = 94; // ^
  KeyShift7 = 38; // &
  KeyShift8 = 42; // * KeyAsterisk = 42 //Shift+8( * )
  KeyShift9 = 40; // (
  KeyShift0 = 41; // )
  // Ctrl + number
  KeyCtrl1 = OneByte+2; //
  KeyCtrl2 = OneByte+3; //
  KeyCtrl3 = OneByte+4; //
  KeyCtrl4 = OneByte+5; //
  KeyCtrl5 = OneByte+6; //
  KeyCtrl6 = OneByte+7; //
  KeyCtrl7 = OneByte+8; //
  KeyCtrl8 = OneByte+9; //
  KeyCtrl9 = OneByte+10; //
  KeyCtrl0 = OneByte+11; //
  // "F" keys
  KeyF1 = OneByte+78;
  KeyF2 = OneByte+9;
  KeyF3 = OneByte+8;
  KeyF4 = OneByte+2;
  KeyF5 = OneByte+121;//OneByte+119
  KeyF6 = OneByte+32;
  KeyF7 = OneByte+46;
  KeyF8 = OneByte+48;
  KeyF9 = OneByte+67;
  KeyF10 = OneByte+68;
  KeyF11 = OneByte+51;
  KeyF12 = OneByte+85;
  {$EndRegion}
  {$ENDIF}

type
  TKeyCode =
  {$IFDEF WINDOWSMODE}
  Byte;
  {$ELSE}
  0..511;//OneByte*2-1;//-255..255;
  {$ENDIF}

  TKeys = array of TKeyCode;

  TKeySet = set of Byte;//TKeyCode;

  {TString = AnsiString;
  TStrings = array of TString;}

  TIntPointHelperCRT = type helper(TIntPointHelper) for TIntPoint
  public
    function IsItOnScreen: Boolean; // Console screen
  end;

  TFloatPointHelperCRT = type helper(TFloatPointHelper) for TFloatPoint
  public
    function IsItOnScreen: Boolean; // Console screen
  end;

  TProcessMessages = procedure of object;

var
  ScreenWidth: Word = 80;//0;
  ScreenHeight: Word = 25;//0;

  {$IFDEF WINDOWSMODE}
  ScreenCanvas: TCanvas = nil;
  KeysQueue: TIntList = nil;
  ProcessMessages: TProcessMessages = nil;
  StopGetKey: Boolean = False; // Temp! Maybe not..
  RepaintScreen: procedure of object = nil;
  {$ENDIF}

{$IFDEF WINDOWSMODE}
procedure VSCRTWinModeSetup(AScreenCanvas: TCanvas;
  AProcessMessages: TProcessMessages;
  AScreenWidth: Word; AScreenHeight: Word);
{$ENDIF}

function KeyPressed: Boolean;
function GetKey: TKeyCode;
function IsGetKeyEscape: Boolean;
procedure GetKeyEscape;
procedure GetOneOfTheseKeys(constref AKeys: TKeys);

function GoToXY(AX, AY: Integer): Boolean;
function GoToXY(constref APoint: TIntPoint): Boolean;
function WhereX: Byte;
function WhereY: Byte;
procedure ClrScr;
procedure TextColor(AColor: Byte);
procedure TextBackground(AColor: Byte);
procedure CursorOn;
procedure CursorOff;

function IsXOnScreen(AX: TIntCoord): Boolean;
function IsYOnScreen(AY: TIntCoord): Boolean;
function IsPointOnScreen(AX, AY: TIntCoord;
  AIncludingBottomRight: Boolean = False): Boolean;
function IsPointOnScreen(constref APoint: TIntPoint;
  AIncludingBottomRight: Boolean = False): Boolean;

function WriteStringXY(AX, AY: TIntCoord; constref AString: TString): Boolean;
function WriteStringXY(constref APosition: TIntPoint;
  constref AString: TString): Boolean;
procedure WriteStringXYInRect(
  AX, AY: TIntCoord; constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord =1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight;
  ADontWriteInRightBottomCorner: Boolean = True);
procedure WriteStringXYInRect(
  constref APosition: TIntPoint; constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord =1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight;
  ADontWriteInRightBottomCorner: Boolean = True);

procedure WriteStrings(constref AStrings: TStrings; ALineInterval: Byte = 0);
procedure WriteStringsXY(
  AX, AY: TIntCoord; constref AStrings: TStrings;
  AColCopyNumber: SmallInt = 1; ARowCopyNumber: SmallInt = 1;
  ALineInterval: Byte = 0; ACentered: Boolean = False;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight);
procedure WriteStringsXY(
  constref APosition: TIntPoint; constref AStrings: TStrings;
  AColCopyNumber: SmallInt = 1; ARowCopyNumber: SmallInt = 1;
  ALineInterval: Byte = 0; ACentered: Boolean = False;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight);
function GetPosForScreenCenter(constref AStrings: TStrings): TIntPoint; // Rename this!

//function VSDelay(ADelayTime: LongWord): Boolean; // Wait WaitFor
function Wait(ADelayTime: {Word}Integer{Int64}; AStopIfKeyPressed: Boolean = False;
  constref AStopKeys: TKeys = nil): Boolean;
{function Wait(ADelayTime: Word; AStartTime: LongWord;
  AStopIfKeyPressed: Boolean = False;
  constref AStopKeys: TKeys = nil): Boolean;}
// This one can't include keys after 255 (arrows, F* and others) because set can't do it
{function Wait2(ADelayTime: Word; AStopIfKeyPressed: Boolean = False;
  constref AStopKeys: TKeySet = []): Boolean;}

operator in (constref ALeft: TKeyCode; constref ARight: TKeys): Boolean;

type
  TKeyNames = array[TKeyCode] of AnsiString;

var//const
  DefaultKeyNames: TKeyNames;


implementation

uses
  {$IFNDEF WINDOWSMODE}
  CRT,
  {$ENDIF}
  SysUtils;


{$IFDEF WINDOWSMODE}
function DoRepaint: Boolean;
begin
  Result:=RepaintScreen <> nil;
  if Result then
    RepaintScreen();
end;

function DoProcessMessages: Boolean;
begin
  Result:=ProcessMessages <> nil;
  if Result then
    ProcessMessages();
end;
{$ENDIF}

// TIntPointHelperCRT = type helper(TIntPointHelper) for TIntPoint

function TIntPointHelperCRT.IsItOnScreen: Boolean; // Console screen
begin
  Result:=IsPointOnScreen(Self, False);
end;

// TFloatPointHelperCRT = type helper(TFloatPointHelper) for TFloatPoint

function TFloatPointHelperCRT.IsItOnScreen: Boolean; // Console screen
begin
  Result:=IsPointOnScreen(Self, False);
end;

{$IFDEF WINDOWSMODE}
procedure VSCRTWinModeSetup(AScreenCanvas: TCanvas;
  AProcessMessages: TProcessMessages;
  AScreenWidth: Word; AScreenHeight: Word);
begin
  ScreenCanvas:=AScreenCanvas;
  ProcessMessages:=AProcessMessages;
  ScreenWidth:=AScreenWidth;
  ScreenHeight:=AScreenHeight;
end;
{$ENDIF}

function KeyPressed: Boolean;
begin
  {$IFDEF WINDOWSMODE}
  (*if not DoProcessMessages then
    Exit(False);
  {if ProcessMessages = nil then
    Exit(False)
  else
    ProcessMessages();}
  Result:=not KeysQueue.IsEmpty;*)

  Result:=DoProcessMessages and not KeysQueue.IsEmpty;
  {$ELSE}
  Result:=CRT.KeyPressed;
  {$ENDIF}
end;

function GetKey: TKeyCode;
begin
  {$IFDEF WINDOWSMODE}
  StopGetKey:=False;
  {while not KeyPressed and (ProcessMessages <> nil) and not StopGetKey do
    ProcessMessages();}
  while not KeyPressed and DoProcessMessages and not StopGetKey do;
  if not KeysQueue.IsEmpty then
    Result:=KeysQueue.Get
  else
    Result:=0;
  {$ELSE}
  Result:=Ord(ReadKey);
  if Result = 0 then
    Result:=Ord(ReadKey)+OneByte;//-Ord(ReadKey);
  {$ENDIF}
end;

function IsGetKeyEscape: Boolean;
begin
  Result:=GetKey = KeyEscape;
end;

procedure GetKeyEscape;
begin
  while GetKey <> KeyEscape do;
end;

procedure GetOneOfTheseKeys(constref AKeys: TKeys);
begin
  if Length(AKeys) = 0 then
    Exit;
  while not (GetKey in AKeys) do ;
end;

procedure CursorOn;
begin
  {$IFNDEF WINDOWSMODE}
  CRT.CursorOn;
  {$ENDIF}
end;

procedure CursorOff;
begin
  {$IFNDEF WINDOWSMODE}
  CRT.CursorOff;
  {$ENDIF}
end;

function GoToXY(AX, AY: Integer): Boolean;
begin
  {$IFDEF WINDOWSMODE}
  Result:=True;
  {$ELSE}
  //Result:=IsXOnScreen(AX) and IsYOnScreen(AY);
  Result:=IsPointOnScreen(AX, AY);
  if Result then
    CRT.GoToXY(AX, AY);
  {$ENDIF}
end;

function GoToXY(constref APoint: TIntPoint): Boolean;
begin
  Result:=GoToXY(APoint.X, APoint.Y);
end;

function WhereX: Byte;
begin
  {$IFNDEF WINDOWSMODE}
  Result:=CRT.WhereX;
  {$ELSE}
  Result:=0;
  {$ENDIF}
end;

function WhereY: Byte;
begin
  {$IFNDEF WINDOWSMODE}
  Result:=CRT.WhereY;
  {$ELSE}
  Result:=0;
  {$ENDIF}
end;

procedure ClrScr;
begin
  {$IFDEF WINDOWSMODE}
  if ScreenCanvas = nil then
    Exit;
  WriteStrings(FillStrings(' ', ScreenWidth, ScreenHeight), 0);
  {$ELSE}
  CRT.ClrScr;
  {$ENDIF}
end;

{$IFDEF WINDOWSMODE}
function CRTColorToWin(AColor: Byte): TColor;
begin
  {case AColor of
    0: Result:=clBlack;
    1: Result:=clMaroon;
    2: Result:=clGreen;
    3: Result:=clOlive;
    4: Result:=clNavy;
    5: Result:=clPurple;
    6: Result:=clTeal;
    7: Result:=clGray;
    8: Result:=clSilver;
    9: Result:=clRed;
    10: Result:=clLime;
    11: Result:=clYellow;
    12: Result:=clBlue;
    13: Result:=clFuchsia;
    14: Result:=clAqua;
    15: Result:=clWhite;
    else Result:=AColor;
  end;}
  case AColor of
    Black: Result:=clBlack;
    Blue: Result:=clNavy;
    Green: Result:=clGreen;
    Cyan: Result:=clTeal;
    Red: Result:=clMaroon;
    Magenta: Result:=clPurple;
    Brown: Result:=clOlive;
    LightGray: Result:=clSilver;

    DarkGray: Result:=clGray;
    LightBlue: Result:=clBlue;
    LightGreen: Result:=clLime;
    LightCyan: Result:=clAqua;
    LightRed: Result:=clRed;
    LightMagenta: Result:=clFuchsia;
    Yellow: Result:=clYellow;
    White: Result:=clWhite;
    else Result:=AColor;
  end;
  {clBlack   = TColor($000000);
  clMaroon  = TColor($000080);
  clGreen   = TColor($008000);
  clOlive   = TColor($008080);
  clNavy    = TColor($800000);
  clPurple  = TColor($800080);
  clTeal    = TColor($808000);
  clGray    = TColor($808080);
  clSilver  = TColor($C0C0C0);
  clRed     = TColor($0000FF);
  clLime    = TColor($00FF00);
  clYellow  = TColor($00FFFF);
  clBlue    = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua    = TColor($FFFF00);
  clLtGray  = TColor($C0C0C0); // clSilver alias
  clDkGray  = TColor($808080); // clGray alias
  clWhite   = TColor($FFFFFF); }
  {Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;}
end;
{$ENDIF}

procedure TextColor(AColor: Byte);
begin
  {$IFDEF WINDOWSMODE}
  if ScreenCanvas = nil then
    Exit;
  ScreenCanvas.Font.Color:={AColor;//}CRTColorToWin(AColor);
  {$ELSE}
  CRT.TextColor(AColor);
  {$ENDIF}
end;

procedure TextBackground(AColor: Byte);
begin
  {$IFDEF WINDOWSMODE}
  if ScreenCanvas = nil then
    Exit;
  ScreenCanvas.Brush.Color:={AColor;//}CRTColorToWin(AColor);
  {$ELSE}
  CRT.TextBackground(AColor);
  {$ENDIF}
end;

function IsXOnScreen(AX: TIntCoord): Boolean;
begin
  //Result:=IsIntInRange(AX, 1, ScreenWidth);
  Result:=(AX >= 1) and (AX <= ScreenWidth);
end;

function IsYOnScreen(AY: TIntCoord): Boolean;
begin
  //Result:=IsIntInRange(AY, 1, ScreenHeight);
  Result:=(AY >= 1) and (AY <= ScreenHeight);
end;

function IsPointOnScreen(AX, AY: TIntCoord;
  AIncludingBottomRight: Boolean = False): Boolean;
begin
  Result:=//IsPointInRectangle(AX, AY, 1, 1, ScreenWidth, ScreenHeight) and
    IsXOnScreen(AX) and IsYOnScreen(AY) and
    not ((AX = ScreenWidth) and (AY = ScreenHeight) and
    not AIncludingBottomRight);
  {Result:=IsItInRectangle(X, Y, WindMinX, WindMinY, WindMaxX, WindMaxY) and
    not ((X=WindMaxX) and (Y=WindMaxY) and (not IncludingBottomRight));}
end;

function IsPointOnScreen(constref APoint: TIntPoint;
  AIncludingBottomRight: Boolean = False): Boolean;
begin
  Result:=IsPointOnScreen(APoint.X, APoint.Y, AIncludingBottomRight);
end;

function WriteStringXY(AX, AY: TIntCoord; constref AString: TString): Boolean;
begin
  {$IFDEF WINDOWSMODE}
  if ScreenCanvas = nil then
    Exit(False);
  Result:=True;
  with ScreenCanvas do
    TextOut((AX-1)*TextWidth(' '), (AY-1)*TextHeight(' '), AString);
  DoRepaint;
  {$ELSE}
  Result:=GoToXY(AX, AY);
  if Result then
  begin
    Write(AString);
    //Flush(Output);
  end;
  {$ENDIF}
end;

function WriteStringXY(constref APosition: TIntPoint;
  constref AString: TString): Boolean;
begin
  Result:=WriteStringXY(APosition.X, APosition.Y, AString);
end;

procedure WriteStringXYInRect(
  AX, AY: TIntCoord; constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord =1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight;
  ADontWriteInRightBottomCorner: Boolean = True);
var
  TempStr: TString;
begin
  if {$IFDEF WINDOWSMODE}
    (ScreenCanvas = nil) or //??
    {$ENDIF}
    //not IsIntInRange(Y, ATop, ABottom) or
    ((AY < ATop) or (AY > ABottom)) or
    not IsYOnScreen(AY) or
    (AX > ScreenWidth) or (AX > ARight) or
    (ADontWriteInRightBottomCorner and
    (AX = ScreenWidth) and (AY = ScreenHeight)) then
    Exit;

  TempStr:=AString;
  if AX < ALeft then
  begin
    Delete(TempStr, 1, ALeft-AX);
    AX:=ALeft;
  end;

  if ADontWriteInRightBottomCorner and (AY = ScreenHeight) and
    (ARight = ScreenWidth) then
    ARight:=ARight-1;

  if AX+Length(TempStr)-1 > ARight then
    Delete(TempStr, ARight-AX+2, (AX+Length(TempStr)-1)-ARight);

  if Length(TempStr) > 0 then
  begin
    {GoToXY(AX, AY);
    Write(TempStr);}
    WriteStringXY(AX, AY, TempStr);
  end;
end;

procedure WriteStringXYInRect(
  constref APosition: TIntPoint; constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord =1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight;
  ADontWriteInRightBottomCorner: Boolean = True);
begin
  WriteStringXYInRect(APosition.X, APosition.Y, AString, ALeft, ATop,
    ARight, ABottom, ADontWriteInRightBottomCorner);
end;

procedure WriteStrings(constref AStrings: TStrings; ALineInterval: Byte = 0);
var
  i, j: Integer;
begin
  {$IFDEF WINDOWSMODE}
  for i:=Low(AStrings) to High(AStrings) do
    WriteStringXY(1, i*(ALineInterval+1)+1, AStrings[i]);
  {$ELSE}
  for i:=0 to High(AStrings) do
  begin
    WriteLn(AStrings[i]);
    for j:=1 to ALineInterval do
      WriteLn;
  end;
  {$ENDIF}
end;

procedure WriteStringsXY(
  AX, AY: TIntCoord; constref AStrings: TStrings;
  AColCopyNumber: SmallInt = 1; ARowCopyNumber: SmallInt = 1;
  ALineInterval: Byte = 0; ACentered: Boolean = False;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight);
var
  i, j, k, n: Integer;
  LStringsWidth, LStringsHeight: Word;
begin
  {$IFDEF WINDOWSMODE}
  if ScreenCanvas = nil then
    Exit;
  {$ENDIF}
  LStringsHeight:=Length(AStrings);
  LStringsWidth:=StringsMaxLength(AStrings);
  if (LStringsHeight = 0) or (LStringsWidth = 0) or
     (AColCopyNumber <= 0) or (ARowCopynumber <= 0) then
    Exit;

  for j:=0 to ARowCopyNumber-1 do
    for i:=0 to AColCopyNumber-1 do
      for k:=0 to High(AStrings) do
      begin
        // This is not good, I guess
        if ACentered then
          n:=Round((LStringsWidth-Length(AStrings[k]))/2)
        else
          n:=0;
        if (AX+LStringsWidth*i+n > ScreenWidth) or
           (AY+LStringsHeight*j+k*(ALineInterval+1) > ScreenHeight) then
          Continue;
        WriteStringXYInRect(
          AX+LStringsWidth*i+n, AY+LStringsHeight*j+k*(ALineInterval+1),
          AStrings[k], ALeft, ATop, ARight, ABottom, True);
      end;
end;

procedure WriteStringsXY(
  constref APosition: TIntPoint; constref AStrings: TStrings;
  AColCopyNumber: SmallInt = 1; ARowCopyNumber: SmallInt = 1;
  ALineInterval: Byte = 0; ACentered: Boolean = False;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  ARight: TIntCoord = DefaultScreenWidth;
  ABottom: TIntCoord = DefaultScreenHeight);
begin
  WriteStringsXY(APosition.X, APosition.Y, AStrings, AColCopyNumber,
    ARowCopyNumber, ALineInterval, ACentered, ALeft, ATop, ARight, ABottom);
end;

function GetPosForScreenCenter(constref AStrings: TStrings): TIntPoint;
begin
  Result:=GetPosForCenter(AStrings, 1, 1, ScreenWidth, ScreenHeight);
end;

(*function VSDelay(ADelayTime: LongWord): Boolean;
var
  LeadTime: QWord;
begin
  LeadTime:=GetTickCount64;
  while GetTickCount64-LeadTime < ADelayTime do
    if KeyPressed {and GetKey = KeySomething} then
    begin
      //GetKey;//??
      Exit(False);
    end;
  Result:=True;
end;*)

// Wait without stop (AStopIfKeyPressed = False)
// Stop if (any) key pressed (AStopKeys is empty ([] or nil)
// Stop if pressed one of the keys (AStopKeys)

function Wait(ADelayTime: {Word}Integer; AStopIfKeyPressed: Boolean = False;
  constref AStopKeys: TKeys = nil): Boolean;
var
  LeadTime: {Q}LongWord;
begin
  //KeysQueue.Clear;
  if ADelayTime <= 0 then
    Exit(True);

  LeadTime:=GetTickCount{64};
  while GetTickCount{64}-LeadTime < ADelayTime do
    if AStopIfKeyPressed and KeyPressed and
      ({(AStopKeys = nil) or} (Length(AStopKeys) = 0) or
      (GetKey in AStopKeys)) then
    begin
      if {(AStopKeys = nil) or} (Length(AStopKeys) = 0) then
        GetKey;
      //ProcessMessages();
      Exit(False);
    end;
  Result:=True;
end;

{function Wait(ADelayTime: Word; AStartTime: LongWord;
  AStopIfKeyPressed: Boolean = False;
  constref AStopKeys: TKeys = nil): Boolean;
var
  t: Integer;
begin
  t:=ADelayTime-(GetTickCount64-AStartTime);
  if t >= 0 then
    Result:=Wait(t, AStopIfKeyPressed, AStopKeys)
  else
    Result:=True;
end;}

{function Wait2(ADelayTime: Word; AStopIfKeyPressed: Boolean = False;
  constref AStopKeys: TKeySet = []): Boolean;
var
  LeadTime: QWord;
begin
  LeadTime:=GetTickCount64;
  while GetTickCount64-LeadTime < ADelayTime do
    if AStopIfKeyPressed and KeyPressed and
      ((AStopKeys = []) or (GetKey in AStopKeys)) then
    begin
      if AStopKeys = [] then
        GetKey;
      Exit(False);
    end;
  Result:=True;
end;}

operator in (constref ALeft: TKeyCode; constref ARight: TKeys): Boolean;
var
  i: Integer;
begin
  for i:=Low(ARight) to High(ARight) do
    if ALeft = ARight[i] then
      Exit(True);
  Result:=False;
end;

procedure SetKeyNames(var AKeyNames: TKeyNames);
const
  PreName = '#';
var
  LKey: TKeyCode;
begin
  for LKey:=Low(LKey) to High(LKey) do
    //if (LKey >= 0) {and (Chr(LKey) in ['a'..'z', 'A'..'Z', '0'..'9', ''..''])} then
    if LKey < OneByte then
      AKeyNames[LKey]:=Chr(LKey)
    else
      AKeyNames[LKey]:=PreName+IntToStr(LKey);

  // Key codes
  // arrow controls
  AKeyNames[KeyUp]:='^';//'Arrow up';
  AKeyNames[KeyDown]:='v';//'Arrow down';
  AKeyNames[KeyRight]:='->';//'Arrow right';
  AKeyNames[KeyLeft]:='<-';//'Arrow left';
  // "Pop" keys
  AKeyNames[KeySpace]:='Space';
  AKeyNames[KeyEnter]:='Enter';
  {AKeyNames[KeyDelete]:='Delete';
  AKeyNames[KeyPlus]:='+';
  AKeyNames[KeyMinus]:='-';}
  AKeyNames[KeyEscape]:='Escape';
  AKeyNames[KeyBackspace]:='B.space';//'Backspace';
  AKeyNames[KeyTab]:='Tab';
  //AKeyNames[KeyHome]:='Home';
  // Numbers
  {Key0 = 48;
  Key1 = 49;
  Key2 = 50;
  Key3 = 51;
  Key4 = 52;
  Key5 = 53;
  Key6 = 54;
  Key7 = 55;
  Key8 = 56;
  Key9 = 57;}
  // Letters small
  {KeyA = 97;
  KeyW = 119;
  KeyS = 115;
  KeyD = 100;
  KeyQ = 113;
  KeyY = 121;
  KeyN = 110;
  KeyP = 112;}
  // Letters big (with Shift or Caps Lock)
  {KeyBigA = 65;
  KeyBigW = 87;
  KeyBigS = 83;
  KeyBigD = 68;
  KeyBigQ = 81;
  KeyBigY = 89;
  KeyBigN = 78;
  KeyBigP = 80;}
  // Shift + number
  {KeyShift1 = 33; // !
  KeyShift2 = 64; // @
  KeyShift3 = 35; // #
  KeyShift4 = 36; // $
  KeyShift5 = 37; // %
  KeyShift6 = 94; // ^
  KeyShift7 = 38; // &
  KeyShift8 = 42; // * KeyAsterisk = 42 //Shift+8(*)
  KeyShift9 = 40; // (
  KeyShift0 = 41; // )}
  // Ctrl + number
  {KeyCtrl1 = -2; //
  KeyCtrl2 = -3; //
  KeyCtrl3 = -4; //
  KeyCtrl4 = -5; //
  KeyCtrl5 = -6; //
  KeyCtrl6 = -7; //
  KeyCtrl7 = -8; //
  KeyCtrl8 = -9; //
  KeyCtrl9 = -10; //
  KeyCtrl0 = -11; //}
  // "F" keys
  {AKeyNames[KeyF1]:='F1';
  AKeyNames[KeyF2]:='F2';
  AKeyNames[KeyF3]:='F3';
  AKeyNames[KeyF4]:='F4';
  AKeyNames[KeyF5]:='F5';
  AKeyNames[KeyF6]:='F6';
  AKeyNames[KeyF7]:='F7';
  AKeyNames[KeyF8]:='F8';
  AKeyNames[KeyF9]:='F9';
  AKeyNames[KeyF10]:='F10';
  AKeyNames[KeyF11]:='F11';
  AKeyNames[KeyF12]:='F12';}
end;


initialization
  {$IFNDEF WINDOWSMODE}
  ScreenWidth:=WindMaxX-WindMinX+1;
  ScreenHeight:=WindMaxY-WindMinY+1;
  {$ENDIF}

  SetKeyNames(DefaultKeyNames);

finalization
  {$IFDEF WINDOWSMODE}
  KeysQueue.Clear;
  {$ENDIF}
end.
