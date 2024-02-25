(*
  (c) Vitaly Smirnov (VSdev)
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSTextVisualEffects;

{$mode objfpc}
//{$modeswitch advancedrecords}
{$modeswitch typehelpers}
//{$I-}
{$H+} // String is AnsiString
//{$J}
//{$B}

interface

uses
  VSStrings, VSCRT;

type
  TStringsVisualEffects = type helper(TStringsHelper) for TStrings
  public
    function TVEFallDownFromTo(
      AFrom: Word = 0; ATo: Word = High(Word);
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
    function TVEFallUpFromTo(
      AFrom: Word = High(Word); ATo: Word = 0;
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
    function TVEFallDown(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
    function TVEFallUp(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
    function TVEFallDownErase(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AWidth: TIntCoord = DefaultScreenWidth;
      AHeight: TIntCoord = DefaultScreenHeight;
      ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
    function TVEFallUpErase(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AWidth: TIntCoord = DefaultScreenWidth;
      AHeight: TIntCoord = DefaultScreenHeight;
      ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
    function TVEFallDownWithFrame(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AFillChar: Char = #0;
      AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
      ADrawLastLineFrame: Boolean = True): Boolean;
    function TVEFallUpWithFrame(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
      AFillChar: Char = #0;
      AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
      ADrawLastLineFrame: Boolean = True): Boolean;
    function TVEFallDownLine(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
      ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
    function TVEFallUpLine(
      ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
      ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
      AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
  end;

// They return False if they have finished earlier then expected

function TVETypeLine(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVETypeLineBack(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEExpandLine(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVECollapseLine(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;

function TVEFallDownFromTo(constref AStrings: TStrings;
  AFrom: Word = 0; ATo: Word = High(Word);
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer{Word} = 30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEFallUpFromTo(constref AStrings: TStrings;
  AFrom: Word = High(Word); ATo: Word = 0;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEFallDown(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEFallUp(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEFallDownErase(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AWidth: TIntCoord = DefaultScreenWidth;
  AHeight: TIntCoord = DefaultScreenHeight;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEFallUpErase(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AWidth: TIntCoord = DefaultScreenWidth;
  AHeight: TIntCoord = DefaultScreenHeight;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEFallDownWithFrame(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0;
  AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
  ADrawLastLineFrame: Boolean = True): Boolean;
function TVEFallUpWithFrame(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0;
  AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
  ADrawLastLineFrame: Boolean = True): Boolean;
function TVEFallDownLine(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
function TVEFallUpLine(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
// It's strange
function TVEFallDownGlare(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; //ALineWidth: Byte = 3;
  AGlareChar: Char = '.'; AExceptChar: Char = ' '; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
{function TVEFallUpGlare(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; //ALineWidth: Byte = 3;
  AGlareChar: Char = '.'; AExceptChar: Char = ' '; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;}


implementation

uses
  SysUtils,
  VSUtils;


function TVETypeLine(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  t: LongWord;
begin
  Result:=True;
  {if AString.IsEmpty then
    Exit;}
  for i:=Low(AString) to High(AString) do
  begin
    t:=GetTickCount;
    WriteStringXYInRect(ALeft+i-Low(AString), ATop, AString[i]);
    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
end;

function TVETypeLineBack(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  t: LongWord;
begin
  Result:=True;
  {if AString.IsEmpty then
    Exit;}
  for i:=High(AString) downto Low(AString) do
  begin
    t:=GetTickCount;
    WriteStringXYInRect(ALeft+i-Low(AString), ATop, AString[i]);
    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
end;

function TVEExpandLine(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  LHalfStr: Word;
  t: LongWord;
begin
  Result:=True;
  if AString.Length = 0 then
    Exit;

  LHalfStr:=(AString.Length+1) div 2;
  for i:=Low(AString) to LHalfStr do
  begin
    t:=GetTickCount;
    WriteStringXYInRect(ALeft+LHalfStr-i, ATop,
      Copy(AString, LHalfStr-i+1, i*2-Byte(Odd(AString.Length))));

    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
end;

function TVECollapseLine(constref AString: TString;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  LStrLen: Word;
  t: LongWord;
begin
  Result:=True;
  if AString.Length = 0 then
    Exit;

  LStrLen:=AString.Length;
  for i:=0 to (LStrLen-1) div 2 do
  begin
    t:=GetTickCount;
    WriteStringXYInRect(ALeft+i, ATop, Copy(AString, i+1, 1));
    WriteStringXYInRect(ALeft+LStrLen-i-1, ATop, Copy(AString, LStrLen-i, 1));

    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
end;

function TVEFallDownFromTo(constref AStrings: TStrings;
  AFrom: Word = 0; ATo: Word = High(Word);
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: {Word}Integer = 30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  Fills: AnsiString;//TString;
  t: LongWord;
begin
  Result:=True;
  if (Length(AStrings) = 0) or {(AStrings.MaxLength = 0) or}
    (AFrom > High(AStrings)) or (AFrom > ATo) then
  begin
    Exit;
  end;
  if ATo > High(AStrings) then
    ATo:=High(AStrings);

  //Fills:=FillString(AFillChar, StringsMaxLength(AStrings));
  Fills:=FillString(AFillChar, AStrings[Low(AStrings)].Length);
  for i:=AFrom to ATo do
  begin
    t:=GetTickCount{64};
    if i > AFrom then
      WriteStringXYInRect(ALeft, ATop+i-1, AStrings[i-1]);
    if AFillChar = #0 then
      WriteStringXYInRect(ALeft, ATop+i, AStrings[i])
    else
    begin
      if Fills.Length <> AStrings[i].Length then
        Fills:=FillString(AFillChar, AStrings[i].Length);
      WriteStringXYInRect(ALeft, ATop+i, Fills);
    end;

    if Result and not Wait(ADelayTime-(GetTickCount{64}-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
  WriteStringXYInRect(ALeft, ATop+ATo, AStrings[ATo]);
  //Wait(ADelayTime, False); //??
end;

function TVEFallUpFromTo(constref AStrings: TStrings;
  AFrom: Word = High(Word); ATo: Word = 0;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  Fills: AnsiString;//TString;
  t: LongWord;
begin
  Result:=True;
  if (Length(AStrings) = 0) or {(AStrings.MaxLength = 0) or}
    (ATo > High(AStrings)) or (AFrom < ATo) then
  begin
    Exit;
  end;
  if AFrom > High(AStrings) then
    AFrom:=High(AStrings);

  //Fills:=FillString(AFillChar, StringsMaxLength(AStrings));
  Fills:=FillString(AFillChar, AStrings[AFrom].Length);
  for i:=AFrom downto ATo do
  begin
    t:=GetTickCount;
    if i < AFrom then
      WriteStringXYInRect(ALeft, ATop+i+1, AStrings[i+1]);
    if AFillChar = #0 then
      WriteStringXYInRect(ALeft, ATop+i, AStrings[i])
    else
    begin
      if Fills.Length <> AStrings[i].Length then
        Fills:=FillString(AFillChar, AStrings[i].Length);
      WriteStringXYInRect(ALeft, ATop+i, Fills);
    end;

    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
  WriteStringXYInRect(ALeft, ATop+ATo, AStrings[ATo]);
  //Wait(ADelayTime, False);//??
end;

function TVEFallDown(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=TVEFallDownFromTo(AStrings, Low(AStrings), High(AStrings),
    ALeft, ATop, AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TVEFallUp(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=TVEFallUpFromTo(AStrings, High(AStrings), Low(AStrings), ALeft, ATop,
    AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TVEFallDownErase(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AWidth: TIntCoord = DefaultScreenWidth;
  AHeight: TIntCoord = DefaultScreenHeight;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=TVEFallDownFromTo(FillStrings(ABackgroundChar, AWidth, AHeight),
    0, AHeight, ALeft, ATop, AFillChar, ADelayTime,
    AStopIfKeyPressed, AStopKeys);
end;

function TVEFallUpErase(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AWidth: TIntCoord = DefaultScreenWidth;
  AHeight: TIntCoord = DefaultScreenHeight;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=TVEFallUpFromTo(FillStrings(ABackgroundChar, AWidth, AHeight),
    AHeight, 0, ALeft, ATop, AFillChar, ADelayTime,
    AStopIfKeyPressed, AStopKeys);
end;

function TVEFallDownWithFrame(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0;
  AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
  ADrawLastLineFrame: Boolean = True): Boolean;
  procedure SetZeroDelay;
  begin
    AHorizontalDelay:=0;
    AVerticalDelay:=0;
    Result:=False;
  end;
  var
    LStr: AnsiString;
    n: Byte;
begin
  Result:=True;
  if Length(AStrings) = 0 {or (AStrings.MaxLength = 0)} then
    Exit;

  if AFillChar = #0 then
    LStr:=AStrings[Low(AStrings)]
  else
    LStr:=FillString(AFillChar, AStrings[Low(AStrings)].Length);
  if not TVEExpandLine(LStr, ALeft, ATop, AHorizontalDelay,
    AStopIfKeyPressed, AStopKeys) then
    SetZeroDelay;
  WriteStringXYInRect(ALeft, ATop, AStrings[Low(AStrings)]);

  n:=Byte(ADrawLastLineFrame); //!
  if not TVEFallDownFromTo(AStrings, Low(AStrings)+1, High(AStrings)-n,
    ALeft, ATop, AFillChar, AVerticalDelay, AStopIfKeyPressed, AStopKeys) then
    SetZeroDelay;

  if not ADrawLastLineFrame then
    Exit;

  if AFillChar <> #0 then
    WriteStringXYInRect(ALeft, ATop+High(AStrings),
      FillString(AFillChar, AStrings[High(AStrings)].Length));
  if not TVECollapseLine(AStrings[High(AStrings)], ALeft, ATop+High(AStrings),
    AHorizontalDelay, AStopIfKeyPressed, AStopKeys) then
    Result:=False;
end;

function TVEFallUpWithFrame(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0;
  AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
  ADrawLastLineFrame: Boolean = True): Boolean;
  procedure SetZeroDelay;
  begin
    AHorizontalDelay:=0;
    AVerticalDelay:=0;
    Result:=False;
  end;
  var
    LStr: AnsiString;
    n: Byte;
begin
  Result:=True;
  if Length(AStrings) = 0 {or (AStrings.MaxLength = 0)} then
    Exit;

  if AFillChar = #0 then
    LStr:=AStrings[High(AStrings)]
  else
    LStr:=FillString(AFillChar, AStrings[High(AStrings)].Length);
  if not TVEExpandLine(LStr, ALeft, ATop+High(AStrings), AHorizontalDelay,
    AStopIfKeyPressed, AStopKeys) then
    SetZeroDelay;
  WriteStringXYInRect(ALeft, ATop+High(AStrings), AStrings[High(AStrings)]);

  n:=Byte(ADrawLastLineFrame); //!
  if not TVEFallUpFromTo(AStrings, High(AStrings)-1, Low(AStrings)+n,
    ALeft, ATop, AFillChar, AVerticalDelay, AStopIfKeyPressed, AStopKeys) then
    SetZeroDelay;

  if not ADrawLastLineFrame then
    Exit;

  if AFillChar <> #0 then
    WriteStringXYInRect(ALeft, ATop,
      FillString(AFillChar, AStrings[Low(AStrings)].Length));
  if not TVECollapseLine(AStrings[Low(AStrings)], ALeft, ATop,
    AHorizontalDelay, AStopIfKeyPressed, AStopKeys) then
    Result:=False;
end;

function TVEFallDownLine(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  t: LongWord;
  LFills, LBgFills: AnsiString;
begin
  Result:=True;
  if (Length(AStrings) = 0) {or (ALineWidth = 0)} then
    Exit;

  LFills:='';//FillString(AFillChar, AStrings[Low(AStrings)].Length);
  LBgFills:='';
  for i:=Low(AStrings) to High(AStrings)+ALineWidth do
  begin
    t:=GetTickCount;
    if i-ALineWidth >= Low(AStrings) then
    begin
      if LBgFills.Length <> AStrings[i-ALineWidth].Length then
        LBgFills:=FillString(ABackgroundChar, AStrings[i-ALineWidth].Length);
      WriteStringXYInRect(ALeft, ATop+i-ALineWidth, LBgFills);
    end;

    if IsIntInRange(i-1, Low(AStrings), High(AStrings)) then
      WriteStringXYInRect(ALeft, ATop+i-1, AStrings[i-1]);

    if i <= High(AStrings) then
    begin
      if AFillChar = #0 then
        WriteStringXYInRect(ALeft, ATop+i, AStrings[i])
      else
      begin
        if LFills.Length <> AStrings[i].Length then
          LFills:=FillString(AFillChar, AStrings[i].Length);
        WriteStringXYInRect(ALeft, ATop+i, LFills);
      end;
    end;

    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
end;

function TVEFallUpLine(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  t: LongWord;
  LFills, LBgFills: AnsiString;
begin
  Result:=True;
  if (Length(AStrings) = 0) {or (ALineWidth = 0)} then
    Exit;

  LFills:='';//FillString(AFillChar, AStrings[Low(AStrings)].Length);
  LBgFills:='';
  for i:=High(AStrings) downto Low(AStrings)-ALineWidth do
  begin
    t:=GetTickCount;
    if i+ALineWidth <= High(AStrings) then
    begin
      if LBgFills.Length <> AStrings[i+ALineWidth].Length then
        LBgFills:=FillString(ABackgroundChar, AStrings[i+ALineWidth].Length);
      WriteStringXYInRect(ALeft, ATop+i+ALineWidth, LBgFills);
    end;

    if IsIntInRange(i+1, Low(AStrings), High(AStrings)) then
      WriteStringXYInRect(ALeft, ATop+i+1, AStrings[i+1]);

    if i >= Low(AStrings) then
    begin
      if AFillChar = #0 then
        WriteStringXYInRect(ALeft, ATop+i, AStrings[i])
      else
      begin
        if LFills.Length <> AStrings[i].Length then
          LFills:=FillString(AFillChar, AStrings[i].Length);
        WriteStringXYInRect(ALeft, ATop+i, LFills);
      end;
    end;

    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
end;

function TVEFallDownGlare(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; //ALineWidth: Byte = 3;
  AGlareChar: Char = '.'; AExceptChar: Char = ' '; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
var
  i: Integer;
  t: LongWord;
begin
  Result:=True;
  if Length(AStrings) = 0 then
    Exit;

  for i:=Low(AStrings) to High(AStrings)+1 do
  begin
    t:=GetTickCount;
    if i-1 >= Low(AStrings) then
      WriteStringXYInRect(ALeft, ATop+i-1, AStrings[i-1]);

    if i <= High(AStrings) then
      WriteStringXYInRect(ALeft, ATop+i,
        ReplaceCharsExcept(AStrings[i], AGlareChar, AExceptChar));

    if Result and not Wait(ADelayTime-(GetTickCount-t),
      AStopIfKeyPressed, AStopKeys) then
    begin
      ADelayTime:=0;
      Result:=False;
    end;
  end;
end;

{function TVEFallUpGlare(constref AStrings: TStrings;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; //ALineWidth: Byte = 3;
  AGlareChar: Char = '.'; AExceptChar: Char = ' '; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
end;}


// TStringsVisualEffects = type helper(TStringsHelper) for TStrings

function TStringsVisualEffects.TVEFallDownFromTo(
  AFrom: Word = 0; ATo: Word = High(Word);
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallDownFromTo(Self, AFrom, ATo,
    ALeft, ATop, AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TStringsVisualEffects.TVEFallUpFromTo(
  AFrom: Word = High(Word); ATo: Word = 0;
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallUpFromTo(Self, AFrom, ATo,
    ALeft, ATop, AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TStringsVisualEffects.TVEFallDown(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallDown(Self, ALeft, ATop,
    AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TStringsVisualEffects.TVEFallUp(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallUp(Self, ALeft, ATop,
    AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TStringsVisualEffects.TVEFallDownErase(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AWidth: TIntCoord = DefaultScreenWidth;
  AHeight: TIntCoord = DefaultScreenHeight;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallDownErase(ALeft, ATop, AWidth, AHeight,
    ABackgroundChar, AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TStringsVisualEffects.TVEFallUpErase(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AWidth: TIntCoord = DefaultScreenWidth;
  AHeight: TIntCoord = DefaultScreenHeight;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallUpErase(ALeft, ATop, AWidth, AHeight,
    ABackgroundChar, AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TStringsVisualEffects.TVEFallDownWithFrame(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0;
  AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
  ADrawLastLineFrame: Boolean = True): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallDownWithFrame(Self, ALeft, ATop,
    AFillChar, AHorizontalDelay, AVerticalDelay,
    AStopIfKeyPressed, AStopKeys, ADrawLastLineFrame);
end;

function TStringsVisualEffects.TVEFallUpWithFrame(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1;
  AFillChar: Char = #0;
  AHorizontalDelay: Integer =  10; AVerticalDelay: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil;
  ADrawLastLineFrame: Boolean = True): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallUpWithFrame(Self, ALeft, ATop,
    AFillChar, AHorizontalDelay, AVerticalDelay,
    AStopIfKeyPressed, AStopKeys, ADrawLastLineFrame);
end;

function TStringsVisualEffects.TVEFallDownLine(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallDownLine(Self, ALeft, ATop, ALineWidth,
    ABackgroundChar, AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;

function TStringsVisualEffects.TVEFallUpLine(
  ALeft: TIntCoord = 1; ATop: TIntCoord = 1; ALineWidth: Byte = 3;
  ABackgroundChar: Char = ' '; AFillChar: Char = #0; ADelayTime: Integer =  30;
  AStopIfKeyPressed: Boolean = False; AStopKeys: TKeys = nil): Boolean;
begin
  Result:=VSTextVisualEffects.TVEFallUpLine(Self, ALeft, ATop, ALineWidth,
    ABackgroundChar, AFillChar, ADelayTime, AStopIfKeyPressed, AStopKeys);
end;


///////////// Experimental. Not finished! /////////////////////

procedure VisualEffect7(constref AStrings: TStrings;
  ALeft: Integer = 1; ATop: Integer = 1;
  AHeight: Byte = 3; ADelay: Integer =  30);
var
  i: Integer;
begin
  if (Length(AStrings) = 0) or (AHeight = 0) then
    Exit;
  //TextBackground(Black);//??

  for i:=Low(AStrings) to High(AStrings)+AHeight do
  begin
    if i-AHeight >= Low(AStrings) then
      WriteStringXYInRect(ALeft, ATop+i-AHeight,
        FillString(' ', Length(AStrings[i-AHeight])));

    if IsIntInRange(i-1, Low(AStrings), High(AStrings)) then
    begin
      TextColor(Green);
      WriteStringXYInRect(ALeft, ATop+i-1, AStrings[i-1]);
    end;

    if i <= High(AStrings) then
    begin
      TextColor(LightGreen);
      WriteStringXYInRect(ALeft, ATop+i,
        ReplaceCharsExcept(AStrings[i], '.', ' '));
    end;

    //ReadLn;
    Wait(ADelay);
  end;
end;

procedure VisualEffect8(constref AStrings: TStrings;
  ALeft: Integer = 1; ATop: Integer = 1;
  AHeight: Byte = 3; ADelay: Integer =  30);
var
  i: Integer;
begin
  if (Length(AStrings) = 0) or (AHeight = 0) then
    Exit;
  //TextBackground(Black);//??

  for i:=High(AStrings) downto Low(AStrings)-AHeight do
  begin
    if i+AHeight <= High(AStrings) then
      WriteStringXYInRect(ALeft, ATop+i+AHeight,
        FillString(' ', Length(AStrings[i+AHeight])));

    if IsIntInRange(i+1, Low(AStrings), High(AStrings)) then
    begin
      TextColor(Green);
      WriteStringXYInRect(ALeft, ATop+i+1, AStrings[i+1]);
    end;

    if i >= Low(AStrings) then
    begin
      TextColor(LightGreen);
      WriteStringXYInRect(ALeft, ATop+i,
        ReplaceCharsExcept(AStrings[i], '.', ' '));
    end;

    //ReadLn;
    Wait(ADelay);
  end;
end;


end.
