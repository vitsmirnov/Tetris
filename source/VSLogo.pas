(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSLogo;

interface

uses
  VSCRT, VSStrings;

//{$J-}
const
  {$I VSLogoData}

  VSLogoBackgroundColor = Black;
  VSLogoTextColor = Green;

type
  // -1 is something like nil for pointers
  TVSLogoColor = -1..255;//15;

procedure PlayVSLogo1(AStopIfKeyPressed: Boolean = True;
  constref AStopKeys: TKeys = nil; ADoClrScr: Boolean = True;
  ARestoreBgColor: TVSLogoColor = -1; ARestoreTxtColor: TVSLogoColor = -1);
procedure PlayVSLogo2(AStopIfKeyPressed: Boolean = True;
  constref AStopKeys: TKeys = nil; ADoClrScr: Boolean = True;
  ARestoreBgColor: TVSLogoColor = -1; ARestoreTxtColor: TVSLogoColor = -1);
procedure PlayVSLogo3(AStopIfKeyPressed: Boolean = True;
  constref AStopKeys: TKeys = nil; ADoClrScr: Boolean = True;
  ARestoreBgColor: TVSLogoColor = -1; ARestoreTxtColor: TVSLogoColor = -1);


implementation

uses
  VSTextVisualEffects;

procedure RestoreColors(ABgColor, ATxtColor: TVSLogoColor; ADoClrScr: Boolean);
begin
  if ABgColor <> -1 then
    TextBackground(ABgColor);
  if ATxtColor <> -1 then
    TextColor(ATxtColor);
  if ADoClrScr then
    ClrScr;
end;

procedure PlayVSLogo1(AStopIfKeyPressed: Boolean = True;
  constref AStopKeys: TKeys = nil; ADoClrScr: Boolean = True;
  ARestoreBgColor: TVSLogoColor = -1; ARestoreTxtColor: TVSLogoColor = -1);
begin
  TextBackground(VSLogoBackgroundColor);
  TextColor(VSLogoTextColor);
  //CursorOff;
  ClrScr;

  with GetPosForScreenCenter(VSLogoStrings1) do
    if not Wait(500, AStopIfKeyPressed, AStopKeys) or
       not TVEFallDown(VSLogoStrings1, X, Y, #0, 45, True, AStopKeys) or
       not Wait(1000, AStopIfKeyPressed, AStopKeys) or
       not TVEFallDownErase(X, Y, VSLogoStrings1.MaxLength,
        Length(VSLogoStrings1),' ', #0, 45, True, AStopKeys) or
       not Wait(1000, AStopIfKeyPressed, AStopKeys) then;

  RestoreColors(ARestoreBgColor, ARestoreTxtColor, ADoClrScr);
end;

procedure PlayVSLogo2(AStopIfKeyPressed: Boolean = True;
  constref AStopKeys: TKeys = nil; ADoClrScr: Boolean = True;
  ARestoreBgColor: TVSLogoColor = -1; ARestoreTxtColor: TVSLogoColor = -1);
begin
  TextBackground(VSLogoBackgroundColor);
  TextColor(VSLogoTextColor);
  //CursorOff;
  ClrScr;

  with GetPosForCenter(VSLogoStrings1) do
    if not Wait(200, AStopIfKeyPressed, AStopKeys) or
       not TVEFallDownLine(VSLogoStrings1, X, Y, 3, ' ', #0, 35,
         True, AStopKeys) or
       not Wait(600, AStopIfKeyPressed, AStopKeys) or
       not TVEFallUpLine(VSLogoStrings1, X, Y, 3, ' ', #0, 35,
         True, AStopKeys) or
       not Wait(600, AStopIfKeyPressed, AStopKeys) or
       not TVEFallDownLine(VSLogoStrings1, X, Y, 50, ' ', #0, 35,
         True, AStopKeys) or
       //WriteStringsXY(LLeft, LTop, VSLogoStrings1);
       not Wait(1000, AStopIfKeyPressed, AStopKeys) then;

  RestoreColors(ARestoreBgColor, ARestoreTxtColor, ADoClrScr);
end;

procedure PlayVSLogo3(AStopIfKeyPressed: Boolean = True;
  constref AStopKeys: TKeys = nil; ADoClrScr: Boolean = True;
  ARestoreBgColor: TVSLogoColor = -1; ARestoreTxtColor: TVSLogoColor = -1);
const
  VSLogoBy1 = 'by Vitaly Smirnov';
  VSLogoLeft = 37;
  VSLogoTop = 0;
  function VSLogoTypeLine(AX, AY: Integer): Boolean;
  begin
    Result:=True;
    CursorOn;
    if not TVETypeLine(VSLogoBy1, AX+VSLogoLeft, AY+VSLogoTop, 30,
         True, AStopKeys) or
       not Wait(1300, AStopIfKeyPressed, AStopKeys) then
       Result:=False;
    CursorOff;
  end;
begin
  TextBackground(VSLogoBackgroundColor);
  TextColor(VSLogoTextColor);
  //CursorOff;
  ClrScr;

  with GetPosForCenter(VSLogoStrings1) do
    if not Wait(200, AStopIfKeyPressed, AStopKeys) or
       not TVEFallDownLine(VSLogoStrings1, X, Y, 3, ' ', #0, 35,
         True, AStopKeys) or
       not Wait(600, AStopIfKeyPressed, AStopKeys) or
       not TVEFallUpLine(VSLogoStrings1, X, Y, 3, ' ', #0, 35,
         True, AStopKeys) or
       not Wait(600, AStopIfKeyPressed, AStopKeys) or
       not TVEFallDown(VSLogoStrings1, X, Y, #0, 35, True, AStopKeys) or
       not Wait(500, AStopIfKeyPressed, AStopKeys) or
       not VSLogoTypeLine(X, Y) or
       not TVEFallDownErase(X, Y, VSLogoStrings1.MaxLength,
         Length(VSLogoStrings1), ' ', #0, 25, True, AStopKeys) or
       not Wait(700, AStopIfKeyPressed, AStopKeys) then;

  RestoreColors(ARestoreBgColor, ARestoreTxtColor, ADoClrScr);
end;


end.
