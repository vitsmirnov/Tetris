(*
  (c) Vitaly Smirnov (VSdev)
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSUtils;

{$mode objfpc}
//{$modeswitch advancedrecords}
//{$I-}
//{$B-}

interface

type
  TOpenFileMode = (ofmReset, ofmRewrite, ofmAppend);

function OpenFile(var AFile: TextFile; constref AFileName: AnsiString;
  constref AFilePaths: array of AnsiString;
  Mode: TOpenFileMode): Boolean;
procedure LogMessage(constref AFileName, AMessage: AnsiString);
procedure FreeAndNil(var AObject);
//procedure FreeAndNil(var AObject: TObject);

function VSTrunc(X: Extended): LongInt;
function IncInRange(var N: Integer; MinN, MaxN: Integer; GoThrough:
  Boolean = False): Integer;
function DecInRange(var N: Integer; MinN, MaxN: Integer; GoThrough:
  Boolean = False): Integer;
function IsIntInRange(AValue, ABegin, AEnd: LongInt): Boolean; // Do we need this or IsFloatInRange is enough?
function IsFloatInRange(AValue, ABegin, AEnd: Extended): Boolean;
function IsPointInRectangle(AX, AY, ALeftX, ATopY, ARightX, ABottomY:
  Extended): Boolean;


implementation

uses
  SysUtils;


function OpenFile(var AFile: TextFile; constref AFileName: AnsiString;
  constref AFilePaths: array of AnsiString;
  Mode: TOpenFileMode): Boolean;
var
  i: Integer;
begin
  if AFileName = '' then
    Exit(False);
  {$I-}
  i:=0;
  repeat
    if {(AFilePaths <> nil) and }(Length(AFilePaths) > 0) then
      Assign(AFile, AFilePaths[i]+AFileName)
    else
      Assign(AFile, AFileName);
    case Mode of
      ofmReset: Reset(AFile);
      ofmRewrite: Rewrite(AFile);
      ofmAppend: Append(AFile);
    end;
    Result:=IOResult = 0;
    i:=i+1;
  until Result or (i > High(AFilePaths));
  {if not Result then //??
    Close(AFile);}
  {$I+} //??
end;

procedure LogMessage(constref AFileName, AMessage: AnsiString);
var
  LFile: TextFile;
begin
  Assign(LFile, AFileName);
  {$I+}
  try
    try
      if FileExists(AFileName) then
        Append(LFile)
      else
        Rewrite(LFile);
      WriteLn(LFile, DateToStr(Date), ' ', TimeToStr(Time), ' ', AMessage);
    finally
      Close(LFile);
    end;
  except
    //
  end;
end;

procedure FreeAndNil(var AObject);
begin
  {if not (AObject is TObject) then
    Exit;}
  {$I+}
  try //???
    TObject(AObject).Free;
    TObject(AObject):=nil;
  except
  end;
end;

{procedure FreeAndNil(var AObject: TObject);
begin
  AObject.Free;
  AObject:=nil;
end;}

function VSTrunc(X: Extended): LongInt;
begin
  Result:=System.Trunc(X);
  if X < 0 then
    Result:=Result-1;
end;

function IncInRange(var N: Integer; MinN, MaxN: Integer; GoThrough:
  Boolean = False): Integer;
begin
  if N >= MaxN then
  begin
    if GoThrough then
      N:=MinN
    else
      N:=MaxN;
  end
  else
    N:=N+1;
  Result:=N;
end;

function DecInRange(var N: Integer; MinN, MaxN: Integer; GoThrough:
  Boolean = False): Integer;
begin
  if N <= MinN then
  begin
    if GoThrough then
      N:=MaxN
    else
      N:=MinN;
  end
  else
    N:=N-1;
  Result:=N;
end;

function IsIntInRange(AValue, ABegin, AEnd: LongInt): Boolean;
begin
  Result:=(AValue >= ABegin) and (AValue <= AEnd);
end;

function IsFloatInRange(AValue, ABegin, AEnd: Extended): Boolean;
begin
  Result:=(AValue >= ABegin) and (AValue <= AEnd);
end;

function IsPointInRectangle(AX, AY, ALeftX, ATopY, ARightX, ABottomY:
  Extended): Boolean;
begin
  //Result:=IsIntInRange(X, LeftX, RightX) and IsIntInRange(Y, TopY, BottomY);
  Result:=IsFloatInRange(AX, ALeftX, ARightX) and
          IsFloatInRange(AY, ATopY, ABottomY);
end;


end.
