(*
  (c) Vitaly Smirnov (VSdev)
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSStrings;

{$mode objfpc}
//{$modeswitch advancedrecords}
{$modeswitch typehelpers}
//{$I-}
//{$H+} // String is AnsiString
//{$J}
//{$B}

interface

uses
  VSPoints;

type
  TString = AnsiString;

  TStrings = array of TString;

  PString = ^TString;
  PStrings = ^TStrings;

  TIntCoord = Integer;

  TStringLength = Word;

  TStringHelper = type helper for TString
  public
    function Length: TStringLength;
    function IsEmpty: Boolean;
    function Fill(constref AString: TString = ' '; ANumber:
      SmallInt = 0): TString;
  end;

  TStringsHelper = type helper for TStrings
  public
    procedure Clear;
    function LoadFromFile(var AFile: TextFile; ANumber: Word): Boolean;

    //function Length: Word;
    function MaxLength: TStringLength;
    procedure Fill(constref AString: TString = ' ';
      AWidth: SmallInt = 0; AHeight: SmallInt = 0);
    procedure CopyFrom(constref AStrings: TStrings; X, Y: TIntCoord;
     ColCopyNumber: SmallInt = 1; RowCopyNumber: SmallInt = 1);
    procedure PutString(AX, AY: TIntCoord; constref AString: TString);
    procedure PutStrings(AX, AY: TIntCoord; constref InStrings: TStrings);
    function GetPosForCenter(ALeft: Integer = 1; ATop: Integer = 1;
      ARight: Integer = 80; ABottom: Integer = 25): TIntPoint;
  end;


// String (singular)

function IntToStr(X: Integer; AStrLength: Byte = 0): TString;
function FloatToStr(X: Extended; AStrLength1: Byte = 0;
  AStrLength2: Byte = 2): TString;
function FillString(constref AString: TString = ' '; ANumber:
  SmallInt = 0): TString;
function FillString2(constref AString: TString = ' '; ANumber:
  SmallInt = 0): TString;
procedure InsertString(var OutString: TString; constref InString: TString;
  APosition: TStringLength);
procedure ReplaceString(var OutString: TString; constref InString: TString;
  APosition: Integer);
procedure ReplaceChars(var AString: TString; ASourceChar, ADestChar: Char);
function ReplaceCharsExcept(constref InString: AnsiString;
  AToChar: Char = '.'; AExceptChar: Char = ' '): AnsiString;


// Strings (plural)

function StringsMaxLength(constref AStrings: TStrings): TStringLength;
function FillStrings(constref AString: TString = ' '; AWidth:
  SmallInt = 0; AHeight: SmallInt = 0): TStrings;
{procedure FillStrings(out OutStrings: TStrings; constref InStrings =
  TStrings = [' ']; HorizontalCopyNumber: SmallInt = 1);}
procedure CopyStrings(var OutStrings: TStrings;
  constref InStrings: TStrings; X, Y: TIntCoord;
  ColCopyNumber: SmallInt = 1; RowCopyNumber: SmallInt = 1);
procedure PutStringOnStrings(var OutStrings: TStrings; AX, AY: TIntCoord;
  constref AString: TString);
procedure PutStringsOnStrings(var OutStrings: TStrings; AX, AY: TIntCoord;
  constref InStrings: TStrings);
{procedure PutStringsOnStrings(var OutStrings: TStrings; constref
  APosition: TIntPoint; constref InStrings: TStrings);}
procedure ReplaceChars(var AStrings: TStrings; ASourceChar, ADestChar: Char);

procedure SaveStringsToTextFile(constref AStrings: TStrings;
  constref AFileName: TString);
(*procedure LoadStringsFromTextFile(out{var} AStrings;
  constref AFileName: TString);*)

procedure StringsToString(out{var} OutString: TString;
  constref InStrings: TStrings);
function StringsToString(constref AStrings: TStrings): TString;

function GetPosForCenter(constref AStrings: TStrings;
  ALeft: Integer = 1; ATop: Integer = 1;
  ARight: Integer = 80; ABottom: Integer = 25): TIntPoint;


implementation

function IntToStr(X: Integer; AStrLength: Byte = 0): TString;
begin
  Str(X:AStrLength, Result);
end;

function FloatToStr(X: Extended; AStrLength1: Byte = 0;
  AStrLength2: Byte = 2): TString;
begin
  Str(X:AStrLength1:AStrLength2, Result);
end;

function FillString(constref AString: TString = ' '; ANumber:
  SmallInt = 0): TString;
begin
  if ANumber > 0 then
    Result:=AString+FillString(AString, ANumber-1)
  else
    Result:='';
end;

function FillString2(constref AString: TString = ' '; ANumber:
  SmallInt = 0): TString;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to ANumber do
    Result:=Result+AString;
end;

procedure InsertString(var OutString: TString; constref InString: TString;
  APosition: TStringLength);
begin
  if (APosition < 1) or (APosition > Length(OutString)+1) then
    Exit;
  Insert(InString, OutString, APosition);
end;

procedure ReplaceString(var OutString: TString; constref InString: TString;
  APosition: Integer);
var
  TempStr: TString;
begin
  //if not IsIntInRange(APosition, 1-Length(InString)+1, Length(OutString){+1}) then
  if (APosition < 1-Length(InString)+1) or
     (APosition > Length(OutString){+1}) then
    Exit;

  TempStr:=InString;
  if APosition < 1 then
  begin
    Delete(TempStr, 1, 1-APosition);
    APosition:=1;
  end;
  TempStr:=Copy(TempStr, 1, (Length(OutString)-APosition)+1);
  Delete(OutString, APosition, Length(TempStr));
  Insert(TempStr, OutString, APosition);
end;

procedure ReplaceChars(var AString: TString; ASourceChar, ADestChar: Char);
var
  i: Integer;
begin
  for i:=Low(AString) to High(AString) do
    if AString[i] = ASourceChar then
      AString[i]:=ADestChar;
end;

function ReplaceCharsExcept(constref InString: AnsiString;
  AToChar: Char = '.'; AExceptChar: Char = ' '): AnsiString;
var
  i: Integer;
begin
  Result:=InString;
  for i:=Low(Result) to High(Result) do
    if Result[i] <> AExceptChar then
      Result[i]:=AToChar;
end;

function StringsMaxLength(constref AStrings: TStrings): TStringLength;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to High(AStrings) do
    if Length(AStrings[i])>Result then
      Result:=Length(AStrings[i]);
end;

function FillStrings(constref AString: TString = ' ';
  AWidth: SmallInt = 0; AHeight: SmallInt = 0): TStrings;
var
  i: Integer;
  TempStr: TString;
begin
  if AHeight <= 0 then
    Exit(nil);
  TempStr:=FillString(AString, AWidth);
  SetLength(Result, AHeight);
  for i:=0 to High(Result) do
    Result[i]:=TempStr;
end;

procedure CopyStrings(var OutStrings: TStrings;
  constref InStrings: TStrings; X, Y: TIntCoord;
  ColCopyNumber: SmallInt = 1; RowCopyNumber: SmallInt = 1);
var
  i, j, k: Integer;
  InStringsWidth, InStringsHeight: Byte;
  TempStr: TString;
begin
  // This isn't done!
  InStringsHeight:=Length(InStrings);
  InStringsWidth:=StringsMaxLength(InStrings);
  if (InStringsHeight = 0) or (InStringsWidth = 0) or
     (ColCopyNumber <= 0) or (RowCopyNumber <= 0) or
     (X < 1) or {(X>OutStringsWidth) or} (Y < 0) or (Y > High(OutStrings)) then
    Exit;

  for j:=0 to RowCopyNumber-1 do
    for i:=0 to InStringsHeight-1 do
      for k:=0 to ColCopyNumber-1 do
      begin
        if (Y+i+j*InStringsHeight < 0) or
           (Y+i+j*InStringsHeight > High(OutStrings)) then
          Exit;
        ReplaceString(OutStrings[Y+i+j*InStringsHeight], InStrings[i],
          X+k*InStringsWidth);
      end;
end;

// Check this!
procedure PutStringOnStrings(var OutStrings: TStrings; AX, AY: TIntCoord;
  constref AString: TString);
begin
  //if not IsIntInRange(AY, 0, High(OutStrings)) then
  if (AY < 0) or (AY > High(OutStrings)) then
    Exit;
  ReplaceString(OutStrings[AY], AString, AX);
end;

procedure PutStringsOnStrings(var OutStrings: TStrings; AX, AY: TIntCoord;
  constref InStrings: TStrings);
var
  i: Integer;
begin
  for i:=0 to High(InStrings) do
    if (AY+i) <= High(OutStrings) then
      PutStringOnStrings(OutStrings, AX, AY+i, InStrings[i])
    else
      Exit;//Break;
end;

procedure ReplaceChars(var AStrings: TStrings; ASourceChar, ADestChar: Char);
var
  i: Integer;
begin
  for i:=Low(AStrings) to High(AStrings) do
    ReplaceChars(AStrings[i], ASourceChar, ADestChar)
end;

{procedure PutStringsOnStrings(var OutStrings: TStrings; constref
  APosition: TIntPoint; constref InStrings: TStrings);
begin
  PutStringsOnStrings(OutStrings, APosition.X, APosition.Y, InStrings);
end;}

procedure SaveStringsToTextFile(constref AStrings: TStrings;
  constref AFileName: TString);
var
  AOutFile: TextFile;
  i: Word;
begin
  if Length(AStrings)=0 then
    Exit;
  //try
    Assign(AOutFile, AFileName);
    Rewrite(AOutFile);
  //except
  //end;
  for i:=0 to High(AStrings) do
    WriteLn(AOutFile, AStrings[i]);
  Close(AOutFile);
end;

procedure LoadStringsFromTextFile(out{var} AStrings;
  constref AFileName: TString);
begin

end;

procedure StringsToString(out OutString: TString;
  constref InStrings: TStrings);
var i: Integer;
begin
  OutString:='';
  for i:=0 to High(InStrings) do
    OutString:=OutString+InStrings[i];
end;

function StringsToString(constref AStrings: TStrings): TString;//AnsiString;
begin
  StringsToString(Result, AStrings);
end;

function GetPosForCenter(constref AStrings: TStrings;
  ALeft: Integer = 1; ATop: Integer = 1;
  ARight: Integer = 80; ABottom: Integer = 25): TIntPoint;
var
  LWidth, LHeight: Integer;
begin
  LWidth:=ARight-ALeft+1 - AStrings.MaxLength;
  LHeight:=ABottom-ATop+1 - Length(AStrings);
  Result.Setup(ALeft+LWidth div 2, ATop+LHeight div 2);
end;


// TStringHelper = type helper for TString

function TStringHelper.Length: TStringLength;
begin
  Result:=System.Length(Self);
end;

function TStringHelper.IsEmpty: Boolean;
begin
  Result:=Length = 0;
end;

function TStringHelper.Fill(constref AString: TString = ' ';
  ANumber: SmallInt = 0): TString;
begin
  Self:=FillString(AString, ANumber);
  Result:=Self;
end;

// TStringsHelper = type helper for TStrings

procedure TStringsHelper.Clear;
var
  i: Integer;
begin
  // It's not neáessary I guess
  for i:=Low(Self) to High(Self) do
    SetLength(Self[i], 0);
  SetLength(Self, 0);
end;

function TStringsHelper.LoadFromFile(var AFile: TextFile;
  ANumber: Word): Boolean;
var
  i: Integer;
  LRes: TStrings;
begin
  {$I+}
  try
    SetLength(LRes, ANumber);
    ReadLn(AFile);
    for i:=0 to ANumber-1 do
      ReadLn(AFile, LRes[i]);

    Self.Clear;
    Self:=LRes;
    Result:=True;
  except
    LRes.Clear;
    Result:=False;
  end;
end;

{function TStringsHelper.Length: Word;
begin
  Result:=System.Length(Self);
end;}

function TStringsHelper.MaxLength: TStringLength;
begin
  Result:=StringsMaxLength(Self);
end;

procedure TStringsHelper.Fill(constref AString: TString = ' ';
  AWidth: SmallInt = 0; AHeight: SmallInt = 0);
begin
  Self.Clear;
  Self:=FillStrings(AString, AWidth, AHeight);
end;

procedure TStringsHelper.CopyFrom(constref AStrings: TStrings;
  X, Y: TIntCoord; ColCopyNumber: SmallInt = 1; RowCopyNumber: SmallInt = 1);
begin
  Self.Clear;
  CopyStrings(Self, AStrings, X, Y, ColCopyNumber, RowCopyNumber);
end;

procedure TStringsHelper.PutString(AX, AY: TIntCoord;
  constref AString: TString);
begin
  PutStringOnStrings(Self, AX, AY, AString);
end;

procedure TStringsHelper.PutStrings(AX, AY: TIntCoord;
  constref InStrings: TStrings);
begin
  PutStringsOnStrings(Self, AX, AY, InStrings);
end;

function TStringsHelper.GetPosForCenter(ALeft: Integer = 1; ATop: Integer = 1;
  ARight: Integer = 80; ABottom: Integer = 25): TIntPoint;
begin
  Result:=VSStrings.GetPosForCenter(Self, ALeft, ATop, ARight, ABottom);
end;


end.
