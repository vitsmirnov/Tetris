(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2023
*)

unit VSIntList;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

type
  {PSingleList = ^TSingleList;

  TSingleList = record
  end;}

  PIntListNode = ^TIntListNode;

  TIntListNode = record
    Value: Integer;
    Next: PIntListNode;
  end;

  TIntList = PIntListNode;

  TIntListHelper = type helper for TIntList
    procedure Init;
    class function CreateNode(AValue: Integer;
      ANext: PIntListNode): PIntListNode; static;
    procedure Push(AValue: Integer);//: PIntNode;
    function Pop: Integer;//PIntNode;
    procedure Put(AValue: Integer);
    function Get: Integer;
    function PopLast: Integer;// Temp!
    procedure Clear;
    function IsEmpty: Boolean;

    function AddSortNoDup(AValue: Integer): PIntListNode;
    function GoToNext: PIntListNode;
    function GetValue: Integer;
    function GetCount: Word;

    function ToString(constref ASeparator: AnsiString = ' '): AnsiString;
    procedure Print;

    property Value: Integer read GetValue;
    property Count: Word read GetCount;
  end;


implementation

uses
  SysUtils;


// TIntList = record

procedure TIntListHelper.Init;
begin
  Self:=nil;
end;

class function TIntListHelper.CreateNode(AValue: Integer;
  ANext: PIntListNode): PIntListNode;
begin
  New(Result);
  Result^.Value:=AValue;
  Result^.Next:=ANext;
end;

procedure TIntListHelper.Push(AValue: Integer);//: PIntNode;
begin
  Self:=CreateNode(AValue, Self);
end;

function TIntListHelper.Pop: Integer;//PIntNode;
var
  TempNode: PIntListNode;
begin
  if Self = nil then
    Exit(-1);

  Result:=Self^.Value;
  TempNode:=Self;
  Self:=Self^.Next;
  Dispose(TempNode);
end;

procedure TIntListHelper.Put(AValue: Integer);
var
  TempNode: PIntListNode;
begin
  if Self = nil then
    Self:=CreateNode(AValue, nil)
  else
  begin
    TempNode:=Self;
    while TempNode^.Next <> nil do
      TempNode:=TempNode^.Next;
    TempNode^.Next:=CreateNode(AValue, nil);
  end;
end;

function TIntListHelper.Get: Integer;
begin
  Result:=Pop;
end;

function TIntListHelper.PopLast: Integer;
var
  Iter: ^PIntListNode;
  TempNode: PIntListNode;
begin
  if Self = nil then
    Exit(-1);

  Iter:=@Self;
  while {(Iter^ <> nil) and} (Iter^^.Next <> nil) do
    Iter:=@(Iter^^.Next);

  TempNode:=Iter^;
  Iter^:=nil;//}Iter^^.Next;
  Result:=TempNode^.Value;
  Dispose(TempNode);

  // This is better, but need to be checked.
  (*Result:=Iter^^.Value;
  Dispose(Iter^);
  Iter^:=nil;//*)
end;//*)

procedure TIntListHelper.Clear;
var
  TempNode: PIntListNode;
begin
  while Self<>nil do
  begin
    TempNode:=Self;
    Self:=Self^.Next;
    Dispose(TempNode);
  end;
end;

function TIntListHelper.IsEmpty: Boolean;
begin
  Result:=Self = nil;
end;

function TIntListHelper.AddSortNoDup(AValue: Integer): PIntListNode;
var
  Iter: ^PIntListNode;
  TempNode: PIntListNode;
begin
  Iter:=@Self;
  while (Iter^ <> nil) and (AValue > Iter^^.Value) do
    Iter:=@(Iter^^.Next);

  if (Iter^ = nil) or (Iter^^.Value > AValue) then
  begin
    New(TempNode);
    TempNode^.Value:=AValue;
    TempNode^.Next:=Iter^;
    Iter^:=TempNode;
  end;
end;

function TIntListHelper.GoToNext: PIntListNode;
begin
  if Self = nil then
    Exit(nil);

  Self:=Self^.Next;
  Result:=Self;
end;

function TIntListHelper.GetValue: Integer;
begin
  if Self = nil then
    Exit(-1);//??

  Result:=Self^.Value;
end;

function TIntListHelper.GetCount: Word;
begin
  if Self = nil then
    Exit(0);
  Result:=1+Self^.Next.Count;
end;

function TIntListHelper.ToString(constref ASeparator: AnsiString = ' '):
  AnsiString;
begin
  if Self = nil then
    Exit('');
  Result:=IntToStr(Self^.Value)+ASeparator+Self^.Next.ToString(ASeparator);
end;

procedure TIntListHelper.Print;
begin
  if Self = nil then
    Exit;
  WriteLn(Self^.Value);
  Self^.Next.Print;
end;


end.
