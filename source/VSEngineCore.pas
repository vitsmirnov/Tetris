(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2023
*)

unit VSEngineCore;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
//{$J-}
//{$B-}
//{$I-}

interface

type
  (*TGameObject = TObject;
  //TBasicGameObject
  TGameObjectCore = class(TGameObject)
  public//protected
    procedure Update(ADeltaTime: TTime{; var AOutputData: Pointer}); virtual; //abstract;
  end;*)

  //Rename this! ?
  TKeyCode = Word;//-255..255; // It already exists in VSKeys. Delete this?

  TKeys = array of TKeyCode; // It already exists in VSKeys. Delete this?

  TPlayerCommand = procedure of object;

  //TPlayerCommands = array{[TKeyCode]} of TPlayerCommand; // Should it be a dynamyc array?

  TPlayerCommands = record
  strict private //??
    FCommands: array of TPlayerCommand;//TPlayerCommands;
  public
    procedure SetLength(ACount: TKeyCode);
    procedure Reset;
    procedure Clear;

    function DoesCommandExist(Index: TKeyCode): Boolean;
    function GetCommand(Index: TKeyCode): TPlayerCommand;
    procedure SetCommand(Index: TKeyCode; ACommand: TPlayerCommand);
    function GetCount: TKeyCode;
    function DoCommand(const AKey: TKeyCode): TPlayerCommand;

    procedure SetCommands(ACommand: TPlayerCommand; AKeys: TKeys;
      ADeletePrevious: Boolean = True);

    property Commands[Index: TKeyCode]: TPlayerCommand read
      GetCommand write SetCommand; default;
    property Count: TKeyCode read GetCount;// write SetLength;
  end;

  TTime = LongInt;//LongWord; // LongInt is needed possibility for go to back in time

  TGameEngineCore = class(TObject)
  private
    //FPlayerCommands: TPlayerCommands; //???
    FStartLoopTime: LongWord;
    FStopGameLoop: Boolean;
  protected
    procedure GetAndDoPlayerCommand(var ABreakGameLoop: Boolean); virtual;
    procedure UpdateGameLogic(ADeltaTime: TTime;
      var ABreakGameLoop: Boolean); virtual;
    procedure UpdateScreen; virtual;

    procedure ResetLoopTime;
    property StartLoopTime: LongWord read FStartLoopTime;
  public
    constructor Create;

    procedure RunGameLoop; virtual; // Should it be protected! ?
    procedure StopGameLoop;
  end;


implementation

uses
  SysUtils;


// TPlayerCommands = record

procedure TPlayerCommands.SetLength(ACount: TKeyCode);
begin
  System.SetLength(FCommands, ACount);
  Reset;
end;

procedure TPlayerCommands.Reset;
var
  i: Integer;
begin
  for i:=Low(FCommands) to High(FCommands) do
    FCommands[i]:=nil;
end;

procedure TPlayerCommands.Clear;
begin
  System.SetLength(FCommands, 0);
end;

function TPlayerCommands.DoesCommandExist(Index: TKeyCode): Boolean;
begin
  Result:=(Index >= Low(FCommands)) and (Index <= High(FCommands));
end;

function TPlayerCommands.GetCommand(Index: TKeyCode): TPlayerCommand;
begin
  if DoesCommandExist(Index) then
    Result:=FCommands[Index]
  else
    Result:=nil;
end;

procedure TPlayerCommands.SetCommand(Index: TKeyCode;
  ACommand: TPlayerCommand);
begin
  if DoesCommandExist(Index) then
    FCommands[Index]:=ACommand;
end;

function TPlayerCommands.GetCount: TKeyCode;
begin
  Result:=Length(FCommands);
end;

function TPlayerCommands.DoCommand(const AKey: TKeyCode):
  TPlayerCommand;
begin
  if GetCommand(AKey) = nil then
    Exit(nil);
  FCommands[AKey]();
  Result:=FCommands[AKey];
end;

procedure TPlayerCommands.SetCommands(ACommand: TPlayerCommand;
  AKeys: TKeys; ADeletePrevious: Boolean = True);
var
  i: Integer;
begin
  if Length(AKeys) = 0 then
    Exit;
  if ADeletePrevious then
    for i:=Low(FCommands) to High(FCommands) do
      if FCommands[i] = ACommand then
        FCommands[i]:=nil;

  for i:=Low(AKeys) to High(AKeys) do
    SetCommand(AKeys[i], ACommand);
end;


// TGameEngineCore = class

constructor TGameEngineCore.Create;
begin
  inherited Create;
  ResetLoopTime;
  FStopGameLoop:=False;
end;

procedure TGameEngineCore.ResetLoopTime;
begin
  FStartLoopTime:=GetTickCount;
end;

procedure TGameEngineCore.RunGameLoop;
var
  LDeltaTime: TTime;
  LBreakGameLoop: Boolean;
begin
  UpdateScreen;

  FStopGameLoop:=False;
  LBreakGameLoop:=False;
  FStartLoopTime:=GetTickCount;
  repeat
    GetAndDoPlayerCommand(LBreakGameLoop);
    // LBreakGameLoop is needed to go to menu
    if LBreakGameLoop then
      Break;

    LDeltaTime:=GetTickCount-FStartLoopTime;
    FStartLoopTime:=GetTickCount;

    UpdateGameLogic(LDeltaTime, LBreakGameLoop);
    if LBreakGameLoop then
      Break; //???
    // If LBreakGameLoop then game is over (probably),
    // but before we break game loop we have to update screen (probably)
    // BUT! If procedure "GameOver" will be in UpdateGameLogic, then we
    // need to Break immediately (and do UpdateScreen in UpdateGameLogic
    // before GameOver)!
    UpdateScreen;
  until FStopGameLoop or LBreakGameLoop; //False;
end;

procedure TGameEngineCore.StopGameLoop;
begin
  FStopGameLoop:=True;
end;

procedure TGameEngineCore.GetAndDoPlayerCommand(var ABreakGameLoop: Boolean);
begin
end;

procedure TGameEngineCore.UpdateGameLogic(ADeltaTime: TTime;
  var ABreakGameLoop: Boolean);
begin

  {if IsGameOver then
  begin
    GameOver;
    ABreakGameLoop:=True;
    Exit;
  end;}
end;

procedure TGameEngineCore.UpdateScreen;
begin
end;


//initialization
//begin
//finalization
end.

