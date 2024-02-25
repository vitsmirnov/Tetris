(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

unit TetrisEngine;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$J-}
//{$B-}
//{$I-}

interface

uses
  VSIntList, VSPoints,
  VSEngineCore, VSEngineComponents;

type
  TTetrisGameEngine = class;

  //TTime = VSEngineCore.TTime;//LongInt;//LongWord;

  TPlayerCommand = VSEngineCore.TPlayerCommand;//procedure of object;

  TTetrisFigure = class helper for TFigure
  public
    procedure MoveLeft;
    procedure MoveRight;
    procedure MoveUp; // Temp
    procedure MoveDown;
    procedure RotateLeft;
    procedure RotateRight;
    procedure DropDown;
    //procedure FallDown;
  end;

  TTetrisScoreRules = array of Integer;

  TTetrisGameSettings = record
  public
    GameFieldWidth: TSize;
    GameFieldHeight: TSize;
    CupHeight: Word;
    SpawnPosition: TFloatPoint;
    StartSpeed: Extended;
    DeltaSpeed: Extended;
    LinesToChangeSpeed: Word;
    ScoreRules: TTetrisScoreRules;

    function LoadFromFile(constref AFileName: AnsiString): Boolean;
    function LoadFromFile(constref AFileName: AnsiString;
      constref AFilePaths: array of AnsiString): Boolean;
    function LoadFromFile(var AFile: TextFile): Boolean;
  end;

  PTetrisGameSettings = ^TTetrisGameSettings;

  TLinesChangedEvent = procedure of object;
  TScoreChangedEvent = procedure of object;
  TSpeedChangedEvent = procedure of object;
  //TLevelChangedEvent = procedure of object;
  THasLandedEvent = procedure of object;
  TDestroyLinesEvent = procedure (ALines: TIntList) of object;

  // TGameEngine -> TTetrisGameEngine ??
  TTetrisGameEngine = class(TGameEngineCore)
  public const
    {$J-}
    DefaultFieldWidth = 10;
    DefaultFieldHeight = 25;
    DefaultCupHeight = 20;
    DefaultSpawnPosition: TFloatPoint = (X: 4; Y: 20);
    DefaultSpeed = 3.5;
    DefaultDeltaSpeed = 0.2;
    DefaultLinesToChangeSpeed = 5;

    ScoreFor0Line = 10;
    ScoreFor1Line = 100;
    ScoreFor2Lines = 300;
    ScoreFor3Lines = 700;
    ScoreFor4Lines = 1500; // 4 or/and more lines. Tetris!
    DefaultScoreRules: TTetrisScoreRules = (
      ScoreFor0Line,
      ScoreFor1Line,
      ScoreFor2Lines,
      ScoreFor3Lines,
      ScoreFor4Lines);
  private
    FGameField: TGameField;
    FPlayer: TFigure; //FCurrentFigure
    FFigures: TFigures;
    FPlayerCommands: TPlayerCommands;

    // FSettins: TTetrisGameSettings; ????
    FCupHeight: Word;
    FSpawnPosition: TFloatPoint;
    FStartSpeed: Extended;
    FDeltaSpeed: Extended;
    FLinesToChangeSpeed: Word;
    FScoreRules: TTetrisScoreRules;

    FLines: LongWord;
    FScore: Integer;//LongWord;
    //FLevel: Word;
    //FBestScore: TBestScore; // record = (name, score)

    FLastChangeSpeedLines: LongWord;
    FNextFigure: TFigure;

    FOnLinesChanged: TLinesChangedEvent;
    FOnScoreChanged: TScoreChangedEvent;
    FOnSpeedChanged: TSpeedChangedEvent;
    //FOnLevelChanged: TSpeedChangedEvent;
    FOnHasLanded: THasLandedEvent;
    FOnDestroyLines: TDestroyLinesEvent;
  protected // Parent methods (TGameEngineCore)
    procedure UpdateGameLogic(ADeltaTime: TTime;
      var ABreakGameLoop: Boolean); override;
  protected
    property PlayerCommands: TPlayerCommands read FPlayerCommands;
  protected//private
    procedure SetFigures(const AFigures: TFigures);
    function GetFiguresNumber: Integer;
    function GetScoreRule(ALine: Word): Integer;
    procedure SetScoreRule(ALine: Word; AValue: Integer);
    procedure SetLines(ALines: LongWord); virtual;
    procedure SetScore(AScore: LongWord); virtual;
    procedure SetSpeed(ASpeed: Extended); virtual;
    //procedure SetLevel(ALevel: Word); virtual;
    function GetSpeed: Extended;
    procedure SetLinesToChangeSpeed(AValue: Word);
  protected
    function IsGameOver: Boolean; virtual; //GameOverCondition
    procedure GameOver; virtual;
    procedure UpdateGameField; virtual;
    procedure UpdateScore(ADestroyedLines: Word); virtual;
    procedure UpdateSpeed; virtual;
    function SpawnFigure: Boolean; virtual;
    procedure ChooseNextFigure; //virtual;

    procedure SpeedUp;
    procedure SpeedDown;
  public
    constructor Create;
    constructor Create(AFieldWidth, AFieldHeight: TSize; ACupHeight: Word;
      ASpawnPosition: TFloatPoint; AStartSpeed, ADeltaSpeed: Extended;
      ALinesToChangeSpeed: Word; constref AScoreRules: TTetrisScoreRules;
      constref AFigures: TFigures);
    constructor Create(constref ASettings: TTetrisGameSettings; constref
      AFigures: TFigures);

    constructor Create(constref AFileName: AnsiString);
    constructor Create(constref AFileName: AnsiString; constref
      AFilePaths: array of AnsiString);
    constructor Create(var AFile: TextFile);
    destructor Destroy; override;

    procedure SetBasicSettings(AFieldWidth, AFieldHeight: TSize;
      ACupHeight: Word; ASpawnPosition: TFloatPoint;
      AStartSpeed, ADeltaSpeed: Extended; ALinesToChangeSpeed: Word;
      constref AScoreRules: TTetrisScoreRules);
    procedure SetBasicSettings(constref ASettings: TTetrisGameSettings);
    // Rename to LoadBasicSettingsFromFile?
    function LoadSettingsFromFile(constref AFileName: AnsiString):
      Boolean; //virtual;
    function LoadSettingsFromFile(constref AFileName: AnsiString;
      constref AFilePaths: array of AnsiString): Boolean; //virtual;
    function LoadSettingsFromFile(var AFile: TextFile): Boolean; //virtual;

    procedure ResetGame; virtual;
    {procedure Start;
    procedure NewGame;
    procedure Pause;
    procedure BackToGame;
    procedure GoToMenu; //virtual; abstract;}

    procedure SetScoreRules(const AScoreRules: TTetrisScoreRules);

    // Should they be (opened)?
    property Player: TFigure read FPlayer;
    property GameField: TGameField read FGameField;

    property Figures: TFigures read FFigures write SetFigures;
    property FiguresNumber: Integer read GetFiguresNumber;
    property NextFigure: TFigure read FNextFigure;//GetNextFigure;

    property CupHeight: Word read FCupHeight write FCupHeight default
      DefaultCupHeight;
    property SpawnPosition: TFloatPoint read FSpawnPosition write
      FSpawnPosition;
    property StartSpeed: Extended read FStartSpeed write FStartSpeed;
    property DeltaSpeed: Extended read FDeltaSpeed write FDeltaSpeed;
    property LinesToChangeSpeed: Word read FLinesToChangeSpeed write
      SetLinesToChangeSpeed default DefaultLinesToChangeSpeed;
    property ScoreRules[Index: Word]: Integer read GetScoreRule write
      SetScoreRule;

    property Lines: LongWord read FLines  default 0;
    property Score: Integer read FScore  default 0;
    //property Level: Word read FLevel default 1;
    property Speed: Extended read {FPlayer.Speed}GetSpeed {write SetSpeed};// default 3.5;//LongInt;//LongWord; // Chars per seconds

    property OnLinesChanged: TLinesChangedEvent read FOnLinesChanged write
      FOnLinesChanged;
    property OnScoreChanged: TScoreChangedEvent read FOnScoreChanged write
      FOnScoreChanged;
    property OnSpeedChanged: TSpeedChangedEvent read FOnSpeedChanged write
      FOnSpeedChanged;
    {property OnLevelChanged: TSpeedChangedEvent read FOnLevelChanged write
      FOnLevelChanged;}
    property OnHasLanded: THasLandedEvent read FOnHasLanded write
      FOnHasLanded;
    property OnDestroyLines: TDestroyLinesEvent read FOnDestroyLines write
      FOnDestroyLines;
  end;


// ??
procedure AddLog(constref AMessage: AnsiString);

const
  LogFileName{: AnsiString} = '..\resource\'+'Log.txt';

  MoveStep = 1; // Figure
  RotateStep = 90; // Figure

  {$J-}
  //DefaultScoreRules: TTetrisScoreRules = (10, 100, 300, 700, 1500);

  // Move it to implementation?
  DefaultGameSettings: TTetrisGameSettings = (
    GameFieldWidth: 10;
    GameFieldHeight: 25;
    CupHeight: 20;
    SpawnPosition: (X: 4; Y: 20);
    StartSpeed: 3.5;
    DeltaSpeed: 0.2;
    LinesToChangeSpeed: 5;
    ScoreRules: (10, 100, 300, 700, 1500){DefaultScoreRules};
  );

{var
  DebugMode: Boolean = True;}


implementation

uses
  SysUtils,
  VSUtils;


procedure AddLog(constref AMessage: AnsiString);
begin
  LogMessage(LogFileName, AMessage);
end;


// TTetrisFigure = class helper for TFigure

procedure TTetrisFigure.MoveLeft;
begin
  Shift(-MoveStep, 0);
end;

procedure TTetrisFigure.MoveRight;
begin
  Shift(MoveStep, 0);
end;

procedure TTetrisFigure.MoveUp;
begin
  Shift(0, MoveStep);
end;

procedure TTetrisFigure.MoveDown;
begin
  Shift(0, -MoveStep);
end;

procedure TTetrisFigure.RotateLeft;
begin
  Rotate(RotateStep);
end;

procedure TTetrisFigure.RotateRight;
begin
  Rotate(-RotateStep);
end;

procedure TTetrisFigure.DropDown;
var
  LDistance: Extended = 100;//??
begin
  if GameField <> nil then
    LDistance:=MinY+1+1;//FGameField.Height;
  Shift(0, -LDistance);
end;


// TTetrisGameSettings = record

function TTetrisGameSettings.LoadFromFile(constref AFileName: AnsiString):
  Boolean;
begin
  Result:=LoadFromFile(AFileName, ['']);
end;

function TTetrisGameSettings.LoadFromFile(constref AFileName: AnsiString;
  constref AFilePaths: array of AnsiString): Boolean;
var
  AFile: TextFile;
begin
  if not OpenFile(AFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);
  Result:=LoadFromFile(AFile);
  Close(AFile);
end;

function TTetrisGameSettings.LoadFromFile(var AFile: TextFile): Boolean;
var
  i, n: Integer;
  TGS: TTetrisGameSettings;
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile);
    ReadLn(AFile, TGS.GameFieldWidth, TGS.GameFieldHeight);
    ReadLn(AFile);
    ReadLn(AFile, TGS.CupHeight);
    ReadLn(AFile);
    ReadLn(AFile, TGS.SpawnPosition.X, TGS.SpawnPosition.Y);
    ReadLn(AFile);
    ReadLn(AFile, TGS.StartSpeed, TGS.DeltaSpeed, TGS.LinesToChangeSpeed);
    ReadLn(AFile);
    ReadLn(AFile, n);
    ReadLn(AFile);
    SetLength(TGS.ScoreRules, n);
    for i:=0 to n-1 do
      Read(AFile, TGS.ScoreRules[i]);
    ReadLn(AFile);

    Self:=TGS;
    Result:=True;
  except
    Result:=False;
  end;
  SetLength(TGS.ScoreRules, 0);//??
end;


// TTetrisGameEngine = class(TGameEngineCore)

constructor TTetrisGameEngine.Create;
begin
  Create(DefaultFieldWidth, DefaultFieldHeight, DefaultCupHeight,
    DefaultSpawnPosition, DefaultSpeed, DefaultDeltaSpeed,
    DefaultLinesToChangeSpeed, DefaultScoreRules{[10, 100, 300, 700, 1500]},
    []{nil});
end;

constructor TTetrisGameEngine.Create(AFieldWidth, AFieldHeight: TSize;
  ACupHeight: Word; ASpawnPosition: TFloatPoint;
  AStartSpeed, ADeltaSpeed: Extended; ALinesToChangeSpeed: Word;
  constref AScoreRules: TTetrisScoreRules; constref AFigures: TFigures);
begin
  inherited Create;

  FGameField:=TGameField.Create(AFieldWidth, AFieldHeight);
  FPlayer:=TFigure.Create(FGameField);
  FFigures:=AFigures;
  FPlayerCommands.Clear;

  FCupHeight:=ACupHeight;
  FSpawnPosition:=ASpawnPosition;
  FStartSpeed:=AStartSpeed;
  FPlayer.Speed:=FStartSpeed;
  FDeltaSpeed:=ADeltaSpeed;
  {F}LinesToChangeSpeed:=ALinesToChangeSpeed;
  FScoreRules:=Copy(AScoreRules, 0, Length(AScoreRules));

  FLines:=0;
  FScore:=0;
  //FLevel:=1;
  FLastChangeSpeedLines:=0;

  FOnLinesChanged:=nil;
  FOnScoreChanged:=nil;
  FOnSpeedChanged:=nil;
  //FOnLevelChanged:=nil;
  FOnHasLanded:=nil;
  FOnDestroyLines:=nil;

  // ChooseNextFigure and RotateFigure use Random!
  Randomize;
  ChooseNextFigure;
  //SpawnFigure; // It needs if we run "Run" right after "create"
end;

constructor TTetrisGameEngine.Create(constref ASettings: TTetrisGameSettings;
  constref AFigures: TFigures);
begin
  with ASettings do
    Create(GameFieldWidth, GameFieldHeight, CupHeight, SpawnPosition,
      StartSpeed, DeltaSpeed, LinesToChangeSpeed, ScoreRules, AFigures);
end;

constructor TTetrisGameEngine.Create(constref AFileName: AnsiString);
begin
  Create(AFileName, ['']);
end;

constructor TTetrisGameEngine.Create(constref AFileName: AnsiString; constref
  AFilePaths: array of AnsiString);
var
  AFile: TextFile;
begin
  if OpenFile(AFile, AFileName, AFilePaths, ofmReset) then
  begin
    Create(AFile);
    Close(AFile);
  end
  else
    Create;
end;

constructor TTetrisGameEngine.Create(var AFile: TextFile);
var
  TGS: TTetrisGameSettings;
begin
  if TGS.LoadFromFile(AFile) then
    Create(TGS, nil)
  else
    Create;
end;

destructor TTetrisGameEngine.Destroy;
begin
  FPlayerCommands.Clear;
  FFigures.Free; // It's not object!
  FreeAndNil(FPlayer);
  FreeAndNil(FGameField);
  SetLength(FScoreRules, 0);

  inherited Destroy;
end;

procedure TTetrisGameEngine.SetBasicSettings(AFieldWidth, AFieldHeight: TSize;
  ACupHeight: Word; ASpawnPosition: TFloatPoint; AStartSpeed,
  ADeltaSpeed: Extended; ALinesToChangeSpeed: Word; constref
  AScoreRules: TTetrisScoreRules);
begin
  FGameField.Setup(AFieldWidth, AFieldHeight);

  FCupHeight:=ACupHeight;
  FSpawnPosition:=ASpawnPosition;
  FStartSpeed:=AStartSpeed;
  //FPlayer.Speed:=FStartSpeed;//??
  FDeltaSpeed:=ADeltaSpeed;
  {F}LinesToChangeSpeed:=ALinesToChangeSpeed;
  FScoreRules:=Copy(AScoreRules, 0, Length(AScoreRules));
end;

procedure TTetrisGameEngine.SetBasicSettings(constref ASettings:
  TTetrisGameSettings);
begin
  with ASettings do
    SetBasicSettings(GameFieldWidth, GameFieldHeight, CupHeight,
      SpawnPosition, StartSpeed, DeltaSpeed, LinesToChangeSpeed, ScoreRules);
end;

function TTetrisGameEngine.LoadSettingsFromFile(constref AFileName:
  AnsiString): Boolean;
begin
  Result:=LoadSettingsFromFile(AFileName, ['']);
end;

function TTetrisGameEngine.LoadSettingsFromFile(constref AFileName:
  AnsiString; constref AFilePaths: array of AnsiString): Boolean;
var
  AFile: TextFile;
begin
  if not OpenFile(AFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);
  Result:=LoadSettingsFromFile(AFile);
  Close(AFile);
end;

function TTetrisGameEngine.LoadSettingsFromFile(var AFile: TextFile): Boolean;
var
  TGS: TTetrisGameSettings;
begin
  Result:=TGS.LoadFromFile(AFile);
  if Result then
    SetBasicSettings(TGS);
end;

procedure TTetrisGameEngine.UpdateGameLogic(ADeltaTime: TTime;
  var ABreakGameLoop: Boolean);
var
  LHasLanded: Boolean;
begin
  inherited;//??

  FPlayer.Update(ADeltaTime, LHasLanded);
  if not LHasLanded then
    Exit;

  if IsGameOver then
  begin
    ABreakGameLoop:=True;
  end
  else // Update figure and field
  begin
    if FOnHasLanded <> nil then
      FOnHasLanded();

    FPlayer.LeaveElementsOnFieldOrDestroy;
    UpdateGameField;
    // Update score/lines, canvases etc should be here! Now it's in UpdateGameField, so it's not good
    if not SpawnFigure then
    begin
      Player.DestroyElements;
      ABreakGameLoop:=True;
    end;
    ResetLoopTime;
  end;

  if ABreakGameLoop then // ABreakGameLoop means that game over
  begin
    UpdateScreen;
    GameOver;
  end;
end;

function TTetrisGameEngine.IsGameOver: Boolean;
begin
  Result:=Round(Player.MaxY) >= FCupHeight;
end;

procedure TTetrisGameEngine.GameOver;
begin
end;

{procedure TTetrisGameEngine.UpdateScreen;
begin
  inherited;
  // Temp!
  WriteStringXYInRect(15, 25, 'Fig[0] = ('+
    FloatToStr(FPlayer.FElements[0].Position.X, 6, 3)+', '+
    FloatToStr(FPlayer.FElements[0].Position.Y, 6, 3)+')');
end;}

procedure TTetrisGameEngine.ResetGame;
begin
  FPlayer.DestroyElements;
  FGameField.DestroyElements;
  ChooseNextFigure; // We should choose figure which we have to spawn.  ?
  SpawnFigure;

  SetSpeed(FStartSpeed); // Chars per second
  SetLines(0);
  SetScore(0);
  //SetLevel(1);

  FLastChangeSpeedLines:=0;
end;

procedure TTetrisGameEngine.UpdateGameField;
var
  i, j: Integer;
  LineIsFull: Boolean;
  DestroyedLines: Word;//Byte;
  FullLines: TIntList;//PIntNode;
begin
  if FGameField=nil then
    Exit;

  FullLines:=nil;
  for i:=0 to FGameField.Height-1 do
  begin
    LineIsFull:=True;
    for j:=0 to FGameField.Width-1 do
      if FGameField[j, i]=nil then
      begin
        LineIsFull:=False;
        Break;
      end;

    if LineIsFull then
      FullLines.AddSortNoDup(i); // It/there can be used standart Push if cycle goes from High to Low!
  end;

  if FOnDestroyLines<>nil then
    FOnDestroyLines(FullLines);

  ///////////////// Ver 3 //////////////////
  // It should be checked, I guess.
  DestroyedLines:=0;
  if not FullLines.IsEmpty then
  begin
    i:=FullLines^.Value;
    while i+DestroyedLines < FGameField.Height do
    begin
      if not FullLines.IsEmpty and (i = FullLines^.Value-DestroyedLines) then
      begin
        FullLines.Pop;//i:=FullLines.Pop-DestroyedLines;//??
        for j:=0 to FGameField.Width-1 do
        begin
          FGameField[j, i].Free;
          FGameField[j, i]:=nil; //?
        end;
        DestroyedLines:=DestroyedLines+1;
      end
      else
        i:=i+1;
      //FOnDestroyElementUpdate;// Just for testing
      if IsIntInRange(i+DestroyedLines, 0, FGameField.Height-1) then //It's no good.
        for j:=0 to FGameField.Width-1 do
          if FGameField[j, i+DestroyedLines]<>nil then
          begin
            FGameField[j, i]:=FGameField[j, i+DestroyedLines];
            FGameField[j, i].Position.Shift(0, -DestroyedLines);
            FGameField[j, i+DestroyedLines]:=nil;
          end;
      //FOnDestroyElementUpdate;// Just for testing
    end;
  end;

  ///////////////// Ver 2 /////////////////
  {DestroyedLines:=0;
  while not FullLines.IsEmpty do
  begin
    for i:=FullLines.Pop-DestroyedLines to FGameField.Height-1-1 do
      for j:=0 to FGameField.Width-1 do
      begin
        FGameField[j, i].Free;
        if FGameField[j, i+1]<>nil then
        begin
          FGameField[j, i]:=FGameField[j, i+1];
          FGameField[j, i].Position.Shift(0, -1);
          FGameField[j, i+1]:=nil;
        end
        else
          FGameField[j, i]:=nil; //??
      end;
    DestroyedLines:=DestroyedLines+1;
  end;}

  if not FullLines.IsEmpty then
  begin
    AddLog('Error in TetrisEngine.TTetrisGameEngine.UpdateGameField2! '+
      'FullLines isn''t empty.');
    FullLines.Clear;
  end;

  ////////////// Ver 1 ///////////////////
  {DestroyedLines:=0;
  j:=0;
  while j<FGameField.Height do
  begin
    LineIsFull:=True;
    for i:=0 to FGameField.Width-1 do
      if FGameField[i, j]=nil then
      begin
        LineIsFull:=False;
        Break;
      end;

    if LineIsFull then
    begin
      for i:=0 to FGameField.Width-1 do
      begin
        FGameField[i, j].Free;
        FGameField[i, j]:=nil;

        if FOnDestroyElementUpdate<>nil then
          FOnDestroyElementUpdate;
      end;

      for k:=j to FGameField.Height-2 do
      begin
        for i:=0 to FGameField.Width-1 do
          if FGameField[i, k+1]<>nil then
          begin
            FGameField[i, k]:=FGameField[i, k+1];
            FGameField[i, k].Position.Shift(0, -1);
            FGameField[i, k+1]:=nil;//??
          end;

        if FOnShiftLineUpdate<>nil then
          FOnShiftLineUpdate;
      end;

      //if FCanvas<>nil then FCanvas.VisualEffect1('/', 10);
      if FOnShiftLinesUpdate<>nil then
        FOnShiftLinesUpdate;

      DestroyedLines:=DestroyedLines+1;
      //Continue;
    end
    else
      j:=j+1;
  end;}


  //if DestroyedLines>0 then
  begin
    SetLines(FLines+DestroyedLines);
    UpdateScore(DestroyedLines);
    UpdateSpeed;
  end;
end;

procedure TTetrisGameEngine.UpdateScore(ADestroyedLines: Word);
begin
  if IsIntInRange(ADestroyedLines, Low(FScoreRules), High(FScoreRules)) then
    SetScore(FScore+FScoreRules[ADestroyedLines])
  else
    SetScore(FScore+FScoreRules[High(FScoreRules)]);
end;

procedure TTetrisGameEngine.UpdateSpeed;
var
  DeltaLines: LongInt;//LongWord;
begin
  DeltaLines:=FLines-FLastChangeSpeedLines;
  if (DeltaLines >= FLinesToChangeSpeed) and (FLinesToChangeSpeed <> 0) then
  begin
    //if FLinesToChangeSpeed <> 0 then
      SetSpeed(FPlayer.Speed+FDeltaSpeed*
        (DeltaLines div FLinesToChangeSpeed));

    FLastChangeSpeedLines:=FLines-(DeltaLines mod
      FLinesToChangeSpeed);
  end
  else
    SetSpeed(Speed); // Temp!!
end;

function TTetrisGameEngine.SpawnFigure: Boolean;
var
  n: Extended;
begin
  n:=FPlayer.Speed;
  FPlayer.Setup(FNextFigure, False);
  FPlayer.Speed:=n;

  //if FNeedToRotateSpawnFigure then
    FPlayer.GameField:=nil; // Temp
    FPlayer.Rotate(Random(4)*90);
    FPlayer.GameField:=FGameField; // Temp!

  Result:=FPlayer.MoveTo(FSpawnPosition);
  if not Result then
    Exit;

  ChooseNextFigure;
end;

procedure TTetrisGameEngine.ChooseNextFigure;
begin
  if Length(FFigures) > 0 then
    FNextFigure:=FFigures[Random(Length(FFigures))]
  else
    FNextFigure:=nil;
end;

procedure TTetrisGameEngine.SetFigures(const AFigures: TFigures);
begin
  FFigures.Free; // It isn't object!
  FFigures:=AFigures;
end;

function TTetrisGameEngine.GetFiguresNumber: Integer;
begin
  Result:=Length(FFigures);
end;

function TTetrisGameEngine.GetScoreRule(ALine: Word): Integer;
begin
  if IsIntInRange(ALine, 0, High(FScoreRules)) then
    Result:=FScoreRules[ALine]
  else
    Result:=0;
end;

procedure TTetrisGameEngine.SetScoreRule(ALine: Word; AValue: Integer);
begin
  if IsIntInRange(ALine, 0, High(FScoreRules)) then
    FScoreRules[ALine]:=AValue;
end;

procedure TTetrisGameEngine.SetScoreRules(const AScoreRules:
  TTetrisScoreRules);
begin
  FScoreRules:=Copy(AScoreRules, 0, Length(AScoreRules));
end;

procedure TTetrisGameEngine.SetLines(ALines: LongWord);
begin
  FLines:=ALines;
  if FOnLinesChanged <> nil then
    FOnLinesChanged;
end;

procedure TTetrisGameEngine.SetScore(AScore: LongWord);
begin
  FScore:=AScore;
  if FOnScoreChanged<>nil then
    FOnScoreChanged;
end;

procedure TTetrisGameEngine.SetSpeed(ASpeed: Extended);
begin
  FPlayer.Speed:=ASpeed;
  if FOnSpeedChanged<>nil then
    FOnSpeedChanged;
end;

function TTetrisGameEngine.GetSpeed: Extended;
begin
  Result:=FPlayer.Speed;
end;

procedure TTetrisGameEngine.SetLinesToChangeSpeed(AValue: Word);
begin
  if AValue > 0 then
    FLinesToChangeSpeed:=AValue
  else
    FLinesToChangeSpeed:=1;//DefaultLinesToChangeSpeed;
end;

{procedure TTetrisGameEngine.SetLevel(ALevel: Word);
begin
  FLevel:=ALevel;
  if FOnLevelChanged<>nil then
    FOnLevelChanged;
end;}

procedure TTetrisGameEngine.SpeedUp;
begin
  SetSpeed(FPlayer.Speed+FDeltaSpeed);
end;

procedure TTetrisGameEngine.SpeedDown;
begin
  SetSpeed(FPlayer.Speed-FDeltaSpeed);
end;


//initialization
//begin
//finalization
end.
