(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

// FPC = Free Pascal Compiler
unit TetrisGameFPC;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$J-} // Can't := to typed constants
//{$I-} // Input/output error
//{$B-}
//{$H+} // String = AnsiString, String[n] = ShortString

interface

uses
  SysUtils,
  VSPoints, VSStrings, VSTextCanvas, VSCRT, VSMenu, VSIntList,
  VSEngineComponents, TetrisEngine;


{$J-}
const
  ElementWidth = 2;

  DefaultFilePaths: TStrings = (
    '',
    '..\resource\',
    'resource\',
    '..\..\..\resource\');

  //CharSpace = ' ';

  DefaultUserDataFileName = '..\resource\udata.tud'; // tud - tetris user data


type
  TElementFPC = class;
  TTetrisGameFPC = class;

  TKeyCode = VSCRT.TKeyCode;

  TElementSkinFPC = String[ElementWidth];

  TElementSkinsFPC = array of TElementSkinFPC;

  TElementSkinsFPCSet = array of TElementSkinsFPC;

  TElementSkinsFPCHelper = type helper for TElementSkinsFPC
  public
    function LoadFromFile({ASkinsNumber: Word;} constref AFileName: TString
      {; constref AFilePaths: TStrings}): Boolean;
  end;

  TElementFPC = class(TElement)
  public const
    DefaultSkin{: TElementSkinFPC} = '[]';
  private
    FSkin: TElementSkinFPC;
  public
    constructor Create(AOwner: TFigure = nil; AX: TFloatCoord = 0;
      AY: TFloatCoord = 0; ASkin: TElementSkinFPC = DefaultSkin);
    {constructor Create(AOwner: TFigure; APosition: TFloatPoint;
      ASkin: TElementSkinFPC = DefaultSkin);
    constructor Create(AOwner: TFigure; AElement: TElementFPC);}

    function GetCopy(AOwner: TFigure): TElement; override;

    function Setup(AOwner: TFigure = nil; AX: TFloatCoord = 0;
      AY: TFloatCoord = 0; ASkin: TElementSkinFPC = DefaultSkin): TElement;

    property Skin: TElementSkinFPC read FSkin write FSkin;
  end;

  // Simplified data (it isn't used at the moment, but it could be used)
  TFigureTextData = record
  public
    Figure: TStrings;
    RotationPoint: TFloatPoint;
    Skin: TElementSkinFPC;

    function ToStrings(CharToSkip: Char = ' '; FillChar: Char = ' '):
      TStrings;
  end;

  TTextFigures = array of TFigureTextData;

  TFloatPointsHelper = type helper for TFloatPoints
  public
    procedure ImportFromStrings(constref AStrings: TStrings;
      CharToSkip: Char = ' '); // Bottom left point = 0, 0
  end;

  // Full data (for individual skins etc)
  TElementFPCData = record
  public
    Position: TFloatPoint;
    Skin: TElementSkinFPC;
  end;

  TElementsFPCData = array of TElementFPCData;

  TFigureData = record
  public
    ElementsData: TElementsFPCData;
    RotationPoint: TFloatPoint;

    function ToStrings(FillChar: Char = ' '): TStrings;
    procedure ImportFromTextFigure(constref AFigure: TFigureTextData);
  end;

  TFiguresData = array of TFigureData;

  PFiguresData = ^TFiguresData;

  TFiguresDataHelper = type helper for TFiguresData
  public
    function LoadFromFile(var AFile: TextFile): Boolean;
    function LoadFromFile(constref AFileName: TString; constref
      AFilePaths: TStrings): Boolean;
    function LoadFromFileTxt(constref AFileName: TString; constref
      AFilePaths: TStrings): Boolean;

    procedure ImportFromTextFigures(constref AFigures: TTextFigures);
    procedure CopyFrom(constref AFigures: TFiguresData);

    procedure SetSkins(constref ASkins: TElementSkinsFPC);
    procedure SetSkinToAll(ASkin: TElementSkinFPC);
    function LoadSkinsFromFile(constref AFileName: TString; constref
      AFilePaths: TStrings): Boolean;

    procedure Free;
  end;

  TFigureFPCHelper = class helper(TTetrisFigure) for TFigure
  public
    procedure Setup(constref AData: TFigureData; PutOnGameField:
      Boolean = False);
    procedure SetSkin(constref ASkin: TElementSkinFPC);
    //procedure SetSkin(constref ASkins: array of TElementSkinFPC);
    function ToStrings(FillChar: Char = ' '): TStrings;
  end;

  TFiguresFPCHelper = type helper(TFiguresHelper) for TFigures
  public
    procedure ImportFromData(constref AData: TFiguresData;
      AGameField: TGameField);

    procedure SetSkins(constref ASkins: TElementSkinsFPC);
  end;

  // IDs for links
  TOutputDataID = (
    // SB - scoreboard
    odiSBLines, odiSBScore, odiSBSpeed, odiSBBestScore,
    // CK - control keys
    odiCKMoveRight, odiCKMoveLeft, odiCKMoveDown, odiCKDropDown,
    odiCKRotateRight, odiCKRotateLeft, odiCKPause, odiCKMenu,
    // Additional control keys
    odiCKMoveUp, odiCKChangeFigure, odiCKChangeScreenSettingsSet{});

  TOutputLabels = array[TOutputDataID] of TLabel;

  TOutputLabelsHelper = type helper for TOutputLabels
  public
    function SetCaption(AID: TOutputDataID; constref ACaption: AnsiString):
      Boolean;
  end;

  TOutputLabelData = record
  public
    ID: TOutputDataID;
    BoardIndex: Byte;
    LabelIndex: Byte;
  end;

  TOutputLabelsData = array of TOutputLabelData;

  TOutputLabelsDataHelper = type helper for TOutputLabelsData
  public
    function LoadFromFile(var AFile: TextFile): Boolean;
  end;

  TTetrisFPCMessageID = (tmiGameOver, tmiWantToQuit,
    tmiBestScoreCongrats, {BestScore}tmiEnterName, tmiPause{,
    // MS = MenuSettings
    tmiMSScreenSkinCaption, tmiMSScreenSkinSetN,
    tmiMSScreenSkinHint1, tmiMSColorsCaption,
    tmiMSFiguresSkinsCaption, tmiMSFiguresSkinsSetN});

  TTetrisFPCMessages = array[TTetrisFPCMessageID] of AnsiString;

  PTetrisFPCMessages = ^TTetrisFPCMessages;

  TTetrisFPCMessagesHelper = type helper for TTetrisFPCMessages
  public
    function LoadFromFile(var AFile: TextFile): Boolean;
  end;

  TDestroyLinesAnimation = record
  public
    Frames: array of TElementSkinFPC; //Sprites
    ReplaysNumber: Word;
    Delay: Word;

    procedure CopyFrom(constref AAnimation: TDestroyLinesAnimation);

    function LoadFromFile(var AFile: TextFile): Boolean;
    procedure Clear;
  end;

  TDropDownTrace = record
  public
    Show: Boolean;
    Skin: TElementSkinFPC;
    Delay: Word; //LongWord;

    function LoadFromFile(var AFile: TextFile): Boolean;
  end;

  // Should it be personal (not from TOutputDataID)?
  TTetrisFPCGameCommandID = odiCKMoveRight..odiCKChangeScreenSettingsSet;
  TControlKeys = array[TTetrisFPCGameCommandID] of TKeyCode;
  //TControlKeys = array[TKeyCode] of TTetrisFPCGameCommandID;
  PControlKeys = ^TControlKeys;
  TTetrisFPCGameCommands = array[TTetrisFPCGameCommandID] of TPlayerCommand;
  //TTetrisFPCGameCommandName = String[50];
  TTetrisFPCGameCommandNames = array[TTetrisFPCGameCommandID] of String[50];

  TControlKeysHelper = type helper for TControlKeys
  public
    procedure Reset;
    //function LoadFromFile(var AFile: TextFile): Boolean; // All keys
    function LoadFromFile(var AFile: TextFile{; AKeysCount: Byte}): Boolean; // Some keys
    function SaveToFile(var AFile: TextFile): Boolean;
  end;

  TTetrisFPCGameScreenSettings = record
  public
    GameFieldData: TTextCanvasSettings;
    CanvasDeltaPos: TIntPoint; //Rename! GameFieldPosOnCanvas??
    BoardsData: TBoardsData;
    OutputLabelsData: TOutputLabelsData;
    NextFigureCanvasIndex: Byte;
    NextFigurePosOnCanvas: TIntPoint;
    FallenElementSkin: TElementSkinFPC;
    DestroyLinesAnimation: TDestroyLinesAnimation;
    DropDownTrace: TDropDownTrace;
    //Separete these ones?
    BackgroundColor: Byte;
    TextColor: Byte;

    function LoadFromFile(constref AFileName: TString; constref
      AFilePaths: TStrings): Boolean;
    function LoadFromFile(var AFile: TextFile): Boolean;

    procedure Free;
  end;

  PTetrisFPCGameScreenSettings = ^TTetrisFPCGameScreenSettings;

  TTetrisFPCGameScreenSettingsSet = array of TTetrisFPCGameScreenSettings;

  TPlayerName = String[20];

  TBestScore = record
  public
    PlayerName: TPlayerName;
    Score: Integer; // It could be negative (in theory)

    procedure Setup(constref AName: TPlayerName; AScore: Integer);
    function LoadFromFile(constref AFileName: TString{; constref
      AFilePaths: TStrings}): Boolean;
    function SaveToFile(constref AFileName: TString): Boolean;
    function ToString(const ASeparator: TString = ' - ';
      ScoreFirst: Boolean = True): TString;
  end;

  TFileName = String[200];

  TUserData = record
  public
    BestScore: TBestScore;
    ScreenSettingsSetIndex: ShortInt;
    BackgroundColor: Byte;
    TextColor: Byte;
    //BackgroundChar: Char;
    FiguresSkinsSetIndex: ShortInt;
    ScreenSettingsFileName: TFileName;
    FiguresSkinsFileName: TFileName;
    ControlKeys: TControlKeys;

    procedure Setup(constref ABestScore: TBestScore;
      AScreenSettingsSetIndex: ShortInt; ABackgroundColor, ATextColor: Byte;
      AFiguresSkinsSetIndex: ShortInt; constref AScreenSettingsFileName,
      AFiguresSkinsFileName: AnsiString; constref AControlKeys: TControlKeys);
    procedure SetDefault;
    function LoadFromFile(constref AFileName: AnsiString): Boolean;
    function SaveToFile(constref AFileName: AnsiString): Boolean;
  end;

  TTetrisFPCMenuID = (tmiMain, tmiSettings);
  TTetrisFPCMenu = array[TTetrisFPCMenuID] of TMenu;

  // FPC = Free Pascal Compiler
  TTetrisGameFPC = class(TTetrisGameEngine)
  public const
    MainMenuCaptionsNumber = 1+3;
  private
    FGameFieldCanvas: TTextCanvas;
    FGameFieldCanvasDeltaPos: TIntPoint; // Rename this!
    FBoards: TBoards;
    FOutputLabels: TOutputLabels;
    FNextFigureCanvasIndex: Byte;
    FNextFigurePosOnCanvas: TIntPoint;
    FFallenElementSkin: TElementSkinFPC;
    FDestroyLinesAnimation: TDestroyLinesAnimation;
    FDropDownTrace: TDropDownTrace;
    FMenu: TTetrisFPCMenu;
    FMessages: TTetrisFPCMessages;

    FGameCommands: TTetrisFPCGameCommands;
    FGameCommandNames: TTetrisFPCGameCommandNames;

    FIsGameRunning: Boolean;
    FDropDown: Boolean;

    FUserDataFileName: TFileName;
    //FUserData: TUserData;
    FBackgroundColor: Byte;
    FTextColor: Byte;
    FBestScore: TBestScore;
    FScreenSettingsSetIndex: ShortInt;//Word;//Byte
    FFiguresSkinsSetIndex: ShortInt;//Word;//Byte;}
    FScreenSettingsFileName: TFileName;
    FFiguresSkinsFileName: TFileName;
    //function NextScreenSettingsNumber: Word;
  protected
    // Events
    procedure GameChangeElement(AX, AY: TFloatCoord; AElement: TElement);
    procedure LinesChanged;
    procedure ScoreChanged;
    procedure SpeedChanged;
    //procedure OnUpdateField;// What is this?
    procedure HasLanded;

    function GameFieldCoordsToCanvas(AX, AY: TFloatCoord): TIntPoint;
    function GameFieldCoordsToCanvas(constref APosition: TFloatPoint):
      TIntPoint;
  protected
    procedure GetAndDoPlayerCommand(var BreakGameLoop: Boolean); override;
    procedure UpdateScreen; override;
    procedure RunGameLoop; override;
    procedure GameOver; override;
    function SpawnFigure: Boolean; override;
    procedure SetSpeed(ASpeed: Extended); override;

    procedure NewGame;
    procedure BackToGame;
    procedure Pause;
    procedure GoToMenu;

    procedure MainMenuSettings;

    procedure SettingsMenuGameScreenSkin;
    procedure SettingsMenuColors;
    procedure SettingsMenuFiguresSkins;
    procedure SettingsMenuControlKeys;
    procedure SettingsMenuAbout;

    procedure MainMenuExitQuery(var ACanExit: Boolean);
    procedure SettingsMenuExitQuery(var ACanExit: Boolean);

    procedure MainMenuDrawMethod;
    procedure SettingsMenuDrawMethod;

    procedure UpdateNextFigureCanvas;
    procedure GameFieldCanvasRefresh;

    procedure ResetCanvases(AWasChanged: Boolean = True);
    procedure DrawGameScreen(ADoClrScr: Boolean = False);
    procedure GetGameScreenAABB(var ATopLeft: TIntPoint;
      var AWidth, AHeight: Word);
    function GetGameScreenAABBStrings(var ATopLeft: TIntPoint;
      ABackgroundChar: Char = ' '): TStrings;
    function GetGameScreenFullStrings(ABackgroundChar: Char = ' '): TStrings;

    procedure ResetOutputLabels;
    procedure OutputLabelsSetup(constref AData: TOutputLabelsData);
    procedure UpdateControlKeysOutput(constref AControlKeys: TControlKeys);

    procedure BoardsSetup(constref AData: TBoardsData);
  protected // Temp?
    procedure ChangeFigure;
    procedure ChangeScreenSet;
    procedure ChangeFigureSkinsSet;
    procedure LoadAllSettingsFromFile;
    procedure LoadGameFromFile;
  protected
    procedure DestroyLinesEvent(ALines: TIntList);
    procedure DropDownFigure;
    function SaveUserData(AControlKeys: PControlKeys = nil): Boolean;
  protected
    procedure SetupGameCommands;
    procedure SetupMenu(constref AMainMenuCaptions: TStrings);
    procedure SetupControlKeys(constref AControlKeys: TControlKeys;
      DeletePrevious: Boolean = False);
    function GetControlKeys: TControlKeys;
  public
    // It isn't good I guess!
    constructor Create(
      constref AUserDataFileName: AnsiString = DefaultUserDataFileName;//'';
      AGameSettings: PTetrisGameSettings = nil;
      AFiguresData: PFiguresData = nil;
      AScreenSettings: PTetrisFPCGameScreenSettings = nil;
      AMainMenuCaptions: PStrings = nil;
      AMessages: PTetrisFPCMessages = nil);
    constructor CreateFromFile(constref AFileName: AnsiString; constref
      AUserDataFileName: AnsiString = DefaultUserDataFileName);
    destructor Destroy; override;

    procedure Start;

    procedure SetFiguresData(constref AData: TFiguresData);
    procedure SetScreenSettings(constref ASettings: TTetrisFPCGameScreenSettings);
    //procedure SetMainMenuCaptions();
    //procedure SetMessages();
    //procedure SetCaptions();//All of them
    //procedure SetAllFPCSettings; // useless

    function LoadFiguresDataFromFile(constref AFileName: AnsiString;
      constref AFilePaths: TStrings): Boolean;
    function LoadFiguresDataFromFile(var AFile: TextFile): Boolean;
    function LoadFiguresDataFromFileTxt(constref AFileName: AnsiString;
      constref AFilePaths: TStrings): Boolean;
    function LoadScreenSettingsFromFile(constref AFileName: TString;
      constref AFilePaths: TStrings): Boolean;
    function LoadScreenSettingsFromFile(var AFile: TextFile): Boolean;
    function LoadAllSettingsFromFile(constref AFileName: TString;
      constref AFilePaths: TStrings): Boolean;
    function LoadCaptionsFromFile(constref AFileName: TString;
      constref AFilePaths: TStrings): Boolean;

    // Need to add properties!!
    // ??
    property IsGameRunning: Boolean read FIsGameRunning;
  end;


var
  DebugMode: Boolean = False;

implementation

uses
  VSUtils, VSTextVisualEffects, VSLogo;


const
  {$J-}
  //{$I DefaultGameSettings}
  {$I DefaultFigures}
   //{$I DefaultFiguresTxt}
  {$I DefaultCaptions}
  {$I DefaultGameScreenSettings}
  {$I DefaultControlKeys}
  //{$I VSLogoData}

  {DefaultUserData: TUserData = (
    BestScore: (
      PlayerName: 'VSdev';
      Score: 0;
    );
    ScreenSettingsSetIndex: 0;
    BackgroundColor: Black;
    TextColor: White;
    FiguresSkinsSetIndex: 0;
    ScreenSettingsFileName: '';
    FiguresSkinsFileName: '';
  );}


function DebugMessage(constref AMessage: AnsiString): Boolean;
begin
  Result:=DebugMode;
  if Result then
    ShowMessage(AMessage);
end;


// TElementSkinsFPCHelper = type helper for TElementSkinsFPC

function TElementSkinsFPCHelper.LoadFromFile({ASkinsNumber: Word;}
  constref AFileName: TString{; constref AFilePaths: TStrings}): Boolean;
var
  LFile: TextFile;
  ES: TElementSkinsFPC;
  i, n: Integer;
begin
  if not OpenFile(LFile, AFileName, [''], ofmReset) then
    Exit(False);
  {$I+}
  try
    ReadLn(LFile);
    ReadLn(LFile, n);
    SetLength(ES, n);
    for i:=0 to n-1 do
      ReadLn(LFile, ES[i]);

    SetLength(Self, 0);
    Self:=ES;
    Result:=True;
  except
    SetLength(ES, 0);
    Result:=False;
  end;
  Close(LFile);
end;


// TElementFPC

constructor TElementFPC.Create(AOwner: TFigure = nil; AX: TFloatCoord = 0;
  AY: TFloatCoord = 0; ASkin: TElementSkinFPC = DefaultSkin);
begin
  inherited Create(AOwner, AX, AY);
  FSkin:=ASkin;
end;

function TElementFPC.GetCopy(AOwner: TFigure): TElement;
begin
  Result:=TElementFPC.Create(AOwner, X, Y, FSkin);
end;

function TElementFPC.Setup(AOwner: TFigure = nil; AX: TFloatCoord = 0;
  AY: TFloatCoord = 0; ASkin: TElementSkinFPC = DefaultSkin): TElement;
begin
  inherited Setup(AOwner, AX, AY);
  FSkin:=ASkin;
end;

{function TElementFPC.Setup(AOwner: TFigure = nil; AX: TFloatCoord = 0;
  AY: TFloatCoord = 0): TElement; //reintroduce;//virtual;
begin
end;}


// TFigureTextData = record

function TFigureTextData.ToStrings(CharToSkip: Char = ' ';
  FillChar: Char = ' '): TStrings;
var
  i, j: Integer;
begin
  SetLength(Result, Length(Figure));
  for j:=0 to High(Figure) do
  begin
    Result[j]:='';
    for i:=1 to Length(Figure[j]) do
    begin
      if Figure[j, i] = CharToSkip then
        Result[j]:=Result[j]+FillString(FillChar, ElementWidth)
      else
        Result[j]:=Result[j]+Skin;
    end;
  end;
end;


// TFigureDataHelper = record helper for TFigureData

{function TFigureData.LoadFromFile(var AFile: TextFile): Boolean;
begin
end;}

function TFigureData.ToStrings(FillChar: Char = ' '): TStrings;
var
  i, MinX, MaxX, MinY, MaxY, Height, Width: Integer;
  Fills: TString;
begin
  if Length(ElementsData) = 0 then
    Exit([]{nil});

  MinX:=ElementsData[0].Position.RoundX;
  MaxX:=MinX;//ElementsData[0].Position.RoundX;
  MinY:=ElementsData[0].Position.RoundY;
  MaxY:=MinY;//ElementsData[0].Position.RoundY;
  for i:=1 to High(ElementsData) do
    with ElementsData[i].Position do
    begin
      if RoundX<MinX then
        MinX:=RoundX
      else
      if RoundX>MaxX then
        MaxX:=RoundX;

      if RoundY<MinY then
        MinY:=RoundY
      else
      if RoundY>MaxY then
        MaxY:=RoundY;
    end;

  Height:=MaxY-MinY+1;
  Width:=MaxX-MinX+1;
  Fills:=FillString(FillChar, Width*ElementWidth);
  SetLength(Result, Height);
  for i:=Low(Result) to High(Result) do
    Result[i]:=Fills;
  for i:=Low(ElementsData) to High(ElementsData) do
    with ElementsData[i] do
      ReplaceString(Result[MaxY-Position.RoundY], Skin,
        (Position.RoundX-MinX)*ElementWidth+1);
end;

procedure TFigureData.ImportFromTextFigure(constref AFigure: TFigureTextData);
var
  i: Integer;
  Points: TFloatPoints;
begin
  RotationPoint:=AFigure.RotationPoint;
  Points.ImportFromStrings(AFigure.Figure);
  SetLength(ElementsData, Length(Points));
  for i:=0 to High(ElementsData) do
    with ElementsData[i] do
    begin
      Position:=Points[i];
      Skin:=AFigure.Skin;
    end;
  SetLength(Points, 0);
end;


// TFloatPointsHelper = type helper for TFloatPoints

{function StringsToPoints(constref AStrings: TStrings;
  CharToSkip: Char = ' '): TFloatPoints;}
procedure TFloatPointsHelper.ImportFromStrings(constref AStrings: TStrings;
  CharToSkip: Char = ' ');
var
  i, j, n, FigHigh: Integer;
begin
  if Length(AStrings) = 0 then
    Exit;

  n:=0;
  FigHigh:=High(AStrings);
  for i:=Low(AStrings) to FigHigh do
    for j:=Low(AStrings[i]) to High(AStrings[i]) do
      if AStrings[i, j] <> CharToSkip then
        n:=n+1;

  SetLength(Self, n);
  n:=0;
  for i:=Low(AStrings) to FigHigh do
    for j:=Low(AStrings[i]) to High(AStrings[i]) do
      if AStrings[i, j] <> CharToSkip then
      begin
        Self[n].Setup(j-1, FigHigh-i);
        n:=n+1;
      end;

  {FigHigh:=High(AStrings);
  SetLength(Self, 0);
  for i:=Low(AStrings) to FigHigh do
    for j:=Low(AStrings[i]) to High(AStrings[i]) do
      if AStrings[i, j] <> CharToSkip then
      begin
        SetLength(Self, Length(Self)+1);
        Self[High(Self)].Setup(j-1, FigHigh-i);
      end;}
end;


// TFiguresDataHelper = type helper for TFiuresData

function TFiguresDataHelper.LoadFromFile(var AFile: TextFile): Boolean;
var
  i, j, n, k: Integer;
  c: Char;
  LFiguresData: TFiguresData;
begin
  {$I+}
  try
    ReadLn(AFile); // Comment/title string
    ReadLn(AFile); // Comment string
    ReadLn(AFile, n); // Number of figures
    ReadLn(AFile); // Comment string
    SetLength(LFiguresData, n);
    for i:=0 to n-1 do // Figures
      with LFiguresData[i] do
      begin
        ReadLn(AFile); // Comment string
        ReadLn(AFile, k); // Number of elements
        // Elements
        SetLength(ElementsData, k);
        for j:=0 to k-1 do
          with ElementsData[j] do
            ReadLn(AFile, Position.X, Position.Y, c, Skin);
        ReadLn(AFile, RotationPoint.X, RotationPoint.Y); // Rotation point
      end;

    //SetLength(Self, 0);
    Self.Free;
    Self:=LFiguresData;
    Result:=True;
  except
    //Free;
    LFiguresData.Free;
    Result:=False;
  end;
end;

function TFiguresDataHelper.LoadFromFile(constref AFileName: TString;
  constref AFilePaths: TStrings): Boolean;
var
  AFile: TextFile;
begin
  if not OpenFile(AFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);
  Result:=LoadFromFile(AFile);
  Close(AFile);
end;

function TFiguresDataHelper.LoadFromFileTxt(constref AFileName: TString;
  constref AFilePaths: TStrings): Boolean;
var
  AFile: TextFile;
  i, j, n, k: Integer;
  TextFigure: TStrings;
  Points: TFloatPoints;
  TempSkin: TElementSkinFPC;
begin
  if not OpenFile(AFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);

  {$I+}
  try
    ReadLn(AFile); // Comment string
    ReadLn(AFile, n); // Number of figures
    ReadLn(AFile); // Comment string
    SetLength(Self, n);
    for i:=0 to n-1 do // Figures
      with Self[i] do
      begin
        ReadLn(AFile); // Comment string
        ReadLn(AFile, TempSkin); // Skin
        ReadLn(AFile, k); // Number of elements
        // Elements
        SetLength(TextFigure, k);
        for j:=0 to k-1 do
          ReadLn(AFile, TextFigure[j]);
        Points.ImportFromStrings(TextFigure);
        SetLength(TextFigure, 0);
        SetLength(ElementsData, Length(Points));
        for j:=0 to High(ElementsData) do
          with ElementsData[j] do
          begin
            Position:=Points[j];
            Skin:=TempSkin;
          end;
        SetLength(Points, 0);
        ReadLn(AFile, RotationPoint.X, RotationPoint.Y); // Rotation point
      end;
    Result:=True;
  except
    SetLength(TextFigure, 0);
    Free;
    Result:=False;
  end;
  Close(AFile);
end;

procedure TFiguresDataHelper.ImportFromTextFigures(constref AFigures:
  TTextFigures);
var
  i: Integer;
begin
  if Length(AFigures)=0 then
    Exit;

  SetLength(Self, Length(AFigures));
  for i:=0 to High(AFigures) do
    Self[i].ImportFromTextFigure(AFigures[i]);
end;

procedure TFiguresDataHelper.CopyFrom(constref AFigures: TFiguresData);
var
  i: Integer;
begin
  Self:=Copy(AFigures, 0, Length(AFigures));
  for i:=Low(Self) to High(Self) do
    Self[i].ElementsData:=Copy(AFigures[i].ElementsData, 0,
      Length(AFigures[i].ElementsData));
end;

procedure TFiguresDataHelper.SetSkins(constref ASkins: TElementSkinsFPC);
var
  i, j, n: Integer;
begin
  n:=High(Self);
  if High(ASkins) < n then
    n:=High(ASkins);
  for i:=0 to n do
    for j:=0 to High(Self[i].ElementsData) do
      Self[i].ElementsData[j].Skin:=ASkins[i];
end;

procedure TFiguresDataHelper.SetSkinToAll(ASkin: TElementSkinFPC);
var
  i, j: Integer;
begin
  for i:=0 to High(Self) do
    for j:=0 to High(Self[i].ElementsData) do
      Self[i].ElementsData[j].Skin:=ASkin;
end;

function TFiguresDataHelper.LoadSkinsFromFile(constref AFileName: TString;
  constref AFilePaths: TStrings): Boolean;
begin
  Result:=False;
end;

procedure TFiguresDataHelper.Free;
var
  i: Integer;
begin
  for i:=Low(Self) to High(Self) do
    SetLength(Self[i].ElementsData, 0);
  SetLength(Self, 0);
end;


// TFigureHeplper = class helper for TFigure

procedure TFigureFPCHelper.Setup(constref AData: TFigureData;
  PutOnGameField: Boolean = False);
var
  i: Integer;
  Elems: TElements;
  //TempEl: TElementFPC;
begin
  // Checks?

  PutAwayFromField;
  DestroyElements;

  SetLength(Elems, Length(AData.ElementsData));
  for i:=Low(Elems) to High(Elems) do
    with AData.ElementsData[i] do
    begin
      Elems[i]:=TElementFPC.Create(Self, Position.X, Position.Y, Skin);
      if PutOnGameField and (GameField <> nil) then
        GameField.ElementsOnPos[Position]:=Elems[i];
    end;
  RotationPoint:=AData.RotationPoint;
  AddElements(Elems);
  SetLength(Elems, 0);

  {for i:=0 to High(AData.ElementsData) do
  begin
    with AData.ElementsData[i] do
      TempEl:=TElementFPC.Create(Self, Position.X, Position.Y, Skin);
    AddElement(TempEl); // It uses SetLength every time! But AddElements doesn't work..
    if PutOnGameField and (GameField<>nil) then
      GameField.ElementsOnPos[TempEl.Position]:=TempEl;
  end;
  RotationPoint:=AData.RotationPoint;}
end;

procedure TFigureFPCHelper.SetSkin(constref ASkin: TElementSkinFPC);
var
  i: Integer;
begin
  for i:=0 to Self.ElementsNumber-1 do
    //if Elements[i].ClassType = TElementFPC then
    if Elements[i] is TElementFPC then
      TElementFPC(Elements[i]).Skin:=ASkin;
end;

// It isn't used!
function TFigureFPCHelper.ToStrings(FillChar: Char = ' '): TStrings;
var
  i, MinX, MaxX, MinY, MaxY, Height, Width: Integer;
  Fills: TString;
begin
  // Not optimal solution. Self.M**X execute cycle every time!
  MinX:=Round(Self.MinX);
  MaxX:=Round(Self.MaxX);
  MinY:=Round(Self.MinY);
  MaxY:=Round(Self.MaxY);
  Height:=MaxY-MinY+1;
  Width:=MaxX-MinX+1;
  Fills:=FillString(FillChar, Width*ElementWidth);
  SetLength(Result, Height);
  for i:=0 to High(Result) do
    Result[i]:=Fills;
  for i:=0 to ElementsNumber-1 do
    with Elements[i].Position do
      ReplaceString(Result[MaxY-RoundY], TElementFPC(Elements[i]).Skin,
        (RoundX-MinX)*ElementWidth+1);
end;


// TFiguresFPCHelper = type helper(TFiguresHelper) for TFigures

procedure TFiguresFPCHelper.ImportFromData(constref AData: TFiguresData;
  AGameField: TGameField);
var
  i: Integer;
begin
  Free;

  SetLength(Self, Length(AData));
  for i:=Low(Self) to High(Self) do
  begin
    Self[i]:=TFigure.Create(AGameField);
    Self[i].Setup(AData[i], False);
  end;
end;

procedure TFiguresFPCHelper.SetSkins(constref ASkins: TElementSkinsFPC);
var
  i, j, n: Integer;
begin
  n:=High(Self);
  if High(ASkins) < n then
    n:=High(ASkins);
  for i:=0 to n do
    (*for j:=0 to Self[i].ElementsNumber-1{High(Self[i].ElementsData)} do
      //Self[i].ElementsData[j].Skin:=ASkins[i];
      TElementFPC(Self[i][j]).Skin:=ASkins[i];*)
    Self[i].SetSkin(ASkins[i]);
end;


// TOutputLabelsHelper = type helper for TOutputLabels

function TOutputLabelsHelper.SetCaption(AID: TOutputDataID; constref
  ACaption: AnsiString): Boolean;
begin
  Result:=Self[AID] <> nil;
  if Result then
    Self[AID].Caption:=ACaption;
end;


// TOutputLabelsDataHelper = type helper for TOutputLabalsData

function TOutputLabelsDataHelper.LoadFromFile(var AFile: TextFile): Boolean;
var
  i, n: Integer;
  OLD: TOutputLabelsData;
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile, n);
    SetLength(OLD, n);
    for i:=0 to n-1 do
      with OLD[i] do
        ReadLn(AFile, ID, BoardIndex, LabelIndex);

    SetLength(Self, 0);
    Self:=OLD;
    Result:=True;
  except
    SetLength(OLD, 0);
    Result:=False;
  end;
end;


// TTetrisFPCMessagesHelper = type helper for TTetrisFPCMessages

function TTetrisFPCMessagesHelper.LoadFromFile(var AFile: TextFile): Boolean;
var
  i: TTetrisFPCMessageID;
begin
  {$I+}
  try
    ReadLn(AFile);
    for i:=Low(i) to High(i) do
      ReadLn(AFile, Self[i]);
    Result:=True;
  except
    Result:=False;
  end;
end;


// TDestroyLinesAnimation = record

{class operator TDestroyLinesAnimation.:= (constref AAnimation:
  TDestroyLinesAnimation): TDestroyLinesAnimation;}
procedure TDestroyLinesAnimation.CopyFrom(
  constref AAnimation: TDestroyLinesAnimation);
begin
  Self:=AAnimation;
  Self.Frames:=Copy(AAnimation.Frames, 0, Length(AAnimation.Frames));
end;

function TDestroyLinesAnimation.LoadFromFile(var AFile: TextFile): Boolean;
var
  i, n: Integer;
  DLA: TDestroyLinesAnimation; // Use it!
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile, n);
    SetLength(Frames, n);
    for i:=0 to n-1 do
      ReadLn(AFile, Frames[i]);
    ReadLn(AFile, ReplaysNumber);
    ReadLn(AFile, Delay);
    Result:=True;
  except
    Result:=False;
  end;
end;

procedure TDestroyLinesAnimation.Clear;
begin
  SetLength(Frames, 0);
end;


// TDropDownTrace = record

function TDropDownTrace.LoadFromFile(var AFile: TextFile): Boolean;
var
  LShow: Integer;
  DDT: TDropDownTrace; // Use it!
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile, LShow);
    Show:=Boolean(LShow);
    ReadLn(AFile, Skin);
    ReadLn(AFile, Delay);
    Result:=True;
  except
    Result:=False;
  end;
end;


// TControlKeysHelper = type helper for TControlKeys

procedure TControlKeysHelper.Reset;
var
  GCI: TTetrisFPCGameCommandID;
begin
  for GCI:=Low(GCI) to High(GCI) do
    Self[GCI]:=0;
end;

{function TControlKeysHelper.LoadFromFile(var AFile: TextFile): Boolean; // All keys
begin
end;}

function TControlKeysHelper.LoadFromFile(var AFile: TextFile{;
  AKeysCount: Byte}): Boolean;
var
  GCI: TTetrisFPCGameCommandID;
  i, n: Integer;
  LKey: TKeyCode;
  CK: TControlKeys;
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile, n); //??
    CK.Reset;
    for i:=0 to n{AKeysCount}-1 do
    begin
      ReadLn(AFile, GCI, LKey);
      CK[GCI]:=LKey;
    end;

    Self:=CK;
    Result:=True;
  except
    Result:=False;
  end;
end;

function TControlKeysHelper.SaveToFile(var AFile: TextFile): Boolean;
var
  GCI: TTetrisFPCGameCommandID;
begin
  {$I+}
  try
    WriteLn(AFile, '////////// TControlKeys //////////');
    WriteLn(AFile, Length(Self));
    for GCI:=Low(Self) to High(Self) do
      WriteLn(AFile, GCI, ' ', Self[GCI]);

    Result:=True;
  except
    Result:=False;
  end;
end;


// TTetrisFPCGameScreenSettings = record

function TTetrisFPCGameScreenSettings.LoadFromFile(constref AFileName: TString;
  constref AFilePaths: TStrings): Boolean;
var
  AFile: TextFile;
begin
  if not OpenFile(AFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);
  Result:=LoadFromFile(AFile);
  Close(AFile);
end;

function TTetrisFPCGameScreenSettings.LoadFromFile(var AFile: TextFile): Boolean;
var
  LSettings: TTetrisFPCGameScreenSettings;
begin
  {$I+}
  try
    ReadLn(AFile);
    if not GameFieldData.LoadFromFile(AFile) then
      Exit(False);
    ReadLn(AFile);
    ReadLn(AFile, CanvasDeltaPos.X, CanvasDeltaPos.Y);
    if not BoardsData.LoadFromFile(AFile) then
      Exit(False);//??
    if not OutputLabelsData.LoadFromFile(AFile) then
      Exit(False);
    ReadLn(AFile);
    ReadLn(AFile, NextFigureCanvasIndex,
      NextFigurePosOnCanvas.X, NextFigurePosOnCanvas.Y);
    ReadLn(AFile);
    ReadLn(AFile, FallenElementSkin);
    if not DestroyLinesAnimation.LoadFromFile(AFile) then
      Exit(False);
    if not DropDownTrace.LoadFromFile(AFile) then
      Exit(False);
    ReadLn(AFile);
    ReadLn(AFile, BackgroundColor, TextColor);

    Result:=True;
  except
    Result:=False;
  end;
end;

procedure TTetrisFPCGameScreenSettings.Free;
begin
  BoardsData.Free;
  DestroyLinesAnimation.Clear;
  SetLength(OutputLabelsData, 0);
end;


// TBestScore = record

procedure TBestScore.Setup(constref AName: TPlayerName; AScore: Integer);
begin
  PlayerName:=AName;
  Score:=AScore;
end;

function TBestScore.LoadFromFile(constref AFileName: TString{; constref
  AFilePaths: TStrings}): Boolean;
var
  AFile: File of TBestScore;
  BS: TBestScore;
begin
  {$I+}
  Assign(AFile, AFileName);
  try
    // ??!!
    try
      Reset(AFile);
      Read(AFile, BS);
    finally
      Close(AFile);
    end;
    Self:=BS;
    Result:=True;
  except
    Result:=False;
  end;
end;

function TBestScore.SaveToFile(constref AFileName: TString): Boolean;
var
  AFile: File of TBestScore;
begin
  {$I+}
  Assign(AFile, AFileName);
  try
    Rewrite(AFile);
    Write(AFile, Self);
    Close(AFile);
    Result:=True;
  except
    //ShowMessage('Can''t save best score to file: '''+AFileName+'''');
    AddLog('Can''t save best score to file: '''+AFileName+'''');
    Result:=False;
  end;
end;

function TBestScore.ToString(const ASeparator: AnsiString = ' - ';
  ScoreFirst: Boolean = True): TString;
begin
  if ScoreFirst then
    Result:=IntToStr(Score, 0)+ASeparator+PlayerName
  else
    Result:=PlayerName+ASeparator+IntToStr(Score, 0);
end;


// TUserData = record

procedure TUserData.Setup(constref ABestScore: TBestScore;
  AScreenSettingsSetIndex: ShortInt; ABackgroundColor, ATextColor: Byte;
  AFiguresSkinsSetIndex: ShortInt; constref AScreenSettingsFileName,
  AFiguresSkinsFileName: AnsiString; constref AControlKeys: TControlKeys);
begin
  BestScore:=ABestScore;
  ScreenSettingsSetIndex:=AScreenSettingsSetIndex;
  BackgroundColor:=ABackgroundColor;
  TextColor:=ATextColor;
  FiguresSkinsSetIndex:=AFiguresSkinsSetIndex;
  ScreenSettingsFileName:=AScreenSettingsFileName;
  FiguresSkinsFileName:=AFiguresSkinsFileName;
  ControlKeys:=AControlKeys;
end;

procedure TUserData.SetDefault;
var
  LBS: TBestScore;
begin
  LBS.Setup('VSdev', 0);
  Setup(LBS, 0, Black, White, 0, '', '', DefaultControlKeys);
end;

function TUserData.LoadFromFile(constref AFileName: AnsiString): Boolean;
var
  AFile: File of TUserData;
  UD: TUserData;
begin
  {$I+}
  Assign(AFile, AFileName);
  try
    try // ??!!
      Reset(AFile);
      Read(AFile, UD);
    finally
      Close(AFile);
    end;
    Self:=UD;
    Result:=True;
  except
    Result:=False;
  end;
end;

function TUserData.SaveToFile(constref AFileName: AnsiString): Boolean;
var
  AFile: File of TUserData;
begin
  {$I+}
  Assign(AFile, AFileName);
  try
    Rewrite(AFile);
    Write(AFile, Self);
    Close(AFile);
    Result:=True;
  except
    //ShowMessage('Can''t save user data to file: '''+AFileName+'''');
    AddLog('Can''t save user data to file: '''+AFileName+'''');
    Result:=False;
  end;
end;


// TTetrisGameFPC class(TTetrisGameEngine)

(*procedure TTetrisGameFPC.LoadUserData;
var
  UD: TUserData;
begin
  if not UD.LoadFromFile({A}FUserDataFileName{'udata.tud'}) then
    UD.SetDefault;
  FBestScore:=UD.BestScore;
  FBackgroundColor:=UD.BackgroundColor;
  FTextColor:=UD.TextColor;
  FScreenSettingsSetIndex:=UD.ScreenSettingsSetIndex;
  FFiguresSkinsSetIndex:=UD.FiguresSkinsSetIndex;

  if
end;*)

procedure TTetrisGameFPC.SetupGameCommands;
var
  GCI: TTetrisFPCGameCommandID;
begin
  //if not FGameCommands we should get Commands from UserData
  for GCI:=Low(GCI) to High(GCI) do
    FGameCommands[GCI]:=nil;// TTetrisFPCGameCommands;
  FGameCommands[odiCKMoveRight]:=@Player.MoveRight;
  FGameCommands[odiCKMoveLeft]:=@Player.MoveLeft;
  FGameCommands[odiCKMoveDown]:=@Player.MoveDown;
  FGameCommands[odiCKDropDown]:=@{Player.}DropDownFigure;
  FGameCommands[odiCKRotateRight]:=@Player.RotateRight;
  FGameCommands[odiCKRotateLeft]:=@Player.RotateLeft;

  FGameCommands[odiCKPause]:=@Pause;
  FGameCommands[odiCKMenu]:=@GoToMenu;

  FGameCommands[odiCKMoveUp]:=@Player.MoveUp;
  FGameCommands[odiCKChangeFigure]:=@ChangeFigure;
  FGameCommands[odiCKChangeScreenSettingsSet]:=@ChangeScreenSet;
end;

procedure TTetrisGameFPC.SetupMenu(constref AMainMenuCaptions: TStrings);
begin
  FMenu[tmiSettings]:=TMenu.Create;
  FMenu[tmiSettings].Caption:=[{'', ''}];
  FMenu[tmiSettings].AddButton('Game screen skin', 1, 1,
    @SettingsMenuGameScreenSkin);
  FMenu[tmiSettings].AddButton('Colors', 1, 2, @SettingsMenuColors);
  FMenu[tmiSettings].AddButton('Figures skins', 1, 3,
    @SettingsMenuFiguresSkins);
  FMenu[tmiSettings].AddButton('Control keys', 1, 4, @SettingsMenuControlKeys);
  //FMenu[tmiSettings].AddButton('Load game from file', 1, 5, nil);
  //FMenu[tmiSettings].AddButton('Reset settings', 1, 6, nil);
  FMenu[tmiSettings].AddButton('About', 1, 7, @SettingsMenuAbout);
  FMenu[tmiSettings].AddButton('Back', 1, 8, @FMenu[tmiSettings].ExitMenu);
  //{!!}FMenu[tmiMain].ImportCaptions(AMainMenuCaptions^);
  FMenu[tmiSettings].AlignXToCenter;
  //FMenu[tmiSettings].AlignToTop(4, 1);
  FMenu[tmiSettings].AlignYToCenter(1, ScreenHeight, 1, False);
  FMenu[tmiSettings].OnExitQuery:=@SettingsMenuExitQuery;
  FMenu[tmiSettings].SetControlKey(@LoadGameFromFile, KeyDelete);

  FMenu[tmiMain]:=TMenu.Create;
  FMenu[tmiMain].Caption:=[];//[{'Main menu'}{'Tetris'}CaptionMainMenuTitle, ''];
  FMenu[tmiMain].AddButton(CaptionMainMenuNewGame, 3, 3, @NewGame);
  FMenu[tmiMain].AddButton(CaptionMainMenuSettings, 3, 4, @MainMenuSettings);
  FMenu[tmiMain].AddButton(CaptionMainMenuQuit, 3, 5, @FMenu[tmiMain].ExitMenu);
  {!!}FMenu[tmiMain].ImportCaptions(AMainMenuCaptions);
  FMenu[tmiMain].OnExitQuery:=@MainMenuExitQuery;
  FMenu[tmiMain].AlignXToCenter;
  //FMenu[tmiMain].AlignToTop(7-1, 1);
  FMenu[tmiMain].AlignYToCenter(1, ScreenHeight, 1, False);
  FMenu[tmiMain].SetControlKeys(metExitMenu, [{KeyEscape}], True);
  FMenu[tmiMain].SetControlKeys(@BackToGame, [KeyEscape], False);
  //
  //FMenu[tmiMain].CursorSkin:='<>';
  //FMenu[tmiMain].CursorDeltaX:=-3;

  FMenu[tmiMain].DrawMethod:=@MainMenuDrawMethod;
  FMenu[tmiSettings].DrawMethod:=@SettingsMenuDrawMethod;
end;

procedure TTetrisGameFPC.SetupControlKeys(constref AControlKeys: TControlKeys;
  DeletePrevious: Boolean = False);
var
  GCI: TTetrisFPCGameCommandID;
begin
  for GCI:=Low(GCI) to High(GCI) do
    if AControlKeys[GCI] <> 0 then
      PlayerCommands.SetCommands(FGameCommands[GCI], [AControlKeys[GCI]],
        DeletePrevious);

  {odiCKMoveRight, odiCKMoveLeft, odiCKMoveDown, odiCKDropDown,
  odiCKRotateRight, odiCKRotateLeft, odiCKPause, odiCKMenu,
  odiCKMoveUp, odiCKChangeFigure, odiCKChangeScreenSettingsSet}

  (*PlayerCommands.SetCommands(@GoToMenu, [KeyEscape], DeletePrevious);
  PlayerCommands.SetCommands(@Player.MoveLeft, [KeyLeft, KeyA, KeyBigA], DeletePrevious);
  PlayerCommands.SetCommands(@Player.MoveRight, [KeyRight, KeyD, KeyBigD], DeletePrevious);
  PlayerCommands.SetCommands(@Player.MoveDown, [KeyDown, KeyS, KeyBigS], DeletePrevious);
  PlayerCommands.SetCommands(@DropDownFigure{Player.DropDown}, [KeyEnter], DeletePrevious);
  PlayerCommands.SetCommands(@Player.RotateRight, [KeySpace], DeletePrevious);
  PlayerCommands.SetCommands(@Player.RotateLeft, [KeyBackspace{, KeyEnter}], DeletePrevious);
  PlayerCommands.SetCommands(@Pause, [KeyP, KeyBigP], DeletePrevious);
  // Temp controls
  PlayerCommands.SetCommands(@Player.MoveUp, [KeyUp], DeletePrevious);
  PlayerCommands.SetCommands(@ChangeFigure, [KeyTab], DeletePrevious);*)
  PlayerCommands.SetCommands(@SpeedUp, [KeyPlus], DeletePrevious);
  PlayerCommands.SetCommands(@SpeedDown, [KeyMinus], DeletePrevious);
  //PlayerCommands.SetCommands(@DoClrScr, [Key0], DeletePrevious);
  //PlayerCommands.SetCommands(@VisualEffect1, []);
  //PlayerCommands.SetCommands(@ChangeScreenSet, [KeyDelete], DeletePrevious);
  PlayerCommands.SetCommands(@ChangeFigureSkinsSet, [KeyHome], DeletePrevious);
  PlayerCommands.SetCommands(@LoadAllSettingsFromFile, [Key8], DeletePrevious);
end;

function TTetrisGameFPC.GetControlKeys: TControlKeys;
var
  GCI: TTetrisFPCGameCommandID;
  LKey: TKeyCode;
begin
  for GCI:=Low(GCI) to High(GCI) do
  begin
    Result[GCI]:=0;
    for LKey:=Low(LKey) to High(LKey) do
      if PlayerCommands[LKey] = FGameCommands[GCI] then
        Result[GCI]:=LKey;
  end;
end;

constructor TTetrisGameFPC.Create(
  constref AUserDataFileName: AnsiString = DefaultUserDataFileName;//'';
  AGameSettings: PTetrisGameSettings = nil;
  AFiguresData: PFiguresData = nil;
  AScreenSettings: PTetrisFPCGameScreenSettings = nil;
  AMainMenuCaptions: PStrings = nil;
  AMessages: PTetrisFPCMessages = nil);
var
  i: Integer;
  UD: TUserData;
  SS: TTetrisFPCGameScreenSettings;
  ES: TElementSkinsFPC;
  LFigures: TFigures;
begin
  FUserDataFileName:=AUserDataFileName;
  if not UD.LoadFromFile(FUserDataFileName) then
    UD.SetDefault;
  FBestScore:=UD.BestScore;
  FBackgroundColor:=UD.BackgroundColor;
  FTextColor:=UD.TextColor;
  FScreenSettingsSetIndex:=UD.ScreenSettingsSetIndex;
  FFiguresSkinsSetIndex:=UD.FiguresSkinsSetIndex;
  FScreenSettingsFileName:=UD.ScreenSettingsFileName;
  FFiguresSkinsFileName:=UD.FiguresSkinsFileName;

  if AGameSettings = nil then
    AGameSettings:=@DefaultGameSettings;
  if AFiguresData = nil then
    AFiguresData:=@DefaultFiguresData;

  SetLength(LFigures, 0);//??
  LFigures.ImportFromData(AFiguresData^, nil);
  if AScreenSettings = nil then
  begin
    if not IsIntInRange(FScreenSettingsSetIndex,
      Low(DefaultScreenSettingsSet), High(DefaultScreenSettingsSet)) then
    begin
      if SS.LoadFromFile(UD.ScreenSettingsFileName, ['']) then
        AScreenSettings:=@SS
      else
      begin
        FScreenSettingsSetIndex:=0;
        AScreenSettings:=@DefaultScreenSettingsSet[FScreenSettingsSetIndex];
      end;
    end
    else
      AScreenSettings:=@DefaultScreenSettingsSet[FScreenSettingsSetIndex];//!!
  end;
  if AMainMenuCaptions = nil then
    AMainMenuCaptions:=@DefaultMainMenuCaptions;
  if AMessages = nil then
    AMessages:=@DefaultMessages;

  inherited Create(AGameSettings^, LFigures);
  PlayerCommands.SetLength(High(TKeyCode)+1);//512

  if not IsIntInRange(FFiguresSkinsSetIndex,
    Low(DefaultFiguresSkinsSet), High(DefaultFiguresSkinsSet)) then
  begin
    if ES.LoadFromFile(FFiguresSkinsFileName) then
      Figures.SetSkins(ES)
    else
    begin
      FFiguresSkinsSetIndex:=0;
      Figures.SetSkins(DefaultFiguresSkinsSet[FFiguresSkinsSetIndex]);
    end;
  end
  else
    Figures.SetSkins(DefaultFiguresSkinsSet[FFiguresSkinsSetIndex]);

  with AScreenSettings^ do
  begin
    FGameFieldCanvas:=TTextCanvas.Create(nil, GameFieldData);
    FGameFieldCanvasDeltaPos:=CanvasDeltaPos;

    SetLength(FBoards, Length(BoardsData));
    for i:=Low(BoardsData) to High(BoardsData) do
      FBoards[i]:=TBoard.Create(nil, BoardsData[i]);

    ResetOutputLabels;
    OutputLabelsSetup(OutputLabelsData);

    FNextFigureCanvasIndex:=NextFigureCanvasIndex;
    FNextFigurePosOnCanvas:=NextFigurePosOnCanvas;
    FFallenElementSkin:=FallenElementSkin;

    FDestroyLinesAnimation.CopyFrom(DestroyLinesAnimation);
    FDropDownTrace:=DropDownTrace;
  end;
  TextBackground(FBackgroundColor);
  TextColor(FTextColor);
  CursorOff;

  {if not FBestScore.LoadFromFile('..\resource\bsdata.tbs') then
    FBestScore.Setup('VSDev', 0);}
  FOutputLabels.SetCaption(odiSBBestScore, FBestScore.ToString);

  FMessages:=AMessages^;
  SetupMenu(AMainMenuCaptions^);
  SetupGameCommands;
  FGameCommandNames:=DefaultGameCommandNames;
  SetupControlKeys(UD.ControlKeys, False);
  UpdateControlKeysOutput(UD.ControlKeys);

  GameField.OnChangeElement:=@GameChangeElement;
  OnLinesChanged:=@LinesChanged;
  LinesChanged;
  OnScoreChanged:=@ScoreChanged;
  ScoreChanged;
  OnSpeedChanged:=@SpeedChanged;
  SpeedChanged;
  OnHasLanded:=@HasLanded;
  OnDestroyLines:=@DestroyLinesEvent;

  FIsGameRunning:=False;
  FDropDown:=False;
end;

constructor TTetrisGameFPC.CreateFromFile(constref AFileName: AnsiString;
  constref AUserDataFileName: AnsiString = DefaultUserDataFileName);
var
  AFile: TextFile;
  LGameSettings: TTetrisGameSettings;
  LFiguresData: TFiguresData;
  LScreenSettings: TTetrisFPCGameScreenSettings;
  LMainMenuCaptions: TStrings;
  LMessages: TTetrisFPCMessages;
begin
  if OpenFile(AFile, AFileName, [''], ofmReset) then
  begin
    if LGameSettings.LoadFromFile(AFile) and
       LFiguresData.LoadFromFile(AFile) and
       LScreenSettings.LoadFromFile(AFile) and
       LMainMenuCaptions.LoadFromFile(AFile, MainMenuCaptionsNumber) and//??
       LMessages.LoadFromFile(AFile) then
    begin
      Create(AUserDataFileName, @LGameSettings, @LFiguresData, @LScreenSettings,
        @LMainMenuCaptions, @LMessages);
      LFiguresData.Free;
      LScreenSettings.Free;
      SetLength(LMainMenuCaptions, 0);
    end
    else
      Create(AUserDataFileName{, nil, nil, nil, nil, nil});
    Close(AFile);
  end
  else
    Create(AUserDataFileName{, nil, nil, nil, nil, nil});
end;

destructor TTetrisGameFPC.Destroy;
var
  i: Integer;
begin
  for i:=Ord(Low(FMenu)) to Ord(High(FMenu)) do
    FreeAndNil(FMenu[TTetrisFPCMenuID(i)]);
  for i:=High(FBoards) downto Low(FBoards) do
    FreeAndNil(FBoards[i]);
  SetLength(FBoards, 0);
  FreeAndNil(FGameFieldCanvas);
  FDestroyLinesAnimation.Clear;

  inherited Destroy;
end;

procedure TTetrisGameFPC.SetFiguresData(constref AData: TFiguresData);
begin
  Figures.ImportFromData(AData, nil);

  // ???
  //FFiguresData.SetSkins(DefaultFiguresSkinsSet[FFiguresSkinsSetIndex]);
end;

procedure TTetrisGameFPC.BoardsSetup(constref AData: TBoardsData);
var
  i, n: Integer;
begin
  n:=Length(AData)-Length(FBoards);
  if n > 0 then
  begin
    SetLength(FBoards, Length(AData));
    for i:=High(FBoards)-n+1 to High(FBoards) do
      FBoards[i]:=TBoard.Create(nil, AData[i]);
  end
  else
  if n < 0 then
  begin
    for i:=High(AData)+1 to High(FBoards) do
      FreeAndNil(FBoards[i]);
    SetLength(FBoards, Length(AData));
    n:=0;
  end;

  for i:=Low(FBoards) to High(FBoards)-n do
    FBoards[i].Setup(AData[i]);
end;

procedure TTetrisGameFPC.SetScreenSettings(constref ASettings:
  TTetrisFPCGameScreenSettings);
begin
  with ASettings do
  begin
    FGameFieldCanvas.Setup(GameFieldData);//It erases everything on FGameFieldCanvas
    FGameFieldCanvasDeltaPos:=CanvasDeltaPos;
    GameFieldCanvasRefresh;// ..so, we need to redraw it.

    BoardsSetup(BoardsData);

    ResetOutputLabels;
    OutputLabelsSetup(OutputLabelsData);

    FNextFigureCanvasIndex:=NextFigureCanvasIndex;
    FNextFigurePosOnCanvas:=NextFigurePosOnCanvas;
    FFallenElementSkin:=FallenElementSkin;

    FDestroyLinesAnimation.CopyFrom(DestroyLinesAnimation);
    FDropDownTrace:=DropDownTrace;

    FBackgroundColor:=BackgroundColor;
    FTextColor:=TextColor;
  end;
  TextBackground(FBackgroundColor);
  {CRT.}TextColor(FTextColor);

  //RefreshBoards;//!!!
  UpdateNextFigureCanvas;
  UpdateControlKeysOutput(GetControlKeys);
  {SetLines(Lines);
  SetScore(Score);
  SetSpeed(Speed);}

  //UpdateCaptions;
  LinesChanged;
  ScoreChanged;
  SpeedChanged;
  FOutputLabels.SetCaption(odiSBBestScore, FBestScore.ToString);

  // Update control keys!!!

  ResetLoopTime;
end;

{procedure TTetrisGameFPC.CanvasesSetup;
begin
end;}

procedure TTetrisGameFPC.Start;
begin
  PlayVSLogo1(True, [KeyEscape, KeyEnter, KeySpace], True,
    FBackgroundColor, FTextColor);
  FMenu[tmiMain].Run(True);
end;

procedure TTetrisGameFPC.ResetOutputLabels;
var
  i: TOutputDataID;
begin
  for i:=Low(i) to High(i) do
    FOutputLabels[i]:=nil;
end;

procedure TTetrisGameFPC.OutputLabelsSetup(constref AData: TOutputLabelsData);
var
  i: Integer;
begin
  for i:=Low(AData) to High(AData) do
    with AData[i] do
      if FBoards.DoesBoardExist(BoardIndex) and
        (FBoards[BoardIndex] <> nil) and
        (FBoards[BoardIndex].Labels[LabelIndex] <> nil) then
      FOutputLabels[ID]:=FBoards[BoardIndex].Labels[LabelIndex];
end;

procedure TTetrisGameFPC.UpdateControlKeysOutput(constref AControlKeys:
  TControlKeys);
var
  GCI: TTetrisFPCGameCommandID;
begin
  for GCI:=Low(GCI) to High(GCI) do
    if AControlKeys[GCI] <> 0 then
      FOutputLabels.SetCaption(GCI, DefaultKeyNames[AControlKeys[GCI]])
    else
      FOutputLabels.SetCaption(GCI, '');
end;

procedure TTetrisGameFPC.GetGameScreenAABB(var ATopLeft: TIntPoint;
  var AWidth, AHeight: Word);
var
  MinX, MaxX, MinY, MaxY, i, n: Integer;
begin
  with FGameFieldCanvas do
  begin
    n:=Length(FrameSkin);
    MinX:=Left-n;
    MaxX:=Left+Width+n-1;
    MinY:=Top-Byte(n>0);
    MaxY:=Top+Height+Byte(n>0)-1;
  end;

  for i:=Low(FBoards) to High(FBoards) do
    with FBoards[i] do
    begin
      n:=Length(FrameSkin);
      if (Left-n) < MinX then
        MinX:=Left-n
      else
      if (Left+Width+n-1) > MaxX then
        MaxX:=Left+Width+n-1;

      if (Top-Byte(n>0)) < MinY then
        MinY:=Top-Byte(n>0)
      else
      if (Top+Height+Byte(n>0)-1) > MaxY then
        MaxY:=Top+Height+Byte(n>0)-1;
    end;

  ATopLeft.Setup(MinX, MinY);
  AWidth:=MaxX-MinX+1;
  AHeight:=MaxY-MinY+1;
end;

function TTetrisGameFPC.GetGameScreenAABBStrings(var ATopLeft: TIntPoint;
  ABackgroundChar: Char = ' '): TStrings;
var
  LWidth, LHeight: Word;
  i: Integer;
begin
  GetGameScreenAABB(ATopLeft, LWidth, LHeight);

  Result:=FillStrings(ABackgroundChar, LWidth, LHeight);
  for i:=High(FBoards) downto Low(FBoards) do
    FBoards[i].DrawOnStrings(Result, -ATopLeft.X+1, -ATopLeft.Y);
  FGameFieldCanvas.DrawOnStrings(Result, -ATopLeft.X+1, -ATopLeft.Y);
end;

function TTetrisGameFPC.GetGameScreenFullStrings(
  ABackgroundChar: Char = ' '): TStrings;
var
  i: Integer;
begin
  Result:=FillStrings(ABackgroundChar, ScreenWidth, ScreenHeight);

  for i:=High(FBoards) downto Low(FBoards) do
    FBoards[i].DrawOnStrings(Result, 0, -1);
  FGameFieldCanvas.DrawOnStrings(Result, 0, -1);
end;

procedure TTetrisGameFPC.ResetCanvases(AWasChanged: Boolean = True);
var
  i: Integer;
begin
  FGameFieldCanvas.ResetChanged(AWasChanged);
  for i:=Low(FBoards) to High(FBoards) do
    FBoards[i].ResetChanged(AWasChanged);
end;

procedure TTetrisGameFPC.DrawGameScreen(ADoClrScr: Boolean = False);
var
  LCanv: TStrings;
  LTopLeft: TIntPoint;
begin
  // ??
  TextBackground(FBackgroundColor);
  TextColor(FTextColor);

  ResetCanvases(False);
  if ADoClrScr then
    ClrScr;
  LCanv:=GetGameScreenAABBStrings(LTopLeft, ' ');
  WriteStringsXY(LTopLeft, LCanv);
  //TVEFallUp(LCanv, LTopLeft.X, LTopLeft.Y, '|', 20, True, [KeyEnter]);
  {TVEFallUpWithFrame(LCanv, LTopLeft.X, LTopLeft.Y, '|', 10, 20,
    True, [KeyEnter]);}
  //ResetLoopTime;
end;

procedure TTetrisGameFPC.RunGameLoop;
begin
  // Without visual effect
  //DrawGameScreen(True);

  // With visual effect
  ResetCanvases(False);
  TVEFallUp(GetGameScreenFullStrings(' '), 1, 1, '/', 20, True, [KeyEnter]);//}

  inherited RunGameLoop;
end;

procedure TTetrisGameFPC.UpdateScreen;
var
  i: Integer;
begin
  inherited UpdateScreen;

  for i:=High(FBoards) downto Low(FBoards) do
    FBoards[i].Refresh;
  FGameFieldCanvas.Refresh;

  if DebugMode and ((GetTickCount-StartLoopTime) <> 0) and False then
    WriteStringXYInRect(1, 25, 'FPS: '+
      FloatToStr(OneSecond/(GetTickCount-StartLoopTime), 4, 1));
end;

procedure TTetrisGameFPC.NewGame;
begin
  ResetGame;
  FIsGameRunning:=True;
  RunGameLoop;
end;

procedure TTetrisGameFPC.Pause;
begin
  {WriteStringXYInRect((ScreenWidth-Length(FMessages[tmiPause])+2) div 2,
    ScreenHeight div 2, FMessages[tmiPause]);}
  WriteStringXYInRect(
    FGameFieldCanvas.Left+(FGameFieldCanvas.Width-FMessages[tmiPause].Length) div 2,
    FGameFieldCanvas.Top+(FGameFieldCanvas.Height-1) div 2,
    FMessages[tmiPause]);
  // It's not good!
  while not (GetKey in [KeyP, {KeyBigP,} KeySpace, KeyEnter, KeyEscape]) do;
  FGameFieldCanvas.DrawOnScreen; // Only FGameFieldCanvas??
  ResetLoopTime;
end;

procedure TTetrisGameFPC.BackToGame;
begin
  if FIsGameRunning then
    RunGameLoop
  else
    FMenu[tmiMain].ExitMenu;
end;

procedure TTetrisGameFPC.GoToMenu;
begin
  FMenu[tmiMain].Draw(True);
end;

procedure TTetrisGameFPC.GameOver;
begin
  FIsGameRunning:=False;

  TextColor(Red);
  {WriteStringXYInRect((ScreenWidth+2-Length(FMessages[tmiGameOver])) div 2,
    ScreenHeight div 2, FMessages[tmiGameOver]);}
  WriteStringXYInRect(
    FGameFieldCanvas.Left+(FGameFieldCanvas.Width-FMessages[tmiGameOver].Length) div 2,
    FGameFieldCanvas.Top+(FGameFieldCanvas.Height-1) div 2,
    FMessages[tmiGameOver]);
  while (GetKey <> KeyEscape) do ;
  TextColor(FTextColor);

  if Score > FBestScore.Score then
  begin
    FBestScore.Score:=Score;
    ClrScr;
    CursorOn;
    WriteStringXYInRect(
     (ScreenWidth+2-FMessages[tmiBestScoreCongrats].Length) div 2,
      ScreenHeight div 2 - 1, FMessages[tmiBestScoreCongrats]);
    WriteStringXYInRect(
     (ScreenWidth+2-FMessages[tmiEnterName].Length) div 2,
      ScreenHeight div 2 + 1, FMessages[tmiEnterName]);
    ReadLn(FBestScore.PlayerName);
    CursorOff;
    FOutputLabels.SetCaption(odiSBBestScore, FBestScore.ToString);
    //FBestScore.SaveToFile('..\resource\bsdata.tbs');
    //GetUserData.SaveToFile(FUserDataFileName);
    SaveUserData;
  end;

  FMenu[tmiMain].Draw(True);
end;

{procedure TTetrisGameFPC.DoClrScr;
begin
  ClrScr;
end;}

procedure TTetrisGameFPC.LinesChanged;
begin
  FOutputLabels.SetCaption(odiSBLines, IntToStr(Lines, 6));
end;

procedure TTetrisGameFPC.ScoreChanged;
begin
  FOutputLabels.SetCaption(odiSBScore, IntToStr(Score, 6));
end;

procedure TTetrisGameFPC.SpeedChanged;
begin
  FOutputLabels.SetCaption(odiSBSpeed, FloatToStr(Speed, 6, 1));
end;

{procedure TTetrisGameFPC.OnUpdateField;
begin
  FGameFieldCanvas.Refresh;
  Wait(500, True, [KeyEnter, KeyEscape, KeySpace]);
  //GetKey;
end;}

procedure TTetrisGameFPC.HasLanded;
begin
  if Length(FFallenElementSkin)<>2 then
    Exit;
  Player.SetSkin(FFallenElementSkin);
  Player.Refresh;
end;

procedure TTetrisGameFPC.MainMenuExitQuery(var ACanExit: Boolean);
begin
  ACanExit:=ShowYesNoDialog(FMessages[tmiWantToQuit], 1);
  if not ACanExit then
    FMenu[tmiMain].DrawStandartMethod(True);//Draw(True);
end;

procedure TTetrisGameFPC.SettingsMenuExitQuery(var ACanExit: Boolean);
begin
  ACanExit:=True;
  FMenu[tmiMain].Draw(True);
end;

procedure TTetrisGameFPC.MainMenuDrawMethod;
var
  LCanv: TStrings;
  LTopLeft: TIntPoint;
  LWidth, LHeight: Word;
begin
  (*//FMenu[tmiMain].Draw(True);
  GetGameScreenAABB(LTopLeft, LWidth, LHeight);
  //LCanv:=GetGameScreenAABBStrings(LTopLeft);
  LCanv:=FillStrings(' ', LWidth, LHeight);
  FMenu[tmiMain].DrawOnStrings(LCanv, -LTopLeft.X+1, -LTopLeft.Y+1);
  TVEFallDown(LCanv, LTopLeft.X, LTopLeft.Y, '|', 20, True, [KeyEscape]);
  {TVEFallDownWithFrame(LCanv, LTopLeft.X, LTopLeft.Y, '|', 10, 20,
    True, [KeyEscape]);}*)

  LCanv:=FillStrings(' ', ScreenWidth, ScreenHeight);
  FMenu[tmiMain].DrawOnStrings(LCanv, 0, 0);
  TVEFallDown(LCanv, 1, 1, '|', 20, True, [KeyEscape]);
end;

procedure TTetrisGameFPC.SettingsMenuDrawMethod;
var
  LCanv: TStrings;
  LTopLeft: TIntPoint;
  LWidth, LHeight: Word;
begin
  (*//FMenu[tmiSettings].Draw(True);
  GetGameScreenAABB(LTopLeft, LWidth, LHeight);
  //LCanv:=GetGameScreenAABBStrings(LTopLeft);
  LCanv:=FillStrings(' ', LWidth, LHeight);
  FMenu[tmiSettings].DrawOnStrings(LCanv, -LTopLeft.X+1, -LTopLeft.Y+1);
  TVEFallUp(LCanv, LTopLeft.X, LTopLeft.Y, '|', 20, True, [KeyEscape]);
  {TVEFallUpWithFrame(LCanv, LTopLeft.X, LTopLeft.Y, '|', 10, 20,
    True, [KeyEscape]);}*)

  LCanv:=FillStrings(' ', ScreenWidth, ScreenHeight);
  FMenu[tmiSettings].DrawOnStrings(LCanv, 0, 0);
  TVEFallUp(LCanv, 1, 1, '|', 20, True, [KeyEscape]);
end;

procedure TTetrisGameFPC.GetAndDoPlayerCommand(var BreakGameLoop: Boolean);
begin
  BreakGameLoop:=False;
  while KeyPressed do
  //if KeyPressed then
    if not BreakGameLoop then
      BreakGameLoop:=PlayerCommands.DoCommand(GetKey) = @GoToMenu
    else
      GetKey;
end;

function TTetrisGameFPC.SpawnFigure: Boolean;
begin
  Result:=inherited SpawnFigure;
  {if not Result then
    Exit;}
  UpdateNextFigureCanvas;
end;

procedure TTetrisGameFPC.SetSpeed(ASpeed: Extended);
begin
  if Speed <> ASpeed then
    with FGameFieldCanvas do
      TVEFallDown(Strings, Left, Top, '|', 20, False, nil);
  inherited SetSpeed(ASpeed);
end;

procedure TTetrisGameFPC.UpdateNextFigureCanvas;
begin
  if IsIntInRange(FNextFigureCanvasIndex, Low(FBoards), High(FBoards)) and
    (FBoards[FNextFigureCanvasIndex] <> nil) then
    with FBoards[FNextFigureCanvasIndex] do
    begin
      Clear;
      //try
        DrawStrings(FNextFigurePosOnCanvas,
          NextFigure.ToStrings(BackgroundChar));
      {except
        ShowMessage('Yep: FiguresData Len = '+IntToStr(Length(FFiguresData))+
          ', FigureNumber = '+IntToStr(NextFigureNumber));
      end;//}
    end;
end;

procedure TTetrisGameFPC.GameFieldCanvasRefresh;
var
  i, j: Integer;
begin
  for i:=0 to GameField.Height-1 do
    for j:=0 to GameField.Width-1 do
      GameField[j, i]:=Gamefield[j, i];
end;

function TTetrisGameFPC.GameFieldCoordsToCanvas(AX, AY: TFloatCoord):
  TIntPoint;
begin
  Result.Setup(
    Round(FGameFieldCanvasDeltaPos.X+AX*ElementWidth),
    Round(FGameFieldCanvasDeltaPos.Y-AY));
  {Result.Setup(
    Round((FGameFieldCanvasDeltaPos.X+1)+AX*ElementWidth),
    Round((-FGameFieldCanvasDeltaPos.Y+FGameFieldCanvas.Height-1)-AY));}
end;

function TTetrisGameFPC.GameFieldCoordsToCanvas(constref APosition:
  TFloatPoint): TIntPoint;
begin
  Result:=GameFieldCoordsToCanvas(APosition.X, APosition.Y);
end;

procedure TTetrisGameFPC.GameChangeElement(AX, AY: TFloatCoord{TIntCoord};
  AElement: TElement);
var
  TempSkin: TElementSkinFPC;
begin
  if FGameFieldCanvas <> nil then
  begin
    if AElement<>nil then
      TempSkin:=TElementFPC(AElement).Skin
    else
    begin
      if FDropDown {and FDropDownTrace.Show} then
        TempSkin:=FDropDownTrace.Skin
      else
        TempSkin:=FillString(FGameFieldCanvas.BackgroundChar, ElementWidth);
    end;

    FGameFieldCanvas.DrawString(GameFieldCoordsToCanvas(AX, AY), TempSkin);
  end;
end;

function TTetrisGameFPC.LoadFiguresDataFromFile(constref AFileName:
  AnsiString; constref AFilePaths: TStrings): Boolean;
var
  LFiguresData: TFiguresData;
begin
  Result:=LFiguresData.LoadFromFile(AFileName, AFilePaths);
  if Result then
    Figures.ImportFromData(LFiguresData, nil{GameField});
  LFiguresData.Free;
end;

function TTetrisGameFPC.LoadFiguresDataFromFile(var AFile: TextFile): Boolean;
var
  LFiguresData: TFiguresData;
begin
  Result:=LFiguresData.LoadFromFile(AFile);
  if Result then
    Figures.ImportFromData(LFiguresData, nil{GameField});
  LFiguresData.Free;
end;

function TTetrisGameFPC.LoadFiguresDataFromFileTxt(constref AFileName:
  AnsiString; constref AFilePaths: TStrings): Boolean;
var
  LFiguresData: TFiguresData;
begin
  Result:=LFiguresData.LoadFromFileTxt(AFileName, AFilePaths);
  if Result then
    Figures.ImportFromData(LFiguresData, nil{GameField});
  LFiguresData.Free;
end;

function TTetrisGameFPC.LoadScreenSettingsFromFile(var AFile: TextFile):
  Boolean;
var
  LSettings: TTetrisFPCGameScreenSettings;
begin
  Result:=LSettings.LoadFromFile(AFile);
  if Result then
    SetScreenSettings(LSettings);
end;

function TTetrisGameFPC.LoadScreenSettingsFromFile(constref AFileName:
  TString; constref AFilePaths: TStrings): Boolean;
var
  LSettings: TTetrisFPCGameScreenSettings;
begin
  Result:=LSettings.LoadFromFile(AFileName, AFilePaths);
  if Result then
    SetScreenSettings(LSettings);
end;

function TTetrisGameFPC.LoadAllSettingsFromFile(constref AFileName: TString;
  constref AFilePaths: TStrings): Boolean;
var
  LFile: TextFile;
begin
  if not OpenFile(LFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);

  Result:={inherited} LoadSettingsFromFile(LFile) and
    LoadFiguresDataFromFile(LFile) and
    LoadScreenSettingsFromFile(LFile) and
    FMenu[tmiMain].LoadCaptionsFromFile(LFile) and
    FMessages.LoadFromFile(LFile);
  Close(LFile);
end;

function TTetrisGameFPC.LoadCaptionsFromFile(constref AFileName: TString;
  constref AFilePaths: TStrings): Boolean;
var
  LFile: TextFile;
  i: Integer;
begin
  if not OpenFile(LFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);

  for i:=Low(FBoards) to High(FBoards) do
  begin
    Result:=FBoards[i].LoadCaptionsFromFile(LFile);
    if not Result then
      Break;
  end;

  Result:=Result and
    FMenu[tmiMain].LoadCaptionsFromFile(LFile) and
    FMessages.LoadFromFile(LFile);
  Close(LFile);
end;

procedure TTetrisGameFPC.DestroyLinesEvent({constref} ALines:
  TIntList);
var
  i, j: Integer;
  TempNode: {TIntList;//}PIntListNode;
  //LStartTime: LongWord;
begin
  with FDestroyLinesAnimation do
  begin
    if ALines.IsEmpty or (Length(Frames) = 0) or (ReplaysNumber = 0) then
      Exit;

    for i:=1 to ReplaysNumber do
      for j:=Low(Frames) to High(Frames) do
      begin
        TempNode:=ALines;
        while not TempNode.IsEmpty do
        begin
          if Length(Frames[j])=0 then
            FGameFieldCanvas.ClearLine(GameFieldCoordsToCanvas(0, TempNode^.Value).Y)
          else
            FGameFieldCanvas.DrawString(GameFieldCoordsToCanvas(0, TempNode^.Value),
              FillString(Frames[j],
              (GameField.Width*ElementWidth) div Length(Frames[j])));
          TempNode:=TempNode^.Next;
        end;
        FGameFieldCanvas.Refresh;
        Wait(Delay, False, nil);
      end;
  end;

  ResetLoopTime;
end;

procedure TTetrisGameFPC.DropDownFigure;
begin
  if FDropDownTrace.Show then
    FDropDown:=True;

  Player.DropDown;

  if FDropDown then
  begin
    FDropDown:=False;
    FGameFieldCanvas.Refresh;
    Wait{Delay}(FDropDownTrace.Delay, False, nil);

    GameFieldCanvasRefresh;
    FGameFieldCanvas.Refresh;
    ResetLoopTime;
  end;
end;

procedure TTetrisGameFPC.MainMenuSettings;
begin
  FMenu[tmiSettings].Run(True);
end;

procedure TTetrisGameFPC.SettingsMenuGameScreenSkin;
const
  MsgMenuSettingsGameScreenSkinHint =
    'Hint: use Ctrl + Scroll up to zoom the window';
  MsgMenuSettingsGameScreenSkinCaption =
    'Choose set (use keys up/down, enter):';
var
  i: Integer;
  MenuSet: TStrings;
  BgColor, TxtColor: Byte;
  //LFileName: AnsiString;
begin
  //ClrScr;
  DrawGameScreen(True);

  BgColor:=FBackgroundColor;
  TxtColor:=FTextColor;
  SetLength(MenuSet, Length(DefaultScreenSettingsSet)+1);
  for i:=Low(MenuSet) to High(MenuSet)-1 do
    MenuSet[i]:=IntToStr(i+1);
  MenuSet[High(MenuSet)]:='file';
  if IsIntInRange(FScreenSettingsSetIndex, Low(MenuSet), High(MenuSet)) then
    MenuSet[FScreenSettingsSetIndex]:=MenuSet[FScreenSettingsSetIndex]+'<';
  CursorOn;
  repeat
    WriteStringXYInRect(1, ScreenHeight, MsgMenuSettingsGameScreenSkinHint);
    i:=ReadIntFromMenuXY(
      [MsgMenuSettingsGameScreenSkinCaption, ''],
      MenuSet, 1, 1, 0, False, FScreenSettingsSetIndex, True, False);
    if i = -1 then
      Break;
    if i = FScreenSettingsSetIndex then
      Continue;

    if i = High(MenuSet) then
    begin
      FScreenSettingsFileName{LFileName}:=ReadString('Enter file name:', True);
      if not LoadScreenSettingsFromFile(FScreenSettingsFileName{LFileName},
        ['']) then
      begin
        ShowMessage('Can''t open/read file: '''+FScreenSettingsFileName
          {LFileName}+'''');
        DrawGameScreen(True);
        Continue;
      end;
    end
    else
      SetScreenSettings(DefaultScreenSettingsSet[i{FScreenSettingsSetIndex}]);

    SetLength(MenuSet[FScreenSettingsSetIndex],
      Length(MenuSet[FScreenSettingsSetIndex])-1);
    FScreenSettingsSetIndex:=i;
    MenuSet[FScreenSettingsSetIndex]:=MenuSet[FScreenSettingsSetIndex]+'<';

    FBackgroundColor:=BgColor;
    FTextColor:=TxtColor;
    SaveUserData;
    DrawGameScreen(True);
  until False;

  SetLength(MenuSet, 0);
  CursorOff;
  FMenu[tmiSettings].Draw(True);
end;

procedure TTetrisGameFPC.SettingsMenuColors;
const
  MsgMenuSettingsColorsCaption = 'Choose background/text color ('+
    'use keys up/down/left/right and enter):';
  //MsgSettingsMenuColorsUseKeys = 'Use keys up/down/left/right and enter';
  LTop = 1+2;
  LLeft = 1;
  LBgFills = '||';
  LTxtFills = '||';
  LCurSkin = '<>';
  Map: array[0..1] of Byte = (LightGray, White);
var
  i: Integer;
  p: ^Byte;
  CurPos, TP: TIntPoint;
  Key: TKeyCode;
begin
  CurPos.Setup(0, FBackgroundColor);

  repeat
    CursorOff;
    //TextBackground(FBackgroundColor);
    //TextColor(FTextColor);
    DrawGameScreen(True);
    //WriteStringXYInRect(1, 25, MsgSettingsMenuColorsUseKeys);
    WriteStringXYInRect(1, 1, MsgMenuSettingsColorsCaption);

    // Draw color menu
    for i:=Black to Map[0]{LightWhite} do
    begin
      TextBackground(i);
       //TextColor(FTextColor);
      if i <> FBackgroundColor then
        WriteStringXYInRect(LLeft, LTop+i, LBgFills)
      else
        WriteStringXYInRect(LLeft, LTop+i,
           FillString(LCurSkin, Length(LBgFills) div Length(LCurSkin)));
    end;
    TextBackground(FBackgroundColor);

    for i:=Black to Map[1]{White} do
    begin
      //TextBackground(FBackgroundColor);
      TextColor(i);
      if i <> FTextColor then
        WriteStringXYInRect(LLeft+Length(LBgFills), LTop+i, LTxtFills)
      else
        WriteStringXYInRect(LLeft+Length(LBgFills), LTop+i,
          FillString(LCurSkin, Length(LTxtFills) div Length(LCurSkin)));
    end;
    TextColor(FTextColor);

    CursorOn;
    repeat
      TP.Setup(LLeft+CurPos.X*Length(LBgFills), LTop+CurPos.Y);
      if TP.IsItOnScreen then
        GoToXY(TP.X, TP.Y);

      Key:=GetKey;
      case Key of
        KeyUp:
          if CurPos.Y > 0 then
            CurPos.Shift(0, -1)
          else
            CurPos.Shift(0, Map[CurPos.X]);
        KeyDown:
          if CurPos.Y < Map[CurPos.X] then
            CurPos.Shift(0, 1)
          else
            CurPos.Y:=0;
        KeyLeft:
          if (CurPos.X > 0) and (CurPos.Y <= Map[CurPos.X-1]) then
            CurPos.Shift(-1, 0);
        KeyRight:
          if (CurPos.X < High(Map)) then
            CurPos.Shift(1, 0);
        KeyEnter: begin
          case CurPos.X of
            0: p:=@FBackgroundColor;
            1: p:=@FTextColor;
          end;
          if p^ <> CurPos.Y then
          begin
            p^:=CurPos.Y;
            SaveUserData;
            Break;
          end;
        end;
        KeyEscape: Break;
      end;
    until False;
  until Key = KeyEscape;

  //SaveUserData;

  CursorOff;
  FMenu[tmiSettings].Draw(True);
end;

{//Hint
function ReadIntFromMenuXY(constref AHeader: TStrings;
  constref AMenu: TStrings; X0: Byte = 1; Y0: Byte = 1;
  LineInterval: Byte = 0; Centered: Boolean = False; CurPos: SmallInt = 0;
  CurGoThrough: Boolean = False; DoClrScr: Boolean = True): SmallInt;//Byte;}

procedure TTetrisGameFPC.SettingsMenuFiguresSkins;
const
  MsgSettingsMenuFiguresSkinsCaption1 =
    'Choose skins set (use keys up/down and enter):';
  MsgSettingsMenuFiguresSkinsCaption2 = '       I  L  J  S  Z  T  O';
  CurSkin = '<';
var
  i, j: Integer;
  LMenu: TStrings;
  ES: TElementSkinsFPC;
begin
  //ClrScr;//??

  SetLength(LMenu, Length(DefaultFiguresSkinsSet)+1);
  for i:=Low(DefaultFiguresSkinsSet) to High(DefaultFiguresSkinsSet) do
  begin
    LMenu[i]:='Set '+IntToStr(i+1)+': ';
    for j:=Low(DefaultFiguresSkinsSet[i]) to High(DefaultFiguresSkinsSet[i]) do
    begin
      LMenu[i]:=LMenu[i]+DefaultFiguresSkinsSet[i, j];
      if j <> High(DefaultFiguresSkinsSet[i]) then
        LMenu[i]:=LMenu[i]+' ';
    end;
  end;
  LMenu[High(LMenu)]:='Load from file ('''+FFiguresSkinsFileName+''')';
  if IsIntInRange(FFiguresSkinsSetIndex, Low(LMenu), High(LMenu)) then
    LMenu[FFiguresSkinsSetIndex]:=LMenu[FFiguresSkinsSetIndex]+CurSkin;

  repeat
    CursorOff;
    ClrScr;
    for i:=0 to FiguresNumber-1 do
      WriteStringsXY(7+i*5*2+1, 12, Figures[i].ToStrings(' '));

    repeat
      CursorOff;
      j:=ReadIntFromMenuXY([MsgSettingsMenuFiguresSkinsCaption1,
        MsgSettingsMenuFiguresSkinsCaption2],
        LMenu, 1, 1, 0, False, FFiguresSkinsSetIndex, True, False);
    until (j = -1) or (j <> FFiguresSkinsSetIndex);

    if j = -1 then
      Break;

    if j = High(LMenu) then
    begin
      FFiguresSkinsFileName:=ReadString('Enter file name:', True);
      if not ES.LoadFromFile(FFiguresSkinsFileName) then
      //if not LoadScreenSettingsFromFile(FFiguresSkinsFileName, ['']) then
      begin
        ShowMessage('Can''t open/read file: '''+FFiguresSkinsFileName+'''');
        //DrawGameScreen(True);
        Continue;
      end
      else
        Figures.SetSkins(ES);
    end
    else
      Figures.SetSkins(DefaultFiguresSkinsSet[j{FFiguresSkinsSetIndex}]);

    SetLength(LMenu[FFiguresSkinsSetIndex],
      Length(LMenu[FFiguresSkinsSetIndex])-Length(CurSkin));
    FFiguresSkinsSetIndex:=j;
    LMenu[FFiguresSkinsSetIndex]:=LMenu[FFiguresSkinsSetIndex]+CurSkin;

    SaveUserData;
  until False;

  //SaveUserData;

  SetLength(LMenu, 0);
  CursorOff;
  FMenu[tmiSettings].Draw(True);
end;

procedure TTetrisGameFPC.SettingsMenuControlKeys;
const
  MsgSettingsMenuControlKeysHint =
    'Hint: Use keys up/down to navigate and Enter to change the key';
    {'Hint: Use keys up/down to choose the '+
    'command you want to change, press Enter and press the new key'}
  KeysToChange: array of TTetrisFPCGameCommandID = (
    odiCKMoveRight, odiCKMoveLeft, odiCKMoveDown, odiCKDropDown,
    odiCKRotateRight, odiCKRotateLeft, odiCKPause{, odiCKMenu,
    odiCKMoveUp, odiCKChangeFigure, odiCKChangeScreenSettingsSet});
  LineInterval = 2;
  Separator = ': ';
  BgChar = ' ';
  NoKeyCapt = 'none';
var
  CKs: TControlKeys;
  Pos, HalfScrWidth, LTop, LLeft, ScrY: Integer;
  GCI: TTetrisFPCGameCommandID;
  LKey: TKeyCode;
  NeedToDraw: Boolean;

  procedure DrawScreen;
  var
    i: Integer;
    Str: AnsiString;
  begin
    CursorOff;
    ClrScr;
    //WriteLn(MsgSettingsMenuControlKeysHint);
    WriteStringXYInRect(1, 1, MsgSettingsMenuControlKeysHint);
    for i:=Low(KeysToChange) to High(KeysToChange) do
    begin
      if CKs[KeysToChange[i]] <> 0 then
        Str:=DefaultKeyNames[CKs[KeysToChange[i]]]
      else
        Str:=NoKeyCapt;
      WriteStringXYInRect(HalfScrWidth-Length(
        FGameCommandNames[KeysToChange[i]]),
        1+LTop+i*LineInterval,
        FGameCommandNames[KeysToChange[i]]+Separator+Str);
    end;
    CursorOn;
  end;
begin
  DefaultKeyNames[0]:=NoKeyCapt;//??

  CKs:=GetControlKeys;
  HalfScrWidth:=ScreenWidth div 2;
  LTop:=1+(ScreenHeight-Length(KeysToChange)*LineInterval) div 2;
  LLeft:=HalfScrWidth+Length(Separator);
  Pos:=Low(KeysToChange);
  NeedToDraw:=True;
  repeat
    if NeedToDraw then
    begin
      DrawScreen;
      NeedToDraw:=False;
    end;

    ScrY:=1+LTop+Pos*LineInterval;
    GoToXY(LLeft, ScrY);
    case GetKey of
      KeyUp: DecInRange(Pos, Low(KeysToChange), High(KeysToChange), True);
      KeyDown: IncInRange(Pos, Low(KeysToChange), High(KeysToChange), True);
      KeyEnter: begin
        WriteStringXYInRect(LLeft, ScrY, FillString(BgChar,
          Length(DefaultKeyNames[CKs[KeysToChange[Pos]]])));
        NeedToDraw:=True;
        GoToXY(LLeft, ScrY);
        LKey:=GetKey;
        if LKey = KeyEscape then
          Continue;

        for GCI:=Low(GCI) to High(GCI) do
          if (GCI <> KeysToChange[Pos]) and (CKs[GCI] = LKey) then
          begin
            //PlayerCommands[{CKs[GCI]}LKey]:=nil;
            CKs[GCI]:=0;
          end;
        CKs[KeysToChange[Pos]]:=LKey;
        //PlayerCommands[LKey]:=FGameCommands[KeysToChange[Pos]];
        SetupControlKeys(CKs, True);// It isn't an optimal solution
        SaveUserData(@CKs);
        UpdateControlKeysOutput(CKs);
      end;
      KeyEscape: Break;
    end;
  until False;

  {var TempBoard: TBoard;
  ClrScr;
  TempBoard:=TBoard.Create(nil, DefaultControlKeysBoardData);
  TempBoard.Left:=(ScreenWidth-TempBoard.Width) div 2 + 1;
  TempBoard.Top:=(ScreenHeight-TempBoard.Height) div 2 + 1;
  TempBoard.DrawOnScreen;
  GetKeyEscape;
  TempBoard.Free;}

  CursorOff;
  FMenu[tmiSettings].Draw(True);
end;

procedure TTetrisGameFPC.SettingsMenuAbout;
begin
  PlayVSLogo3(True, [KeyEscape], True, -1, -1);//Black, Green);//FBackgroundColor, FTextColor);

  WriteStringsXY(
    1+(ScreenWidth-StringsMaxLength(VSAboutStrings1)) div 2,
    0+(ScreenHeight-Length(VSAboutStrings1)) div 2, VSAboutStrings1);

  while GetKey <> KeyEscape do;

  TextBackground(FBackgroundColor);
  TextColor(FTextColor);

  FMenu[tmiSettings].Draw(True);
end;

function TTetrisGameFPC.SaveUserData(AControlKeys: PControlKeys = nil):
  Boolean;
var
  UD: TUserData;
  CKs: TControlKeys;
begin
  if AControlKeys = nil then
  begin
    CKs:=GetControlKeys;
    AControlKeys:=@CKs;
  end;
  UD.Setup(FBestScore, FScreenSettingsSetIndex, FBackgroundColor, FTextColor,
    FFiguresSkinsSetIndex, FScreenSettingsFileName, FFiguresSkinsFileName,
    AControlKeys^);
  Result:=UD.SaveToFile(FUserDataFileName);
end;

procedure TTetrisGameFPC.ChangeFigure;
begin
  SpawnFigure;
end;

procedure TTetrisGameFPC.ChangeScreenSet;
var
  LBkgCol, LTxtCol: Byte;
begin
  if FScreenSettingsSetIndex >= High(DefaultScreenSettingsSet) then
    FScreenSettingsSetIndex:=0
  else
    FScreenSettingsSetIndex:=FScreenSettingsSetIndex+1;

  {LBkgCol:=FBackgroundColor;
  LTxtCol:=FTextColor;}
  SetScreenSettings(DefaultScreenSettingsSet[FScreenSettingsSetIndex]);
  {FBackgroundColor:=LBkgCol;
  FTextColor:=LTxtCol;}
  //TextBackground(FBackgroundColor);
  //TextColor(FTextColor;);

  SaveUserData;

  //??
  DrawGameScreen(True);
end;

procedure TTetrisGameFPC.ChangeFigureSkinsSet;
begin
  if FFiguresSkinsSetIndex >= High(DefaultFiguresSkinsSet) then
    FFiguresSkinsSetIndex:=0
  else
    FFiguresSkinsSetIndex:=FFiguresSkinsSetIndex+1;

  Figures.SetSkins(DefaultFiguresSkinsSet[FFiguresSkinsSetIndex]);
end;

procedure TTetrisGameFPC.LoadAllSettingsFromFile;
begin
  if not LoadAllSettingsFromFile('Settings.txt', DefaultFilePaths) then
    ShowMessage('Can''t load all settings from file');

  UpdateNextFigureCanvas;

  DrawGameScreen(True);
  ResetLoopTime;
end;

procedure TTetrisGameFPC.LoadGameFromFile;
var
  LFileName: AnsiString;
begin
  ClrScr;
  CursorOn;

  LFileName:=ReadString('Load game from file. Enter the file name:');
  CursorOff;

  if LoadAllSettingsFromFile(LFileName, DefaultFilePaths) then
  begin
    FScreenSettingsSetIndex:=High(DefaultScreenSettingsSet)+1;
    FFiguresSkinsSetIndex:=High(DefaultFiguresSkinsSet)+1;

    FScreenSettingsFileName:='';
    FFiguresSkinsFileName:='';

    SaveUserData;
  end
  else
    ShowMessage('Can''t open/read file '''+LFileName+'''');

  FMenu[tmiSettings].Draw(True);
end;


//begin
//initialization
//finalization
end.

