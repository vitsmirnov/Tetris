(*
  (c) Vitaly Smirnov (VSdev)
  mrmaybelately@gmail.com
  2022-2023
*)

unit VSTextCanvas;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
//{$I-}
{$H+} // String is AnsiString
//{$J}
//{$B}

interface

uses
  VSPoints, VSStrings;

const
  // ??
  //SpaceChar = ' ';
  //EmptyTextPixel = ' ';
  {Max}FrameWidth = 2;

type
  // Classes list
  TTextCanvas = class;
  TLabel = class;
  TBoard = class;

  //TTextPixel = Char;

  TStringsObject = TObject;

  TChangedLines = array of Boolean;

  TFrameSkin = String[FrameWidth{2}];

  TTextCanvasSettings = record
    Position: TIntPoint;
    Width: TSize;
    Height: TSize;
    BackgroundChar: Char;
    FrameSkin: TFrameSkin;

    function LoadFromFile(var AFile: TextFile): Boolean;
  end;

  TTextCanvas = class(TStringsObject)
  public const
    DefaultLeft = 1;
    DefaultTop = 1;
    DefaultWidth = 20;
    DefaultHeight = 10;
    DefaultBackgroundChar = ' ';
    DefaultFrameSkin = '';
  private
    FOwner: TTextCanvas;
    //FBackground: TStrings;
    FCanvas: TStrings;
    FChangedLines: TChangedLines;
    FPosition: TIntPoint;
    FBackgroundChar: Char;
    FFrameSkin: TFrameSkin;
    FFrameWasChanged: Boolean;

    function GetChar(AX, AY: TIntCoord): Char;
    procedure SetChar(AX, AY: TIntCoord; AValue: Char);
    function GetChar(const APosition: TIntPoint): Char;
    procedure SetChar(const APosition: TIntPoint; AValue: Char);
    function GetLeft: TIntCoord;
    procedure SetLeft(AValue: TIntCoord);
    function GetTop: TIntCoord;
    procedure SetTop(AValue: TIntCoord);
    function GetPosition: TIntPoint;
    function GetWidth: TSize;
    function GetHeight: TSize;
    procedure SetFrameSkin(AValue: TFrameSkin);
  protected
    function CanvasWithFrame: TStrings;
    function CleanPosition: TIntPoint;
    function CleanPosition(AX, AY: TIntCoord): TIntPoint;
  public
    constructor Create(AOwner: TTextCanvas = nil;
      ALeft: TIntCoord = DefaultLeft; ATop: TIntCoord = DefaultTop;
      AWidth: TSize = DefaultWidth; AHeight: TSize = DefaultHeight;
      ABackgroundChar: Char = DefaultBackgroundChar;
      AFrameSkin: TFrameSkin = DefaultFrameSkin);
    constructor Create(AOwner: TTextCanvas; constref AData:
      TTextCanvasSettings);
    destructor Destroy; override;

    procedure Setup(ALeft, ATop: TIntCoord; AWidth, AHeight: TSize;
      ABackgroundChar: Char; AFrameSkin: TFrameSkin);
    procedure Setup(constref AData: TTextCanvasSettings);
    {function LoadSettingsFromFile(constref AFileName: TString; constref
      AFilePaths: TStrings): Boolean;}

    procedure ResetChanged(WasChanged: Boolean = True);

    procedure DrawOnScreen(DoClrScr: Boolean = False); //virtual;
    procedure DrawOnStrings(var AStrings: TStrings; ADeltaX: TIntCoord = 0;
      ADeltaY: TIntCoord = 0);
    procedure DrawOnCanvas(ACanvas: TTextCanvas; ADeltaX: TIntCoord = 0;
      ADeltaY: TIntCoord = 0);
    procedure Refresh; virtual;

    procedure Fill(AChar: Char);
    procedure Clear; virtual;
    procedure ClearLine(ALine: TIntCoord);
    procedure ClearLines(constref ALines: array of TIntCoord);
    procedure DrawString(AX, AY: TIntCoord; constref AString: TString);
    procedure DrawString(constref APosition: TIntPoint; constref AString:
      TString);
    procedure EraseString(AX, AY: TIntCoord; ALength: Word);
    procedure DrawStrings(AX, AY: TIntCoord; constref AStrings: TStrings);
    procedure DrawStrings(constref APosition: TIntPoint; constref AStrings:
      TStrings);
    procedure DrawCanvas(AX, AY: TIntCoord; ACanvas: TTextCanvas);
    procedure DrawCanvas(constref APosition: TIntPoint; ACanvas: TTextCanvas);
    function DoesCharExist(AX, AY: TIntCoord): Boolean;
    function DoesCharExist(constref APosition: TIntPoint): Boolean;

    // Add visual effects

    property Strings: TStrings read FCanvas; // It it good?
    property Owner: TTextCanvas read FOwner write FOwner;
    property Char[AX, AY: TIntCoord]: Char read GetChar write SetChar; default;
    property CharOnPos[APosition: TIntPoint]: Char read GetChar write SetChar;
    //property Position: TIntPoint read GetPosition;{;//FPosition;}
    property Left: TIntCoord read GetLeft write SetLeft;
    property Top: TIntCoord read GetTop write SetTop;
    property Width: TSize read GetWidth;//FWidth;
    property Height: TSize read GetHeight;//FHeight;
    property BackgroundChar: Char read FBackgroundChar write FBackgroundChar
      default DefaultBackgroundChar;
    property FrameSkin: TFrameSkin read FFrameSkin write SetFrameSkin;// default DefaultFrameSkin;
  end;

  //TBoardLabel ??
  TLabel = class(TStringsObject)
  private
    FOwner: TBoard;//TTextCanvas;//TStringsObject;
    FPosition: TIntPoint;
    FCaption: TString;
    //FPreCaption: TString;
    //FVisible: Boolean;

    procedure SetPosition(AValue: TIntPoint);
    procedure SetX(AValue: TIntCoord);
    function GetX: TIntCoord;
    procedure SetY(AValue: TIntCoord);
    function GetY: TIntCoord;
    procedure SetCaption(const AValue: TString);
  public
    constructor Create(AOwner: TBoard = nil; AX: TIntCoord = 1; AY: TIntCoord = 0;
      constref ACaption: TString = '');
    constructor Create(AOwner: TBoard {= nil}; constref APosition: TIntPoint{ = (FX: 1; FY: 0)};
      constref ACaption: TString = '');

    function Setup(AX: TIntCoord{ = 1}; AY: TIntCoord{ = 0};
      constref ACaption: TString{ = ''}): TLabel;
    function Setup(constref APosition: TIntPoint;
      constref ACaption: TString{ = ''}): TLabel;

    procedure EraseLabel; virtual;
    procedure WriteLabel; virtual;

    property Owner: TBoard read FOwner write FOwner default nil;
    //property Position: TIntPoint read FPosition write SetPosition;
    property Left: TIntCoord read GetX write SetX default 1;
    property Top: TIntCoord read GetY write SetY default 0;
    property Caption: TString read FCaption write SetCaption;// default '';
  end;

  // 2C = two captions
  TLabel2C = class(TLabel)
  private
    FPreCaption: TString;
    procedure SetPreCaption(const AValue: TString);
  public
    constructor Create(AOwner: TBoard = nil; AX: TIntCoord = 1;
      AY: TIntCoord = 0; constref ACaption: TString = ''; constref
      APreCaption: TString = '');
    constructor Create(AOwner: TBoard {= nil}; constref APosition: TIntPoint;
      constref ACaption: TString = ''; constref APreCaption: TString = '');

    function Setup(AX: TIntCoord; AY: TIntCoord; constref ACaption:
      TString; constref APreCaption: TString): TLabel;
    function Setup(constref APosition: TIntPoint; constref ACaption:
      TString; constref APreCaption: TString): TLabel;

    procedure EraseLabel; override;
    procedure WriteLabel; override;

    property PreCaption: TString read FPreCaption write SetPreCaption;
  end;

  TLabels = array of TLabel;

  TLabelData = record
    Position: TIntPoint;
    Caption: TString;

    procedure Setup(AX, AY: TIntCoord; constref ACaption: TString);
    procedure Setup(constref APosition: TIntPoint; constref
      ACaption: TString);
  end;

  TLabelsData = array of TLabelData;

  TLabelsDataHelper = type helper for TLabelsData
    function DoesDataExist(Index: Integer): Boolean;
  end;

  TBoardData = record
    CanvasSettings: TTextCanvasSettings;
    LabelsData: TLabelsData;

    procedure Setup(AX, AY: TIntCoord; AWidth, AHeight: TSize;
      ABackgroundChar: Char; AFrameSkin: TFrameSkin);
    procedure Setup(constref APosition: TIntPoint; AWidth, AHeight: TSize;
      ABackgroundChar: Char; AFrameSkin: TFrameSkin);

    function LoadFromFile(var AFile: TextFile): Boolean;

    procedure AddLabelData(AX, AY: TIntCoord; constref ACaption: TString);
    procedure AddLabelData(constref APosition: TIntPoint; constref ACaption: TString);
    function DoesLabelDataExist(Index: Integer): Boolean;
    procedure ClearLabelsData;

    property Left: TIntCoord read CanvasSettings.Position.X write
      CanvasSettings.Position.Y;
    property Top: TIntCoord read CanvasSettings.Position.Y write
      CanvasSettings.Position.Y;
    property Width: TSize read CanvasSettings.Width write CanvasSettings.Width;
    property Height: TSize read CanvasSettings.Height write CanvasSettings.Height;
    property BackgroundChar: Char read CanvasSettings.BackgroundChar write
      CanvasSettings.BackgroundChar;
    property FrameSkin: TFrameSkin read CanvasSettings.FrameSkin write
      CanvasSettings.FrameSkin;
  end;

  TBoardsData = array of TBoardData;

  TBoardsDataHelper = type helper for TBoardsData
    function LoadFromFile(var AFile: TextFile): Boolean;
    procedure Free;
  end;

  TBoard = class(TTextCanvas)
  private
    FLabels: TLabels;

    function GetLabel(Index: Integer): TLabel;
    function GetLabelsNumber: Integer;
  public
    constructor Create(AOwner: TTextCanvas = nil;
      ALeft: TIntCoord = DefaultLeft; ATop: TIntCoord = DefaultTop;
      AWidth: TSize = DefaultWidth; AHeight: TSize = DefaultHeight;
      ABackgroundChar: Char = DefaultBackgroundChar;
      AFrameSkin: TFrameSkin = DefaultFrameSkin);
    constructor Create(AOwner: TTextCanvas; constref ABoardData: TBoardData);
    destructor Destroy; override;

    {procedure Setup(ALeft, ATop: TIntCoord; AWidth, AHeight: TSize;
      ABackgroundChar: Char = DefaultBackgroundChar);}
    procedure Setup(constref AData: TBoardData);

    // Is it good?
    procedure Clear{(SaveLabels: Boolean = True); //}; override;

    procedure DestroyLabels;
    procedure WriteLabels;//RefreshLabels;
    procedure Refresh{(DoRefreshLabels: Boolean = False)}; override;

    function DoesLabelExists(ALabelIndex: Integer): Boolean;
    function SetLabelCaption(ALabelIndex: Integer; constref
      ACaption: TString): Boolean;
    function LoadCaptionsFromFile(var AFile: TextFile): Boolean;

    function AddLabel(ALabel: TLabel): TLabel;
    function AddLabel(AX: TIntCoord = 1; AY: TIntCoord = 0;
      constref ACaption: TString = ''): TLabel;
    function AddLabel(constref APosition: TIntPoint;
      constref ACaption: TString = ''): TLabel;
    //procedure DeleteLabel(AIndex: Integer);

    procedure LoadLabelsData(constref ALabelsData: TLabelsData);
    //procedure LoadFromData(constref ABoardData: TBoardData);
    function LoadFromFile(constref AFileName: TString): Boolean;
    function LoadFromFile(constref AFileName: TString; constref
      AFilePaths: TStrings): Boolean;
    function LoadFromFile(var AFile: TextFile): Boolean;

    property Labels[Index: Integer]: TLabel read GetLabel; default;
    property LabelsNumber: Integer read GetLabelsNumber;
  end;

  TBoards = array of TBoard;

  TBoardsHelper = type helper for TBoards
    function DoesBoardExist(Index: Integer): Boolean;
  end;


implementation

uses
  VSCRT, VSUtils;


// TTextCanvasSettings = record

function TTextCanvasSettings.LoadFromFile(var AFile: TextFile): Boolean;
var
  c: Char;
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile);
    ReadLn(AFile, Position.X, Position.Y, Width, Height);
    ReadLn(AFile);
    ReadLn(AFile, BackgroundChar, c, FrameSkin);
    Result:=True;
  except
    Result:=False;
  end;
end;


// TTextCanvas = class;

constructor TTextCanvas.Create(AOwner: TTextCanvas = nil;
  ALeft: TIntCoord = DefaultLeft; ATop: TIntCoord = DefaultTop;
  AWidth: TSize = DefaultWidth; AHeight: TSize = DefaultHeight;
  ABackgroundChar: Char = DefaultBackgroundChar;
  AFrameSkin: TFrameSkin = DefaultFrameSkin);
begin
  inherited Create;
  //FBackground: TStrings;
  FOwner:=AOwner;
  Setup(ALeft, ATop, AWidth, AHeight, ABackgroundChar, AFrameSkin);
end;

constructor TTextCanvas.Create(AOwner: TTextCanvas; constref AData:
  TTextCanvasSettings);
begin
  with AData do
    Create(AOwner, Position.X, Position.Y, Width, Height, BackgroundChar,
      FrameSkin);
  {inherited Create;
  FOwner:=AOwner;
  Setup(AData);}
end;

destructor TTextCanvas.Destroy;
begin
  SetLength(FCanvas, 0);
  //SetLength(FBackground, 0);
  SetLength(FChangedLines, 0);
  inherited Destroy;
end;

procedure TTextCanvas.Setup(ALeft, ATop: TIntCoord; AWidth, AHeight: TSize;
  ABackgroundChar: Char; AFrameSkin: TFrameSkin);
var
  i: Integer;
begin
  //if (AWidth<=0) or (AHeight<=0) then Exit;
  //FWidth:=AWidth;
  //FHeight:=AHeight;
  //FOwner:=AOwner;
  FPosition.Setup(ALeft, ATop);
  FBackgroundChar:=ABackgroundChar;
  FFrameSkin:=AFrameSkin;
  FFrameWasChanged:=True;
  SetLength(FCanvas, AHeight);
  SetLength(FChangedLines, AHeight);
  //Clear;
  for i:=0 to High(FCanvas) do
  begin
    FCanvas[i]:=FillString(FBackgroundChar, AWidth);
    FChangedLines[i]:=True;
  end;
end;

procedure TTextCanvas.Setup(constref AData: TTextCanvasSettings);
begin
  with AData do
    Setup(Position.X, Position.Y, Width, Height, BackgroundChar, FrameSkin);
end;

procedure TTextCanvas.Fill(AChar: Char);
var
  i: Integer;
begin
  for i:=Low(FCanvas) to High(FCanvas) do
  begin
    FCanvas[i]:=FillString(AChar, Length(FCanvas[i]));
    FChangedLines[i]:=True;
  end;
end;

procedure TTextCanvas.Clear;
begin
  Fill(FBackgroundChar);
end;

procedure TTextCanvas.ClearLine(ALine: TIntCoord);
begin
  if not IsIntInRange(ALine, 0, High(FCanvas)) then
    Exit;
  DrawString(1, ALine, FillString(FBackgroundChar, Length(FCanvas[ALine])));
end;

procedure TTextCanvas.ClearLines(constref ALines: array of TIntCoord);
var
  i: Integer;
begin
  (*if {(ALines=nil) or }(Length(ALines)=0) then
    Exit;*)
  for i:=0 to High(ALines) do
    ClearLine(ALines[i]);
end;

function TTextCanvas.GetChar(AX, AY: TIntCoord): Char;
begin
  if DoesCharExist(AX, AY) then
    Result:=FCanvas[AY, AX]
  else
    Result:=#0;
end;

procedure TTextCanvas.SetChar(AX, AY: TIntCoord; AValue: Char);
begin
  if DoesCharExist(AX, AY) then
  begin
    FCanvas[AY, AX]:=AValue;
    FChangedLines[AY]:=True;
  end;
end;

function TTextCanvas.GetChar(const APosition: TIntPoint): Char;
begin
  Result:=GetChar(APosition.X, APosition.Y);
end;

procedure TTextCanvas.SetChar(const APosition: TIntPoint; AValue: Char);
begin
  SetChar(APosition.X, APosition.Y, AValue);
end;

function TTextCanvas.GetPosition: TIntPoint;
begin
  Result:=FPosition;
end;

function TTextCanvas.GetLeft: TIntCoord;
begin
  Result:=FPosition.X;
end;

procedure TTextCanvas.SetLeft(AValue: TIntCoord);
begin
  FPosition.X:=AValue;
  ResetChanged(True);
end;

function TTextCanvas.GetTop: TIntCoord;
begin
  Result:=FPosition.Y;
end;

procedure TTextCanvas.SetTop(AValue: TIntCoord);
begin
  FPosition.Y:=AValue;
  ResetChanged(True);
end;

function TTextCanvas.GetWidth: TSize;
begin
  Result:=StringsMaxLength(FCanvas);
  {if Length(FCanvas)>0 then
    Result:=Length(FCanvas[0])
  else
    Result:=0;}
end;

function TTextCanvas.GetHeight: TSize;
begin
  Result:=Length(FCanvas);
end;

procedure TTextCanvas.SetFrameSkin(AValue: TFrameSkin);
begin
  FFrameWasChanged:=True;
  FFrameSkin:=AValue;
end;

procedure TTextCanvas.ResetChanged(WasChanged: Boolean = True);
var
  i: Integer;
begin
  for i:=0 to High(FChangedLines) do
    FChangedLines[i]:=WasChanged;
  FFrameWasChanged:=WasChanged;
end;

function TTextCanvas.CanvasWithFrame: TStrings;
begin
  if Length(FFrameSkin) > 0 then
  begin
    Result:=FillStrings(FFrameSkin, (Width div Length(FFrameSkin))+2,
      Height+2);
    PutStringsOnStrings(Result, 1+Length(FFrameSkin), 1, FCanvas);
  end
  else
    Result:=FCanvas;
end;

function TTextCanvas.CleanPosition: TIntPoint;
begin
  Result:=CleanPosition(FPosition.X, FPosition.Y);
end;

function TTextCanvas.CleanPosition(AX, AY: TIntCoord): TIntPoint;
begin
  Result.Setup(AX-Length(FFrameSkin), AY-Byte(Length(FFrameSkin) > 0));
end;

procedure TTextCanvas.DrawOnScreen(DoClrScr: Boolean = False);
begin
  if FOwner = nil then
  begin
    if DoClrScr then
      ClrScr;
    WriteStringsXY(CleanPosition, CanvasWithFrame);
  end
  else
  begin
    if DoClrScr then
      FOwner.Clear;
    DrawOnCanvas(FOwner, 0, 0);
  end;

  ResetChanged(False);//??
end;

procedure TTextCanvas.DrawOnStrings(var AStrings: TStrings; ADeltaX:
  TIntCoord = 0; ADeltaY: TIntCoord = 0);
begin
  {PutStringsOnStrings(AStrings, CleanPosition(Left+ADeltaX, Top+ADeltaY),
    CanvasWithFrame);}
  with CleanPosition(Left+ADeltaX, Top+ADeltaY) do
    PutStringsOnStrings(AStrings, X, Y, CanvasWithFrame);
end;

procedure TTextCanvas.DrawOnCanvas(ACanvas: TTextCanvas; ADeltaX:
  TIntCoord = 0; ADeltaY: TIntCoord = 0);
begin
  if ACanvas<>nil then
    DrawOnStrings(ACanvas.FCanvas, ADeltaX, ADeltaY);
end;

procedure TTextCanvas.Refresh{(DoClrScr: Boolean = False)};
var
  i: Integer;
begin
  {if DoClrScr then
    ClrScr;}

  if FFrameWasChanged then
  begin
    FFrameWasChanged:=False;
    if Length(FFrameSkin) > 0 then
    begin
      // If we draw frame that we draw all canvas and we don't need to
      // refresh other strings
      DrawOnScreen(False);
      //ResetChanged(False);// It has already done in DrawOnStrings.
      Exit;
    end;
  end;

  for i:=0 to High(FCanvas) do
    if FChangedLines[i] then
    begin
      if FOwner = nil then
        WriteStringXYInRect(FPosition.X, FPosition.Y+i, FCanvas[i])
      else
        FOwner.DrawString(FPosition.X, FPosition.Y+i, FCanvas[i]);
      FChangedLines[i]:=False;
    end;
end;

procedure TTextCanvas.DrawString(AX, AY: TIntCoord; constref AString:
  TString);
begin
  {PutStringOnStrings(FCanvas, AX, AY, AString);
  FChangedLines[AY]:=True;}
  if not IsIntInRange(AY, Low(FCanvas), High(FCanvas)) then
    Exit;
  //InsertString(FCanvas[AY], AString, AX);
  ReplaceString(FCanvas[AY], AString, AX);
  FChangedLines[AY]:=True;//?}
end;

procedure TTextCanvas.DrawString(constref APosition: TIntPoint; constref
  AString: TString);
begin
  DrawString(APosition.X, APosition.Y, AString);
end;

procedure TTextCanvas.EraseString(AX, AY: TIntCoord; ALength: Word);
{var
  i, n: Integer;}
begin
  {if not DoesCharExist(AX, AY) then
    Exit;
  n:=AX+ALength-1;
  if n > High(FCanvas[AY]) then
    n:=High(FCanvas[AY]);
  for i:=AX to n do
    FCanvas[AY, i]:=FBackgroundChar;
  if n>=AX then
    FChangedLines[AY]:=True;}

  DrawString(AX, AY, FillString{2}(FBackgroundChar, ALength));
end;

procedure TTextCanvas.DrawStrings(AX, AY: TIntCoord; constref
  AStrings: TStrings);
var
  i, Y0, Y1: Integer;
begin
  if (AY > High(FCanvas)) or ((AY+Length(AStrings)-1) < Low(FCanvas)) then
    Exit;

  {Y0:=AY;
  if Y0 < Low(FCanvas) then
    Y0:=Low(FCanvas);
  Y1:=AY+Length(AStrings)-1;
  if Y1 > High(FCanvas) then
    Y1:=High(FCanvas);

  for i:=Y0 to Y1 do
  begin
    //DrawString(AX, AY+i, AStrings[i]); // Excess cheks!
    ReplaceString(FCanvas[i], AStrings[Abs(AY)+i], AX);
    FChangedLines[i]:=True;
  end;}

  for i:=Low(AStrings) to High(AStrings) do
    DrawString(AX, AY+i, AStrings[i]);

  //PutStringsOnStrings(FCanvas, AX, AY, AStrings);
  {for i:=Low(AStrings) to High(AStrings) do
    if (AY+i)<=High(FCanvas) then
      DrawString(AX, AY+i, AStrings[i])
    else
      Exit;//Break;}
end;

procedure TTextCanvas.DrawStrings(constref APosition: TIntPoint; constref
  AStrings: TStrings);
begin
  DrawStrings(APosition.X, APosition.Y, AStrings);
end;

procedure TTextCanvas.DrawCanvas(AX, AY: TIntCoord; ACanvas: TTextCanvas);
begin
  if ACanvas<>nil then
    DrawStrings(AX, AY, ACanvas.FCanvas);
end;

procedure TTextCanvas.DrawCanvas(constref APosition: TIntPoint; ACanvas:
  TTextCanvas);
begin
  DrawCanvas(APosition.X, APosition.Y, ACanvas);
end;

function TTextCanvas.DoesCharExist(AX, AY: TIntCoord): Boolean;
begin
  Result:=IsIntInRange(AY, Low(FCanvas), High(FCanvas)) and
          IsIntInRange(AX, Low(FCAnvas[AY]), High(FCanvas[AY]));
end;

function TTextCanvas.DoesCharExist(constref APosition: TIntPoint): Boolean;
begin
  Result:=DoesCharExist(APosition.X, APosition.Y);
end;


// TLabel = class(TStringsObject)

constructor TLabel.Create(AOwner: TBoard = nil; AX: TIntCoord = 1;
  AY: TIntCoord = 0; constref ACaption: TString = '');
begin
  inherited Create;
  FOwner:=AOwner;
  //Setup(AX, AY, ACaption); // Setup does EraseLabel!! i.e. it erases nothing!
  FPosition.Setup(AX, AY);
  FCaption:=ACaption;
  WriteLabel;
end;

constructor TLabel.Create(AOwner: TBoard; constref APosition: TIntPoint;
  constref ACaption: TString = '');
begin
  Create(AOwner, APosition.X, APosition.Y, ACaption);
end;

function TLabel.Setup(AX: TIntCoord{ = 1}; AY: TIntCoord{ = 0};
  constref ACaption: TString{ = ''}): TLabel;
begin
  EraseLabel;
  FPosition.Setup(AX, AY);
  FCaption:=ACaption;
  WriteLabel;
  Result:=Self;
end;

function TLabel.Setup(constref APosition: TIntPoint;
  constref ACaption: TString{ = ''}): TLabel;
begin
  Result:=Setup(APosition.X, APosition.Y, ACaption);
end;

procedure TLabel.SetPosition(AValue: TIntPoint);
begin
  EraseLabel;
  FPosition:=AValue;
  WriteLabel;
end;

procedure TLabel.SetX(AValue: TIntCoord);
begin
  EraseLabel;
  FPosition.X:=AValue;
  WriteLabel;
end;

function TLabel.GetX: TIntCoord;
begin
  Result:=FPosition.X;
end;

procedure TLabel.SetY(AValue: TIntCoord);
begin
  EraseLabel;
  FPosition.Y:=AValue;
  WriteLabel;
end;

function TLabel.GetY: TIntCoord;
begin
  Result:=FPosition.Y;
end;

procedure TLabel.SetCaption(const AValue: TString);
begin
  EraseLabel;
  FCaption:=AValue;
  WriteLabel;
end;

procedure TLabel.EraseLabel;
begin
  if FOwner<>nil then
    FOwner.DrawString(FPosition, FillString(FOwner.BackgroundChar,
      Length({FPreCaption+}FCaption)))
  else
    WriteStringXYInRect(FPosition.X, FPosition.Y,
      FillString(' ', Length({FPreCaption+}FCaption)));
end;

procedure TLabel.WriteLabel;
begin
  if FOwner<>nil then
    FOwner.DrawString(FPosition, {FPreCaption+}FCaption)
  else
    WriteStringXYInRect(FPosition{.X, FPosition.Y}, FCaption);
end;


// TLabel2C = class(TLabel)

constructor TLabel2C.Create(AOwner: TBoard = nil; AX: TIntCoord = 1;
  AY: TIntCoord = 0; constref ACaption: TString = ''; constref
  APreCaption: TString = '');
begin
  FPreCaption:=APreCaption;
  inherited Create(AOwner, AX, AY, ACaption);
end;

constructor TLabel2C.Create(AOwner: TBoard {= nil}; constref APosition:
  TIntPoint; constref ACaption: TString = ''; constref APreCaption:
  TString = '');
begin
  Create(AOwner, APosition.X, APosition.Y, ACaption, APreCaption);
end;

procedure TLabel2C.SetPreCaption(const AValue: TString);
begin
  EraseLabel;
  FPreCaption:=AValue;
  WriteLabel;
end;

function TLabel2C.Setup(AX: TIntCoord; AY: TIntCoord; constref ACaption:
  TString; constref APreCaption: TString): TLabel;
begin
  EraseLabel;
  FPosition.Setup(AX, AY);
  FCaption:=ACaption;
  FPreCaption:=APreCaption;
  WriteLabel;
  Result:=Self;
end;

function TLabel2C.Setup(constref APosition: TIntPoint; constref ACaption:
  TString; constref APreCaption: TString): TLabel;
begin
  Result:=Setup(APosition.X, APosition.Y, ACaption, APreCaption);
end;

procedure TLabel2C.EraseLabel;
var
  TempStr: TString;
begin
  {TempStr:=FCaption;
  FCaption:=FPreCaption+FCaption;
  inherited EraseLabel;
  FCaption:=TempStr;}

  if FOwner<>nil then
    FOwner.DrawString(FPosition, FillString(FOwner.BackgroundChar,
      Length(FPreCaption+FCaption)))
  else
    WriteStringXYInRect(FPosition.X, FPosition.Y,
      FillString(' ', Length(FPreCaption+FCaption)));
end;

procedure TLabel2C.WriteLabel;
var
  TempStr: TString;
begin
  {TempStr:=FCaption;
  FCaption:=FPreCaption+FCaption;
  inherited EraseLabel;
  FCaption:=TempStr;}

  if FOwner<>nil then
    FOwner.DrawString(FPosition, FPreCaption+FCaption)
  else
    WriteStringXYInRect(FPosition.X, FPosition.Y, FCaption);
end;


// TBoardsDataHelper = type helper for TBoardsData

procedure TBoardsDataHelper.Free;
var
  i: Integer;
begin
  for i:=Low(Self) to High(Self) do
    Self[i].ClearLabelsData;
  SetLength(Self, 0);
end;

function TBoardsDataHelper.LoadFromFile(var AFile: TextFile): Boolean;
var
  i, n: Integer;
  BD: TBoardsData;
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile, n);
    SetLength(BD, n);
    for i:=0 to n-1 do
      if not BD[i].LoadFromFile(AFile) then
      begin
        BD.Free;
        Exit(False);
      end;

    Self.Free;
    Self:=BD;
    //BD.Free;
    Result:=True;
  except
    BD.Free;
    Result:=False;
  end;
end;


//  TBoard = class(TTextCanvas)

constructor TBoard.Create(AOwner: TTextCanvas = nil;
  ALeft: TIntCoord = DefaultLeft; ATop: TIntCoord = DefaultTop;
  AWidth: TSize = DefaultWidth; AHeight: TSize = DefaultHeight;
  ABackgroundChar: Char = DefaultBackgroundChar;
  AFrameSkin: TFrameSkin = DefaultFrameSkin);
begin
  inherited Create(AOwner, ALeft, ATop, AWidth, AHeight, ABackgroundChar,
    AFrameSkin);
  SetLength(FLabels, 0);
end;

constructor TBoard.Create(AOwner: TTextCanvas; constref ABoardData:
  TBoardData);
begin
  with ABoardData do
    inherited Create(AOwner, Left, Top, Width, Height, BackgroundChar,
      FrameSkin);
  LoadLabelsData(ABoardData.LabelsData);
end;

destructor TBoard.Destroy;
begin
  DestroyLabels;
  inherited Destroy;
end;

{procedure TBoard.Setup(constref ABoardData: TBoardData);
begin
  with ABoardData do
    Setup(Position.X, Position.Y, Width, Height, BackgroundChar);
  //LoadFromData(ABoardData);
end;}

procedure TBoard.DestroyLabels;
var
  i: Integer;
begin
  for i:=0 to High(FLabels) do
    FLabels[i].Free;
  SetLength(FLabels, 0);
end;

procedure TBoard.WriteLabels;//RefreshLabels;
var
  i: Integer;
begin
  for i:=0 to High(FLabels) do
    FLabels[i].WriteLabel;
end;

function TBoard.DoesLabelExists(ALabelIndex: Integer): Boolean;
begin
  Result:=IsIntInRange(ALabelIndex, Low(FLabels), High(FLabels));
end;

function TBoard.SetLabelCaption(ALabelIndex: Integer; constref
  ACaption: TString): Boolean;
begin
  Result:=DoesLabelExists(ALabelIndex);
  if Result then
    Labels[ALabelIndex].Caption:=ACaption;
end;

function TBoard.LoadCaptionsFromFile(var AFile: TextFile): Boolean;
var
  i, n: Integer;
  TempStr: AnsiString;
begin
  {$I+}
  try
    ReadLn(AFile);
    ReadLn(AFile, n);
    for i:=0 to n-1 do
    begin
      ReadLn(AFile, TempStr);
      SetLabelCaption(i, TempStr);
    end;

    Result:=True;
  except
    Result:=False;
  end;
end;

procedure TBoard.Refresh{(DoRefreshLabels: Boolean = False)};
begin
  {if DoRefreshLabels then
    WriteLabels;//RefreshLabels;}
  inherited Refresh;
end;

function TBoard.GetLabelsNumber: Integer;
begin
  Result:=Length(FLabels);
end;

function TBoard.GetLabel(Index: Integer): TLabel;
begin
  if IsIntInRange(Index, Low(FLabels), High(FLabels)) then
    Result:=FLabels[Index]
  else
    Result:=nil;
end;

function TBoard.AddLabel(ALabel: TLabel): TLabel;
begin
  SetLength(FLabels, Length(FLabels)+1);
  Result:=ALabel;//TLabel.Create(Self, AX, AY, ACaption);
  FLabels[High(FLabels)]:=Result;
end;

function TBoard.AddLabel(AX: TIntCoord = 1; AY: TIntCoord = 0;
  constref ACaption: TString = ''): TLabel;
begin
  SetLength(FLabels, Length(FLabels)+1);
  Result:=TLabel.Create(Self, AX, AY, ACaption);
  FLabels[High(FLabels)]:=Result;
end;

function TBoard.AddLabel(constref APosition: TIntPoint;
  constref ACaption: TString = ''): TLabel;
begin
  Result:=AddLabel(APosition.X, APosition.Y, ACaption);
end;

//procedure DeleteLabel(AIndex: Integer);

procedure TBoard.LoadLabelsData(constref ALabelsData: TLabelsData);
var
  i: Integer;
begin
  DestroyLabels;
  SetLength(FLabels, Length(ALabelsData));
  for i:=0 to High(ALabelsData) do
    with ALabelsData[i] do
      FLabels[i]:=TLabel.Create(Self, Position, Caption);
end;

//procedure TBoard.LoadFromData(constref ABoardData: TBoardData);
procedure TBoard.Setup(constref AData: TBoardData);
begin
  with AData do
    inherited Setup(Left, Top, Width, Height, BackgroundChar, FrameSkin);
  LoadLabelsData(AData.LabelsData);
end;

procedure TBoard.Clear{(SaveLabels: Boolean = True)};
begin
  //Fill(FBackgroundChar);
  inherited Clear;
  //if SaveLabels then
    WriteLabels;
end;

function TBoard.LoadFromFile(constref AFileName: TString): Boolean;
begin
  Result:=LoadFromFile(AFileName, ['']);
end;

function TBoard.LoadFromFile(constref AFileName: TString; constref
  AFilePaths: TStrings): Boolean;
var
  AFile: TextFile;
begin
  if not OpenFile(AFile, AFileName, AFilePaths, ofmReset) then
    Exit(False);
  Result:=LoadFromFile(AFile);
  Close(AFile);
end;

function TBoard.LoadFromFile(var AFile: TextFile): Boolean;
var
  BD: TBoardData;
begin
  if not BD.LoadFromFile(AFile) then
    Exit(False);
  Setup(BD);
  SetLength(BD.LabelsData, 0);
  Result:=True;
end;


// TLabelData = record

procedure TLabelData.Setup(AX, AY: TIntCoord; constref ACaption: TString);
begin
  Position.Setup(AX, AY);
  Caption:=ACaption;
end;

procedure TLabelData.Setup(constref APosition: TIntPoint; constref ACaption: TString);
begin
  Setup(APosition.X, APosition.Y, ACaption);
end;


// TLabelsDataHelper = type helper for TLabelsData

function TLabelsDataHelper.DoesDataExist(Index: Integer): Boolean;
begin
  Result:=IsIntInRange(Index, Low(Self), High(Self));
end;


// TBoardData = record

procedure TBoardData.Setup(AX, AY: TIntCoord; AWidth, AHeight: TSize;
  ABackgroundChar: Char; AFrameSkin: TFrameSkin{; ALabelsData: TLabelsData});
begin
  with CanvasSettings do
  begin
    Position.Setup(AX, AY);
    Width:=AWidth;
    Height:=AHeight;
    BackgroundChar:=ABackgroundChar;
    FrameSkin:=AFrameSkin;
  end;
  //LabelsData: TLabelsData;
end;

procedure TBoardData.Setup(constref APosition: TIntPoint; AWidth,
  AHeight: TSize; ABackgroundChar: Char; AFrameSkin: TFrameSkin);
begin
  Setup(APosition.X, APosition.Y, AWidth, AHeight, ABackgroundChar,
    AFrameSkin);
end;

function TBoardData.LoadFromFile(var AFile: TextFile): Boolean;
var
  BD: TBoardData;
  i, n: Integer;
  c: Char;
begin
  {$I+}
  try
    if not BD.CanvasSettings.LoadFromFile(AFile) then
      Exit(False);
    ReadLn(AFile);
    ReadLn(AFile, n);
    SetLength(BD.LabelsData, n);
    for i:=0 to n-1 do
      with BD.LabelsData[i] do
        ReadLn(AFile, Position.X, Position.Y, c, Caption);

    Self:=BD; //??
    Result:=True;
  except
    Result:=False;
  end;

  {if Result then
    Self:=BD;}
  SetLength(BD.LabelsData, 0); // ??
  //BD.ClearLabelsData;
end;

procedure TBoardData.AddLabelData(AX, AY: TIntCoord; constref
  ACaption: TString);
begin
  SetLength(LabelsData, Length(LabelsData)+1);
  LabelsData[High(LabelsData)].Setup(AX, AY, ACaption);
end;

procedure TBoardData.AddLabelData(constref APosition: TIntPoint;
  constref ACaption: TString);
begin
  AddLabelData(APosition.X, APosition.Y, ACaption);
end;

function TBoardData.DoesLabelDataExist(Index: Integer): Boolean;
begin
  Result:=LabelsData.DoesDataExist(Index);
  //Result:=IsIntInRange(Index, Low(LabelsData), High(LabelsData));
end;

procedure TBoardData.ClearLabelsData;
begin
  SetLength(LabelsData, 0);
end;


// TBoardsHelper = type helper for TBoards

function TBoardsHelper.DoesBoardExist(Index: Integer): Boolean;
begin
  Result:=IsIntInRange(Index, Low(Self), High(Self));
end;


end.
