(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

  LabelNextFigure = 'Next:';

  LabelScoreboard = 'Scoreboard:';
  LabelLines = 'Lines:';
  LabelScore = 'Score:';
  LabelSpeed = 'Speed:';
  LabelBestScore = 'Best score:';

  LabelControlKeys = 'Control keys:';
  LabelMoveRight = 'Move right:';
  LabelMoveLeft = 'Move left:';
  LabelMoveDown = 'Move down:';
  LabelDropDown = 'Drop down:';
  LabelRotateRight = 'Rotate >:';
  LabelRotateLeft = 'Rotate <:';
  //'Change skin',;
  LabelPause = 'Pause:';
  LabelMenu = 'Menu:';

  MsgGameOver = 'Game over';
  MsgWantToQuit = 'Are you sure you want to quit?';
  MsgBestScoreCongrats = 'Congratulations! You''ve got the best score!';
  MsgEntrerName = 'Enter your name: ';
  MsgPause = 'Pause ||';
  //LabelGameOverPos: TIntPoint = ();
  //LabelPausePos: TIntPoint = ();

  CaptionMainMenuTitle = {'Main menu';//}'';
  CaptionMainMenuNewGame = 'New game';
  CaptionMainMenuSettings = 'Settings';
  CaptionMainMenuQuit = 'Quit';

  DefaultMainMenuCaptions: TStrings{TMainMenuCaptions} = (
    CaptionMainMenuTitle,
    CaptionMainMenuNewGame,
    CaptionMainMenuSettings,
    CaptionMainMenuQuit
  );

  DefaultMessages: TTetrisFPCMessages = (
    MsgGameOver,
    MsgWantToQuit,
    MsgBestScoreCongrats,
    MsgEntrerName,
    MsgPause
  );

