(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

    {odiCKMoveRight, odiCKMoveLeft, odiCKMoveDown, odiCKDropDown,
    odiCKRotateRight, odiCKRotateLeft, odiCKeyPause, odiCKMenu,
    odiCKMoveUp, odiCKChangeFigure, odiCKChangeScreenSettingsSet}

  DefaultControlKeys: TControlKeys = (
    KeyRight,
    KeyLeft,
    KeyDown,
    KeyEnter,
    KeySpace,
    KeyBackspace,

    KeyP,
    KeyEscape,

    0,//KeyUp,
    KeyTab,
    KeyDelete
  );

  DefaultGameCommandNames: TTetrisFPCGameCommandNames = (
    'Move right',
    'Move left',
    'Move down',
    'Drop down',
    'Rotate right',
    'Rotate left',

    'Pause',
    'Menu',

    'Move up',
    'Change figure',
    'Change screen skin'
  );

