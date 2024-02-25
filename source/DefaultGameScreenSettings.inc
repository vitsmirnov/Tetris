(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

  //{$J-}

  {TTetrisFPCGameScreenSettings = record
    GameFieldData: TTextCanvasSettings;
    CanvasDeltaPos: TIntPoint; //Rename! GameFieldPosOnCanvas??
    BoardsData: TBoardsData;
    OutputLabelsData: TOutputLabelsData;
    NextFigureCanvasIndex: Byte;
    NextFigurePosOnCanvas: TIntPoint;
    FallenElementSkin: TElementSkinFPC;
    DestroyLinesAnimation: TDestroyLinesAnimation;
    DropDownTrace: TDropDownTrace;
    //Separete it?
    BackgroundColor: Byte;
    TextColor: Byte;
  end;}

  DefaultFrameSkin = '#';
  {Default}LabelBackgroundChar = ' '; //?

  LabelTab = 3;
  LabelTab2 = 10;
  LabelTab3 = 15;

  DefaultScreenSettingsSet: TTetrisFPCGameScreenSettingsSet = (
    (// Settings 1
    // Ctrl keys - game field - scoreboard
    // 80 = 6 22 2 20 2 22 6 // without frame
    // 80 = 4 26 0 20 0 26 4 // with frame
    //      7      31   53
      GameFieldData: (
        Position: (X: 31; Y: 3);
        Width: 20;
        Height: 20;
        BackgroundChar: LabelBackgroundChar;
        FrameSkin: '[]';//DefaultFrameSkin;
      );
      CanvasDeltaPos: (X: 1; Y: 19);
      BoardsData: (
        ( // Next figure board
          CanvasSettings: (
            Position: (X: 53; Y: 3);
            Width: 22;
            Height: 7;
            BackgroundChar: LabelBackgroundChar;
            FrameSkin: '[]';//DefaultFrameSkin;
          );
          LabelsData: (
            (
              Position: (X: 1; Y: 0);
              Caption: LabelNextFigure
            )
          )
        ),
        ( // Scoreboard
          CanvasSettings: (
            Position: (X: 53; Y: 11);
            Width: 22;
            Height: 12;
            BackgroundChar: LabelBackgroundChar;
            FrameSkin: '[]';//DefaultFrameSkin;
          );
          LabelsData: (
            (
              Position: (X: 1; Y: 0);
              Caption: LabelScoreboard
            ),
            (
              Position: (X: LabelTab; Y: 2);
              Caption: LabelLines
            ),
            (
              Position: (X: LabelTab2; Y: 2);
              Caption: ''
            ),
            (
              Position: (X: LabelTab; Y: 4);
              Caption: LabelScore
            ),
            (
              Position: (X: LabelTab2; Y: 4);
              Caption: ''
            ),
            (
              Position: (X: LabelTab; Y: 6);
              Caption: LabelSpeed
            ),
            (
              Position: (X: LabelTab2; Y: 6);
              Caption: ''
            ),
            (
              Position: (X: 1; Y: 9);
              Caption: LabelBestScore
            ),
            (
              Position: (X: LabelTab; Y: 11);
              Caption: '18500 - Vitaly.....'
            )
          )
        ),
        ( // Control keys board
          CanvasSettings: (
            Position: (X: 7; Y: 3);
            Width: 22;
            Height: 20;
            BackgroundChar: LabelBackgroundChar;
            FrameSkin: '[]';//DefaultFrameSkin;
          );
          LabelsData: (
            (
              Position: (X: 1; Y: 0);
              Caption: LabelControlKeys
            ),
            (
              Position: (X: LabelTab; Y: 3);
              Caption: 'Move right:'
            ),
            (
              Position: (X: LabelTab3; Y: 3);
              Caption: '->'
            ),
            (
              Position: (X: LabelTab; Y: 5);
              Caption: 'Move left:'
            ),
            (
              Position: (X: LabelTab3; Y: 5);
              Caption: '<-'
            ),
            (
              Position: (X: LabelTab; Y: 7);
              Caption: 'Move down:'
            ),
            (
              Position: (X: LabelTab3; Y: 7);
              Caption: 'V'
            ),
            (
              Position: (X: LabelTab; Y: 9);
              Caption: 'Drop down:'
            ),
            (
              Position: (X: LabelTab3; Y: 9);
              Caption: 'Enter'
            ),
            (
              Position: (X: LabelTab; Y: 11);
              Caption: 'Rotate >:'
            ),
            (
              Position: (X: LabelTab3; Y: 11);
              Caption: 'Space'
            ),
            (
              Position: (X: LabelTab; Y: 13);
              Caption: 'Rotate <:'
            ),
            (
              Position: (X: LabelTab3; Y: 13);
              Caption: 'B.space'
            ),
            (
              Position: (X: LabelTab; Y: 17);
              Caption: 'Pause:'
            ),
            (
              Position: (X: LabelTab3; Y: 17);
              Caption: 'P'
            ),
            (
              Position: (X: LabelTab; Y: 19);
              Caption: 'Menu:'
            ),
            (
              Position: (X: LabelTab3; Y: 19);
              Caption: 'Escape'
            )
          )
        )
      );
      OutputLabelsData: (
        // Scoreboard labels
        (
          ID: odiSBLines;
          BoardIndex: 1;
          LabelIndex: 2
        ),
        (
          ID: odiSBScore;
          BoardIndex: 1;
          LabelIndex: 4
        ),
        (
          ID: odiSBSpeed;
          BoardIndex: 1;
          LabelIndex: 6
        ),
        (
          ID: odiSBBestScore;
          BoardIndex: 1;
          LabelIndex: 8
        ),
        // Control keys labels
        (
          ID: odiCKMoveRight;
          BoardIndex: 2;
          LabelIndex: 2;
        ),
        (
          ID: odiCKMoveLeft;
          BoardIndex: 2;
          LabelIndex: 4;
        ),
        (
          ID: odiCKMoveDown;
          BoardIndex: 2;
          LabelIndex: 6;
        ),
        (
          ID: odiCKDropDown;
          BoardIndex: 2;
          LabelIndex: 8;
        ),
        (
          ID: odiCKRotateRight;
          BoardIndex: 2;
          LabelIndex: 10;
        ),
        (
          ID: odiCKRotateLeft;
          BoardIndex: 2;
          LabelIndex: 12;
        ),
        (
          ID: odiCKPause;
          BoardIndex: 2;
          LabelIndex: 14;
        ),
        (
          ID: odiCKMenu;
          BoardIndex: 2;
          LabelIndex: 16;
        )
      );
      NextFigureCanvasIndex: 0;
      NextFigurePosOnCanvas: (X: 3+6; Y: 2);
      FallenElementSkin: '%%';
      DestroyLinesAnimation: (
        Frames: (
          //'{}', '()', '<>', '-', ''
          //'. ', 'o ', 'O ', '()', '  '
          //'()', 'O ', 'o ', '. ', '  '
          //'-', '<>', '()', '{}', '[]'
          '-', ' '
          //'-', ''
          //'<-', '-<'
          //'<>', '><'
          //'< ', ' <'
        );
        ReplaysNumber: 3;
        Delay: 150;
      );
      DropDownTrace: (
        Show: True;
        Skin: '||';
        Delay: 50;
      );
      BackgroundColor: Black;
      TextColor: White;
    ),
    (// Settings 2
    // Game field - scoreboard
    // 80 = 17 20 6 20 17 // without frame
    //         18   44
      GameFieldData: (
        Position: (X: 18; Y: 3);
        Width: 20;
        Height: 20;
        BackgroundChar: ' ';
        FrameSkin: '#';//DefaultFrameSkin;
      );
      CanvasDeltaPos: (X: 1; Y: 19);
      BoardsData: (
        ( // Next figure board
          CanvasSettings: (
            Position: (X: 44; Y: 3);
            Width: 20;
            Height: 7;
            BackgroundChar: ' ';//LabelBackgroundChar;
            FrameSkin: '#';//DefaultFrameSkin;
          );
          LabelsData: (
            (
              Position: (X: 1; Y: 0);
              Caption: LabelNextFigure
            )
          )
        ),
        ( //Scoreboard
          CanvasSettings: (
            Position: (X: 44; Y: 11);
            Width: 20;
            Height: 12;
            BackgroundChar: ' ';//LabelBackgroundChar;
            FrameSkin: '#';//DefaultFrameSkin;
          );
          LabelsData: (
            (
              Position: (X: 1; Y: 0);
              Caption: LabelScoreboard
            ),
            (
              Position: (X: LabelTab; Y: 2);
              Caption: LabelLines
            ),
            (
              Position: (X: LabelTab2; Y: 2);
              Caption: ''
            ),
            (
              Position: (X: LabelTab; Y: 4);
              Caption: LabelScore
            ),
            (
              Position: (X: LabelTab2; Y: 4);
              Caption: ''
            ),
            (
              Position: (X: LabelTab; Y: 6);
              Caption: LabelSpeed
            ),
            (
              Position: (X: LabelTab2; Y: 6);
              Caption: ''
            ),
            (
              Position: (X: 1; Y: 9);
              Caption: LabelBestScore
            ),
            (
              Position: (X: LabelTab; Y: 11);
              Caption: '0 - VSDev'
            )
          )
        )
      );
      OutputLabelsData: (
        (
          ID: odiSBLines;
          BoardIndex: 1;
          LabelIndex: 2
        ),
        (
          ID: odiSBScore;
          BoardIndex: 1;
          LabelIndex: 4
        ),
        (
          ID: odiSBSpeed;
          BoardIndex: 1;
          LabelIndex: 6
        ),
        (
          ID: odiSBBestScore;
          BoardIndex: 1;
          LabelIndex: 8
        )
      );
      NextFigureCanvasIndex: 0;
      NextFigurePosOnCanvas: (X: 3+6; Y: 2);
      FallenElementSkin: '';
      DestroyLinesAnimation: (
        Frames: (
          //'{}', '()', '<>', '-', ''
          //'. ', 'o ', 'O ', '()', '  '
          //'()', 'O ', 'o ', '. ', '  '
          //'-', '<>', '()', '{}', '[]'
          '-', ' '
          //'-', ''
          //'<-', '-<'
          //'<>', '><'
          //'< ', ' <'
        );
        ReplaysNumber: 3;
        Delay: 150;
      );
      DropDownTrace: (
        Show: True;
        Skin: '||';
        Delay: 50;
      );
      BackgroundColor: Black;//Green;//Blue;//White;
      TextColor: {LightCyan;//Magenta;//}LightGreen;//Yellow;//Black;//White;
    ),
    (// Settings 3
    // Game field - scoreboard
    // 80 = 17 20 6 20 17 // without frame
    //         18   44
      GameFieldData: (
        Position: (X: 18; Y: 3);
        Width: 20;
        Height: 20;
        BackgroundChar: '.';
        FrameSkin: '';//DefaultFrameSkin;
      );
      CanvasDeltaPos: (X: 1; Y: 19);
      BoardsData: (
        ( // Next figure board
          CanvasSettings: (
            Position: (X: 44; Y: 3);
            Width: 20;
            Height: 7;
            BackgroundChar: '.';//LabelBackgroundChar;
            FrameSkin: '';//DefaultFrameSkin;
          );
          LabelsData: (
            (
              Position: (X: 1; Y: 0);
              Caption: LabelNextFigure
            )
          )
        ),
        ( //Scoreboard
          CanvasSettings: (
            Position: (X: 44; Y: 11);
            Width: 20;
            Height: 12;
            BackgroundChar: '.';//LabelBackgroundChar;
            FrameSkin: '';//DefaultFrameSkin;
          );
          LabelsData: (
            (
              Position: (X: 1; Y: 0);
              Caption: LabelScoreboard
            ),
            (
              Position: (X: LabelTab; Y: 2);
              Caption: LabelLines
            ),
            (
              Position: (X: LabelTab2; Y: 2);
              Caption: ''
            ),
            (
              Position: (X: LabelTab; Y: 4);
              Caption: LabelScore
            ),
            (
              Position: (X: LabelTab2; Y: 4);
              Caption: ''
            ),
            (
              Position: (X: LabelTab; Y: 6);
              Caption: LabelSpeed
            ),
            (
              Position: (X: LabelTab2; Y: 6);
              Caption: ''
            ),
            (
              Position: (X: 1; Y: 9);
              Caption: LabelBestScore
            ),
            (
              Position: (X: LabelTab; Y: 11);
              Caption: '0 - VSDev'
            )
          )
        )
      );
      OutputLabelsData: (
        (
          ID: odiSBLines;
          BoardIndex: 1;
          LabelIndex: 2
        ),
        (
          ID: odiSBScore;
          BoardIndex: 1;
          LabelIndex: 4
        ),
        (
          ID: odiSBSpeed;
          BoardIndex: 1;
          LabelIndex: 6
        ),
        (
          ID: odiSBBestScore;
          BoardIndex: 1;
          LabelIndex: 8
        )
      );
      NextFigureCanvasIndex: 0;
      NextFigurePosOnCanvas: (X: 3+6; Y: 2);
      FallenElementSkin: '';
      DestroyLinesAnimation: (
        Frames: (
          //'{}', '()', '<>', '-', ''
          //'. ', 'o ', 'O ', '()', '  '
          //'()', 'O ', 'o ', '. ', '  '
          //'-', '<>', '()', '{}', '[]'
          '-', ' '
          //'-', ''
          //'<-', '-<'
          //'<>', '><'
          //'< ', ' <'
        );
        ReplaysNumber: 3;
        Delay: 150;
      );
      DropDownTrace: (
        Show: True;
        Skin: '||';
        Delay: 50;
      );
      BackgroundColor: Black;//Green;//Blue;//White;
      TextColor: LightCyan;//Magenta;//LightGreen;//Yellow;//Black;//White;
    )
  );

  // It's no longer in use.
  DefaultControlKeysBoardData: TBoardData = (
    CanvasSettings: (
      Position: (X: 7; Y: 3);
      Width: 22;
      Height: 20;
      BackgroundChar: LabelBackgroundChar;
      FrameSkin: '[]';//DefaultFrameSkin;
    );
    LabelsData: (
      (
        Position: (X: 1; Y: 0);
        Caption: LabelControlKeys
      ),
      (
        Position: (X: LabelTab; Y: 3);
        Caption: 'Move right:'
      ),
      (
        Position: (X: LabelTab3; Y: 3);
        Caption: '->'
      ),
      (
        Position: (X: LabelTab; Y: 5);
        Caption: 'Move left:'
      ),
      (
        Position: (X: LabelTab3; Y: 5);
        Caption: '<-'
      ),
      (
        Position: (X: LabelTab; Y: 7);
        Caption: 'Move down:'
      ),
      (
        Position: (X: LabelTab3; Y: 7);
        Caption: 'V'
      ),
      (
        Position: (X: LabelTab; Y: 9);
        Caption: 'Drop down:'
      ),
      (
        Position: (X: LabelTab3; Y: 9);
        Caption: 'Enter'
      ),
      (
        Position: (X: LabelTab; Y: 11);
        Caption: 'Rotate >:'
      ),
      (
        Position: (X: LabelTab3; Y: 11);
        Caption: 'Space'
      ),
      (
        Position: (X: LabelTab; Y: 13);
        Caption: 'Rotate <:'
      ),
      (
        Position: (X: LabelTab3; Y: 13);
        Caption: 'B.space'
      ),
      (
        Position: (X: LabelTab; Y: 17);
        Caption: 'Pause:'
      ),
      (
        Position: (X: LabelTab3; Y: 17);
        Caption: 'P'
      ),
      (
        Position: (X: LabelTab; Y: 19);
        Caption: 'Menu:'
      ),
      (
        Position: (X: LabelTab3; Y: 19);
        Caption: 'Escape'
      )
    )
  );
