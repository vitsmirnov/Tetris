(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

  //{$J-}

  DefaultTextFiguresTest: TTextFigures = (
    (
      Figure: (
        '0  0'
      );
      RotationPoint: (X: 0; Y: 0);
      Skin: '><'
    )
  );

  // I  L  J  S  Z  T  O
  // || () <> // \\ {} []

  DefaultTextFigures: TTextFigures = (
    ( // I
      Figure: (
        '0',
        '0',
        '0',
        '0'
      );
      RotationPoint: (X: 0; Y: 2);
      Skin: '||'
    ),
    ( // L
      Figure: (
        '0',
        '0',
        '00'
      );
      RotationPoint: (X: 0; Y: 1);
      Skin: '<>'
    ),
    ( // J
      Figure: (
       ' 0',
       ' 0',
       '00'
      );
      RotationPoint: (X: 1; Y: 1);
      Skin: '()'
    ),
    ( // S
      Figure: (
        ' 00',
        '00'
      );
      RotationPoint: (X: 1; Y: 1);
      Skin: '//'
    ),
    ( // Z
      Figure: (
        '00',
        ' 00'
      );
      RotationPoint: (X: 1; Y: 1);
      Skin: '\\'
    ),
    ( // T
      Figure: (
        '000',
        ' 0'
      );
      RotationPoint: (X: 1; Y: 1);
      Skin: '{}'
    ),
    ( // O
      Figure: (
        '00',
        '00'
      );
      RotationPoint: (X: 0.5; Y: 0.5);
      Skin: '[]'
    )
  );
