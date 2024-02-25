(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

  //{$J-}

  // I  L  J  S  Z  T  O
  // || () <> // \\ {} []

  {DefaultFigureData: TFigureData = (
    ElementsData: (
    );
    RotationPoint
  );}

  DefaultFiguresSkinsSet: TElementSkinsFPCSet = (
    ('||', '()', '<>', '//', '\\', '{}', '[]'),
    ('[]', '()', '()', '<>', '<>', '{}', '[]'),
    ('[]', '[]', '[]', '[]', '[]', '[]', '[]'),
    ('()', '()', '()', '()', '()', '()', '()'),
    ('{}', '{}', '{}', '{}', '{}', '{}', '{}')
  );

  DefaultFiguresTest: TFiguresData = (
    ( // I
      ElementsData: (
        (Position: (X: 0; Y: 0); Skin: '\/'),
        (Position: (X: 0; Y: 1); Skin: '/\')
      );
      RotationPoint: (X: 0; Y: 2)
    )
  );

  DefaultFiguresData: TFiguresData = (
    ( // I
      ElementsData: (
        (Position: (X: 0; Y: 0); Skin: '||'),
        (Position: (X: 0; Y: 1); Skin: '||'),
        (Position: (X: 0; Y: 2); Skin: '||'),
        (Position: (X: 0; Y: 3); Skin: '||')
      );
      RotationPoint: (X: 0; Y: 2)
    ), ( // L
      ElementsData: (
        (Position: (X: 0; Y: 0); Skin: '()'),
        (Position: (X: 0; Y: 1); Skin: '()'),
        (Position: (X: 0; Y: 2); Skin: '()'),
        (Position: (X: 1; Y: 0); Skin: '()')
      );
      RotationPoint: (X: 0; Y: 1)
    ), ( // J
      ElementsData: (
        (Position: (X: 0; Y: 0); Skin: '<>'),
        (Position: (X: 1; Y: 0); Skin: '<>'),
        (Position: (X: 1; Y: 1); Skin: '<>'),
        (Position: (X: 1; Y: 2); Skin: '<>')
      );
      RotationPoint: (X: 1; Y: 1)
    ), ( // S
      ElementsData: (
        (Position: (X: 0; Y: 0); Skin: '//'),
        (Position: (X: 1; Y: 0); Skin: '//'),
        (Position: (X: 1; Y: 1); Skin: '//'),
        (Position: (X: 2; Y: 1); Skin: '//')
      );
      RotationPoint: (X: 1; Y: 1)
    ), ( // Z
      ElementsData: (
        (Position: (X: 0; Y: 1); Skin: '\\'),
        (Position: (X: 1; Y: 0); Skin: '\\'),
        (Position: (X: 1; Y: 1); Skin: '\\'),
        (Position: (X: 2; Y: 0); Skin: '\\')
      );
      RotationPoint: (X: 1; Y: 1)
    ), ( // T
      ElementsData: (
        (Position: (X: 0; Y: 1); Skin: '{}'),
        (Position: (X: 1; Y: 0); Skin: '{}'),
        (Position: (X: 1; Y: 1); Skin: '{}'),
        (Position: (X: 2; Y: 1); Skin: '{}')
      );
      RotationPoint: (X: 1; Y: 1)
    ), ( // O
      ElementsData: (
        (Position: (X: 0; Y: 0); Skin: '[]'),
        (Position: (X: 0; Y: 1); Skin: '[]'),
        (Position: (X: 1; Y: 0); Skin: '[]'),
        (Position: (X: 1; Y: 1); Skin: '[]')
      );
      RotationPoint: (X: 0.5; Y: 0.5)
    )
  );

  DefaultFigures2: TFiguresData = (
    ( //
      ElementsData: (
        (
          Position: (X: 0; Y: 0);
          Skin: '<>'
        ), (
          Position: (X: 1; Y: 1);
          Skin: '()'
        ), (
          Position: (X: 2; Y: 2);
          Skin: '[]'
        ), (
          Position: (X: 3; Y: 3);
          Skin: '{}'
        )
      );
      RotationPoint: (X: 1; Y: 1)
    ),
    ( //
      ElementsData: (
        (Position: (X: 0; Y: 0); Skin: '||'),
        (Position: (X: 0; Y: 1); Skin: '()'),
        (Position: (X: 1; Y: 0); Skin: '[]'),
        (Position: (X: 2; Y: 0); Skin: '{}'),
        (Position: (X: 2; Y: 1); Skin: '<>')
      );
      RotationPoint: (X: 1; Y: 0)
    )
  );

(*///////////////////////////////

[] () {} <> || \\ //

[]
[]
[]
[]

[]{}   //\\  \\//  ||||  \\\\  ////
<>()   \\//  //\\  ||||  ////  ////

{}{}{}
  {}

()
()
()()

  ()
  ()
()()

<><>
  <><>

  <><>
<><>

///////////////////////////////*)
