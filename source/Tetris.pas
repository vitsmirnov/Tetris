(*
  (c) Vitaly Smirnov [VSdev]
  mrmaybelately@gmail.com
  2022-2023
*)

program Tetris;

{$mode objfpc}

uses
  TetrisGameFPC;


var
  TetrisGame: TTetrisGameFPC;


begin
  TetrisGame:=TTetrisGameFPC.CreateFromFile('..\resource\Settings.txt');
  TetrisGame.Start;
  TetrisGame.Free;
end.
