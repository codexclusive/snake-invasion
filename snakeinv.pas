
{------ Snake Invasion ------------------------------------------

        by Gabor Kotik
        GitHub: codexclusive

        Demonstrates how to implement graphics in Free
        Pascal. The program draws a given number of snakes
        on the screen and moves them around automatically.
        Snakes avoid obstacles and do not crash with
        each other. To create maintainable code, the snake
        is defined as an object. Drawing is done by using
        the Mem[] array.

 ----------------------------------------------------------------}


program SnakeInv;

uses Crt, Ports;

type
    { a point on the character screen }
    Point = record
        X : Shortint;
        Y : Shortint;
    end;

const

    VideoSeg = $B800;
    VB_speed = 6;
    nSnakes  = 40;

    { textures from ASCII table }

    tEmpty = $00;
    tFrame = $DB;   { a square  }
    tWall  = $DB;
    tHead  = $E9;
    tBody  = $04;   { a diamond }

    { colors }

    cEmpty = $00;   { black     }
    cFrame = $08;   { dark gray }
    cWall  = $04;   { red       }
    cHead  = $0F;   { white     }

    { body colors }

    cBody : array[1..8] of Byte =

    ($0A,       { light green   }
     $0B,       { light cyan    }
     $0C,       { light red     }
     $0D,       { light magenta }
     $0E,       { yellow        }
     $02,       { green         }
     $03,       { cyan          }
     $05);      { magenta       }

    { directions }

    Left   : Point = (X : -1; Y :  0);
    Right  : Point = (X :  1; Y :  0);
    Up     : Point = (X :  0; Y : -1);
    Down   : Point = (X :  0; Y :  1);
    NoMove : Point = (X :  0; Y :  0);


{------------------------- Snake object -------------------------}


type

    Snake = object

        Pos       : array[0..8] of Point;
        NextMoves : array[1..3] of Point;

        constructor Init(Dir: Point);

        procedure Draw;
        procedure Move;
        procedure Turn;
        procedure GetNextMoves;
        procedure ValidateMoves;

        function ValidPoint(P : Point)  : Boolean;
        function ValidStart(StartP, Dir : Point) : Boolean;
        function ValidMove(Dir : Point) : Boolean;
        function CanMove : Boolean;
    end;

var

    Snakes   : array[1..nSnakes] of Snake;

    OrigMode : Integer; { original text mode }


{----------------- global procedures & functions ----------------}


procedure DrawChar(X, Y, Txt, Col : Byte);

var Offset : Word;

begin
      Offset := X * 2 + Y * 160;
      Mem[VideoSeg:Offset] := Txt; { store texture }

      Inc(Offset);
      Mem[VideoSeg:Offset] := Col; { store color }
end;

procedure InitScreen;

var I, J : Byte;

begin
      { remember original text mode }
      OrigMode := LastMode;
      { switch to 80x50 text mode }
      TextMode(C80 + Font8x8);

      { draw empty characters }
      for I := 0 to 79 do
          for J := 0 to 49 do
              DrawChar(I, J, tEmpty, cEmpty);
end;

procedure DoneScreen;

begin
      { switch back to original mode }
      TextMode(OrigMode);
end;

function VB_in_progress : Boolean;

begin
      VB_in_progress :=
          ((Port[$03DA] and $08) = 0);
end;

procedure VerticalBlank(Speed : Byte);

var I : Byte;

begin
      for I := 1 to Speed do
      begin
            repeat
            until not (VB_in_progress);

            repeat
            until (VB_in_progress);
      end;
end;

procedure DrawFrame;

var I : Byte;

begin
      { top & bottom }
      for I := 0 to 79 do
      begin
            DrawChar(I,  0, tFrame, cFrame);
            DrawChar(I,  1, tFrame, cFrame);
            DrawChar(I, 48, tFrame, cFrame);
            DrawChar(I, 49, tFrame, cFrame);
      end;

      { left & right }
      for I := 1 to 48 do
      begin
            DrawChar( 0, I, tFrame, cFrame);
            DrawChar( 1, I, tFrame, cFrame);
            DrawChar(78, I, tFrame, cFrame);
            DrawChar(79, I, tFrame, cFrame);
      end;
end;

procedure DrawWalls;

var I, J : Byte;
    P    : Point;

begin
      for I := 0 to 4 do begin { top & bottom }

          for J := 0 to 7 do begin

                P.X := I * 14 + 11;
                P.Y := J + 2;

                DrawChar(P.X, P.Y, tWall, cWall);
                DrawChar(P.X, P.Y + 38, tWall, cWall);

                Inc(P.X);
                DrawChar(P.X, P.Y, tWall, cWall);
                DrawChar(P.X, P.Y + 38, tWall, cWall);
          end;
      end;

      for I := 0 to 3 do begin { middle }

          for J := 0 to 7 do begin

                P.X := I * 14 + 18;
                P.Y := J + 21;

                DrawChar(P.X, P.Y, tWall, cWall);

                Inc(P.X);
                DrawChar(P.X, P.Y, tWall, cWall);
          end;
      end;

end;

procedure InitSnakes;

var I : Byte;

begin
      for I := 1 to nSnakes do
      begin
            case Random(4) of

                 0 : Snakes[I].Init(Left);
                 1 : Snakes[I].Init(Right);
                 2 : Snakes[I].Init(Up);
                 3 : Snakes[I].Init(Down);
            end;
            Snakes[I].Draw;
      end;

      ReadKey;
end;

procedure MoveSnakes;

var I : Byte;

begin
      repeat
             VerticalBlank(VB_speed);

             { move all snakes }
             for I := 1 to nSnakes do
                 Snakes[I].Move;

      until KeyPressed;

      ReadKey; { from buffer }
end;


{-------------------- methods of Snake object -------------------}


constructor Snake.Init(Dir : Point);

var Start : Point;
    I     : Byte;

begin
      repeat
             Start.X := Random(68) + 6;
             Start.Y := Random(38) + 6;

      until ValidStart(Start, Dir);

      for I := 0 to 8 do

      begin
            Pos[I].X := Start.X - Dir.X * I;
            Pos[I].Y := Start.Y - Dir.Y * I;
      end;
end;

procedure Snake.Draw;

var I : Byte;

begin
      DrawChar(Pos[0].X, Pos[0].Y, tHead, cHead);

      for I := 1 to 8 do

          DrawChar(Pos[I].X, Pos[I].Y,
          tBody, cBody[I]);
end;

procedure Snake.Move;

var I : Byte;

begin
      if not (CanMove) then Turn;
      if not (CanMove) then Turn else

      begin
            { clear tail from screen }
            DrawChar(Pos[8].X, Pos[8].Y,
                tEmpty, cEmpty);

            { shift positions }
            for I := 7 downto 0 do
                Pos[I + 1] := Pos[I];

            { define new head }
            Inc(Pos[0].X, NextMoves[1].X);
            Inc(Pos[0].Y, NextMoves[1].Y);

            Self.Draw;
      end;
end;

procedure Snake.Turn;

var NewPos : array[0..8] of Point;
    I      : Byte;

begin
      for I := 0 to 8 do
          NewPos[I] := Pos[8 - I];

      for I := 0 to 8 do
          Pos[I] := NewPos[I];
end;

procedure Snake.GetNextMoves;
{ find possible directions for the next move }
var PrevMove : Point;
    TempMove : Point;

begin
      PrevMove.X := Pos[0].X - Pos[1].X;
      PrevMove.Y := Pos[0].Y - Pos[1].Y;

      NextMoves[1] := PrevMove;
      if (PrevMove.X = 0) then
      { previous move vertical }
      begin
            NextMoves[2] := Left;
            NextMoves[3] := Right;
      end;

      if (PrevMove.Y = 0) then
      { previous move horizontal }
      begin
            NextMoves[2] := Up;
            NextMoves[3] := Down;
      end;

      { shuffle directions according to given probabilities }
      if (Random(2) = 0) then
      begin
            TempMove     := NextMoves[2];
            NextMoves[2] := NextMoves[3];
            NextMoves[3] := TempMove;
      end;

      if (Random(10) = 0) then
      begin
            TempMove     := NextMoves[1];
            NextMoves[1] := NextMoves[2];
            NextMoves[2] := TempMove;
      end;
end;

procedure Snake.ValidateMoves;
{ store the next valid move to NextMoves[1] }
{ if there is no valid move, NextMoves[1]=(0,0) }
var NewDir : Point;

begin
      if ValidMove(NextMoves[1]) then
         NewDir := NextMoves[1]  else

      if ValidMove(NextMoves[2]) then
         NewDir := NextMoves[2]  else

      if ValidMove(NextMoves[3]) then
         NewDir := NextMoves[3]  else

         NewDir := NoMove;

      NextMoves[1] := NewDir;
end;

function Snake.ValidPoint(P : Point) : Boolean;

var Offset   : Integer;
    OnScreen : Boolean;
    Occupied : Boolean;

begin
      Offset := P.X * 2 + P.Y * 160;

      OnScreen := (P.X >=  0) and
                  (P.X <= 79) and
                  (P.Y >=  0) and
                  (P.Y <= 49);

      Occupied := (Mem[VideoSeg:Word(Offset)] <> tEmpty);

      ValidPoint := (OnScreen) and (not(Occupied));
end;

function Snake.ValidStart(StartP, Dir : Point) : Boolean;
{ checks whether the rectangle around the Snake is empty }
{                        }
{ example: Dir = Left    }
{                        }
{   R = rectangle        }
{   S = starting point   }
{                        }
{   RRRRRRRRRRR          }
{   RSRRRRRRRRR          }
{   RRRRRRRRRRR          }
{                        }
{   Rectangle points     }
{   and starting point   }
{   must be empty,       }
{   otherwise invalid.   }

var I, J   : Byte;
    V      : Boolean;
    Corner : Point;
    APoint : Point;

begin
      { find one corner of the rectangle }
      Corner := StartP;
      Inc(Corner.X, Dir.X + Dir.Y);
      Inc(Corner.Y, Dir.X + Dir.Y);
      V := True;

      { loop through every point }
      APoint := Corner;
      for I := 0 to 2 do
      begin
            for J := 0 to 10 do
            begin
                  if not(ValidPoint(APoint))
                  then V := False;

                  Dec(APoint.X, Dir.X);
                  Dec(APoint.Y, Dir.Y);
            end;

            Dec(APoint.X, Dir.Y);
            Dec(APoint.Y, Dir.X);

            Inc(APoint.X, Dir.X * 11);
            Inc(APoint.Y, Dir.Y * 11);
      end;

      { valid if all points are empty }
      ValidStart := V;
end;

function Snake.ValidMove(Dir : Point) : Boolean;
{ checks whether the six points in front of the snake are empty }
{                        }
{ example: Dir = Down    }
{                        }
{   H = head             }
{   B = body             }
{   T = test points      }
{                        }
{       BBBBBB           }
{       B    B           }
{       H                }
{      TTT               }
{      TTT               }
{                        }
{   All test points      }
{   must be empty,       }
{   otherwise the move   }
{   is invalid.          }

var Point1 : Point;
    Point2 : Point;
    V      : Boolean;
    I      : Byte;

begin
      Point1 := Pos[0];
      V := True;

      Inc(Point1.X, Dir.X + Dir.Y);
      Inc(Point1.Y, Dir.X + Dir.Y);

      Point2.X := Point1.X + Dir.X;
      Point2.Y := Point1.Y + Dir.Y;

      for I := 0 to 2 do
      begin
            if not(ValidPoint(Point1))
            then V := False;

            if not(ValidPoint(Point2))
            then V := False;

            Dec(Point1.X, Dir.Y);
            Dec(Point1.Y, Dir.X);
            Dec(Point2.X, Dir.Y);
            Dec(Point2.Y, Dir.X);
      end;

      ValidMove := V;
end;

function Snake.CanMove : Boolean;

var C1, C2 : Boolean;

begin
      Self.GetNextMoves;
      Self.ValidateMoves;

      C1 := (NextMoves[1].X = 0);
      C2 := (NextMoves[1].Y = 0);

      CanMove := C1 xor C2;
end;


{ main program }


begin
      Randomize;

      InitScreen;
      DrawFrame;
      DrawWalls;
      InitSnakes;
      MoveSnakes;
      DoneScreen;
end.

