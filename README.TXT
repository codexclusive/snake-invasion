Snake Invasion

by Gabor Kotik
GitHub: codexclusive


This program demonstrates how to create graphics in text mode. A predefined
number of snakes are drawn to the screen and when the user presses a key,
they start moving around, avoiding walls and obstacles.

To see a video of the program, click on:

https://youtu.be/dNiUc2V3rK0

The source code was developed and compiled using the Free Pascal IDE for
GO32 V2 DOS extender, version 1.0.12 2017/10/08. To recompile the source code,
you need to select "Target: GO32 V2 DOS extender". The program requires
DPMI to run. For a free and open-source version, you may refer to CWSDPMI.


About the code
==============

The program uses the 80x50 text mode and the ASCII character set for drawing.
In Pascal, the programmer can directly access memory by using the Mem[] array.
The procedure "DrawChar" is responsible for drawing a character on the screen
to a given position with a given texture and color.

The procedure called "InitScreen" sets up the 80x50 text mode and fills the
screen with empty characters. Afterwards, a frame and some walls are drawn,
and finally, 40 snakes are displayed (the number can be modified by changing
the nSnakes constant).

Each snake is defined as an object, which means they all contain some data about
themselves, as well as methods that define their behavior. Snakes know where
they are on the screen (defined by the Pos[] array), and they know in which
direction they are allowed to move (cf. NextMoves[]). They know how to draw
themselves, they move, and turn around when necessary.

Four boolean functions have been created to analyze the snakes' positions.
The function "ValidPoint" checks if a point P is a valid place to put the snake's
head or body. It returns true if the point is on the screen and is empty.
The function ValidStart analyzes if a snake can be drawn to a specific place
with a specific direction on startup.

"ValidMove" checks if the snake can move to a given direction. If there are
six empty points in front of its head, it returns true. "CanMove" returns true
if there is a valid move for the snake. If the result is false, it means the
snake got stuck and needs to turn back.

The speed of the snakes can be modified by changing the value of the "VB_speed" 
constant. The larger the number, the slower the execution. The program terminates
when the user presses a key.

