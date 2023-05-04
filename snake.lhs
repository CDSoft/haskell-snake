% Snake Puzzle Solver in Haskell
% Christophe Delord
% 24 May 2018

License
=======

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see http://www.gnu.org/licenses/.

Introduction
============

Once I have been offered a snake puzzle.
It's made of 64 cubes of wood, some of them can turn.
The goal is to fold this snake into a 4x4x4 cube.

After a while trying to solve this cube I decided to write a solver in Prolog.
I present here an Haskell version of this solver.

Model of the snake
==================

The snake is made of 64 cubes. Cubes are joined in a way that
the next cube is either in the same direction, either in a perpendicular direction.
We will model theses constraints by a list of terms `F` or `T`:

- `F`: the next cube goes forward
- `T`: the next cube "turns"

> import Data.Array
> import Control.Parallel
> import Control.Parallel.Strategies
>
> data SnakeSection = F | T deriving (Eq) -- Forward or Turn
>
> snake :: [SnakeSection]
> snake = [ F,F,T,T,F,T,T,T,
>           F,F,T,T,F,T,T,F,
>           T,T,F,T,T,T,T,T,
>           T,T,T,T,F,T,F,T,
>           T,T,T,T,T,F,T,F,
>           F,T,T,T,T,F,F,T,
>           T,F,T,T,T,T,T,T,
>           T,T,T,T,F,F,T   ]

Model of the cube
=================

The cube is a 4x4x4 array of booleans.
`True` means the cell is occupied by the partial solution
and `False` means the cells is still available.

> type Cube = Array (Int,Int,Int) Bool
> type Position = (Int,Int,Int)
> type Direction = (Int,Int,Int)

Solutions
=========

A solution is a list of terms indicating the direction to follow in the cube
to fill it while walking throught the snake.

> data Move = Forward | Backward | Left | Right | Up | Down deriving (Show)
> type Solution = [Move]

Solver
======

The solver is a brute force backtracking solver.
Given a partial solution, a current position and direction it tries
all the possibilities and concat them.
`solve` returns a list of all the solutions.
Thanks to the lazyness of Haskell we will only compute the first one.
There are a lot of solutions because of symetries.

So the solver starts with:

- an empty partial solution
- a cube fill with the first snake cube
- at any positions in the cube
- in any directions

> solve :: [SnakeSection] -> [Solution]
> solve snake = concat [ solve [] (emptyCube//[(p,True)]) p d snake
>                      | p <- r3D, d <- dirs
>                      ]
>     where

The size of the cube is $\sqrt[3]{1 + length(snake)}$[^1].
The cube is a 3D array. `i3D` and `r3D` are the coordinates of each small cubes.

[^1]: If you don't see a cubic root here, blame your browser and try Firefox instead ;-).

>         size = round (fromIntegral (length snake + 1) ** (1/3))
>         i3D = ((1,1,1),(size,size,size))
>         r3D = range i3D

The initial empty cube is filled with `False` values (no cube occupied yet).

>         emptyCube :: Cube
>         emptyCube = array i3D [(p,False) | p <- r3D]

Here is the real solver.
There are two possibilities at each stage.

- if all the snake cubes have been placed in the cube, the partial solution is a complete.
- if a snake cube must still be placed, the solver tries continuing in all the possible directions
  from the current position and direction.
  A new position is possible only if it is in the big cube and if it is not yet occupied.

>         solve :: Solution -> Cube -> Position -> Direction -> [SnakeSection] -> [Solution]
>         solve path cube _ _ [] = [path]
>         solve path cube p d (s:ss) = concat [
>                 solve (dp p p' : path) (cube//[(p',True)]) p' d' ss
>             |   d' <- turn s d,
>                 let p' = nextPos p d',
>                 inRange i3D p', not (cube!p')
>             ]

The recursive search can be performed in parallel on several cores.
This is pretty easy in Haskell. `parL` is a strategy that evaluates
items in a list in parallel:

>         solve' :: Solution -> Cube -> Position -> Direction -> [SnakeSection] -> [Solution]
>         solve' path cube _ _ [] = [path]
>         solve' path cube p d (s:ss) = concat $ (if s==T then id else parL) [
>                 solve' (dp p p' : path) (cube//[(p',True)]) p' d' ss
>             |   d' <- turn s d,
>                 let p' = nextPos p d',
>                 inRange i3D p', not (cube!p')
>             ]

Directions are 3D unit vectors describing the eight possible directions in the cube.

>         dirs :: [Direction]
>         dirs = [(-1,0,0), (1,0,0), (0,-1,0), (0,1,0), (0,0,-1), (0,0,1)]

`turn` computes the next possible directions from the current position and direction.

- if the snake goes Forward, the only possible direction is the current one
- if the snake turns, the possible directions are perpendicular to the current one

>         turn :: SnakeSection -> Direction -> [Direction]
>         turn F d = [d]
>         turn T (_,0,0) = [d | d@(0,_,_) <- dirs]
>         turn T (0,_,0) = [d | d@(_,0,_) <- dirs]
>         turn T (0,0,_) = [d | d@(_,_,0) <- dirs]

Computing the next position is just a matter of adding vectors.

>         nextPos :: Position -> Direction -> Position
>         nextPos (x,y,z) (dx,dy,dz) = (x+dx, y+dy, z+dz)

A step in the solution is simply the move required to go from one position to the next one.

>         dp :: Position -> Position -> Move
>         dp (x,y,z) (x',y',z') | δx == 1   = Forward
>                               | δx == -1  = Backward
>                               | δy == 1   = Main.Right
>                               | δy == -1  = Main.Left
>                               | δz == 1   = Up
>                               | δz == -1  = Down
>             where (δx, δy, δz) = (x'-x, y'-y, z'-z)

`parL` is a strategy that evaluate items of a list in parallel.
This fasten significally the search
(note: it seems that with ghc 8.0.2, the non concurrent version is faster).

> parL = withStrategy (parList rseq)

Solution
========

There are many solutions because of symetries. Let's take only the first one.
`main` takes the first solution, enumerates and prints all the steps.

> main = printSol $ zip [1..] $ reverse $ head $ solve snake
>     where printSol ((i,d):ds) = do
>               putStrLn (show i ++ ": " ++ show d)
>               printSol ds
>           printSol [] = return ()

Execution
=========

It's better to compile the script with ghc.
The interpreted version is 17 times slower than the compiled one.

~~~~~
$ snake

@(script.bash ".build/snake")
~~~~~

Source
======

The Haskell source code is here: [snake.lhs](snake.lhs)
