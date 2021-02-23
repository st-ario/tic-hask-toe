module Tests where

import Game
import TicUI
import Rules
import TreeZipper
import MCTS

import qualified System.Random as R
import           Control.Monad
import           Data.Vector(fromList)
import qualified Data.Vector as V

gx = toGrid $ fromList [x,x,x,em,o,o,em,em,o]
gx' = toGrid $ fromList [em,o,o,x,x,x,em,o,x]
gx2 = toGrid $ fromList [em,x,em,em,x,o,em,x,o]
gx3 = toGrid $ fromList [em,x,x,em,x,o,x,x,o]
go = toGrid $ fromList [o,x,em,x,o,em,x,x,o]
go' = toGrid $ fromList [em,o,x,x,o,x,em,o,em]
gI = toGrid $ fromList $ take 9 $ cycle $ [em]
gI' = toGrid $ fromList [x,em,o,em,x,em,o,em,em]
gT = toGrid $ fromList [o,x,o,o,o,x,x,o,x]
gT' = toGrid $ fromList [x,x,o,o,o,x,x,o,o]

g0 = Grid $ fromList [em,em,x,em,em,em,em,em,em]
g1 = Grid $ fromList [em,em,em,em,o,em,em,em,em]
g2 = Grid $ fromList [em,em,em,em,o,em,em,em,em]
g3 = Grid $ fromList [em,em,em,em,em,em,em,em,em]
g4 = Grid $ fromList [o,x,em,em,x,em,em,em,x]
g5 = Grid $ fromList [em,em,em,em,em,em,em,em,em]
g6 = Grid $ fromList [em,em,em,em,em,em,em,em,em]
g7 = Grid $ fromList [em,em,em,em,em,em,em,em,em]
g8 = Grid $ fromList [em,em,em,em,x,em,em,em,em]

s = fromList [Left True, Left True, Left True,
              Left True, Left True, Left True,
              Left True, Left True, Left True]

met = fromList [g0,g1,g2,g3,g4,g5,g6,g7,g8]

mat = Match $ Grid $ V.zip met s

move0 = (Move (Coord (0,0)) (Coord (0,0)) x)
move1 = (Move (Coord (0,1)) (Coord (2,2)) x)
move2 = (Move (Coord (1,2)) (Coord (1,1)) x)
move3 = (Move (Coord (1,1)) (Coord (0,1)) x)
move4 = (Move (Coord (2,1)) (Coord (2,0)) x)
move5 = (Move (Coord (2,0)) (Coord (1,2)) x)
moveX = (Move (Coord (1,1)) (Coord (0,0)) o)

metNew = fromList [gI,gI,gI,gI,gI,gI,gI,gI,gI]
statusNew = fromList [Left True,Left True,Left True,Left True,Left True,Left True,Left True,Left True,Left True]

matNew = Match $ Grid $ V.zip metNew statusNew

gM = (move0, mat)

main = do
  g <- R.newStdGen
  -- return $ simulationUTTT g (moveX, mat, (Left True))
  return $ getBestMove g 1 gM