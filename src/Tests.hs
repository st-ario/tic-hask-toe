module Tests where

import Game
import TicUI
import Rules
import TreeZipper
import MCTS

import           Data.Tree (Tree, Forest, unfoldTree, unfoldForest)
import qualified Data.Tree as T
import qualified System.Random as R
import           Control.Monad
import           Data.Vector(fromList)

gx = toGrid $ fromList [x,x,x,em,o,o,em,em,o]     -- won by x
gx' = toGrid $ fromList [em,o,o,x,x,x,em,o,x]     -- won by x
gx2 = toGrid $ fromList [em,x,em,em,x,o,em,x,o]  -- won by x
gx3 = toGrid $ fromList [em,x,x,em,x,o,x,x,o]  -- won by x
go = toGrid $ fromList [o,x,em,x,o,em,x,x,o]      -- won by o
go' = toGrid $ fromList [em,o,x,x,o,x,em,o,em]    -- won by o
gI = toGrid $ fromList $ take 9 $ cycle $ [em]      -- in progress
gI' = toGrid $ fromList [x,em,o,em,x,em,o,em,em]  -- in progress
gT = toGrid $ fromList [o,x,o,o,o,x,x,o,x]        -- tied
gT' = toGrid $ fromList [x,x,o,o,o,x,x,o,o]       -- tied

gM2 = (o, Just (Coord 1 1), matNew2)
metNew2 = toGrid $ fromList [g1,g2,g3,g4,g5,g6,g7,g8,g9]   -- won by o
g1 = toGrid $ fromList [o,o,o,em,x,x,em,em,em]   -- won by x
g2 = toGrid $ fromList [x,o,x,x,em,em,x,em,x]   -- won by o
g3 = toGrid $ fromList [x,o,em,em,o,em,o,x,em]   -- won by x
g4 = toGrid $ fromList [em,o,em,o,x,o,em,em,x]     -- in progress
g5 = toGrid $ fromList [x,x,x,x,o,em,o,em,em]   -- won by o
g6 = toGrid $ fromList [em,o,em,em,em,em,x,x,x]   -- in progress
g7 = toGrid $ fromList [em,o,em,x,o,em,x,o,em]   -- in progress
g8 = toGrid $ fromList [o,em,x,em,o,em,em,em,em]   -- in progress
g9 = toGrid $ fromList [x,em,o,em,em,o,em,em,o]   -- won by x

g0 = toGrid $ fromList [em,em,em,em,em,em,em,em,em]   -- won by x
g1' = toGrid $ fromList [em,em,em,em,em,em,em,em,em]   -- won by x
g5' = toGrid $ fromList [em,em,em,em,em,em,em,em,em]   -- won by o
g6' = toGrid $ fromList [em,em,em,em,em,em,em,em,em]   -- in progress

mat0 = toMatch $ met0  -- won by x
mat1 = toMatch $ met1  -- won by o
mat2 = toMatch $ met2  -- in progress
mat3 = toMatch $ met3  -- tied
mat4 = toMatch $ met4
mat5 = toMatch $ met5
mat6 = toMatch $ met6
mat7 = toMatch $ met7
mat8 = toMatch $ met8
mat9 = toMatch $ met9 -- empty game

met0 = toGrid $ fromList [gx,gI,gI',gT,gx',go,go',gx,gx']  -- won by x
met1 = toGrid $ fromList [gT,gI,go,gT',go,gx,go,gT,gx']    -- won by o
met2 = toGrid $ fromList [gx,gI,gI',gT,go,gI,gT,gI,go']    -- in progress
met3 = toGrid $ fromList [gx,gx',gT',gT,go,gT,go',gx,gx']  -- tied
met4 = toGrid $ fromList [gx,gI,gI',gT,go,gI,gx,gx,gx']
met5 = toGrid $ fromList [gx,gI,gI',gT,go,gI,gx,gx,gx']
met6 = toGrid $ fromList [gx,gI,gI',gT,go,gI,gx,gx,gx']
met7 = toGrid $ fromList [gx,gI,gI',gT,go,gI,gx,gx,gx']
met8 = toGrid $ fromList [gx,gI,gI',gT,go,gI,gx,gx,gx']
met9 = toGrid $ fromList [gI,gI,gI,gI,gI,gI,gI,gI,gI]

metNew = toGrid $ fromList [gI,gI,gI,gI,gI,gI,gI,gI,gI] -- in progress
metNew0 = toGrid $ fromList [g0,g1',g2,g3,g4,g5',g6,g7,g8] -- in progress
metNew1 = toGrid $ fromList [g0,g1,g2,g3,g4,g5,g6,g7,g8]  -- in progress
matNew = toMatch metNew
matNew0 = toMatch metNew0
matNew1 = toMatch metNew1
matNew2 = toMatch metNew2

gM = (o, Nothing, matNew)
gM0 = (o, Just (Coord 0 1), matNew0)
gM1 = (x, Just (Coord 0 1), matNew1)


move1 = Move (Coord 0 0) (Coord 0 0) x
move2 = Move (Coord 2 2) (Coord 0 0) o
move3 = Move (Coord 1 0) (Coord 0 0) x
move4 = Move (Coord 0 2) (Coord 0 0) o
move5 = Move (Coord 0 0) (Coord 2 1) x
move6 = Move (Coord 2 2) (Coord 2 1) o
move7 = Move (Coord 1 0) (Coord 1 2) x
move8 = Move (Coord 0 2) (Coord 1 2) o

gG = toGrid $ fromList [o,em,em,em,x,em,em,em,x]

t n = fmap snd $ (T.levels $ unfoldTree playTTT (o,gG)) !! n

main = do
  g <- R.newStdGen
  -- return $ simulationTTT g (o,gI)
  -- return $ simulationUTTT g gM
  return $ getBestMove g gM2