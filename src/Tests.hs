module Tests where

import Game
import TicUI
import Rules
import TreeZipper
import MCTS

import           Data.Tree (Tree, Forest, unfoldTree, unfoldForest)
import qualified Data.Tree as T
import qualified System.Random as R
import Control.Monad

gX = toGrid [X,X,X,EM,O,O,EM,EM,O]     -- won by X
gX' = toGrid [EM,O,O,X,X,X,EM,O,X]     -- won by X
gO = toGrid [O,X,EM,X,O,EM,X,X,O]      -- won by O
gO' = toGrid [EM,O,X,X,O,X,EM,O,EM]    -- won by O
gI = toGrid $ take 9 $ cycle [EM]      -- in progress
gI' = toGrid [X,EM,O,EM,X,EM,O,EM,EM]  -- in progress
gT = toGrid [O,X,O,O,O,X,X,O,X]        -- tied
gT' = toGrid [X,X,O,O,O,X,X,O,O]       -- tied

mat0 = toMatch $ met0  -- won by X
mat1 = toMatch $ met1  -- won by O
mat2 = toMatch $ met2  -- in progress
mat3 = toMatch $ met3  -- tied
mat4 = toMatch $ met4
mat5 = toMatch $ met5
mat6 = toMatch $ met6
mat7 = toMatch $ met7
mat8 = toMatch $ met8
mat9 = toMatch $ met9 -- empty game

met0 = toGrid [gX,gI,gI',gT,gX',gO,gO',gX,gX']  -- won by X
met1 = toGrid [gT,gI,gO,gT',gO,gX,gO,gT,gX']    -- won by O
met2 = toGrid [gX,gI,gI',gT,gO,gI,gT,gI,gO']    -- in progress
met3 = toGrid [gX,gX',gT',gT,gO,gT,gO',gX,gX']  -- tied
met4 = toGrid [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met5 = toGrid [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met6 = toGrid [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met7 = toGrid [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met8 = toGrid [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met9 = toGrid [gI,gI,gI,gI,gI,gI,gI,gI,gI]

move1 = Move (Coord 0 0) (Coord 0 0) X
move2 = Move (Coord 2 2) (Coord 0 0) O
move3 = Move (Coord 1 0) (Coord 0 0) X
move4 = Move (Coord 0 2) (Coord 0 0) O
move5 = Move (Coord 0 0) (Coord 2 1) X
move6 = Move (Coord 2 2) (Coord 2 1) O
move7 = Move (Coord 1 0) (Coord 1 2) X
move8 = Move (Coord 0 2) (Coord 1 2) O

fT1 = (T.Node 1 [])
fT2 = (T.Node 2 [fT1])
fT3 = (T.Node 3 [])
fT4 = (T.Node 4 [])
fT5 = (T.Node 5 [fT4])
fT6 = (T.Node 6 [])
fT7 = (T.Node 7 [fT3,fT2])
fT8 = (T.Node 8 [])
fT9 = (T.Node 9 [fT8,fT7,fT6,fT5])

gG = toGrid [EM,EM,EM,EM,EM,EM,EM,EM,EM]

main = do
  g <- R.newStdGen
  --return $ simulationTTT g (O,gI)
  return $ getBestMove g (O,gG)
  -- return $ getBestMove g (X,gG)