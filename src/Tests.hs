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

gX = toGrid $ fromList [X,X,X,EM,O,O,EM,EM,O]     -- won by X
gX' = toGrid $ fromList [EM,O,O,X,X,X,EM,O,X]     -- won by X
gX'' = toGrid $ fromList [EM,X,EM,EM,X,O,EM,X,O]  -- won by X
gO = toGrid $ fromList [O,X,EM,X,O,EM,X,X,O]      -- won by O
gO' = toGrid $ fromList [EM,O,X,X,O,X,EM,O,EM]    -- won by O
gI = toGrid $ fromList $ take 9 $ cycle $ [EM]      -- in progress
gI' = toGrid $ fromList [X,EM,O,EM,X,EM,O,EM,EM]  -- in progress
gT = toGrid $ fromList [O,X,O,O,O,X,X,O,X]        -- tied
gT' = toGrid $ fromList [X,X,O,O,O,X,X,O,O]       -- tied

gM2 = (O, Just (Coord 1 1), matNew2)
metNew2 = toGrid $ fromList [g1,g2,g3,g4,g5,g6,g7,g8,g9]   -- won by O
g1 = toGrid $ fromList [O,O,O,EM,X,X,EM,EM,EM]   -- won by X
g2 = toGrid $ fromList [X,O,X,X,EM,EM,X,EM,X]   -- won by O
g3 = toGrid $ fromList [X,O,EM,EM,O,EM,O,X,EM]   -- won by X
g4 = toGrid $ fromList [EM,O,EM,O,X,O,EM,EM,X]     -- in progress
g5 = toGrid $ fromList [X,X,X,X,O,EM,O,EM,EM]   -- won by O
g6 = toGrid $ fromList [EM,O,EM,EM,EM,EM,X,X,X]   -- in progress
g7 = toGrid $ fromList [EM,O,EM,X,O,EM,X,O,EM]   -- in progress
g8 = toGrid $ fromList [O,EM,X,EM,O,EM,EM,EM,EM]   -- in progress
g9 = toGrid $ fromList [X,EM,O,EM,EM,O,EM,EM,O]   -- won by X

g0 = toGrid $ fromList [EM,EM,EM,EM,EM,EM,EM,EM,EM]   -- won by X
g1' = toGrid $ fromList [EM,EM,EM,EM,EM,EM,EM,EM,EM]   -- won by X
g5' = toGrid $ fromList [EM,EM,EM,EM,EM,EM,EM,EM,EM]   -- won by O
g6' = toGrid $ fromList [EM,EM,EM,EM,EM,EM,EM,EM,EM]   -- in progress

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

met0 = toGrid $ fromList [gX,gI,gI',gT,gX',gO,gO',gX,gX']  -- won by X
met1 = toGrid $ fromList [gT,gI,gO,gT',gO,gX,gO,gT,gX']    -- won by O
met2 = toGrid $ fromList [gX,gI,gI',gT,gO,gI,gT,gI,gO']    -- in progress
met3 = toGrid $ fromList [gX,gX',gT',gT,gO,gT,gO',gX,gX']  -- tied
met4 = toGrid $ fromList [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met5 = toGrid $ fromList [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met6 = toGrid $ fromList [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met7 = toGrid $ fromList [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met8 = toGrid $ fromList [gX,gI,gI',gT,gO,gI,gX,gX,gX']
met9 = toGrid $ fromList [gI,gI,gI,gI,gI,gI,gI,gI,gI]

metNew = toGrid $ fromList [gI,gI,gI,gI,gI,gI,gI,gI,gI] -- in progress
metNew0 = toGrid $ fromList [g0,g1',g2,g3,g4,g5',g6,g7,g8] -- in progress
metNew1 = toGrid $ fromList [g0,g1,g2,g3,g4,g5,g6,g7,g8]  -- in progress
matNew = toMatch metNew
matNew0 = toMatch metNew0
matNew1 = toMatch metNew1
matNew2 = toMatch metNew2

gM = (O, Nothing, matNew)
gM0 = (O, Just (Coord 0 1), matNew0)
gM1 = (X, Just (Coord 0 1), matNew1)


move1 = Move (Coord 0 0) (Coord 0 0) X
move2 = Move (Coord 2 2) (Coord 0 0) O
move3 = Move (Coord 1 0) (Coord 0 0) X
move4 = Move (Coord 0 2) (Coord 0 0) O
move5 = Move (Coord 0 0) (Coord 2 1) X
move6 = Move (Coord 2 2) (Coord 2 1) O
move7 = Move (Coord 1 0) (Coord 1 2) X
move8 = Move (Coord 0 2) (Coord 1 2) O

gG = toGrid $ fromList [O,EM,EM,EM,X,EM,EM,EM,X]

t n = fmap snd $ (T.levels $ unfoldTree playTTT (O,gG)) !! n

main = do
  g <- R.newStdGen
  -- return $ simulationTTT g (O,gI)
  -- return $ simulationUTTT g gM
  return $ getBestMove g gM2