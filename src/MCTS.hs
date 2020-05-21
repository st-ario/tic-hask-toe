{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module MCTS where

import Game
import Rules

import qualified System.Random as R
import           Control.Monad.ST
import           Data.STRef
import           Control.Lens
import           Data.Tree (Tree, Forest, unfoldTree)
import qualified Data.Tree as T
import           Debug.Trace (trace)

-- ############################  Simulation Steps  #############################

-- given an initial state (last player, current board) play an ordinary
-- tic-tac-toe game randomly until the end
simulationTTT :: R.StdGen -> (Token, Grid Token) -> (Token, Grid Token)
simulationTTT gen state
  | isOver (snd state) = state
  | otherwise = simulationTTT newGen (nextMoves !! rnd)
  -- For debug purposes: print all the moves made in the random game
  -- | otherwise = trace (show state) $ simulationTTT newGen (nextMoves !! rnd)
    where nextMoves = legalGridMoves state
          len = length nextMoves - 1
          (rnd,newGen) = R.randomR (0, len) gen

-- given an initial state (last player, current board) play an
-- ultimate tic-tac-toe game randomly until the end
simulation :: R.StdGen -> (Token, Match Token) -> (Token, Match Token)
simulation gen state = undefined
-- TODO

-- ###################  Monte Carlo Tree Data Structures #######################

type Weight s = Maybe (ST s (STRef s (Integer,Integer)))

-- nodes for the baby case
data MCNode' s = MCN { _lastPlayer :: Token
                     , _currentGrid :: Grid Token
                     , _weight :: Weight s
                     }

makeLenses ''MCNode'

-- if the STRef has been initialized, show it as "Something"
instance (forall s. Show (MCNode' s)) where
  show (MCN p g Nothing) = "("++ show p ++ ", " ++ show g ++", Nothing)"
  show (MCN p g (Just _)) = "("++ show p ++ ", " ++ show g ++", Something)"

notInitialized :: MCNode' s -> Bool
notInitialized (MCN p g Nothing) = True
notInitialized (MCN p g (Just _)) = False

type MCTree s = Tree (MCNode' s)

-- given the current node, return all legal positions reachable from it as
-- children
legalNodes' :: MCNode' s -> [MCNode' s]
legalNodes' (MCN last g _)
  | last == EM = error "EM can't move"
  | isOver g = []
  | otherwise = [ MCN p (setGridEl (Coord m n) p g) Nothing |
                  m <- [0..2], n <- [0..2],
                  not(isOver $ getGridEl (Coord m n) g) ]
    where p = nextPlayer last

-- seed for the game tree of a single grid
seedGrid :: MCNode' s -> (MCNode' s,[MCNode' s])
seedGrid node = (node, legalNodes' node)

-- game tree of a single tic-tac-toe match
babyMCTree :: MCTree s
babyMCTree = unfoldTree seedGrid $ MCN O gI Nothing
  where gI = toGrid $ take 9 $ cycle [EM]

babyMCTList = T.levels babyMCTree