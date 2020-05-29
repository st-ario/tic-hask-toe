{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module MCTS where

import Game
import Rules
import TreeZipper

import qualified System.Random as R
import           Control.Monad.ST
import           Control.Applicative
import           Data.STRef
import           Control.Lens
import           Data.Tree (Tree, Forest, unfoldTree)
import qualified Data.Tree as T
import           Debug.Trace (trace)
import           Data.Maybe (isNothing, fromJust)
import           Data.Either (fromRight, fromLeft, isLeft)
import           Control.Monad (forM, forM_)
import           Data.List.Extras.Argmax
import           Data.List (elemIndices)

trd :: (a,b,c) -> c
trd (_,_,x) = x

-- ############################  Simulation Steps  #############################

-- given an initial state (last player, current board) play an
-- ultimate tic-tac-toe game randomly until the end
simulationUTTT :: R.StdGen -> (Token, Maybe Coord, Match Token) -> (Token, Maybe Coord, Match Token)
simulationUTTT gen state
  | isOver (trd state) = state
  | otherwise = simulationUTTT newGen $! (nextMoves !! rnd)
    where nextMoves = legalMatchMoves state
          len = length nextMoves - 1
          (rnd,newGen) = (R.randomR $! (0, len)) $! gen

-- ###################  Monte Carlo Tree Data Structures #######################

type ValueOfNode = Double
type NumberOfVisits = Integer
type Weight s = Maybe (STRef s (ValueOfNode,NumberOfVisits))

data MCNode s = MCN { _lastPlayer :: !Token
                     , _lastMove :: !(Maybe Coord)
                     , _currentMatch :: !(Match Token)
                     , _weight :: !(Weight s)
                     }

makeLenses ''MCNode

type MCTree s = Tree (MCNode s)

getWeight :: MCTree s -> STRef s (ValueOfNode,NumberOfVisits)
getWeight = fromJust . _weight . T.rootLabel

-- given the current node, return all legal positions reachable from it as
-- children
legalNodes :: MCNode s -> [MCNode s]
legalNodes (MCN lp lc lm _)
  | lp == EM = error "EM can't move"
  | isOver lm = []
  | otherwise = [ MCN p c m Nothing |
                  (p, c, m) <- legalMatchMoves (lp, lc, lm)]

-- seed for the game tree
seedGrid :: MCNode s -> (MCNode s,[MCNode s])
seedGrid node = (node, legalNodes node)

-- game tree starting from a given (last player, last move, current board) state
unfoldGame :: (Token, Maybe Coord, Match Token) -> ST s (MCTree s)
unfoldGame (lp,lc,lm) = return $! unfoldTree seedGrid $ MCN lp lc lm Nothing

-- ###################  Monte Carlo Tree Search Algorithm ######################
-- NB: ZipNode (MCNode' s) = (MCTree s, [Position (MCNode' s)])

-- #### Parameters to Tune:
tieWeight   = 0
winWeight   = 1
lossWeight  = -1
--ucbConst    = 1 * sqrt(2)
ucbConst    = 1.01 * sqrt(2)
computationalBudget = 4000 :: Integer
-- tie = 2 win = 2 loss = -2 C = 1.2 OK for UCB1, TTT
-- a bit inconsistent, sometimes loses against optimal play

-- tie = 1 win = 2 loss = -1 C <= ~ computationalBudget / 10 
-- very good for UCB-Minimal, TTT
-- guarantees tie playing as X, but ties when it could win

-- tie = 1.1 win = 2 loss = -0.1 C = 45
-- great for UCB-Minimal, TTT
-- mostly plays optimally as X

-- sets the weight of a node to (0,0)
setToZero :: ST s (MCTree s) -> ST s (MCTree s)
setToZero t = do
  (T.Node r sf) <- t
  x <- newSTRef (0,0)
  let newRoot = set weight (Just $! x) $! r
  return (T.Node newRoot sf)

-- determine if the grid is won, tied or lost by the given player
-- and assigns weights accordingly
-- works properly only if called on a game that is over
matchOutcome :: (Eq w, Winnable w) =>  w -> Match w -> Double
matchOutcome p m
  | isLeft status = tieWeight
  | p == (getWinner $ status) = winWeight
  | otherwise = lossWeight
  where status = matchStatus m
        getWinner = fromRight undefined

-- Upper Confidence Bound formula
ucb :: Double -> Integer -> Integer -> Double
-- UCB1:
ucb v nChild nParent  = (v / nc) + ucbConst * sqrt ((log np) / nc)
-- UCB-Minimal:
-- ucb v nChild nParent  = (v / nc) + ucbConst * (1 / nc)
  where nc = fromInteger nChild :: Double
        np = fromInteger nParent :: Double

-- backpropagation algorithm
backprop :: ZipNode (MCNode s) -> Double -> ST s ()
backprop (t,ps) diff = do
  let incr (v,n) =  (v + diff , n + 1)
  modifySTRef' (getWeight t) $! incr
  if length ps == 0
    then return ()
    else (backprop $! stepBack (t,ps)) $! (-diff)

-- returns the index of a child maximizing ucb
-- if multiple children maximize ucb, pick one randomly
bestChild :: R.StdGen -> MCTree s -> ST s Int
bestChild gen t = do
  parentWeight <- readSTRef $! getWeight t
  let parentN = snd $! parentWeight
  let refValues = [x | x <- fmap getWeight $! (T.subForest t)]
  valuesList <- (forM $! refValues) readSTRef
  trace ("the weigh of the parent is " ++ show parentN) $ pure ()
  trace ("the weights of the children are " ++ show valuesList) $ pure ()
  let toMaximize (v,c) = ucb v c $! parentN
  let bestWeight = argmax toMaximize $! valuesList
  let positions = (elemIndices $! bestWeight) valuesList
  trace ("ucb is maximized by the ones in position " ++ show positions) $ return ()
  trace ("the UCBs are " ++ show (fmap toMaximize valuesList)) $ return ()
  let len = (length $! positions) - 1
  if len == 0
    then return (positions !! 0)
    else do let r = fst $! R.randomR (0,len) gen
            trace ("let's pick " ++ show (positions !! r)) $ return ()
            return (positions !! r)

expand :: R.StdGen -> Token -> ZipNode (MCNode s) -> ST s (MCTree s)
expand gen winner (t,ps) = do
  newTree <- setToZero $ return t
  let state = T.rootLabel $! newTree
  let result = simulationUTTT gen $! (state^.lastPlayer, state^.lastMove, state^.currentMatch)
  let diff = matchOutcome winner $! (trd result)
  if winner == state^.lastPlayer
    then backprop (newTree, ps) $! diff
    else backprop (newTree, ps) $! (-diff)
  return newTree

mctsAlgorithm :: R.StdGen -> Integer -> Token -> ST s (ZipNode (MCNode s)) -> ST s (Token, Maybe Coord, Match Token)
mctsAlgorithm gen iteration winner wZipper
  -- after computationalBudget iterations, return the best children of root
  -- according to ucb
  | iteration >= computationalBudget = do
      (t,ps) <- wZipper
      n <- bestChild gen t
      let child = T.rootLabel $! (T.subForest t !! n)
      return (child^.lastPlayer, child^.lastMove, child^.currentMatch)
  | otherwise = do
      (t,ps) <- wZipper
      if
        -- when reaching a leaf of the game tree, backpropagate and restart from root
        | length (T.subForest t) == 0 -> do
            let diff = matchOutcome winner $! (_currentMatch . T.rootLabel $ t)
            if winner == (_lastPlayer . T.rootLabel $ t)
              then backprop (t,ps) $! diff
              else backprop (t,ps) $! (-diff)
            let newZipper = return $! allTheWayBack (t,ps)
            mctsAlgorithm gen (iteration + 1) winner $! newZipper
        -- when reaching a node whose children's weights are not initialized,
        -- initialize all children, run simulations and backpropagate once for
        -- each of them, then restart from root
        | isNothing $ _weight $ T.rootLabel (T.subForest t !! 0) -> do
            let n = (length $! (T.subForest t)) -1
            let f x = expand gen winner $! (descendTo (t,ps) x)
            newSubForest <- forM [0..n] f
            let newTree = T.Node {T.rootLabel=(T.rootLabel t) , T.subForest=newSubForest}
            pair <- readSTRef . getWeight $! newTree
            let newZipper = return $! (allTheWayBack (newTree,ps))
            mctsAlgorithm gen (iteration + 1) winner $! newZipper
        -- when reaching a node whose children's weights have already been
        -- initialized, pick the best children according to ucb and
        -- restart from it
        | otherwise -> do
            n <- bestChild gen t
            let newZipper = return $! (descendTo (t, ps) n)
            mctsAlgorithm gen iteration winner $! newZipper
            -- NB the iteration number is not incremented, as the algorithm
            -- is not getting back to the root

-- remove from the available moves all the ones that can make the opponent
-- win in one; if all the moves available result in a loss in one, do nothing
precondition :: MCTree s -> MCTree s
precondition tree
  | length newSubForest == 0 = tree
  | otherwise = tree { T.subForest = newSubForest }
    where newSubForest = filter (not . isBlunder) $! (T.subForest tree)

-- if a move results in a loss in one, return True, otherwise return False
isBlunder :: MCTree s -> Bool
isBlunder (T.Node r sub) =
  let moves = legalMatchMoves $! (r^.lastPlayer, r^.lastMove, r^.currentMatch)
  in any isWon $! (fmap trd moves)

getBestMove :: R.StdGen -> (Token, Maybe Coord, Match Token) -> (Token, Maybe Coord, Match Token)
getBestMove gen (lp,lc,lm)
  | isOver lm = error "No legal moves available"
  | otherwise = runST $ do
      gameTree <- setToZero $! fmap precondition $! unfoldGame (lp,lc,lm)
      -- if after conditioning there is only one move available, just return it
      -- otherwise, run the MCTS algorithm
      if length (T.subForest gameTree) == 1
        then do let unique = T.rootLabel $ (T.subForest gameTree) !! 0
                return $ (unique^.lastPlayer, unique^.lastMove, unique^.currentMatch)
        else do let gameZipper = return $ (gameTree, [])
                mctsAlgorithm gen 0 (nextPlayer lp) $! gameZipper