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
import           Data.Tree (Tree, unfoldTree)
import qualified Data.Tree as T
import           Debug.Trace (trace)
import           Data.Maybe (isNothing, fromJust)
import           Data.Either (fromRight, isRight, fromLeft, isLeft)
import           Control.Monad (forM, forM_)
import           Data.List.Extras.Argmax
import           Data.List (elemIndices)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Control.Monad (join)

fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,x,_) = x

trd :: (a,b,c) -> c
trd (_,_,x) = x

getWeight :: MCTree s -> STRef s (ValueOfNode,NumberOfVisits)
getWeight = _weight . _root

-- given the current node, return all legal positions reachable from it as
-- children
legalNodes :: MCNode s -> ST s (Forest s)
legalNodes (MCN lp lc lm _ _ _) = do
  match <- readSTRef lm
  w <- newSTRef (0,0)
  let legals = legalMatchMoves (lp, lc, match)
  childrenMatches <- mapM (\(_,_,m)-> newSTRef m) $! legals
  let childrenPairs = zip legals $! childrenMatches
  let children = V.fromList $! [MCT (MCN p c m w False Nothing) V.empty | (a@(p,c,_),m) <- childrenPairs]
  return $! children

-- ############################  Simulation Steps  #############################

-- given an initial state (last player, current board) play an
-- ultimate tic-tac-toe game randomly until the end
simulationUTTT :: R.StdGen -> (Token, Maybe Coord, Match Token) -> (Token, Maybe Coord, Match Token)
simulationUTTT gen state
  | outcome /= Left True = state
  | otherwise = simulationUTTT newGen $! (nextMoves !! rnd)
    where nextMoves = legalMatchMoves $! state
          len = length nextMoves - 1
          (rnd,newGen) = (R.randomR $! (0, len)) $! gen
          outcome = matchStatus $! (trd $! state)

-- ###################  Monte Carlo Tree Search Algorithm ######################

-- #### Parameters to Tune:
tieWeight   = 0
winWeight   = 1
lossWeight  = -1
--ucbConst    = 1 * sqrt(2)
ucbConst    = 1.01 * sqrt(2)
computationalBudget = 4000 :: Int
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
  (MCT r sf) <- t
  x <- newSTRef (0,0)
  let newRoot = set weight x $! r
  return (MCT newRoot sf)

matchOutcome :: Maybe Token -> Token -> Double
matchOutcome mt t
  | isNothing mt = tieWeight
  | y == t = winWeight
  | otherwise = lossWeight
  where y = fromJust mt

-- Upper Confidence Bound formula
ucb :: Double -> Int -> Int -> Double
-- UCB1:
ucb v nChild nParent  = (v / nc) + ucbConst * sqrt ((log np) / nc)
-- UCB-Minimal:
-- ucb v nChild nParent  = (v / nc) + ucbConst * (1 / nc)
  where nc = fromIntegral nChild :: Double
        np = fromIntegral nParent :: Double

-- backpropagation algorithm
backprop :: ZipNode s -> Double -> ST s ()
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
  let refValues = [x | x <- V.toList $! V.map getWeight $! (_sForest t)]
  valuesList <- (forM $! refValues) readSTRef
  trace ("the weigh of the parent is " ++ show parentN) $ pure ()
  -- trace ("the weights of the children are " ++ show valuesList) $ pure ()
  let toMaximize (v,c) = ucb v c $! parentN
  let bestWeight = argmax toMaximize $! valuesList
  let positions = (elemIndices $! bestWeight) valuesList
  trace ("ucb is maximized by the ones in position " ++ show positions) $ return ()
  -- trace ("the UCBs are " ++ show (fmap toMaximize valuesList)) $ return ()
  let len = (length $! positions) - 1
  if len == 0
    then return (positions !! 0)
    else do let r = fst $! R.randomR (0,len) gen
            trace ("let's pick " ++ show (positions !! r)) $ return ()
            return (positions !! r)

{--
expand :: R.StdGen -> Token -> ZipNode s -> ST s (MCTree s)
expand gen winner (t,ps) = do
  newTree <- setToZero $ return t
  let state = _root $! newTree
  m <- readSTRef $! state^.currentMatch
  let result = simulationUTTT gen $! (state^.lastPlayer, state^.lastMove, m)
  let diff = matchOutcome winner $! (trd result)
  if winner == state^.lastPlayer
    then backprop (newTree, ps) $! diff
    else backprop (newTree, ps) $! (-diff)
  return newTree
--}

expand :: R.StdGen -> Token -> ZipNode s -> ST s (ZipNode s)
expand gen winner (t,ps) = do
  state <- readSTRef $ t^.root.currentMatch
  let legalMoves = legalMatchMoves (t^.root.lastPlayer, t^.root.lastMove, state)
  -- if an actual leaf of the game tree is reached, update the node with
  -- this information, backpropagate and return root zipper
  if null legalMoves
    then do
      let status = matchStatus state
      let oldRoot = t^.root
      (newRoot,diff) <- if isLeft status
        then do -- the match ends in a tie
          let newRoot = oldRoot{_isOver=True}
          let diff = matchOutcome Nothing winner
          return $! (newRoot,diff)
        else do -- the match ends with a win
          let matchWinner = Just $! fromRight undefined status
          let newRoot = oldRoot{_isOver=True, _winner=matchWinner}
          let diff = matchOutcome matchWinner winner
          return $! (newRoot,diff)
      let newZipper = (MCT newRoot V.empty,ps)
      backprop newZipper diff
      return $! allTheWayBack newZipper
    -- otherwise, update the subforest, simulate and backpropagate once
    -- for every child node, then return root zipper
    -- (tuples with STRefs call for a bit of a monadic mess)
    else do
      let len = length legalMoves - 1
      newWeights <- forM [0..len] (\_ -> newSTRef $ (0,0))
      newStates <- forM legalMoves (newSTRef . trd)
      let pairs = zip newStates newWeights
      let z = zip pairs legalMoves -- :: [(pair,triplet)]
      let newSubForest = V.fromList $! [MCT (MCN p c nextState nextWeight False Nothing) V.empty | (pair@(nextState,nextWeight),triplet@(p,c,_))<-z]
      let newZipper = (t{_sForest=newSubForest},ps)
      forM [0..len] (simAndBackprop gen winner newZipper)
      return $! allTheWayBack newZipper

simAndBackprop :: R.StdGen -> Token -> ZipNode s -> Int -> ST s ()
simAndBackprop gen winner zipper n = do --undefined
  let targetZipper@(t,ps) = descendTo zipper n
  state <- readSTRef $!  _currentMatch . _root $ t
  let (_,_,overGame) = simulationUTTT gen (t^.root.lastPlayer, t^.root.lastMove, state)
  let endingStatus = matchStatus overGame
  let diff = if isLeft endingStatus
               then matchOutcome Nothing winner
               else matchOutcome (Just $! fromRight undefined endingStatus) winner
  backprop targetZipper diff

mctsAlgorithm :: R.StdGen -> Int -> Token -> ST s (ZipNode s)
  -> ST s (Token, Maybe Coord, Match Token)
mctsAlgorithm gen iteration winner wZipper
  -- after computationalBudget iterations, return the best children of root
  -- according to ucb
  | iteration >= computationalBudget = do
      (t,ps) <- wZipper
      n <- bestChild gen t
      let child = _root $! (_sForest t ! n)
      m <- readSTRef $! child^.currentMatch
      return (child^.lastPlayer, child^.lastMove, m)
  | otherwise = do
      (t,ps) <- wZipper
      if
        -- leaf of the game tree
        | t^.root.isOver -> do
            m <- readSTRef (_currentMatch . _root $ t)
            let diff = matchOutcome (_winner . _root $ t) winner
            backprop (t,ps) $! diff
            let newZipper = return $! allTheWayBack (t,ps)
            mctsAlgorithm gen (iteration + 1) winner $! newZipper
        -- node whose children have not been visited
        | (V.null $ t^.sForest) -> do
            let newZipper = expand gen winner (t,ps)
            mctsAlgorithm gen (iteration + 1) winner $! newZipper
        -- node whose children's have already been visited
        | otherwise -> do
            n <- bestChild gen t
            let newZipper = return $! (descendTo (t, ps) n)
            mctsAlgorithm gen iteration winner $! newZipper
            -- NB the iteration number is not incremented, as the algorithm
            -- is not getting back to the root

getBestMove :: R.StdGen -> (Token, Maybe Coord, Match Token)
  -> (Token, Maybe Coord, Match Token)
getBestMove gen (lp,lc,lm)
  | matchStatus lm /= Left True = error "No legal moves available"
  | otherwise = runST $ do
      match <- newSTRef lm
      weight <- newSTRef (0,0)
      let gameTree = MCT (MCN lp Nothing match weight False Nothing) V.empty
      let gameZipper = return $ (gameTree, [])
      mctsAlgorithm gen 0 (nextPlayer lp) $! gameZipper