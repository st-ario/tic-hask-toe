{-# LANGUAGE MultiWayIf #-}

module MCTS
( getBestMove
--, simulationUTTT
) where

import Game
import Rules
import TreeZipper

import qualified System.Random as R
import           Control.Monad.ST
import           Data.STRef
import           Control.Lens
import           Debug.Trace (trace)
import           Data.Maybe (isNothing, fromJust)
import           Data.Either (isLeft)
import           Data.Either.Extra (eitherToMaybe)
import           Control.Monad (forM, forM_, liftM, (<$!>))
import           Data.List (elemIndices)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- ############################  Simulation Step  ##############################

simulationUTTT :: R.StdGen -> (Move, Match Token, Status)
  -> (Move, Match Token, Status)
simulationUTTT gen (move,match,status)
  | outcome /= Left True = (move,match,outcome)
  | otherwise = simulationUTTT newGen $! ((!) $! next) $! rnd
    where nextMoves = legalMatchMoves move match
          next = V.map (\(a,b)-> (a,b,Left True)) nextMoves
          len = (length $! nextMoves) - 1
          (rnd,newGen) = (R.randomR $! (0, len)) gen
          outcome = smartMatchStatus (move,match)

-- ###################  Monte Carlo Tree Search Algorithm ######################

-- #### Parameters to Tune:
tieWeight   = 0
winWeight   = 1
lossWeight  = 0
ucbConst    = 1
computationalBudget = 20000 :: Int
-- ########################

matchOutcome :: Maybe Token -> Token -> Double
matchOutcome mt t
  | isNothing mt = tieWeight
  | y == t = winWeight
  | otherwise = lossWeight
  where y = fromJust mt

-- Upper Confidence Bound formula
ucb :: Double -> Int -> Int -> Double
-- UCB1:
ucb v nChild nParent  = (v / nc) + ucbConst * sqrt (2 * (log np) / nc)
-- UCB-Minimal:
-- ucb v nChild nParent  = (v / nc) + ucbConst * (1 / nc)
  where nc = fromIntegral nChild :: Double
        np = fromIntegral nParent :: Double

getWeight :: MCTree s -> STRef s (ValueOfNode,NumberOfVisits)
getWeight = _weight . _root

backprop :: ZipNode s -> Double -> ST s ()
backprop (t,ps) diff = do
  let incr (v,n) =  (v + diff , n + 1)
  modifySTRef' (getWeight t) $! incr
  if length ps == 0
    then return ()
    else (backprop $! stepBack (t,ps)) $! diff

argMaxIndices :: Ord b => (a -> b) -> Vector a -> Vector Int
argMaxIndices f vs = (V.elemIndices $! max) results
  where results = V.map f vs
        max = V.maximum $! results

-- returns the index of a child maximizing ucb
-- if multiple children maximize ucb, pick one randomly
bestChild :: R.StdGen -> MCTree s -> ST s Int
bestChild gen t = do
  parentN <- snd <$!> (readSTRef . getWeight) t
  valuesList <- mapM (readSTRef . getWeight) $! t^.sForest
  --trace ("the weigh of the parent is " ++ show parentN) $ pure ()
  --trace ("the weights of the children are " ++ (show $ fmap snd valuesList)) $ pure ()
  let toMaximize (v,c) = ucb v c $! parentN
  let positions = argMaxIndices toMaximize $! valuesList
  --trace ("ucb is maximized by the ones in position " ++ show positions) $ return ()
  --trace ("the UCBs are " ++ show (fmap toMaximize valuesList)) $ return ()
  let len = (length $! positions) - 1
  if len == 0
    then return (positions ! 0)
    else do let r = fst $! R.randomR (0,len) gen
            --trace ("let's pick " ++ show (positions !! r)) $ return ()
            return (positions ! r)

expand :: R.StdGen -> Token -> ZipNode s -> ST s (ZipNode s)
expand gen toWin (t,ps) = do
  state <- readSTRef $! t^.root.currentMatch
  let status = smartMatchStatus $! (t^.root.lastMove,state)
  -- if an actual leaf of the game tree is reached, update the node with
  -- this information, backpropagate and return root zipper
  if status /= Left True
    then do
      let actualWinner = eitherToMaybe status
      let oldRoot = t^.root
      (newRoot,diff) <- if isLeft status
        then return $! (oldRoot{_isOver=True},tieWeight)
        else return $! (oldRoot{_isOver=True, _winner=actualWinner}
                       ,matchOutcome actualWinner toWin)
      let newZipper = ((MCT $! newRoot) V.empty,ps)
      (backprop $! newZipper) $! diff
      return $! allTheWayBack newZipper
    -- otherwise, update the subforest, simulate and backpropagate once
    -- for every child node, then return root zipper
    else do
      let (legalMoves,legalMatches) = V.unzip $ legalMatchMoves (t^.root.lastMove) state
      let len = length legalMoves
      newWeights <- forM ((V.replicate $ len) ()) (\_ -> newSTRef $ (0,0))
      newStates <- mapM newSTRef legalMatches
      let triplets = V.zip3 legalMoves newStates newWeights
      let newSubForest = V.map (\(m,s,w) -> MCT (MCN m s w False Nothing) V.empty) triplets
      let newZipper = (t{_sForest=newSubForest},ps)
      forM_ [0..(len-1)] (simAndBackprop gen toWin $! newZipper)
      return $! (allTheWayBack $! newZipper)

simAndBackprop :: R.StdGen -> Token -> ZipNode s -> Int -> ST s ()
simAndBackprop gen toWin zipper n = do
  let targetZipper@(t,_) = descendTo zipper n
  state <- readSTRef $! t^.root.currentMatch
  let (_,_,endingStatus) = simulationUTTT gen $! (t^.root.lastMove, state, Left True)
  let diff = if isLeft endingStatus
               then tieWeight
               else matchOutcome (eitherToMaybe endingStatus) toWin
  (backprop $! targetZipper) $! diff

mctsAlgorithm :: R.StdGen -> Int -> Token -> ST s (ZipNode s)
  -> ST s (Move, Match Token)
mctsAlgorithm gen iteration toWin wZipper
  | iteration >= computationalBudget = do
      --tree <- fmap fst wZipper
      tree <- fmap (fst . allTheWayBack) wZipper
      bChild <- (!) <$!> pure (tree^.sForest) <*> (bestChild gen $! tree)
      bChildState <- readSTRef $! bChild^.root.currentMatch
      return $! (bChild^.root.lastMove, bChildState)
  | otherwise = do
      z@(t,_) <- wZipper
      if | t^.root.isOver -> do -- leaf of the game tree
             backprop z $! (matchOutcome $! (t^.root.winner)) toWin
             mctsAlgorithm gen (iteration + 1) toWin $! pure $! allTheWayBack z
         | (V.null $ t^.sForest) -> -- node whose children have not been visited
             mctsAlgorithm gen (iteration + 1) toWin $! expand gen toWin z
         | otherwise -> do -- node whose children's have already been visited
             n <- bestChild gen t
             mctsAlgorithm gen iteration toWin $! pure (descendTo z n)

getBestMove :: R.StdGen -> (Move, Match Token)
  -> (Move, Match Token)
getBestMove gen pair@(move,match)
  | smartMatchStatus pair /= Left True = error "No legal moves available"
  | otherwise = runST $ do
      m <- newSTRef match
      weight <- newSTRef (0,0)
      let gameTree = MCT ((MCN move m $! weight) False Nothing) V.empty
      mctsAlgorithm gen 0 (nextPlayer $ move^.agent) $ pure $! (gameTree, [])