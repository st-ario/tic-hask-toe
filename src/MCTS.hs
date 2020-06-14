{-# LANGUAGE MultiWayIf #-}

module MCTS
( getBestMove
--, simulationUTTT
, getBestMoveParam
) where

import Game
import Rules
import TreeZipper

import qualified System.Random as R
import           System.IO
import           Data.IORef
import           Control.Lens
import           Debug.Trace (trace)
import           Data.Maybe (isNothing, fromJust)
import           Data.Either (isLeft)
import           Data.Either.Extra (eitherToMaybe)
import           Control.Monad (forM, forM_, (<$!>))
import           Data.List (elemIndices)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Control.Concurrent as C

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
lossWeight  = -1
ucbConst    = 1.73264
computationalBudget = 25 :: Int -- sec * 10^-1
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
ucb v nChild nParent  = (v / nc) + ucbConst * sqrt (2* (log np) / nc)
-- UCB-Minimal:
-- ucb v nChild _  = (v / nc) + ucbConst * (1 / nc)
  where nc = fromIntegral nChild :: Double
        np = fromIntegral nParent :: Double

getWeight :: MCTree -> IORef (ValueOfNode,NumberOfVisits)
getWeight = _weight . _root

backprop :: ZipNode -> Double -> IO ()
backprop (t,ps) diff = do
  let incr (v,n) =  (v + diff , n + 1)
  modifyIORef' (getWeight t) $! incr
  if length ps == 0
    then return ()
    else (backprop $! stepBack (t,ps)) $! diff

argMaxIndices :: Ord b => (a -> b) -> Vector a -> Vector Int
argMaxIndices f vs = (V.elemIndices $! max) results
  where results = V.map f vs
        max = V.maximum $! results

-- returns the index of a child maximizing ucb
-- if multiple children maximize ucb, pick one randomly
bestChild :: R.StdGen -> MCTree -> IO Int
bestChild gen t = do
  parentN <- snd <$!> (readIORef . getWeight) t
  valuesList <- mapM (readIORef . getWeight) $! t^.sForest
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
            -- trace ("let's pick " ++ show (positions !! r)) $ return ()
            return (positions ! r)

expand :: R.StdGen -> Token -> ZipNode -> IO (ZipNode)
expand gen toWin (t,ps) = do
  state <- readIORef $! t^.root.currentMatch
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
      newWeights <- forM ((V.replicate $ len) ()) (\_ -> newIORef $ (0,0))
      newStates <- mapM newIORef legalMatches
      let triplets = V.zip3 legalMoves newStates newWeights
      let newSubForest = V.map (\(m,s,w) -> MCT (MCN m s w False Nothing) V.empty) triplets
      let newZipper = (t{_sForest=newSubForest},ps)
      forM_ [0..(len-1)] (simAndBackprop gen toWin $! newZipper)
      return $! (allTheWayBack $! newZipper)

simAndBackprop :: R.StdGen -> Token -> ZipNode -> Int -> IO ()
simAndBackprop gen toWin zipper n = do
  let targetZipper@(t,_) = descendTo zipper n
  state <- readIORef $! t^.root.currentMatch
  let (_,_,endingStatus) = simulationUTTT (snd . R.next $ gen) $! (t^.root.lastMove, state, Left True)
  let diff = if isLeft endingStatus
               then tieWeight
               else matchOutcome (eitherToMaybe endingStatus) toWin
  (backprop $! targetZipper) $! diff

mctsAlgorithm :: R.StdGen -> Token -> IO (ZipNode) -> IO ()
mctsAlgorithm gen toWin wZipper = do
    z@(t,_) <- wZipper
    if | t^.root.isOver -> do -- leaf of the game tree
           backprop z $! (matchOutcome $! (t^.root.winner)) toWin
           mctsAlgorithm gen toWin $! pure $! allTheWayBack z
       | (V.null $ t^.sForest) -> do -- node whose children have not been visited
           let (g,g') = R.split gen
           mctsAlgorithm g toWin $! expand g' toWin z
       | otherwise -> do -- node whose children's have already been visited
           n <- bestChild gen t
           mctsAlgorithm gen toWin $! pure (descendTo z n)

getBestMove :: R.StdGen -> (Move, Match Token)
  -> IO (Move, Match Token)
getBestMove gen pair@(move,match)
  | smartMatchStatus pair /= Left True = error "No legal moves available"
  | otherwise = do
      m <- newIORef match
      weight <- newIORef (0,0)
      let gameTree = MCT ((MCN move m $! weight) False Nothing) V.empty
      let toWin = (nextPlayer $ move^.agent)
      let (g,g') = R.split gen
      wZipper@(finalTree,_) <- expand g toWin $! (gameTree,[])
      tid <- C.forkIO $ mctsAlgorithm g' toWin $! pure $ wZipper
      C.threadDelay (computationalBudget * 10^5)
      C.killThread tid
      bChild <- (!) <$!> pure (finalTree^.sForest) <*> (bestChild gen $! finalTree)
      bChildState <- readIORef $! bChild^.root.currentMatch
      sims <- readIORef (_weight . _root $ finalTree)
      trace ("Simulations: " ++ show sims) $ return ()
      return $! (bChild^.root.lastMove, bChildState)

-- Parametric variants, for parameter tuning (the constant in UCB is an argument)

getBestMoveParam :: R.StdGen -> Double -> (Move, Match Token)
  -> IO (Move, Match Token)
getBestMoveParam gen cons pair@(move,match)
  | smartMatchStatus pair /= Left True = error "No legal moves available"
  | otherwise = do
      m <- newIORef match
      weight <- newIORef (0,0)
      let gameTree = MCT ((MCN move m $! weight) False Nothing) V.empty
      let toWin = (nextPlayer $ move^.agent)
      let (g,g') = R.split gen
      wZipper@(finalTree,_) <- expand g toWin $! (gameTree,[])
      tid <- C.forkIO $ mctsAlgorithmParam g' cons toWin $! pure $ wZipper
      C.threadDelay (computationalBudget * 10^5)
      C.killThread tid
      bChild <- (!) <$!> pure (finalTree^.sForest) <*> (bestChildParam gen cons $! finalTree)
      bChildState <- readIORef $! bChild^.root.currentMatch
      sims <- readIORef (_weight . _root $ finalTree)
      --trace ("Simulations: " ++ show sims) $ return ()
      return $! (bChild^.root.lastMove, bChildState)

mctsAlgorithmParam :: R.StdGen -> Double -> Token -> IO (ZipNode) -> IO ()
mctsAlgorithmParam gen cons toWin wZipper = do
    z@(t,_) <- wZipper
    if | t^.root.isOver -> do -- leaf of the game tree
           backprop z $! (matchOutcome $! (t^.root.winner)) toWin
           mctsAlgorithmParam gen cons toWin $! pure $! allTheWayBack z
       | (V.null $ t^.sForest) -> do -- node whose children have not been visited
           let (g,g') = R.split gen
           mctsAlgorithmParam g cons toWin $! expand g' toWin z
       | otherwise -> do -- node whose children's have already been visited
           n <- bestChildParam gen cons t
           mctsAlgorithmParam gen cons toWin $! pure (descendTo z n)

bestChildParam :: R.StdGen -> Double -> MCTree -> IO Int
bestChildParam gen cons t = do
  parentN <- snd <$!> (readIORef . getWeight) t
  valuesList <- mapM (readIORef . getWeight) $! t^.sForest
  let toMaximize (v,c) = ucbParam cons v c $! parentN
  let positions = argMaxIndices toMaximize $! valuesList
  let len = (length $! positions) - 1
  if len == 0
    then return (positions ! 0)
    else do let r = fst $! R.randomR (0,len) gen
            return (positions ! r)

ucbParam :: Double -> Double -> Int -> Int -> Double
ucbParam c v nChild nParent  = (v / nc) + c * sqrt (2 * (log np) / nc)
  where nc = fromIntegral nChild :: Double
        np = fromIntegral nParent :: Double