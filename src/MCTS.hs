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
--import           Debug.Trace (trace)
import           Data.Maybe (isNothing, fromJust)
import           Data.Either (isLeft)
import           Data.Either.Extra (eitherToMaybe)
import           Control.Monad (forM, forM_, (<$!>))
import           Data.List (elemIndices)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Control.Concurrent as C

trd :: (a,b,c) -> c
trd (_,_,c) = c

-- TODO
-- use only parametric version, and default it to one using ucbConst

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

-- ###### Constants ######
-- these three values MUST be different, otherwise backprop breaks
-- TODO change to int, use same trick of tokens
tieWeight   = 0        :: Double
winWeight   = 1        :: Double
lossWeight  = -1       :: Double
-- #######################

computationalBudget = 20 :: Int -- sec * 10^-1

-- Upper Confidence Bound formula
-- UCB1:
ucbConst    = 1
ucb :: Int -> Int -> Int -> Int -> Double
ucb wins losses visits parentVisits  =
  mean + ucbConst * sqrt (2 * (log nP) / n)
  where w = fromIntegral wins
        l = fromIntegral losses
        n = fromIntegral visits
        nP = fromIntegral parentVisits
        mean = (w - l) / n

{--
-- UCB1-Tuned
ucb :: Int -> Int -> Int -> Int -> Double
ucb wins losses 1 parentVisits =
  mean + sqrt (minimum [0.25, auxValue] * (log nP))
  where w = fromIntegral wins
        l = fromIntegral losses
        nP = fromIntegral parentVisits
        mean = (w - l)
        unbiasedVariance = (w + l - (w - l)^2)
        auxValue = unbiasedVariance +  sqrt(2* (log nP))
ucb wins losses visits parentVisits =
  mean + sqrt (minimum [0.25, auxValue] * (log nP) / n)
  where w = fromIntegral wins
        l = fromIntegral losses
        n = fromIntegral visits
        nP = fromIntegral parentVisits
        mean = (w - l) / n
        unbiasedVariance = (w + l - ((w - l)^2)/n) / (n-1)
        auxValue = unbiasedVariance +  sqrt(2* (log nP) / n)
--}

getWeight :: MCTree -> IORef (NumberOfWins,NumberOfLosses,NumberOfVisits)
getWeight = _weight . _root

incr :: Double -> (Int,Int,Int) -> (Int,Int,Int)
incr diff (w,l,n)
  | diff == tieWeight  = (w  ,l  ,n+1)
  | diff == winWeight  = (w+1,l  ,n+1)
  | diff == lossWeight = (w  ,l+1,n+1)
  | otherwise = undefined

backprop :: ZipNode -> Double -> IO ()
backprop (t,ps) diff = do
  modifyIORef' (getWeight t) $! (incr diff)
  if | null ps -> return ()
     | diff == winWeight  -> (backprop $! stepBack (t,ps)) $! lossWeight
     | diff == lossWeight -> (backprop $! stepBack (t,ps)) $! winWeight
     | diff == tieWeight  -> (backprop $! stepBack (t,ps)) $! tieWeight

argMaxIndices :: Ord b => (a -> b) -> Vector a -> Vector Int
argMaxIndices f vs = (V.elemIndices $! max) results
  where results = V.map f vs
        max = V.maximum $! results

-- returns the index of a child maximizing ucb
-- if multiple children maximize ucb, pick one randomly
bestChild :: R.StdGen -> MCTree -> IO Int
bestChild gen t = do
  parentN <- trd <$!> (readIORef . getWeight) t
  valuesList <- mapM (readIORef . getWeight) $! t^.sForest
  --trace ("the weigh of the parent is " ++ show parentN) $ pure ()
  --trace ("the weights of the children are " ++ (show $ valuesList)) $ pure ()
  let toMaximize (wins,losses,visits) = ucb wins losses visits $! parentN
  let positions = argMaxIndices toMaximize $! valuesList
  --trace ("ucb is maximized by the ones in position " ++ show positions) $ return ()
  --trace ("the UCBs are " ++ show (fmap toMaximize valuesList)) $ return ()
  let len = (length $! positions) - 1
  if len == 0
    then return (positions ! 0)
    else do let r = fst $! R.randomR (0,len) gen
            --trace ("let's pick " ++ show (positions ! r)) $ return ()
            return (positions ! r)

expand :: R.StdGen -> Token -> ZipNode -> IO (ZipNode)
expand gen toWin (t,ps) = do
  let (lM, state) = (t^.root.lastMove, t^.root.currentMatch)
  let status = smartMatchStatus $! (lM,state)
  -- if an actual leaf of the game tree is reached, update the node with
  -- this information, backpropagate and return root zipper
  if status /= Left True
    then do
      let actualWinner = eitherToMaybe status
      let oldRoot = t^.root
      (newRoot,diff) <- if isLeft status
        then return $! (oldRoot{_isOver=True},tieWeight)
        else return $! (oldRoot{_isOver=True, _winner=actualWinner},winWeight)
      --let (newRoot,diff) = if | actualWinner == Nothing ->
      --                          (oldRoot{_isOver=True},tieWeight)
      --                        | actualWinner == Just toWin ->
      --                          (oldRoot{_isOver=True, _winner=actualWinner},winWeight)
      --                        | otherwise ->
      --                          (oldRoot{_isOver=True, _winner=actualWinner},lossWeight)
      -- ###### the above is wrong, but double check the weight
      -- ###### backprop should always use the winning weight for an actual leaf
      let newZipper = ((MCT $! newRoot) V.empty,ps)
      (backprop $! newZipper) $! diff
      return $! allTheWayBack newZipper
    -- otherwise, update the subforest, simulate and backpropagate once
    -- for every child node, then return root zipper
    else do
      let (legalMoves,legalMatches) = V.unzip $! legalMatchMoves lM state
      let len = length $! legalMoves
      newWeights <- forM ((V.replicate $ len) ()) (\_ -> newIORef $ (0,0,0))
      let triplets = ((V.zip3 $! legalMoves) $! legalMatches) $! newWeights
      let newSubForest = V.map (\(m,s,w) -> MCT (MCN m s w False Nothing) V.empty) $! triplets
      let newZipper = (t{_sForest=newSubForest},ps)
      -- TODO should change gen every time
      forM_ [0..(len-1)] (simAndBackprop gen toWin $! newZipper)
      return $! (allTheWayBack $! newZipper)

simAndBackprop :: R.StdGen -> Token -> ZipNode -> Int -> IO ()
simAndBackprop gen toWin zipper n = do
  let targetZipper@(t,_) = descendTo zipper n
  let (state, lastAgent) = (t^.root.currentMatch, t^.root.lastMove.agent)
  let (_,_,endingStatus) = simulationUTTT (snd . R.next $ gen) $! (t^.root.lastMove, state, Left True)
  let diff = if | endingStatus == Left False -> tieWeight
                | endingStatus == Right lastAgent -> winWeight
                | otherwise -> lossWeight
  (backprop $! targetZipper) $! diff

mctsAlgorithm :: R.StdGen -> Token -> IO (ZipNode) -> IO ()
mctsAlgorithm gen toWin wZipper = do
    z@(t,_) <- wZipper
    if | t^.root.isOver -> do -- leaf of the game tree
           let leafWinner = t^.root.winner
           if | leafWinner == Nothing      -> backprop z tieWeight
              | otherwise                  -> backprop z winWeight
           mctsAlgorithm gen toWin $! pure $! allTheWayBack z
       | (V.null $ t^.sForest) -> do -- node whose children have not been visited
           let (g,g') = R.split gen
           mctsAlgorithm g toWin $! expand g' toWin z
       | otherwise -> do -- node whose children's have already been visited
           n <- bestChild gen t
           mctsAlgorithm gen toWin $! pure $! (descendTo z n)

getBestMove :: R.StdGen -> (Move, Match Token)
  -> IO (Move, Match Token)
getBestMove gen pair@(move,match)
  | smartMatchStatus pair /= Left True = error "No legal moves available"
  | otherwise = do
      weight <- newIORef (0,0,0)
      let gameTree = MCT ((MCN move match $! weight) False Nothing) V.empty
      let toWin = (nextPlayer $ move^.agent)
      let (g,g') = R.split gen
      wZipper@(finalTree,_) <- expand g toWin $! (gameTree,[])
      tid <- C.forkIO $ mctsAlgorithm g' toWin $! pure $ wZipper
      C.threadDelay (computationalBudget * 10^5)
      C.killThread tid
      bChild <- (!) <$!> pure (finalTree^.sForest) <*> (bestChild gen $! finalTree)
      let bChildState = bChild^.root.currentMatch
      sims <- readIORef (_weight . _root $ finalTree)
      --trace ("Simulations: " ++ show sims) $ return ()
      return $! (bChild^.root.lastMove, bChildState)

-- Parametric variants, for parameter tuning (the constant in UCB is an argument)

getBestMoveParam :: R.StdGen -> Double -> (Move, Match Token)
  -> IO (Move, Match Token)
getBestMoveParam gen cons pair@(move,match)
  | smartMatchStatus pair /= Left True = error "No legal moves available"
  | otherwise = do
      weight <- newIORef (0,0,0)
      let gameTree = MCT ((MCN move match $! weight) False Nothing) V.empty
      let toWin = (nextPlayer $ move^.agent)
      let (g,g') = R.split gen
      wZipper@(finalTree,_) <- expand g toWin $! (gameTree,[])
      tid <- C.forkIO $ mctsAlgorithmParam g' cons toWin $! pure $ wZipper
      C.threadDelay (computationalBudget * 10^5)
      C.killThread tid
      bChild <- (!) <$!> pure (finalTree^.sForest) <*> (bestChildParam gen cons $! finalTree)
      let bChildState = bChild^.root.currentMatch
      sims <- readIORef (_weight . _root $ finalTree)
      --trace ("Simulations: " ++ show sims) $ return ()
      return $! (bChild^.root.lastMove, bChildState)

mctsAlgorithmParam :: R.StdGen -> Double -> Token -> IO (ZipNode) -> IO ()
mctsAlgorithmParam gen cons toWin wZipper = do
    z@(t,_) <- wZipper
    if | t^.root.isOver -> do -- leaf of the game tree
           let leafWinner = t^.root.winner
           -- maybe write function instead of nesting an if
           if | leafWinner == Nothing      -> backprop z tieWeight
              | otherwise                  -> backprop z winWeight
           mctsAlgorithmParam gen cons toWin $! pure $! allTheWayBack z
       | (V.null $ t^.sForest) -> do -- node whose children have not been visited
           let (g,g') = R.split gen
           mctsAlgorithmParam g cons toWin $! expand g' toWin z
       | otherwise -> do -- node whose children's have already been visited
           n <- bestChildParam gen cons t
           mctsAlgorithmParam gen cons toWin $! pure (descendTo z n)

bestChildParam :: R.StdGen -> Double -> MCTree -> IO Int
bestChildParam gen cons t = do
  parentN <- trd <$!> (readIORef . getWeight) t
  valuesList <- mapM (readIORef . getWeight) $! t^.sForest
  let toMaximize (wins,losses,visits) = ucbParam cons wins losses visits $! parentN
  let positions = argMaxIndices toMaximize $! valuesList
  let len = (length $! positions) - 1
  if len == 0
    then return (positions ! 0)
    else do let r = fst $! R.randomR (0,len) gen
            return (positions ! r)

ucbParam :: Double -> Int -> Int -> Int -> Int -> Double
ucbParam cons wins losses visits parentVisits  =
  mean + cons * sqrt (2 * (log nP) / n)
  where w = fromIntegral wins
        l = fromIntegral losses
        n = fromIntegral visits
        nP = fromIntegral parentVisits
        mean = (w - l) / n