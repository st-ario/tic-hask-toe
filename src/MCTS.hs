{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MCTS
( getBestMove
--, simulationUTTT
) where

import Game
import Rules
import TreeZipper

import qualified System.Random as R
import           System.IO
import           Data.IORef
import           Control.Lens
-- import           Debug.Trace (trace)
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

-- ############################  Simulation Step  ##############################

simulationUTTT :: R.StdGen -> (Move, Match Token, Status)
  -> (Move, Match Token, Status)
simulationUTTT gen (move,match,status)
  | outcome /= Ongoing = (move,match,outcome)
  | otherwise = simulationUTTT newGen $! ((!) $! next) $! rnd
    where nextMoves = legalMatchMoves move match
          next = V.map (\(a,b)-> (a,b,Ongoing)) nextMoves
          len = (length $! nextMoves) - 1
          (rnd,newGen) = (R.uniformR $! (0 :: Int, len :: Int)) gen
          outcome = matchStatus (move,match)

-- ###################  Monte Carlo Tree Search Algorithm ######################

newtype Outcome = Outcome Int deriving (Eq, Enum)
(loss:tie:win:_) = [Outcome (-1) ..]

computationalBudget = 20 :: Int -- sec * 10^-1

-- Upper Confidence Bound formula
-- UCB1:
ucb :: Double -> Int -> Int -> Int -> Int -> Double
ucb cons wins losses visits parentVisits  =
  mean + cons * sqrt (2 * (log nP) / n)
  where w = fromIntegral wins
        l = fromIntegral losses
        n = fromIntegral visits
        nP = fromIntegral parentVisits
        mean = (w - l) / n

{--
-- UCB1-Tuned
ucb :: Double -> Int -> Int -> Int -> Int -> Double
ucb _ wins losses 1 parentVisits =
  mean + sqrt (minimum [0.25, auxValue] * (log nP))
  where w = fromIntegral wins
        l = fromIntegral losses
        nP = fromIntegral parentVisits
        mean = (w - l)
        unbiasedVariance = (w + l - (w - l)^2)
        auxValue = unbiasedVariance +  sqrt(2* (log nP))
ucb _ wins losses visits parentVisits =
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

incr :: Outcome -> (Int,Int,Int) -> (Int,Int,Int)
incr ending (w,l,n)
  | ending == tie  = (w  ,l  ,n+1)
  | ending == win  = (w+1,l  ,n+1)
  | ending == loss = (w  ,l+1,n+1)
  | otherwise = undefined

backprop :: ZipNode -> Outcome -> IO ()
backprop (t,ps) ending = do
  modifyIORef' (getWeight t) $! (incr ending)
  if | null ps -> return ()
     | ending == win  -> (backprop $! destructiveStepBack (t,ps)) $! loss
     | ending == loss -> (backprop $! destructiveStepBack (t,ps)) $! win
     | ending == tie  -> (backprop $! destructiveStepBack (t,ps)) $! tie

argMaxIndices :: Ord b => (a -> b) -> Vector a -> Vector Int
argMaxIndices f vs = (V.elemIndices $! max) results
  where results = V.map f vs
        max = V.maximum $! results

-- returns the index of a child maximizing ucb
-- if multiple children maximize ucb, pick one randomly
bestChild :: R.StdGen -> Double -> MCTree -> IO Int
bestChild gen cons t = do
  parentN <- trd <$!> (readIORef . getWeight) t
  valuesList <- mapM (readIORef . getWeight) $! t^.sForest
  --trace ("the weigh of the parent is " ++ show parentN) $ pure ()
  --trace ("the weights of the children are " ++ (show $ valuesList)) $ pure ()
  let toMaximize (wins,losses,visits) = ucb cons wins losses visits $! parentN
  let positions = argMaxIndices toMaximize $! valuesList
  --trace ("ucb is maximized by the ones in position " ++ show positions) $ return ()
  --trace ("the UCBs are " ++ show (fmap toMaximize valuesList)) $ return ()
  let len = (length $! positions) - 1
  if len == 0
    then return (positions ! 0)
    else do let r = fst $! R.uniformR (0 :: Int,len :: Int) gen
            --trace ("let's pick " ++ show (positions ! r)) $ return ()
            return (positions ! r)

expand :: R.StdGen -> Token -> ZipNode -> IO (ZipNode)
expand gen toWin (t,ps) = do
  let (lM, state) = (t^.root.lastMove, t^.root.currentMatch)
  let status = matchStatus $! (lM,state)
  -- if an actual leaf of the game tree is reached, update the node with
  -- this information, backpropagate and return root zipper
  if status /= Ongoing 
    then do
      let actualWinner = eitherToMaybe status
      let oldRoot = t^.root
      (newRoot,ending) <- if isLeft status
        then return $! (oldRoot{_isOver=True},tie)
        else return $! (oldRoot{_isOver=True, _isWon=True},win)
      let newZipper = ((MCT $! newRoot) V.empty,ps)
      (backprop $! newZipper) $! ending
      return $! allTheWayBack newZipper
    -- otherwise, update the subforest, simulate and backpropagate once
    -- for every child node, then return root zipper
    else do
      let (legalMoves,legalMatches) = V.unzip $! legalMatchMoves lM state
      let len = length $! legalMoves
      newWeights <- forM ((V.replicate $ len) ()) (\_ -> newIORef $ (0,0,0))
      let triplets = ((V.zip3 $! legalMoves) $! legalMatches) $! newWeights
      let newSubForest = V.map (\(m,s,w) -> MCT (MCN m s w False False) V.empty) $! triplets
      let newZipper = (t{_sForest=newSubForest},ps)
      {--
      -- changing gen every time
      genList <- forM [0..(len-1)] (\_ -> R.newStdGen)
      let auxList = zip genList [0..(len-1)]
      let auxF (g,n) = (simAndBackprop g toWin $! newZipper) n
      forM auxList auxF
      --}
      forM_ [0..(len-1)] (simAndBackprop gen toWin $! newZipper)
      return $! (allTheWayBack $! newZipper)

simAndBackprop :: R.StdGen -> Token -> ZipNode -> Int -> IO ()
simAndBackprop gen toWin zipper n = do
  let targetZipper@(t,_) = destructiveDescendTo zipper n
  let (state, lastAgent) = (t^.root.currentMatch, t^.root.lastMove.agent)
  newGen <- R.newStdGen
  let (_,_,endingStatus) = simulationUTTT newGen $! (t^.root.lastMove, state, Ongoing)
  let ending = if | endingStatus == Tie -> tie
                  | endingStatus == WonBy lastAgent -> win
                  | otherwise -> loss
  (backprop $! targetZipper) $! ending

mctsAlgorithm :: R.StdGen -> Double -> Token -> IO (ZipNode) -> IO ()
mctsAlgorithm gen cons toWin wZipper = do
    z@(t,_) <- wZipper
    if | t^.root.isOver -> do -- leaf of the game tree
           if t^.root.isWon
             then backprop z win
             else backprop z tie
           mctsAlgorithm gen cons toWin $! pure $! allTheWayBack z
       | (V.null $ t^.sForest) -> do -- node whose children have not been visited
           let (g,g') = R.split gen
           mctsAlgorithm g cons toWin $! expand g' toWin z
       | otherwise -> do -- node whose children's have already been visited
           n <- bestChild gen cons t
           mctsAlgorithm gen cons toWin $! pure (descendTo z n)

getBestMove :: R.StdGen -> Double -> (Move, Match Token)
  -> IO (Move, Match Token)
getBestMove gen cons pair@(move,match)
  | matchStatus pair /= Ongoing = error "No legal moves available"
  | otherwise = do
      weight <- newIORef (0,0,0)
      let gameTree = MCT ((MCN move match $! weight) False False) V.empty
      let toWin = (nextPlayer $ move^.agent)
      let (g,g') = R.split gen
      wZipper@(finalTree,_) <- expand g toWin $! (gameTree,[])
      tid <- C.forkIO $ mctsAlgorithm g' cons toWin $! pure $ wZipper
      C.threadDelay (computationalBudget * 10^5)
      C.killThread tid
      bChild <- (!) <$!> pure (finalTree^.sForest) <*> (bestChild gen cons $! finalTree)
      let bChildState = bChild^.root.currentMatch
      sims <- readIORef (_weight . _root $ finalTree)
      -- trace ("Simulations: " ++ show sims) $ return ()
      return $! (bChild^.root.lastMove, bChildState)