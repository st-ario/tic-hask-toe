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
import           Data.Either (fromRight, fromLeft)
import           Control.Monad (forM, forM_)
import           Data.List.Extras.Argmax
import           Data.List (elemIndices)

-- ############################  Simulation Steps  #############################

-- given an initial state (last player, current board) play an ordinary
-- tic-tac-toe game randomly until the end
simulationTTT :: R.StdGen -> (Token, Grid Token) -> (Token, Grid Token)
simulationTTT gen state
  | isOver (snd state) = state
  | otherwise = simulationTTT newGen (nextMoves !! rnd)
  -- For testing purposes: print all the moves made in the random game
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

type ValueOfNode = Integer
type NumberOfVisits = Integer
type Weight s = Maybe (STRef s (ValueOfNode,NumberOfVisits))

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

-- game tree starting from a given (lastPlayer, board) state
unfoldGame :: (Token, Grid Token) -> ST s (MCTree s)
unfoldGame (lastPlayer, g) = return $ unfoldTree seedGrid $ MCN lastPlayer g Nothing

-- ###################  Monte Carlo Tree Search Algorithm ######################
-- NB: ZipNode (MCNode' s) = (MCTree s, [Position (MCNode' s)])

-- sets the weight of a node to (0,0)
setToZero :: ST s (MCTree s) -> ST s (MCTree s)
setToZero t = do
  (T.Node r sf) <- t
  x <- newSTRef (0,0)
  let newRoot = set weight (Just $! x) r
  -- test whether the bang is useful
  return (T.Node newRoot sf)

{--
-- sets all the children's weight to (0,0)
initialize :: ZipNode (MCNode' s) -> ZipNode (MCNode' s)
initialize pos = replaceSubForestBy setToZero pos
--}

-- Upper Confidence Bound formula
ucb :: Integer -> Integer -> Integer -> Double
ucb value nChild nParent  = (v / nc) + const * sqrt (1 * (log np) / nc)
  where v = fromInteger value :: Double
        nc = fromInteger nChild :: Double
        -- const = 1 :: Double -- run tests to see if there are better choices
        const = 1.4 :: Double -- good value for ordinary TTT
        np = fromInteger nParent :: Double

-- backpropagation algorithm
backprop :: ZipNode (MCNode' s) -> Integer -> ST s ()
backprop (t,ps) diff = do
  let incr (v,n) =  (v + diff , n + 1)
  let x = fromJust . _weight . T.rootLabel $ t
  -- x :: STRef (Integer, Integer)
  modifySTRef' x incr
  y <- readSTRef x
  trace ("At depth = " ++ show (depth (t,ps))) $ return ()
  trace ("The current weight is " ++ show y) $ return ()
  if length ps == 0
    then trace ("reached root, let's get going") $ return ()
    else trace ("let's update depth " ++ show (depth (stepBack(t,ps))) ++ " with weight " ++ show diff) $ backprop (stepBack (t,ps)) diff

-- returns the index of a child maximizing ucb
-- if multiple children maximize ucb, it picks one randomly
bestChild :: R.StdGen -> MCTree s -> ST s Int
bestChild gen t = do
  parentWeight <- readSTRef $ (fromJust . _weight . T.rootLabel $ t)
  -- parentWeight :: (ValueOfNode,NumberOfVisits) = (Integer, Integer)
  let parentN = snd parentWeight
  let refValues = [x | x <- fmap (fromJust . _weight . T.rootLabel) (T.subForest t)]
  -- refValues :: [STRef s (Integer,Integer)]
  valuesList <- forM refValues readSTRef
  -- valuesList :: [(Integer, Integer)]
  trace ("the weigh of the parent is " ++ show parentN) $ pure ()
  trace ("the weights of the children are " ++ show valuesList) $ pure ()
  let toMaximize (v,c) = ucb v c parentN
  let bestWeight = argmax toMaximize valuesList
  let positions = elemIndices bestWeight valuesList
  trace ("ucb is maximized by the ones in position " ++ show positions) $ return ()
  trace ("the UCBs are " ++ show (fmap toMaximize valuesList)) $ return ()
  let len = length positions - 1
  let r = fst $ R.randomR (0,len) gen
  trace ("let's pick " ++ show (positions !! r)) $ return ()
  return (positions !! r)

expand :: R.StdGen -> Token -> ZipNode (MCNode' s) -> ST s (MCTree s)
expand gen winner (t,ps) = do
  trace ("first time I think of" ++ show (_currentGrid . T.rootLabel $ t)) $ return ()
  newTree <- setToZero $ return t
  let state = T.rootLabel newTree
  let result = simulationTTT gen (_lastPlayer state, _currentGrid state)
  trace ("I want " ++ show winner ++ " to win") $ return ()
  trace ("from this position I could get here" ++ show result) $ pure ()
  let diff = gridOutcome winner (snd result)
  trace ("the feedback for this position is " ++ show diff) $ return ()
  backprop (newTree, ps) diff
  pair <-readSTRef . fromJust . _weight . T.rootLabel $ newTree
  trace ("the situation at depth " ++ show (depth (t,ps)) ++ " is now" ++ show (_currentGrid . T.rootLabel $ newTree) ++ "with weight " ++ show pair) $ return ()
  return newTree

mctsAlgorithm :: R.StdGen -> Integer -> Token -> ST s (ZipNode (MCNode' s)) -> ST s (Token, Grid Token)
mctsAlgorithm gen iteration winner wZipper --(t, ps)
  -- after computationalBudget iterations, return the best children of root
  -- according to ucb
  | iteration >= computationalBudget = do
      (t,ps) <- wZipper
      n <- bestChild gen t
      let child = T.rootLabel (T.subForest t !! n)
      return (_lastPlayer child, _currentGrid child)
  | otherwise = do
      (t,ps) <- wZipper
      if
        -- when reaching a leaf of the game tree, backpropagate and restart from root
        | length (T.subForest t) == 0 -> do
            trace ("I've been here before") $ return ()
            --trace (show $ _currentGrid . T.rootLabel $ t) $ return ()
            let diff = gridOutcome winner (_currentGrid . T.rootLabel $ t)
            trace ("the outcome is" ++ show diff) $ return ()
            backprop (t,ps) diff
            let newZipper = return $ allTheWayBack (t,ps)
            debugWeight <- (readSTRef. fromJust . _weight . T.rootLabel . fst $ allTheWayBack (t,ps))
            trace ("the weight of the root is now " ++ show debugWeight) $ return ()
            trace ("and the iteration is " ++ show iteration) $ return ()
            mctsAlgorithm gen (iteration + 1) winner newZipper
        -- when reaching a node whose children's weights are not initialized,
        -- initialize all children, run simulations and backpropagate once for
        -- each of them, then restart from root
        | isNothing $ _weight $ T.rootLabel (T.subForest t !! 0) -> do
            trace ("What do I do now?") $ return ()
            -- trace (show $ _currentGrid . T.rootLabel $ t) $ return ()
            let n = length (T.subForest t) -1
            let f x = expand gen winner (descendTo (t,ps) x)
            newSubForest <- forM [0..n] f
            let newTree = T.Node {T.rootLabel=(T.rootLabel t) , T.subForest=newSubForest}
            pair <- readSTRef . fromJust . _weight . T.rootLabel $ newTree
            trace ("The new node ad depth " ++ show (depth (t,ps)) ++ " is" ++ show (_currentGrid . T.rootLabel $ newTree) ++ "with weight " ++ show pair) $ return ()
            trace ("Its children are" ++ show (fmap (_currentGrid . T.rootLabel) newSubForest)) $ return ()
            debugValues <- forM (fmap (fromJust . _weight . T.rootLabel) newSubForest) readSTRef
            trace ("with weights" ++ show debugValues) $ return ()
            let newZipper = return $ (allTheWayBack (newTree,ps))
            mctsAlgorithm gen (iteration + 1) winner newZipper
        -- when reaching a node whose children's weights have already been
        -- initialized, pick the best children according to ucb and restart from it
        | otherwise -> do
            trace ("I know this position") $ return ()
            n <- bestChild gen t
            trace ("I like this move") $ return ()
            trace (show $ _currentGrid . T.rootLabel $ (T.subForest t !! n)) $ return ()
            let newZipper = return $ (descendTo (t, ps) n)
            mctsAlgorithm gen iteration winner newZipper
            -- NB the iteration number is not incremented, as the algorithm
            -- is not getting back to the root
    where computationalBudget = 10^4 :: Integer

getBestMove :: R.StdGen -> (Token, Grid Token) -> (Token, Grid Token)
getBestMove gen (lastPlayer, g) =
  if isOver g
    then error "No legal moves available"
    else runST $ do
      gameTree <- setToZero $ unfoldGame (lastPlayer, g)
      let gameZipper = return $ (gameTree, [])
      -- gameZipper :: ST s (ZipNode (MCNode' s))
      mctsAlgorithm gen 0 (nextPlayer lastPlayer) gameZipper