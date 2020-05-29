{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Game where

import           Data.Either (isRight,fromRight)
import           Data.Maybe (fromJust, isNothing)
import           Data.Vector (Vector,(!),cons,empty,singleton)
import qualified Data.Vector as V

-- the quality of a token being empty, or a game being in progress is
-- different from being resp. X/Y or won/tied
class Winnable a where
  isOver :: a -> Bool
  isWon  :: a -> Bool

-- tokens are either empty, X or O
data Token = EM | X | O deriving Eq

-- a slot is considered won when either player occupied it
-- an empty slot is a "game in progress"
instance Winnable Token where
  isOver EM = False
  isOver X  = True
  isOver O  = True
  isWon  = isOver

-- grids are just lists, the type constructor ensures their length is 9
newtype Grid w = Grid { toList :: Vector w } deriving Eq

toGrid :: Vector w -> Grid w
toGrid a = Grid a
{--
  if V.length a == 9
    then Grid a
    else error "A Grid must have 9 elements"
--}

instance Functor Grid where
  -- fmap f grid = toGrid $ fmap f $ toList grid
  fmap f (Grid a) = Grid (V.map f a)

-- The conditions for a Match to be won are different from the ones of Grid,
-- and it also needs a different instance of Show
newtype Match w = Match { getGrids :: Grid (Grid w) } deriving Eq

toMatch :: Grid (Grid w) -> Match w
toMatch m = Match m

hasWinningTriplets :: (Eq w, Winnable w) => Grid w -> Bool
hasWinningTriplets (Grid a) =
  let r = (a!0)
      s = (a!1)
      t = (a!2)
      u = (a!3)
      v = (a!4)
      w = (a!5)
      x = (a!6)
      y = (a!7)
      z = (a!8)
      cv = isWon v
      cx = isWon x
      ct = isWon t
  -- rows 
  in (ct && r == s && r == t) ||
     (cv && u == v && u == w) ||
     (cx && x == y && x == z) ||
     -- columns
     (cx && r == u && r == x) ||
     (cv && s == v && s == y) ||
     (ct && t == w && t == z) ||
     -- diagonals
     (cv && r == v && r == z) ||
     (cv && t == v && t == x)

getWinner :: Grid Token -> Token
getWinner (Grid a) =
  let r = (a!0)
      s = (a!1)
      t = (a!2)
      u = (a!3)
      v = (a!4)
      w = (a!5)
      x = (a!6)
      y = (a!7)
      z = (a!8)
      cv = isWon v
      cx = isWon x
      ct = isWon t
  in if
        -- rows
        | (ct && r == s && r == t) -> r
        | (cv && u == v && u == w) -> u
        | (cx && x == y && x == z) -> x
        -- columns             
        | (cx && r == u && r == x) -> r
        | (cv && s == v && s == y) -> s
        | (ct && t == w && t == z) -> t
        -- diagonals               
        | (cv && r == v && r == z) -> r
        | (cv && t == v && t == x) -> t
        | otherwise -> EM

-- grids are won when there's a winning triplet
-- grids are over when a player wins or when the grid is full
instance (Eq w, Winnable w) => Winnable (Grid w) where
  isWon grid = hasWinningTriplets grid
  isOver (Grid a) = V.all isOver a || isWon (Grid a)

-- determine the winner for a single Grid, otherwise give Left False if the
-- game is tied and Left True if it is still ongoing
--
-- works properly only on legal positions (at most one winner)
-- ugly but performant
gridStatus :: Grid Token -> Either Bool Token
gridStatus grid@(Grid a)
  | isWon grid = Right $! getWinner grid
  | V.all isOver a = Left False -- tied game
  | otherwise = Left True -- ongoing game

getStatuses :: Match Token -> Grid (Either Bool Token)
getStatuses (Match grids) = fmap gridStatus grids

matchReduction :: Match Token -> Grid Token
matchReduction m = fmap (fromRight EM) $! getStatuses m

-- a match is won when the status of some triplet is all "Right w" for the
-- same w
-- a match is over when is won or when there is no ongoing game in the
-- bigger grid
instance Winnable (Match Token) where
  isWon m = hasWinningTriplets $! matchReduction m
  isOver m =
    let x = toList $! getStatuses m
    in  not (V.any (== Left True) x) || isWon m

-- determine the winner for the whole Match, otherwise give Left False if the
-- game is tied and Left True if it is still ongoing
--
-- works properly only on legal positions (at most one winner)
matchStatus :: Match Token -> Either Bool Token
matchStatus m
  | isWon m = Right $! getWinner $! matchReduction m
  | V.any (== Left True) (toList $! getStatuses m) = Left True -- ongoing
  | otherwise = Left False -- tied