{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Game where

import           Data.Either (isRight,fromRight)
import           Data.Maybe (fromJust, isNothing)
import           Data.Vector (Vector,(!),cons,empty,singleton)
import qualified Data.Vector as V
import           Control.Monad (join)

class Winnable a where
  winStatus :: a -> Either Bool a
  -- Left True  represents an ongoing status
  -- Left False represents a tied status
  -- Right w    represents a status won by w
  -- winStatus /= Left True means the game is over

-- tokens are either empty, X or O
data Token = EM | X | O deriving Eq

-- a slot is considered won when either player occupied it
-- an empty slot is a "game in progress"
instance Winnable Token where
  winStatus EM = Left True -- ongoing
  winStatus X = Right X
  winStatus O = Right O

newtype Grid w = Grid { toList :: Vector w } deriving Eq

toGrid :: Vector w -> Grid w
toGrid a = Grid $! a

instance Functor Grid where
  fmap f (Grid a) = Grid $! (V.map f $! a)

-- The conditions for a Match to be won are different from the ones of Grid,
-- and it also needs a different instance of Show
newtype Match w = Match { getGrids :: Grid (Grid w) } deriving Eq

toMatch :: Grid (Grid w) -> Match w
toMatch m = Match $! m

gridStatus :: (Eq w, Winnable w) => Grid w -> Either Bool w
gridStatus (Grid vs) = 
  if -- rows
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
    | otherwise -> Left $! V.any (== Left True) a
      where a = V.map winStatus $ vs
            r = a!0
            s = a!1
            t = a!2
            u = a!3
            v = a!4
            w = a!5
            x = a!6
            y = a!7
            z = a!8
            cv = isRight v
            cx = isRight x
            ct = isRight t

reduceMatch :: Match Token -> Grid (Either Bool Token)
reduceMatch (Match grids) = fmap gridStatus $! grids

instance Winnable (Either Bool Token) where
  winStatus = Right
-- just a trick for the next function

matchStatus :: Match Token -> Either Bool Token
matchStatus match
  | isRight winner = winner
  | V.any (== Left True) vs = Left True
  | otherwise = Left False
  where redGrid@(Grid vs) = reduceMatch $! match
        winner = join $! gridStatus $! redGrid