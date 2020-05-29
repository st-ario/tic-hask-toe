{-# LANGUAGE FlexibleInstances #-}

module Game where

import Data.Either (isRight, isLeft, fromRight)
import Data.Maybe (fromJust, isNothing)
import Data.List (findIndex)

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
newtype Grid w = Grid { toList :: [w]} deriving Eq

toGrid :: [w] -> Grid w
toGrid a =
  if length a == 9
    then Grid a
    else error "A Grid must have 9 elements"

instance Functor Grid where
  fmap f grid = toGrid $ map f $ toList grid

-- The conditions for a Match to be won are different from the ones of Grid,
-- and it also needs a different instance of Show
newtype Match w = Match { getGrids :: Grid (Grid w) } deriving Eq

toMatch :: Grid (Grid w) -> Match w
toMatch m = Match m

-- create a list picking the values of rows, columns and diagonals
-- (ugly, but more performant than last version)
triplets :: Grid w -> [[w]]
triplets (Grid a) =
  [ -- rows
    [r , s, t]
   ,[u , v, w]
   ,[x , y, z]
    -- colums
   ,[r, u, x]
   ,[s, v, y]
   ,[t, w, z]
    -- diagonals
   ,[r, v, z]
   ,[t, v, x]
  ]
  where r = (a!!0)
        s = (a!!1)
        t = (a!!2)
        u = (a!!3)
        v = (a!!4)
        w = (a!!5)
        x = (a!!6)
        y = (a!!7)
        z = (a!!8)

-- auxiliary function to improve performance
isTris :: (Eq w, Winnable w) => [w] -> Bool
isTris (a:b:c:[]) = isWon a && a == b && a == c
isTris _ = error "Not a triplet"

-- grids are won when there's a winning triplet
-- grids are over when a player wins or when the grid is full
instance (Eq w, Winnable w) => Winnable (Grid w) where
  isWon grid = any isTris $! triplets grid
  isOver (Grid a) = all isOver a || isWon (Grid a)

-- determine the winner for a single Grid, otherwise give Left False if the
-- game is tied and Left True if it is still ongoing
--
-- works properly only on legal positions (at most one winner)
-- ugly but performant
gridStatus :: (Winnable w, Eq w) => Grid w -> Either Bool w
gridStatus grid =
  let index = findIndex isTris $ triplets grid
      winner = head $ (triplets grid) !! (fromJust index)
  in if isOver grid
       then if isNothing index
              then Left False -- tied game
              else Right winner -- game won by winner
       else Left True --ongoing game

getStatuses :: (Eq w, Winnable w) => Match w -> Grid (Either Bool w)
getStatuses m = fmap gridStatus $! (getGrids m)

-- this is a bit unfortunate, but works well
instance (Winnable w) => Winnable (Either Bool w) where
  isOver (Left True) = False -- this is ongoing
  isOver (Left False) = True  -- this is tied
  isOver (Right w) = isOver w
  isWon (Left True) = False
  isWon (Left False) = False
  isWon (Right w) = isWon w

-- a match is won when the status of some triplet is all "Right w" for the
-- same w
-- a match is over when is won or when there is no ongoing game in the
-- bigger grid
instance (Eq w, Winnable w) => Winnable (Match w) where
  isWon m =
    let (Grid lStatuses) = getStatuses m
        x = triplets (Grid lStatuses)
        winningTriplets = filter (\u -> isRight (head u) && isTris u) $ x
    in any isRight lStatuses && winningTriplets /= []
  isOver m =
    let x = toList $! getStatuses m
    in  not (any (== Left True) x) || isWon m

-- determine the winner for the whole Match, otherwise give Left False if the
-- game is tied and Left True if it is still ongoing
--
-- works properly only on legal positions (at most one winner)
matchStatus :: (Winnable w, Eq w) => Match w -> Either Bool w
matchStatus m =
  let (Grid lStatuses) = getStatuses m
      x = triplets (Grid lStatuses)
      winningTriplets = filter (\u -> isRight (head u) && isTris u) $ x
  in if any isRight lStatuses && winningTriplets /= []
       then head (head winningTriplets)  -- return Right winner
       else if isOver m
              then Left False -- tied Game
              else Left True  -- ongoing Game