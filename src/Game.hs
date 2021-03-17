{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Game
( Token(..), o, em, x
, Grid(..)
, Match(..)
, Move(..), inner, outer, agent
, Coord(..), row, col
, Status
, pattern Tie
, pattern Ongoing
, pattern WonBy
, toGrid
, toVector
, gridStatus
, matchStatus
) where

import           Control.Lens
import           Data.Either (isRight)
import           Data.Vector (Vector,(!),(//))
import qualified Data.Vector as V
import           Data.List (and)
import           Data.Coerce

newtype Token = Token Int deriving (Eq, Enum)
(o:em:x:_) = [Token (-1) ..]

instance Show Token where
  show t
    | t == em = " "
    | t == x = "X"
    | t == o = "O"
    | otherwise = error "token out of bound"

-- Status of a TTT or UTTT game
type Status = Either Bool Token

  -- Left True  represents an ongoing status
pattern Ongoing :: Status
pattern Ongoing = Left True

  -- Left False represents a tied status
pattern Tie :: Status
pattern Tie = Left False

  -- Right w    represents a status won by w
pattern WonBy :: Token -> Status
pattern WonBy w =  Right w

newtype Grid w = Grid (Vector w) deriving Eq

toGrid :: Vector w -> Grid w
toGrid = coerce

toVector :: Grid w -> Vector w
toVector = coerce

instance Functor Grid where
  fmap f = coerce (V.map f)

-- UTTT grid (different winning conditions, different instance of Show)
newtype Match w = Match { getGrids :: Grid (Grid w, Status) } deriving Eq

newtype Coord = Coord (Int,Int) deriving (Eq, Show)

-- lensens for Coord
getRow :: Coord -> Int
getRow = coerce (fst :: (Int, Int) -> Int)

getCol :: Coord -> Int
getCol = coerce (snd :: (Int, Int) -> Int)

rawSetRow :: (Int,Int) -> Int -> (Int,Int)
rawSetRow (a,b) x = (x,b)

rawSetCol :: (Int,Int) -> Int -> (Int,Int)
rawSetCol (a,b) y = (a,y)

setRow :: Coord -> Int -> Coord
setRow = coerce rawSetRow

setCol :: Coord -> Int -> Coord
setCol = coerce rawSetCol

row :: Lens' Coord Int
row = lens getRow setRow

col :: Lens' Coord Int
col = lens getCol setCol

data Move = Move { _outer :: Coord
                 , _inner :: Coord
                 , _agent :: Token
                 } deriving Eq

makeLenses ''Move

instance Show Move where
  show (Move (Coord (a,b)) (Coord (x,y)) t) =
    "Outer: " ++ (show (a,b)) ++ " " ++ "Inner: " ++ (show (x,y)) ++ " "
      ++ "Token: " ++ show t

-- given last player, last move and TTT grid, determine its status
-- optimal number of checks
gridStatus :: Token -> Coord -> Grid Token -> Status
gridStatus lp (Coord (r,c)) (Grid vt)
    -- this case can arise when evaluating the status of a status grid
    | vt!pos == em =
      if V.any (==em) (vt // [(pos,x)])
        then Ongoing
        else Tie
    | (r,c) == (1,1) =
      if
        | lp == vt!3 && lp == vt!5 -> WonBy lp -- check row
        | lp == vt!1 && lp == vt!7 -> WonBy lp -- check column
        | lp == vt!0 && lp == vt!8 -> WonBy lp -- check first diagonal
        | lp == vt!2 && lp == vt!6 -> WonBy lp -- check other diagonal
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | (r,c) == (0,0) =
      if
        | lp == vt!1 && lp == vt!2 -> WonBy lp -- check row
        | lp == vt!3 && lp == vt!6 -> WonBy lp -- check column
        | lp == vt!4 && lp == vt!8 -> WonBy lp -- check diagonal
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | (r,c) == (0,2) =
      if
        | lp == vt!0 && lp == vt!1 -> WonBy lp -- check row
        | lp == vt!5 && lp == vt!8 -> WonBy lp -- check column
        | lp == vt!4 && lp == vt!6 -> WonBy lp -- check diagonal
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | (r,c) == (2,0) =
      if
        | lp == vt!7 && lp == vt!8 -> WonBy lp -- check row
        | lp == vt!0 && lp == vt!3 -> WonBy lp -- check column
        | lp == vt!2 && lp == vt!4 -> WonBy lp -- check diagonal
        | V.any (==em) vt -> Ongoing 
        | otherwise -> Tie
    | (r,c) == (2,2) =
      if
        | lp == vt!6 && lp == vt!7 -> WonBy lp -- check row
        | lp == vt!2 && lp == vt!5 -> WonBy lp -- check column
        | lp == vt!0 && lp == vt!4 -> WonBy lp -- check diagonal
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | (r,c) == (0,1) =
      if
        | lp == vt!0 && lp == vt!2 -> WonBy lp -- check row
        | lp == vt!4 && lp == vt!7 -> WonBy lp -- check column
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | (r,c) == (1,0) =
      if
        | lp == vt!4 && lp == vt!5 -> WonBy lp -- check row
        | lp == vt!0 && lp == vt!6 -> WonBy lp -- check column
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | (r,c) == (1,2) =
      if
        | lp == vt!3 && lp == vt!4 -> WonBy lp -- check row
        | lp == vt!2 && lp == vt!8 -> WonBy lp -- check column
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | (r,c) == (2,1) =
      if
        | lp == vt!6 && lp == vt!8 -> WonBy lp -- check row
        | lp == vt!1 && lp == vt!4 -> WonBy lp -- check column
        | V.any (==em) vt -> Ongoing
        | otherwise -> Tie
    | otherwise = undefined
    where pos = 3*r + c

simplify :: Either Bool Token -> Token
simplify (WonBy t) = t
simplify (Left _) = em

-- given last move and current match, return a grid of all the statuses
-- of subgrids and the status of the current match
reduceMatch :: Move -> Match Token -> (Grid Status, Status)
reduceMatch (Move outer inner lp) (Match g@(Grid grids)) = (Grid $! statusGrid,status)
  where statusGrid = V.map snd grids
        status = (gridStatus lp outer) $ Grid $! V.map simplify statusGrid

matchStatus :: (Move, Match Token) -> Status
matchStatus (move,match)
  | isRight s = s
  | V.any (== Ongoing) vs = Ongoing
  | otherwise = Tie
  where (Grid vs,s) = reduceMatch move match