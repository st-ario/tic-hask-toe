{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game where

import           Control.Lens
import           Data.Either (isRight,fromRight)
import           Data.Maybe (fromJust, isNothing)
import           Data.Vector (Vector,(!),cons,empty,singleton)
import qualified Data.Vector as V
import           Control.Monad (join)
import           Data.List (and)

newtype Token = Token Int deriving (Eq, Enum)
(o:em:x:_) = [Token (-1) ..]

type Status = Either Bool Token
  -- Left True  represents an ongoing status
  -- Left False represents a tied status
  -- Right w    represents a status won by w
  -- status /= Left True means the game is over

data Coord = Coord { _row :: Int
                   , _col :: Int
                   } deriving (Eq)

makeLenses ''Coord

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

-- given a legal TTT grid, determine its status (expensive)
gridStatus :: Grid Token -> Status
gridStatus (Grid vt) =
  if --rows
    | (nx0 && x0 == x1 && x1 == x2) -> Right x0
    | (nx4 && x3 == x4 && x4 == x5) -> Right x4
    | (nx8 && x6 == x7 && x7 == x8) -> Right x8
    -- columns
    | (nx0 && x0 == x3 && x3 == x6) -> Right x0
    | (nx4 && x1 == x4 && x4 == x7) -> Right x4
    | (nx8 && x2 == x5 && x5 == x8) -> Right x8
    -- diagonals
    | (nx4 && x0 == x4 && x4 == x8) -> Right x4
    | (nx4 && x2 == x4 && x4 == x6) -> Right x4
    | otherwise -> if V.any (==em) vt
                     then Left True
                     else Left False
      where x0 = vt!0
            x1 = vt!1
            x2 = vt!2
            x3 = vt!3
            x4 = vt!4
            x5 = vt!5
            x6 = vt!6
            x7 = vt!7
            x8 = vt!8
            nx0 = x0 /= em
            nx4 = x4 /= em
            nx8 = x8 /= em

-- given last player, last move and TTT grid, determine its status (fast)
smartGridStatus :: Token -> Maybe Coord -> Grid Token -> Status
smartGridStatus lp coord g@(Grid vt)
  | isNothing coord = gridStatus g
  | otherwise =
      if
        | (r == 0 && c == 0) || (r == 0 && c == 2) || (r == 1 && c == 1) ||
            (r == 2 && c == 0) || (r == 2 && c == 2) ->
          if
          -- the position in the list encoding the grid is the expansion
          -- in base 3 of the coordinates
              | and [lp==vt!(n+r*3) | n<-[0,1,2]] -> Right lp -- check row
              | and [lp==vt!(n+c) | n<-[0,3,6]] -> Right lp -- check column
              | lp == vt!0 && lp == vt!8 -> Right lp -- check first diagonal
              | lp == vt!2 && lp == vt!6 -> Right lp -- check other diagonal
              | V.any (==em) vt -> Left True
              | otherwise -> Left False
        | otherwise ->
            if
              | and [lp==vt!(n+r*3) | n<-[0,1,2]] -> Right lp -- check row
              | and [lp==vt!(n+c) | n<-[0,3,6]] -> Right lp -- check column
              | V.any (==em) vt -> Left True
              | otherwise -> Left False
    where actualCoord@(Coord r c) = fromJust coord

simplify :: Either Bool Token -> Token
simplify (Right t) = t
simplify (Left _) = em

{--
reduceMatch :: Match Token -> (Grid Status, Status)
reduceMatch (Match grids) = (statusGrid,status)
  where statusGrid = fmap gridStatus $! grids
        status = gridStatus $ fmap simplify statusGrid -- keep lazy!
        -- in Rules.hs only the first component is needed
--}

smartReduceMatch :: Token -> Maybe Coord -> Match Token -> (Grid Status, Status)
smartReduceMatch lp lc (Match grids) = (statusGrid,status)
  where statusGrid = fmap gridStatus $! grids
        status = (smartGridStatus lp lc) $ fmap simplify statusGrid -- keep lazy!
        -- in Rules.hs only the first component is needed

{--
matchStatus :: Match Token -> Status
matchStatus match
  | isRight s = s
  | V.any (== Left True) vs = Left True
  | otherwise = Left False
  where (g@(Grid vs),s) = reduceMatch match
--}

smartMatchStatus :: (Token, Maybe Coord, Match Token) -> Status
smartMatchStatus (lp,lc,match)
  | isRight s = s
  | V.any (== Left True) vs = Left True
  | otherwise = Left False
  where (g@(Grid vs),s) = smartReduceMatch lp lc match