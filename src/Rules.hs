{-# LANGUAGE MonadComprehensions #-}

module Rules
( nextPlayer
, legalMatchMoves
, setMatchEl
) where

import Game
import TicUI

import           Control.Lens
import           Control.Monad (guard)
import           Data.Tree (Tree, Forest, unfoldTree)
import qualified Data.Tree as T
import           Data.Maybe (isNothing, fromJust)
import           Data.Vector(Vector, (!), (//), singleton)
import qualified Data.Vector as V

nextPlayer :: Token -> Token
nextPlayer t
  | t == x = o
  | t == o = x
  | otherwise = error "Not a valid player"

-- the position in the list encoding the grid is the expansion
-- in base 3 of the coordinates
-- e.g. the element in row 2 and column 1 is the
-- 2*(3^1) + 1*(3^0) = 7 -th element of the list
getGridEl :: Coord -> Grid w -> w
getGridEl (Coord (r,c)) (Grid vs) = (!) vs $! pos
  where pos = (3*r + c)

-- setters don't check whether a change is a legal move or not
setGridEl :: Coord -> w -> Grid w -> Grid w
setGridEl (Coord (r,c)) w (Grid vs) = Grid $!
  (V.take pos vs) V.++ (singleton w) V.++ (V.drop (pos+1) vs)
    where pos = (3*r + c)
  

-- given a raw position, return the corresponding coordinate
intToCoord :: Int -> Coord
intToCoord n = Coord (row, column)
  where row = n `div` 3
        column = n `mod` 3

-- given a move and a match, return the updated match pair (match grid, status)
setMatchEl :: Move -> Match Token -> Match Token
setMatchEl move (Match m) = Match $!
                              ((setGridEl (Coord (orow,ocol))) $! (newInnerGrid,newInnerStatus)) m
    where orow = move^.outer.row
          ocol = move^.outer.col
          irow  = move^.inner.row
          icol  = move^.inner.col
          player = move^.agent
          target = getGridEl (Coord $! (orow,ocol)) $! m
          newInnerGrid = (setGridEl (Coord $! (irow,icol)) $! player) $! (fst $! target)
          newInnerStatus = ((gridStatus $! player) (Coord $! (irow,icol))) $! newInnerGrid

-- return all the coordinates of slots that are ongoing
ongoingSlots :: Grid Status -> [Coord]
ongoingSlots g = [ (Coord (m,n)) | m <- [0..2], n <- [0..2],
                  Ongoing == getGridEl (Coord (m,n)) g]

gridOngoingSlots :: Grid Token -> [Coord]
gridOngoingSlots g = [Coord (m,n) | m<-[0..2], n<-[0..2],
                      em == getGridEl (Coord (m,n)) g]

-- given last move and current match status, return all legal positions achievable from it
legalMatchMoves :: Move -> Match Token -> Vector (Move, Match Token)
legalMatchMoves (Move _ lastInner lastPlayer) match@(Match m)
  -- if lastInner corresponds to an ongoing grid
  | targetGridStatus == Ongoing = do
      inner <- V.fromList $ gridOngoingSlots $! targetGrid
      -- outer = lastInner
      let outMove = (Move lastInner $! inner) $! p
      return $! (outMove, setMatchEl outMove match)
  | otherwise = do
      outer <- ongoingGrids
      inner <- V.fromList $ gridOngoingSlots $! destinationGrid outer
      let outMove = (Move outer inner $! p)
      return $! (outMove, setMatchEl outMove match)
    where p = nextPlayer lastPlayer
          targetPair = getGridEl lastInner $! m
          targetGrid = fst $! targetPair
          targetGridStatus = snd $! targetPair
          ongoingGrids = V.fromList $ ongoingSlots $! statusGrid
          statusGrid = fmap snd m
          destinationGrid coord = fst $! getGridEl coord m