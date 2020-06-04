module Rules
( nextPlayer
, legalMatchMoves
, setMatchEl
) where

import Game
import TicUI

import           Control.Lens
import           Data.Tree (Tree, Forest, unfoldTree)
import qualified Data.Tree as T
import           Data.Maybe (isNothing, fromJust)
import           Data.Either (isRight)
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
setGridEl (Coord (r,c)) w (Grid vs) = Grid $! vs // [(pos,w)]
  where pos = (3*r + c)

setMatchEl :: Move -> Match Token -> Maybe (Match Token)
setMatchEl move (Match m)
  | isRight (snd target) || Left False == snd target = Nothing
  | otherwise = Just $ Match $!
      ((setGridEl (Coord (orow,ocol))) $! (newInnerGrid,newInnerStatus)) m
    where orow = move^.outer.row
          ocol = move^.outer.col
          irow  = move^.inner.row
          icol  = move^.inner.col
          player = move^.agent
          pos = (3*orow + ocol)
          target = getGridEl (Coord $! (orow,ocol)) m
          innerGrid = fst $! target
          newInnerGrid = (setGridEl (Coord $! (irow,icol)) $! player) innerGrid
          newInnerStatus = ((smartGridStatus $! player) (Coord $! (irow,icol))) newInnerGrid

-- return all the coordinates of slots that are ongoing
ongoingSlots :: Grid Status -> [Coord]
ongoingSlots g = [ (Coord (m,n)) | m <- [0..2], n <- [0..2],
                  Left True == getGridEl (Coord (m,n)) g]

gridOngoingSlots :: Grid Token -> [Coord]
gridOngoingSlots g = [Coord (m,n) | m<-[0..2], n<-[0..2],
                      em == getGridEl (Coord (m,n)) g]

-- given last move and current match status, return all legal positions
-- achievable from it
legalMatchMoves :: Move -> Match Token -> Vector (Move, Match Token)
legalMatchMoves (Move _ lastInner lastPlayer) match@(Match m)
  | not (elem lastInner $! ongoingGrids) = V.fromList $!
    [ (Move outer inner $! p
      , fromJust $! setMatchEl (((Move $! outer) $! inner) $! p) match) |
      outer <- ongoingGrids,
      inner <- gridOngoingSlots $! targetGrid outer]
  | otherwise = V.fromList $!
    [ ((Move lastInner $! inner) $! p
      , fromJust $! setMatchEl ((Move lastInner $! inner) $! p) match) |
      inner <- gridOngoingSlots $! targetGrid lastInner]
    where p = nextPlayer lastPlayer
          statusGrid = fmap snd m
          ongoingGrids = ongoingSlots $! statusGrid
          targetGrid coord = fst $! getGridEl coord m