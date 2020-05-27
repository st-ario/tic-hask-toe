{-# LANGUAGE TemplateHaskell #-}

module Rules where

import Game
import TicUI

import           Control.Lens
import           Data.Tree (Tree, Forest, unfoldTree)
import qualified Data.Tree as T

data Coord = Coord { _row :: Int
                   , _col :: Int
                   }

makeLenses ''Coord

instance Show Coord where
  show coord = "[" ++ show (coord^.row) ++ "|" ++ show(coord^.col) ++ "]"

-- a move consist of coordinates for the outer grid, the inner grid
-- and the token to insert
data Move = Move { _outer :: Coord
                 , _inner :: Coord
                 , _agent :: Token
                 } deriving (Show)

makeLenses ''Move

nextPlayer :: Token -> Token
nextPlayer EM = error "EM is not a valid player"
nextPlayer X  = O
nextPlayer O  = X

-- the position in the list encoding the grid is the expansion
-- in base 3 of the coordinates
--
-- e.g. the element in row 2 and column 1 is the
-- 2*(3^1) + 1*(3^0) = 7 -th element of the list
getGridEl :: Coord -> Grid w -> w
getGridEl c g = (toList g !! pos)
  where m = c^.row
        n = c^.col
        pos = (3*m + n)

-- setters don't check whether a change is a legal move or not
setGridEl :: Coord -> w -> Grid w -> Grid w
setGridEl c w grid
  | m >= 3 || n >= 3 = error "Coordinates in a Grid must be in {0, 1, 2}"
  | otherwise = toGrid $ (take pos xs) ++ [w] ++ drop (pos+1) xs
    where m = c^.row
          n = c^.col
          pos = (3*m + n)
          xs = toList grid

-- return all the coordinates of slots that are not over
ongoingSlots :: (Winnable w) => Grid w -> [Coord]
ongoingSlots g = [ (Coord m n) | m <- [0..2], n <- [0..2],
                  not(isOver $ getGridEl (Coord m n) g)]

setMatchEl :: Move -> Match Token -> Match Token
setMatchEl move match
  | any (>=3) [a,b,m,n] = error "Coordinates in a Match must be in {0, 1, 2}"
  | otherwise = toMatch $ setGridEl (Coord a b) newInnerGrid outerGrid
    where a = move^.outer.row
          b = move^.outer.col
          m = move^.inner.row
          n = move^.inner.col
          w = move^.agent
          pos = (3*a + b)
          outerGrid = getGrids match
          newInnerGrid = setGridEl (Coord m n) w $ (toList outerGrid)!!pos

-- given the last player's token and the actual grid position, return all legal
-- positions achieveable from it
legalGridMoves :: (Token, Grid Token) -> [(Token, Grid Token)]
legalGridMoves (lastPlayer,g)
  | lastPlayer == EM = error "EM can't move"
  | isOver g = []
  | otherwise = [(p, setGridEl coord p g) | coord <- ongoingSlots g]
    where p = nextPlayer lastPlayer

-- given the last player's token, the coordinate of the last move, and the
-- actual game state, return all legal positions achievable from it
legalMatchMoves :: (Token, Coord, Match Token) -> [(Token, Coord, Match Token)]
legalMatchMoves (lastPlayer, lastCoord, state)
  | lastPlayer == EM = error "EM can't move"
  | isOver state = []
  | otherwise =
      -- if the subgrid corresponding to the last move is over, the legal
      -- moves are the one taking place in any subgrid that is not over,
      -- in any spot that is empty
      -- otherwise, the legal moves are the one taking place in any spot that
      -- is empty
      if isOver targetGrid
        then targetGridBusy p state
        else [(p, inner, (updateState lastCoord inner)) | inner <- ongoingSlots targetGrid]
    where p = nextPlayer lastPlayer
          gameGrids = getGrids state
          targetGrid = getGridEl lastCoord gameGrids
          updateState c d = setMatchEl (Move c d p) state

-- auxiliary function, for legalMatchMoves
targetGridBusy :: Token -> Match Token -> [(Token, Coord, Match Token)]
targetGridBusy p state =
  let gameGrids = getGrids state
      availableSubGrids = ongoingSlots gameGrids
      goodPairs = [(outerCoord, innerCoord) |
        outerCoord <- availableSubGrids,
        innerCoord <- ongoingSlots $ getGridEl outerCoord gameGrids]
      updateState c d = setMatchEl (Move c d p) state
  in [(p,inner,updateState outer inner) | (outer, inner) <- goodPairs]

-- seed for the game tree of a single grid
playTTT :: (Token, Grid Token) -> ((Token, Grid Token),[(Token, Grid Token)])
playTTT (t,g) = ((t,g), legalGridMoves (t,g))

-- game tree of a single tic-tac-toe match
ordinaryGameTree = unfoldTree playTTT (O,gI)
  where gI = toGrid $ take 9 $ cycle [EM]

ordG = T.levels ordinaryGameTree