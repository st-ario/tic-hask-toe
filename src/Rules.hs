{-# LANGUAGE TemplateHaskell #-}

module Rules where

import Game
import TicUI

import           Control.Lens
import           Data.Tree (Tree, Forest, unfoldTree)
import qualified Data.Tree as T

data Coord = Coord { _row :: Int
                   , _col :: Int
                   } deriving (Show)

makeLenses ''Coord

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
  where m = view row c
        n = view col c
        pos = (3*m + n)

-- setters don't check whether a change is a legal move or not
setGridEl :: Coord -> w -> Grid w -> Grid w
setGridEl c w grid
  | m >= 3 || n >= 3 = error "Coordinates in a Grid must be in {0, 1, 2}"
  | otherwise = toGrid $ (take pos xs) ++ [w] ++ drop (pos+1) xs
    where m = view row c
          n = view col c
          pos = (3*m + n)
          xs = toList grid

setMatchEl :: Move -> Match Token -> Match Token
setMatchEl move match
  | any (>=3) [a,b,m,n] = error "Coordinates in a Match must be in {0, 1, 2}"
  | otherwise = toMatch $ setGridEl (Coord a b) newInnerGrid outerGrid
    where a = view row $ view outer move
          b = view col $ view outer move
          m = view row $ view inner move
          n = view col $ view inner move
          w = view agent move
          pos = (3*a + b)
          outerGrid = getGrids match
          newInnerGrid = setGridEl (Coord m n) w $ (toList outerGrid)!!pos

-- given the last player's token and the actual grid position, return all legal
-- positions achieveable from it
legalGridMoves :: (Token, Grid Token) -> [(Token, Grid Token)]
legalGridMoves (lastPlayer,g)
  | lastPlayer == EM = error "EM can't move"
  | isOver g = []
  | otherwise = [(p, setGridEl (Coord m n) p g) |
                  m <- [0..2], n <- [0..2],
                  not(isOver $ getGridEl (Coord m n) g)]
    where p = nextPlayer lastPlayer

-- seed for the game tree of a single grid
playTTT :: (Token, Grid Token) -> ((Token, Grid Token),[(Token, Grid Token)])
playTTT (t,g) = ((t,g), legalGridMoves (t,g))

-- game tree of a single tic-tac-toe match
ordinaryGameTree = unfoldTree playTTT (O,gI)
  where gI = toGrid $ take 9 $ cycle [EM]

ordG = T.levels ordinaryGameTree