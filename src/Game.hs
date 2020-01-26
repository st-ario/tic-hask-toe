module Game where

import Data.Either (isRight, fromRight)

-- the quality of a token being empty, or a game being in progress is
-- qualitatively different from being resp. X/Y or won/tied
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
  if length a >= 9
    then Grid $ take 9 a
    else error "Cannot convert to Grid a list with less than 9 elements"

instance Functor Grid where
  fmap f grid = toGrid $ map f $ toList grid

-- The conditions for a Match to be won are different from the ones of Grid,
-- and it also needs a different intance of Show
newtype Match w = Match { getGrids :: Grid (Grid w) } deriving Eq

toMatch :: Grid (Grid w) -> Match w
toMatch m = Match m

-- create a list picking the values of rows, columns and diagonals
triplets :: Grid w -> [[w]]
triplets (Grid a) =
  [ -- rows
    [(a!!0),(a!!1),(a!!2)]
   ,[(a!!3),(a!!4),(a!!5)]
   ,[(a!!6),(a!!7),(a!!8)]
    -- colums
   ,[(a!!0),(a!!3),(a!!6)]
   ,[(a!!1),(a!!4),(a!!7)]
   ,[(a!!2),(a!!5),(a!!8)]
    -- diagonals
   ,[(a!!0),(a!!4),(a!!8)]
   ,[(a!!2),(a!!4),(a!!6)]
  ]

-- games are won when there's a winning triplet
-- games are over when a player wins or when the grid is full
instance (Eq w, Winnable w) => Winnable (Grid w) where
  isWon (Grid a) =
    let isTris x = isWon (head x) && all (==head x) (tail x)
    in  True `elem` map (isTris) (triplets $ Grid a)
  isOver (Grid a) = isWon (Grid a) || all (isOver) a

-- determine the winner for a single Grid, otherwise give Left False if the
-- game is tied and Left True if it is still ongoing
--
-- works properly only on legal positions
gridStatus :: (Winnable w, Eq w) => Grid w -> Either Bool w
gridStatus g
  | isWon g = Right winner
  | isOver g  = Left False
  | otherwise = Left True
    where winner = head [head x | x <- triplets g, all (==head x) (tail x)]

getStatuses :: (Eq w, Winnable w) => Match w -> Grid (Either Bool w)
getStatuses m = fmap gridStatus (getGrids m)

-- a match is won when the status of some triplet is all "Right w" for the
-- same w
-- a match is over when is won or when there is no ongoing game in the
-- bigger grid
instance (Eq w, Winnable w) => Winnable (Match w) where
  isWon m =
    let x = triplets $ getStatuses m
        isTris (y:ys) = isRight y && all (==y) ys
    in  True `elem` map (isTris) x
  isOver m =
    let x = toList $ getStatuses m
    in  isWon m || all (/= Left True) x

-- determine the winner for the whole Match, otherwise give Left False if the
-- game is tied and Left True if it is still ongoing
--
-- works properly only on legal positions
matchStatus :: (Winnable w, Eq w) => Match w -> Either Bool w
matchStatus m
  | isWon m = head [x | (x:xs) <- y, isRight x, all (==x) xs]
  | isOver m  = Left False
  | otherwise = Left True
    where y = triplets $ getStatuses m

-- given two integers m, n in {0,1,2}, a value w and a Grid, replace with w
-- the element with coordinates (m,n)
--
-- the position of a value in the list encoding the grid is the expansion
-- in base 3 of the coordinates
setGridEl :: Int -> Int -> w -> Grid w -> Grid w
setGridEl m n w grid
  | m >= 3 || n >= 3 = error "Coordinates in a Grid must be in {0, 1, 2}"
  | otherwise = toGrid $ (take pos xs) ++ [w] ++ drop (pos+1) xs
    where pos = (3*m + n)
          xs = toList grid

-- given four integers a, b, m, n in {0,1,2}, a value w and a Match, replace
-- with w the element with coordinates (m,n) in the subGrid with coordinates
-- (a,b)
--
-- the position of a value in the list encoding the grid is the expansion
-- in base 3 of the coordinates
setMatchEl :: Int -> Int -> Int -> Int -> w -> Match w -> Match w
setMatchEl a b m n w match
  | any (>=3) [a,b,m,n] = error "Coordinates in a Match must be in {0, 1, 2}"
  | otherwise = toMatch $ setGridEl a b newInnerGrid outerGrid
    where pos = (3*a + b)
          outerGrid = getGrids match
          newInnerGrid = setGridEl m n w $ (toList outerGrid)!!pos
