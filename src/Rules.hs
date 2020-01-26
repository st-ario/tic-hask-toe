module Rules where

import Game

data Coord = Coord { col :: Int
                   , row :: Int
                   } deriving (Show)

data Move = Move { outer :: Coord
                 , inner :: Coord
                 , agent :: Token
                 } deriving (Show)
