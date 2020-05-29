module TicUI where

import Game
import Data.Vector (Vector, (!))

instance Show Token where
  show EM = " "
  show X  = "X"
  show O  = "O"

-- auxiliary function, useful to render differently a single grid, and a grid
-- of grids
showRow :: (Show w) => Int -> Grid w -> String
showRow n (Grid b)
    | n == 1 = (a!0) ++ "|" ++ (a!1) ++ "|" ++ (a!2)
    | n == 2 = (a!3) ++ "|" ++ (a!4) ++ "|" ++ (a!5)
    | n == 3 = (a!6) ++ "|" ++ (a!7) ++ "|" ++ (a!8)
    | otherwise = error "Trying to show meaningless row of a Grid"
      where a = fmap show b

instance (Show w) => Show (Grid w) where
  show (Grid a) = "\n" ++ showRow 1 (Grid a) ++ "\n"
               ++ showRow 2 (Grid a) ++ "\n"
               ++ showRow 3 (Grid a) ++ "\n"

instance (Show w) => Show (Match w) where
  show m =
    "\n" ++
    showRow 1 (g!0) ++ " ║ " ++ showRow 1 (g!1) ++ " ║ " ++ showRow 1 (g!2) ++ "\n" ++
    showRow 2 (g!0) ++ " ║ " ++ showRow 2 (g!1) ++ " ║ " ++ showRow 2 (g!2) ++ "\n" ++
    showRow 3 (g!0) ++ " ║ " ++ showRow 3 (g!1) ++ " ║ " ++ showRow 3 (g!2) ++ "\n" ++
    "══════╬═══════╬══════\n" ++
    showRow 1 (g!3) ++ " ║ " ++ showRow 1 (g!4) ++ " ║ " ++ showRow 1 (g!5) ++ "\n" ++
    showRow 2 (g!3) ++ " ║ " ++ showRow 2 (g!4) ++ " ║ " ++ showRow 2 (g!5) ++ "\n" ++
    showRow 3 (g!3) ++ " ║ " ++ showRow 3 (g!4) ++ " ║ " ++ showRow 3 (g!5) ++ "\n" ++
    "══════╬═══════╬══════\n" ++
    showRow 1 (g!6) ++ " ║ " ++ showRow 1 (g!7) ++ " ║ " ++ showRow 1 (g!8) ++ "\n" ++
    showRow 2 (g!6) ++ " ║ " ++ showRow 2 (g!7) ++ " ║ " ++ showRow 2 (g!8) ++ "\n" ++
    showRow 3 (g!6) ++ " ║ " ++ showRow 3 (g!7) ++ " ║ " ++ showRow 3 (g!8)
    ++ "\n"
      where g = toList $ getGrids m
