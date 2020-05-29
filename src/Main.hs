module Main where

import Game
import TicUI
import Rules
import TreeZipper
import MCTS

import qualified Data.Tree as T
import qualified System.Random as R
import           Control.Monad
import           Data.Char (digitToInt)
import qualified Data.Vector as V

clear = putStrLn "\ESC[2J"

emptyGrid = toGrid $ V.replicate 9 EM
emptyMatch = toMatch $ toGrid $! V.replicate 9 emptyGrid

validInputs = replicateM 4 ['0'..'2']

isValid :: String -> Bool
isValid s = s `elem` validInputs

testInput :: String -> IO (String)
testInput input =
  if isValid input
    then do putStrLn $ "Your move was " ++ input
            return input
    else do putStrLn "Please enter a valid move"
            newInput <- getLine
            testInput newInput
         
gameLoop :: R.StdGen -> (Token, Maybe Coord, Match Token) -> IO ()
gameLoop gen triple = do
  let state = getBestMove gen triple
  let currentBoard = trd state
  clear
  putStrLn $ show currentBoard
  putStrLn "Please write down your move and press ENTER"
  inputMove <- getLine
  input <- testInput inputMove
  let rawCoord = fmap digitToInt input
  let outer = Coord (rawCoord !! 0) (rawCoord !! 1)
  let inner = Coord (rawCoord !! 2) (rawCoord !! 3)
  let userMove = Move outer inner O
  let newState = setMatchEl userMove currentBoard
  gameLoop gen (O, Just inner, newState)
  return ()

main :: IO ()
main = do
  clear
  g <- R.newStdGen
  putStrLn "Welcome to Tic-Hask-Toe!"
  putStrLn "Press any key to start playing, press Ctrl-C to quit."
  getLine
  gameLoop g (O, Nothing, emptyMatch)
  return ()