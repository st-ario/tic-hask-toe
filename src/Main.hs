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
import           Data.Maybe (isNothing, fromJust)

clear = putStrLn "\ESC[2J"

emptyGrid = Grid $ V.replicate 9 em
emptyMatch = Match $ Grid $! V.replicate 9 (emptyGrid, Left True)

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
         
gameLoop :: R.StdGen -> (Move, Match Token) -> IO ()
gameLoop gen pair = do
  let state = getBestMove gen pair
  let currentBoard = snd state
  clear
  putStrLn $ show currentBoard
  putStrLn "Please write down your move and press ENTER"
  inputMove <- getLine
  input <- testInput inputMove
  let rawCoord = fmap digitToInt input
  let outer = Coord (rawCoord !! 0) (rawCoord !! 1)
  let inner = Coord (rawCoord !! 2) (rawCoord !! 3)
  let userMove = Move outer inner o
  let newState = setMatchEl userMove currentBoard
  if isNothing newState
    then undefined -- new prompt
    else gameLoop gen (userMove, fromJust $ newState)
  return ()

main :: IO ()
main = do
  clear
  g <- R.newStdGen
  putStrLn "Welcome to Tic-Hask-Toe!"
  putStrLn "Press any key to start playing, press Ctrl-C to quit."
  getLine
  gameLoop g (Move (Coord 1 1) (Coord 1 1) o, emptyMatch)
  return ()