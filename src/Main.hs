{-# LANGUAGE MultiWayIf #-}

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
import           System.Exit
import           System.Environment (getArgs)

clear = "\ESC[2J\ESC[H"
ucbConst = 0.5

emptyGrid = Grid $ V.replicate 9 em
emptyMatch = Match $ Grid $ V.replicate 9 (emptyGrid, Ongoing)

isValid :: String -> Bool
isValid s = s `elem` validInputs
  where validInputs = replicateM 4 ['0'..'2']

testInput :: String -> Move -> Match Token -> IO (Move)
testInput input move match =
  if isValid input
    then do userMove <- parseMove input (nextPlayer . _agent $ move)
            let legalMoves = V.map fst $ legalMatchMoves move match
            if not(userMove `elem` legalMoves)
              then do putStrLn $ "Please enter a legal move"
                      newInput <- getLine
                      testInput newInput move match
              else return userMove
    else do putStrLn "Please enter a move in a valid format"
            newInput <- getLine
            testInput newInput move match

parseMove :: String -> Token -> IO (Move)
parseMove raw token = do
  let rawCoord = fmap digitToInt raw
  let outer = Coord ((rawCoord !! 0),(rawCoord !! 1))
  let inner = Coord ((rawCoord !! 2),(rawCoord !! 3))
  let userMove = Move outer inner token
  return userMove

gameLoop :: R.StdGen -> (Move, Match Token) -> IO ()
gameLoop gen pair = do
  putStr clear
  putStrLn $ "Your move was " ++ (show $ fst pair)
  putStrLn $ show (snd pair)
  let pairStatus = matchStatus pair
  if pairStatus /= Ongoing
    then do putStr "Game Over, "
            if  | pairStatus == Tie -> putStrLn "tie!" >> exitSuccess
                | pairStatus == WonBy x -> putStrLn "X won!" >> exitSuccess
                | pairStatus == WonBy o -> putStrLn "O won!" >> exitSuccess
                | otherwise -> return ()
    else return ()
  putStrLn "Thinking..."
  state@(computerMove,currentBoard) <- getBestMove gen ucbConst pair
  putStr clear
  putStrLn $ show currentBoard
  putStrLn $ "AI's last move was"
  putStrLn $ show $ computerMove
  let currentStatus = matchStatus state
  if currentStatus /= Ongoing 
    then do putStr "Game Over, "
            if  | currentStatus == Tie -> putStrLn "tie!" >> exitSuccess
                | currentStatus == WonBy x -> putStrLn "X won!" >> exitSuccess
                | currentStatus == WonBy o -> putStrLn "O won!" >> exitSuccess
                | otherwise -> return ()
    else return ()
  putStrLn "Please write down your move and press ENTER"
  inputMove <- getLine
  userMove <- testInput inputMove computerMove currentBoard
  let newBoard = setMatchEl userMove currentBoard
  newGen <- R.newStdGen
  gameLoop newGen (userMove, newBoard)
  return ()

aiVai :: R.StdGen -> (Move, Match Token) -> IO ()
aiVai gen pair = do
  putStrLn "Thinking..."
  state@(computerMove,currentBoard) <- getBestMove gen ucbConst pair
  putStrLn $ clear ++ show currentBoard
  putStrLn $ "Last move was"
  putStrLn $ show $ computerMove
  newGen <- R.newStdGen
  if matchStatus state /= Ongoing 
    then do putStrLn "Game Over"
            exitSuccess
    else aiVai newGen state

selectionO :: R.StdGen -> IO ()
selectionO gen = do
  putStr clear
  putStrLn $ show emptyMatch
  putStrLn "Please write down your first move and press ENTER"
  inputMove <- getLine
  userMove <- testInput' inputMove emptyMatch
  putStrLn $ "Your move was " ++ (show userMove)
  let newState = setMatchEl userMove emptyMatch
  gameLoop gen (userMove, newState)

selectionX :: R.StdGen -> (Move, Match Token) -> IO ()
selectionX gen pair = do
  putStr clear
  putStrLn "Thinking..."
  state@(computerMove,currentBoard) <- getBestMove gen ucbConst pair
  putStr clear
  putStrLn $ show currentBoard
  putStrLn $ "AI's last move was"
  putStrLn $ show $ computerMove
  putStrLn "Please write down your move and press ENTER"
  inputMove <- getLine
  userMove <- testInput inputMove computerMove currentBoard
  let newState = setMatchEl userMove currentBoard
  newGen <- R.newStdGen
  gameLoop newGen (userMove, newState)
  return ()

testInput' :: String -> Match Token -> IO (Move)
testInput' input match =
  if isValid input
    then parseMove input x
    else do putStrLn "Please enter a move in a valid format"
            newInput <- getLine
            testInput' newInput match

-- interactiveMain :: IO ()
-- interactiveMain = do
main :: IO ()
main = do
  putStr clear
  g <- R.newStdGen
  putStrLn "Welcome to Tic-Hask-Toe!"
  putStrLn "Select 0 to play as O (default),"
  putStrLn "Select 1 to play as X,"
  putStrLn "Select 2 to run an AI vs. AI match."
  putStrLn "Press Ctrl-C to quit."
  selection <- getLine
  case selection of
    "0" -> selectionX g (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
    "1" -> selectionO g
    "2" -> aiVai g (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
    _   -> do putStrLn "Cannot parse input. AI will play as X."
              selectionX g (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
  return ()

-- Functions for parameter tuning

tuningMain :: IO ()
tuningMain = do
  args <- getArgs
  if null args
    --then interactiveMain
    then main
    else testingMain

testingMain :: IO ()
testingMain = do
  (scX:scO:_) <- getArgs
  let constX = read scX :: Double
  let constO = read scO :: Double
  g <- R.newStdGen
  aiVaiParamX g constX constO (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
  return ()

aiVaiParamX :: R.StdGen -> Double -> Double -> (Move, Match Token) -> IO ()
aiVaiParamX gen constX constO pair = do
  state@(computerMove,currentBoard) <- getBestMove gen constX pair
  let status = matchStatus state
  newGen <- R.newStdGen
  if status == Tie
    then die "E"
    else  if status == WonBy o
            then die "O"
            else if status == WonBy x
                   then die "X"
                   else aiVaiParamO newGen constX constO state

aiVaiParamO :: R.StdGen -> Double -> Double -> (Move, Match Token) -> IO ()
aiVaiParamO gen constX constO pair = do
  state@(computerMove,currentBoard) <- getBestMove gen constO pair
  let status = matchStatus state
  newGen <- R.newStdGen
  if status == Tie
    then die "E"
    else  if status == WonBy x
            then die "X"
            else if status == WonBy o
                   then die "O"
                   else aiVaiParamX newGen constX constO state