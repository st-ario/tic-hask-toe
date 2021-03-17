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
--import           System.Exit (exitSuccess)
import           System.Environment (getArgs)

--clear = "\ESC[2J\ESC[H"
clear = ""
ucbConst = 0.5

emptyGrid = Grid $ V.replicate 9 em
emptyMatch = Match $ Grid $ V.replicate 9 (emptyGrid, Ongoing)

validInputs = replicateM 4 ['0'..'2']

isValid :: String -> Bool
isValid s = s `elem` validInputs

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

testInput' :: String -> Match Token -> IO (Move)
testInput' input match =
  if isValid input
    then parseMove input x
    else do putStrLn "Please enter a move in a valid format"
            newInput <- getLine
            testInput' newInput match

parseMove :: String -> Token -> IO (Move)
parseMove raw token = do
  let rawCoord = fmap digitToInt raw
  let outer = Coord ((rawCoord !! 0),(rawCoord !! 1))
  let inner = Coord ((rawCoord !! 2),(rawCoord !! 3))
  let userMove = Move outer inner token
  return userMove

gameLoop :: R.StdGen -> (Move, Match Token) -> IO ()
gameLoop gen pair = do
  putStrLn "Thinking..."
  state@(computerMove,currentBoard) <- getBestMove gen ucbConst pair
  putStrLn $ clear ++ show currentBoard
  putStrLn $ "AI's last move was"
  putStrLn $ show $ computerMove
  if matchStatus state /= Ongoing 
    then do putStrLn "Game Over"
            exitSuccess
    else return ()
  putStrLn "Please write down your move and press ENTER"
  inputMove <- getLine
  userMove <- testInput inputMove computerMove currentBoard
  putStrLn $ "Your move was " ++ (show userMove)
  let newState = setMatchEl userMove currentBoard
  if matchStatus (userMove, newState) /= Ongoing 
    then do putStrLn $ show newState
            putStrLn $ "Last move was"
            putStrLn $ show $ computerMove
            putStrLn "Game Over"
            exitSuccess
    else return ()
  newGen <- R.newStdGen
  gameLoop newGen (userMove, newState)
  return ()
  -- BUG : if the user wins, the "Last move was" shows the last move done by the AI
  -- (i.e. the penultimate move) and not the winning one, done by the user.
  -- TODO : show the outcome of the match (TIE!, X WINS!, O WINS!) before exiting

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

selectionY :: R.StdGen -> IO ()
selectionY gen = do
  putStrLn "Please write down your move and press ENTER"
  inputMove <- getLine
  userMove <- testInput' inputMove emptyMatch
  putStrLn $ "Your move was " ++ (show userMove)
  let newState = setMatchEl userMove emptyMatch
  gameLoop gen (userMove, newState)

interactiveMain :: IO ()
interactiveMain = do
  putStr clear
  g <- R.newStdGen
  putStrLn "Welcome to Tic-Hask-Toe!"
  putStrLn "Select 0 to play as O (default),"
  putStrLn "Select 1 to play as X,"
  putStrLn "Select 2 to run an AI vs. AI match."
  putStrLn "Press Ctrl-C to quit."
  selection <- getLine
  case selection of
    "0" -> gameLoop g (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
    "1" -> selectionY g
    --"2" -> aiVai g (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
    --"2" -> aiVaiParamX g (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
    _   -> do putStrLn "Cannot parse input. AI will play as X."
              gameLoop g (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
  return ()

testingMain :: IO ()
testingMain = do
  (scX:scO:_) <- getArgs
  let constX = read scX :: Double
  let constO = read scO :: Double
  g <- R.newStdGen
  aiVaiParamX g constX constO (Move (Coord (1,1)) (Coord (1,1)) o, emptyMatch)
  return ()

main :: IO ()
main = do
  args <- getArgs
  if null args
    then interactiveMain
    else testingMain

-- Parametric versions, for parameter tuning

aiVaiParamX :: R.StdGen -> Double -> Double -> (Move, Match Token) -> IO ()
aiVaiParamX gen constX constO pair = do
  --putStrLn "Thinking..."
  state@(computerMove,currentBoard) <- getBestMove gen constX pair
  --putStrLn $ clear ++ show currentBoard
  --putStrLn $ "Last move was"
  --putStrLn $ show $ computerMove
  let status = matchStatus state
  newGen <- R.newStdGen
  if status == Tie
    then die "E"
    else  if status == WonBy o
            then die "O"
            else if status == WonBy x
                   then die "X"
                   else aiVaiParamO newGen constX constO state
  --if matchStatus state /= Ongoing 
  --  then do putStrLn "Game Over"
  --          exitSuccess
  --  else aiVaiParamO newGen state

aiVaiParamO :: R.StdGen -> Double -> Double -> (Move, Match Token) -> IO ()
aiVaiParamO gen constX constO pair = do
  --putStrLn "Thinking..."
  state@(computerMove,currentBoard) <- getBestMove gen constO pair
  --putStrLn $ clear ++ show currentBoard
  --putStrLn $ "Last move was"
  --putStrLn $ show $ computerMove
  let status = matchStatus state
  newGen <- R.newStdGen
  if status == Tie
    then die "E"
    else  if status == WonBy x
            then die "X"
            else if status == WonBy o
                   then die "O"
                   else aiVaiParamX newGen constX constO state
  -- if matchStatus state /= Ongoing 
  --   then do putStrLn "Game Over"
  --           exitSuccess
  --   else aiVaiParamX newGen state
