{-# LANGUAGE TemplateHaskell #-}

module TreeZipper where

import Game
import Rules

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Control.Lens
import           Data.IORef

-- ###################  Monte Carlo Tree Data Structures #######################
type NumberOfWins = Int
type NumberOfLosses = Int
type NumberOfVisits = Int
type Weight = IORef (NumberOfWins,NumberOfLosses,NumberOfVisits)

data MCNode = MCN { _lastMove :: !Move
                  , _currentMatch :: !(Match Token)
                  , _weight :: !(Weight)
                  , _isOver :: !Bool
                  , _isWon :: !Bool
                  }

makeLenses ''MCNode

data MCTree = MCT { _root :: MCNode
                  , _sForest :: Vector (MCTree)
                  }

makeLenses ''MCTree

type Forest = Vector (MCTree)

-- Zipper structure on Vector Trees

data Position = Node { _leftForest :: Forest -- forest to the left
                     , _rightForest :: Forest -- forest to the right
                     , _parent :: MCNode -- label of parent node
                     }

type ZipNode = (MCTree, [Position])

depth :: ZipNode -> Int
depth (_,p) = length $! p

stepBack :: ZipNode -> ZipNode
stepBack (_,[]) = error "The root has no parent node"
stepBack (t,(p:ps)) = (t',ps)
  where newRoot = _parent $! p
        lF = _leftForest $! p
        rF = _rightForest $! p
        t' = (MCT $! newRoot) $! (lF V.++ (V.singleton $! t) V.++ rF)

destructiveStepBack :: ZipNode -> ZipNode
destructiveStepBack (_,[]) = error "The root has no parent node"
destructiveStepBack (t,(p:ps)) = (t',ps)
  where newRoot = _parent $! p
        t' = (MCT $! newRoot) $! (V.singleton $! t)

descendTo :: ZipNode -> Int -> ZipNode
descendTo (t,ps) n = (t',(((Node $! l)$! r) $! p):ps)
  where sub = _sForest $! t
        t' = sub ! n
        l = V.take n $! sub
        r = V.drop (n+1) $! sub
        p = _root $! t

destructiveDescendTo :: ZipNode -> Int -> ZipNode
destructiveDescendTo (t,ps) n = (t',(((Node $! l)$! r) $! p):ps)
  where sub = _sForest $! t
        t' = sub ! n
        l = V.empty
        r = V.empty
        p = _root $! t

allTheWayBack :: ZipNode -> ZipNode
allTheWayBack (t,ps) =
  if null ps
    then (t,ps)
    else allTheWayBack $! stepBack (t,ps)

destructiveAllTheWayBack :: ZipNode -> ZipNode
destructiveAllTheWayBack (t,ps) =
  if null ps
    then (t,ps)
    else destructiveAllTheWayBack $! destructiveStepBack (t,ps)

replaceNode :: ZipNode -> MCNode -> ZipNode
replaceNode (t,ps) new = (t',ps)
  where t' = t{ _root = new }