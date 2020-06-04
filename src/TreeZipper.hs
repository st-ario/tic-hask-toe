{-# LANGUAGE TemplateHaskell #-}

module TreeZipper where

import Game
import Rules

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Control.Lens
import           Data.STRef

-- ###################  Monte Carlo Tree Data Structures #######################
type ValueOfNode = Double
type NumberOfVisits = Int
type Weight s = STRef s (ValueOfNode,NumberOfVisits)

data MCNode s = MCN { _lastMove :: !Move
                    , _currentMatch :: !(STRef s (Match Token))
                    , _weight :: !(Weight s)
                    , _isOver :: !Bool
                    , _winner :: !(Maybe Token)
                    }

makeLenses ''MCNode

data MCTree s = MCT { _root :: MCNode s
                    , _sForest :: Vector (MCTree s)
                    }

makeLenses ''MCTree

type Forest s = Vector (MCTree s)

-- Zipper structure on Vector Trees

data Position s = Node { _leftForest :: Forest s -- forest to the left
                       , _rightForest :: Forest s -- forest to the right
                       , _parent :: MCNode s -- label of parent node
                       }

type ZipNode s = (MCTree s, [Position s])

depth :: ZipNode s -> Int
depth (_,p) = length p

stepBack :: ZipNode s -> ZipNode s
stepBack (_,[]) = error "The root has no parent node"
stepBack (t,(p:ps)) = (t',ps)
  where newRoot = _parent p
        lF = _leftForest p
        rF = _rightForest p
        t' = MCT newRoot (lF V.++ (V.singleton t) V.++ rF)

descendTo :: ZipNode s -> Int -> ZipNode s
descendTo (t,ps) n = (t',(Node l r p):ps)
  where sub = _sForest t
        t' = sub ! n
        l = V.take n sub
        r = V.drop (n+1) sub
        p = _root t

allTheWayBack :: ZipNode s -> ZipNode s
allTheWayBack (t,ps) =
  if length ps == 0
    then (t,ps)
    else allTheWayBack $! stepBack (t,ps)

replaceNode :: ZipNode s -> MCNode s -> ZipNode s
replaceNode (t,ps) new = (t',ps)
  where t' = t{ _root = new }