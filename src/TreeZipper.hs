module TreeZipper where

import           Data.Tree (Tree, Forest)
import qualified Data.Tree as T

-- Zipper structure on Data.Tree

data Position a = Node { _leftForest :: Forest a -- forest to the left
                       , _rightForest :: Forest a -- forest to the right
                       , _parent :: a -- label of parent node
                       } deriving (Show, Eq)

type ZipNode a = (Tree a, [Position a])

depth :: ZipNode a -> Int
depth (_,p) = length p

stepBack :: ZipNode a -> ZipNode a
stepBack (_,[]) = error "The root has no parent node"
stepBack (t,(p:ps)) = (t',ps)
  where newRoot = _parent p
        lF = _leftForest p
        rF = _rightForest p
        t' = T.Node newRoot (lF ++ [t] ++ rF)

descendTo :: ZipNode a -> Int -> ZipNode a
descendTo (t,ps) n = (t',(Node l r p):ps)
  where sub = T.subForest t
        t' = sub !! n
        l = take n sub
        r = drop (n+1) sub
        p = T.rootLabel t

replaceNode :: ZipNode a -> a -> ZipNode a
replaceNode (t,ps) new = (t',ps)
  where t' = T.Node {T.rootLabel=new , T.subForest=(T.subForest t)}

-- apply a function to all children of the current position
replaceSubForestBy :: ZipNode a -> (Tree a -> Tree a) -> ZipNode a
replaceSubForestBy (t,ps) f = (t',ps)
  where newForest = fmap f (T.subForest t)
        t' = T.Node {T.rootLabel=(T.rootLabel t) , T.subForest=newForest}