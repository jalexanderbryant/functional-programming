module TreeLabelWithoutStateMonad where

import Store
import qualified Data.Map.Lazy as Map
import Data.Maybe



-- label element

labelValue :: Ord a => a -> (Store a Int) -> (Int, Store a Int)
labelValue uKey store
  | (lookupKeyInStore uKey store) == True = (val, store)
  | otherwise = (nextLargest, newStore)
    where val = fromJust( (lookupStore uKey store) )
          nextLargest = (largestValInStore store) + 1
          newStore = (insertStore uKey nextLargest store)


largestValInStore :: Ord a => (Store a Int) -> Int
largestValInStore (Store sto)
  | Map.null sto == True = -1
  | otherwise         = maximum $ Map.elems sto

lookupKeyInStore :: Ord a => a -> (Store a Int) -> Bool
lookupKeyInStore key store
  | Nothing /= (lookupStore key store)  = True 
  | otherwise  =  False


-- label tree

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> (Store a Int) -> (Tree Int, Store a Int)
labelTree Nil ls = (Nil, ls)
  
labelTree (Node val left right) ls =
  (Node labeledValue labeledLeft labeledRight, ls''')
    where (labeledValue, ls')   = labelValue val   ls
          (labeledLeft,  ls'')  = labelTree  left  ls'
          (labeledRight, ls''') = labelTree  right ls''

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ labelTree tree emptyStore
