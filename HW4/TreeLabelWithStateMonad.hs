module TreeLabelWithStateMonad where
import Store
import Control.Monad.State
import Data.Maybe
import qualified Data.Map.Lazy as Map

---------------------------------------------------------
labelValue :: Ord a => a -> State (Store a Int) Int
labelValue val = do
  ss <- get   -- get state
  let ns = if (inStore /= Nothing)  -- is key,value pair in store?
                then ss             -- Yes? newstore = current store
                else (insertStore val nl ss) -- other wise, insert k v pair, new store will be result
                where inStore = lookupStore val ss
                      nl = ((largestValInStore ss) + 1)
      res  = fromJust( (lookupStore val ns) )
  put ns      -- put state 
  return res  -- Return result


largestValInStore :: Ord a => (Store a Int) -> Int
largestValInStore (Store sto)
  | Map.null sto == True = -1
  | otherwise         = maximum $ Map.elems sto
---------------------------------------------------------



-- label tree---

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> State (Store a Int) (Tree Int)
labelTree Nil = do
  return Nil
labelTree (Node val left right) = do
  labeledValue <- labelValue val
  labeledLeft  <- labelTree left
  labeledRight <- labelTree right
  return (Node labeledValue labeledLeft labeledRight)

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ runState (labelTree tree) (emptyStore)
