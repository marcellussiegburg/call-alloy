module ReadmeExample (
  Node (Node),
  instanceToNames,
  ) where

import Control.Monad.Catch              (MonadThrow)
import Data.Set                         (Set)

import Language.Alloy.Call (
  AlloyInstance,
  getDoubleAs,
  getSingleAs,
  getTripleAs,
  int,
  lookupSig,
  object,
  scoped,
  unscoped,
  )
newtype Node = Node Int deriving (Eq, Show, Ord, Read)

instanceToNames
  :: MonadThrow m
  => AlloyInstance
  -> m (Set Node, Set (Node, Int), Set (Node, Node, Int), Set Node, Set Node)
instanceToNames insta = do
  let node :: MonadThrow m => String -> Int -> m Node
      node = object "Node" Node
  n     <- lookupSig (scoped "this" "Node") insta
  nodes <- getSingleAs "" node n
  store <- getDoubleAs "stored" node int n
  flow  <- getTripleAs "flow" node node int n
  x     <- lookupSig (unscoped "$withFlow_x") insta >>= getSingleAs "" node
  y     <- lookupSig (unscoped "$withFlow_y") insta >>= getSingleAs "" node
  return (nodes, store, flow, x, y)
