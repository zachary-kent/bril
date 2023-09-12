module Bril.CFG (CFG (..)) where

-- | Described the operations that can be performed on an immutable control flow graph
class CFG a where
  -- | The type of nodes in the CFG
  type Node a

  -- | A list of all nodes in the CFG
  nodes :: a -> [Node a]

  -- | @succs node g@ is a list of all the successors of `node` in `g`
  succs :: Node a -> a -> [Node a]

  -- | @preds node g@ is a list of all the predecessors of `node` in `g`
  preds :: Node a -> a -> [Node a]
