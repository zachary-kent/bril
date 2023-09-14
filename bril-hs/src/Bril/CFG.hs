module Bril.CFG (IsCFG (..)) where

-- | Described the operations that can be performed on an immutable control flow graph
class IsCFG a where
  -- | The type of nodes in the CFG
  type NodeOf a

  -- | A list of all nodes in the CFG
  nodes :: a -> [NodeOf a]

  -- | @succs node g@ is a list of all the successors of `node` in `g`
  successors :: NodeOf a -> a -> [NodeOf a]

  -- | @preds node g@ is a list of all the predecessors of `node` in `g`
  predecessors :: NodeOf a -> a -> [NodeOf a]
