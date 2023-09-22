module Bril.CFG (IsNode (..), IsCFG (..), reachable, reachableExcluding) where

import Data.Foldable (foldl')
import Data.Set (Set, (\\))
import Data.Set qualified as Set

-- | Describes a CFG node
class IsNode a where
  -- | Whether this node is the start node in the CFG
  isStart :: a -> Bool

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

  -- | @start g@ is the start node of `g`, if any
  start :: a -> Maybe (NodeOf a)

visit :: (Ord (NodeOf g), IsCFG g) => g -> Set (NodeOf g) -> NodeOf g -> Set (NodeOf g)
visit g visited node
  | node `Set.member` visited = visited
  | otherwise = foldl' (visit g) (Set.insert node visited) $ successors node g

-- | @reachable start cfg@ are the nodes in `cfg` reachable from `start`
reachable :: (Ord (NodeOf g), IsCFG g) => NodeOf g -> g -> Set (NodeOf g)
reachable src g = visit g Set.empty src

reachableExcluding :: (Ord (NodeOf g), IsCFG g) => g -> Set (NodeOf g) -> NodeOf g -> Set (NodeOf g)
reachableExcluding g excluded node = visit g excluded node \\ excluded
