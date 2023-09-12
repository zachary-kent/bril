module Bril.Optimizations.Dataflow (Dir (..), Params (..), analyze) where

import Algebra.Lattice
import Bril.CFG
import Data.List qualified as List
import Data.Map ((!))
import Data.Map qualified as Map

-- | The direction of an analysis
data Dir = Foward | Backward

-- | Represents the parameters to a dataflow analysis such where an instance of `Params p node`
-- implements the parameters of a dataflow analysis of type `p` over nodes in the CFG of type `node
class (Eq (Facts p), BoundedMeetSemiLattice (Facts p)) => Params p node where
  type Facts p
  dir :: Dir
  transfer :: Facts p -> node -> Facts p

-- | Run a dataflow analysis, associating every node in the CFG with corresponding dataflow facts.
-- That is, @analyze g node@ are the dataflow facts associated with node `node` in CFG `g`.
analyze :: forall p g. (CFG g, Ord (Node g), Params p (Node g)) => g -> Node g -> Facts p
analyze g = (go initialFacts [] !)
  where
    -- \| Initially, are nodes are associated with the top element of the lattice
    initialFacts = Map.fromList $ map (,top) $ nodes g
    -- \| @go facts w@ iteratively computes the dataflow facts associated with every node of `g`
    -- \| where `w` is a worklist containing the CFG nodes whose equations may not be satisfied
    go facts [] = facts
    go facts (node : w) =
      let Meet inputFacts = mconcat $ map (Meet . (facts !)) $ dependencies node g
          outputFacts = transfer @p inputFacts node
       in if outputFacts == facts ! node
            then -- dataflow facts computed at this node did not change
              go facts w
            else -- dataflow facts computed at this node did change
            -- Add the dependents of this node to the worklist and continue
              go (Map.insert node outputFacts facts) (w `List.union` dependents node g)
    (dependencies, dependents) =
      case dir @p @(Node g) of
        Foward -> (preds, succs)
        Backward -> (succs, preds)
