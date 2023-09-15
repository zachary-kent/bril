module Bril.Dataflow (Dir (..), Params (..), analyze) where

import Algebra.Lattice
import Bril.CFG
import Data.List qualified as List
import Data.Map ((!))
import Data.Map qualified as Map

-- | The direction of an analysis
data Dir = Foward | Backward

-- | Represents the parameters to a dataflow analysis such where an instance of @Params p node@
-- implements the parameters of a dataflow analysis of type `p` over nodes in the CFG of type `node`
class (Eq (Facts p), BoundedMeetSemiLattice (Facts p)) => Params p node where
  -- | The type of facts computed by this analysis
  type Facts p

  -- | The direction of the analysis
  dir :: Dir

  -- | The transfer function
  transfer :: Facts p -> node -> Facts p

-- | Run a dataflow analysis, associating every node in the CFG with corresponding dataflow facts.
-- That is, @analyze g node@ are the dataflow facts associated with node `node` in CFG `g`.
analyze :: forall p g. (IsCFG g, Ord (NodeOf g), Params p (NodeOf g)) => g -> NodeOf g -> (Facts p, Facts p)
analyze g = (go initialFacts (nodes g) !)
  where
    -- Initially, are nodes are associated with the top element of the lattice
    initialFacts = Map.fromList $ map (,(top, top)) $ nodes g
    -- @go facts w@ iteratively computes the dataflow facts associated with every node of `g`
    -- where `w` is a worklist containing the CFG nodes whose equations may not be satisfied
    go facts [] = facts
    go facts (node : w) = go facts' w'
      where
        lookupFacts = snd . (facts !)
        -- The input to the transfer function
        Meet inputFacts = mconcat $ map (Meet . lookupFacts) $ dependencies node g
        -- The output of the transfer function
        outputFacts = transfer @p inputFacts node
        -- The updated dataflow facts after processing this node
        facts' = Map.insert node (inputFacts, outputFacts) facts
        -- The updated worklist after processing this node
        w'
          | outputFacts == lookupFacts node =
              -- dataflow facts computed at this node did not change
              w
          | otherwise =
              -- dataflow facts computed at this node did change
              -- Add the dependents of this node to the worklist
              w `List.union` dependents node g
    (dependencies, dependents) =
      case dir @p @(NodeOf g) of
        Foward -> (predecessors, successors)
        Backward -> (successors, predecessors)