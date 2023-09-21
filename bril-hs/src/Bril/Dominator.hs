module Bril.Dominator (dominators, Tree, tree, frontier) where

import Bril.CFG (IsCFG (..), IsNode (..), reachable)
import Bril.Dataflow (Dir (..), Params (..))
import Bril.Dataflow qualified as Dataflow
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Tree qualified as Tree
import Effectful (runPureEff)
import Effectful.State.Static.Local

transfer :: (Ord node, IsNode node) => Set node -> node -> Set node
transfer facts node
  | isStart node = Set.singleton node
  | otherwise = Set.insert node facts

-- | The Facts computed by the dominator analysis; sets of nodes
type Facts g = Set (NodeOf g)

params :: forall g. (IsCFG g, Ord (NodeOf g), IsNode (NodeOf g)) => Params (Facts g) g
params =
  Params
    { dir = Forward,
      top = Set.fromList . nodes,
      meet = Set.intersection,
      transfer
    }

-- | @dominators cfg node@ is the set of all nodes in `cfg` that dominate `node`
dominators :: (IsCFG g, Ord (NodeOf g), IsNode (NodeOf g)) => g -> NodeOf g -> Facts g
dominators g = snd . Dataflow.analyze params g

-- | A dominance tree, where A is the direct parent of B iff A immediately dominates B
type Tree g = Maybe (Tree.Tree (NodeOf g))

-- | @tree cfg@ is the dominance tree associated with `cfg`
tree :: (IsCFG g, Ord (NodeOf g), IsNode (NodeOf g)) => g -> Tree g
tree g = buildFromRoot <$> start g
  where
    doms = dominators g
    strictDominators b = Set.delete b $ doms b
    buildFromRoot root = go root
      where
        reachableFromStart = reachable root g
        dominatedBy = (foldl' updateForNode initialMap reachableFromStart !)
          where
            updateForNode m dst =
              dst
                & doms -- All nodes `src` such that `src dom dst`
                & foldl' (flip $ Map.adjust $ Set.insert dst) m
            initialMap =
              reachableFromStart
                & Set.toAscList
                & map (,Set.empty)
                & Map.fromAscList
        strictlyDominatedBy src = Set.delete src $ dominatedBy src
        go node =
          Tree.Node
            { rootLabel = node,
              subForest =
                dominatedBy node
                  & Set.toList
                  & filter (\child -> strictlyDominated `Set.disjoint` strictDominators child)
                  & map go
            }
          where
            strictlyDominated = strictlyDominatedBy node

-- | @frontier node cfg@ are the nodes in `cfg` on the dominance frontier of `node`
frontier :: (Ord (NodeOf g), IsCFG g) => Tree g -> g -> NodeOf g -> Set (NodeOf g)
frontier root g =
  case root of
    Nothing -> const $ error "frontier: looking up node of empty graph"
    Just node -> (runPureEff (execState Map.empty $ go node) !)
  where
    go Tree.Node {rootLabel, subForest} = do
      (childDoms, childFrontiers) <- unzip <$> traverse go subForest
      let doms = Set.insert rootLabel $ Set.unions childDoms
          succs = Set.fromList $ successors rootLabel g
          rootFrontier = (succs `Set.union` Set.unions childFrontiers) \\ doms
      modify (Map.insert rootLabel rootFrontier)
      pure (doms, rootFrontier)
