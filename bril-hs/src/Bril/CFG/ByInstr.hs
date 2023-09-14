module Bril.CFG.ByInstr
  ( Node,
    index,
    instr,
    preds,
    succs,
    fromList,
  )
where

import Bril.CFG (IsCFG (..))
import Bril.Instr (Label, SurfaceInstr (..))
import Bril.Instr qualified as Instr
import Data.Foldable (foldl')
import Data.Function (on)
import Data.IntMap (IntMap, Key)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Lens.Micro.Platform

data Node = Node
  { _index :: Key,
    _instr :: SurfaceInstr,
    _preds :: IntSet,
    _succs :: IntSet
  }
  deriving (Show)

makeLenses ''Node

instance Eq Node where
  (==) = (==) `on` view index

instance Ord Node where
  compare = compare `on` view index

type NodeMap = IntMap Node

newtype CFG = CFG NodeMap deriving (Show)

insertEdge :: Key -> Key -> NodeMap -> NodeMap
insertEdge src dst =
  IntMap.adjust (succs %~ IntSet.insert dst) src
    . IntMap.adjust (preds %~ IntSet.insert src) dst

instance IsCFG CFG where
  type NodeOf CFG = Node
  nodes (CFG g) = IntMap.elems g
  successors Node {_succs} (CFG g) = map (g IntMap.!) $ IntSet.toList _succs
  predecessors Node {_preds} (CFG g) = map (g IntMap.!) $ IntSet.toList _preds

labelToKey :: [SurfaceInstr] -> Label -> Key
labelToKey instrs = (Map.fromList labels Map.!)
  where
    labels = flip mapMaybe (zip instrs [0 ..]) \(inst, idx) ->
      case inst of
        Label l -> pure (l, idx)
        Instr _ -> Nothing

forest :: [SurfaceInstr] -> NodeMap
forest instrs =
  IntMap.fromDistinctAscList $
    flip map (zip [0 ..] instrs) \(idx, inst) ->
      let node = Node idx inst IntSet.empty IntSet.empty
       in (idx, node)

fromList :: [SurfaceInstr] -> CFG
fromList instrs =
  CFG $ foldl' addEdgesforInstr (forest instrs) (zip [0 ..] instrs)
  where
    lookupLabel = labelToKey instrs
    addEdgesforInstr g (src, inst)
      | dst < length instrs = insertEdge src dst withEdgesForLabels
      | otherwise = withEdgesForLabels
      where
        dst = succ src
        withEdgesForLabels =
          foldl' (\g' l -> insertEdge src (lookupLabel l) g') g $ Instr.labels inst
