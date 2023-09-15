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
import Bril.Instr (Instr, Label, _Label)
import Bril.Instr qualified as Instr
import Control.Lens (makeLenses, preview, view, (%~))
import Data.Foldable (foldl')
import Data.Function (on)
import Data.IntMap (IntMap, Key)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

-- | A node containing exactly one Bril instruction
data Node = Node
  { -- | The zero-based index of this instruction in the instruction stream
    _index :: Key,
    -- | The instruction wrapped by this node
    _instr :: Instr,
    -- | The indices of the predecessors of this node
    _preds :: IntSet,
    -- | The indices of the successors of this node
    _succs :: IntSet
  }
  deriving (Show)

makeLenses ''Node

instance Eq Node where
  (==) = (==) `on` view index

instance Ord Node where
  compare = compare `on` view index

-- | A mapping from every index in the instruction stream to a CFG
-- node containing the instruction at that index
type NodeMap = IntMap Node

-- | A control flow graph where every node contains exactly one instruction
newtype CFG = CFG NodeMap deriving (Show)

-- | Insert an edge into the CFG
insertEdge :: Key -> Key -> NodeMap -> NodeMap
insertEdge src dst =
  IntMap.adjust (succs %~ IntSet.insert dst) src
    . IntMap.adjust (preds %~ IntSet.insert src) dst

instance IsCFG CFG where
  type NodeOf CFG = Node
  nodes (CFG g) = IntMap.elems g
  successors Node {_succs} (CFG g) = map (g IntMap.!) $ IntSet.toList _succs
  predecessors Node {_preds} (CFG g) = map (g IntMap.!) $ IntSet.toList _preds

-- | Look up the index of a label in the instruction stream
labelToIndex :: [Instr] -> Label -> Key
labelToIndex instrs = (Map.fromList labels Map.!)
  where
    labels = flip mapMaybe (zip instrs [0 ..]) \(inst, idx) ->
      (,idx) <$> preview _Label inst

-- | @forest instrs@ is a CFG with a node for every instruction in @instrs@ but no edges
forest :: [Instr] -> NodeMap
forest instrs =
  IntMap.fromDistinctAscList $
    flip map (zip [0 ..] instrs) \(idx, inst) ->
      let node = Node idx inst IntSet.empty IntSet.empty
       in (idx, node)

-- | Construct a CFG from an instruction stream
fromList :: [Instr] -> CFG
fromList instrs =
  CFG $ foldl' addEdgesforInstr (forest instrs) (zip [0 ..] instrs)
  where
    lookupLabel = labelToIndex instrs
    addEdgesforInstr g (src, inst)
      | Instr.fallsThrough inst && dst < length instrs =
          -- If control flow falls through from this instruction to the next, add an edge
          insertEdge src dst withEdgesForLabels
      | otherwise = withEdgesForLabels
      where
        dst = succ src
        withEdgesForLabels =
          foldl' (\g' l -> insertEdge src (lookupLabel l) g') g $ Instr.labels inst