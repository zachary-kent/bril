module Bril.CFG.NodeMap
  ( Node,
    CFG,
    index,
    value,
    preds,
    succs,
    fromList,
    insertPhi,
  )
where

import Bril.BasicBlock (BasicBlock)
import Bril.BasicBlock qualified as BB
import Bril.CFG (ControlFlow, DynCFG (..), IsCFG (..), IsNode (..), pruneUnreachable)
import Bril.CFG qualified as CFG
import Bril.Expr (Var)
import Bril.Instr (Label)
import Bril.Phi qualified as Phi
import Control.Lens (makeLenses, view, (%~))
import Data.Foldable (foldl')
import Data.Function (on, (&))
import Data.IntMap (IntMap, Key)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

-- | A node containing exactly one Bril instruction
data Node a = Node
  { -- | The zero-based index of this instruction in the instruction stream
    _index :: Key,
    -- | The instruction wrapped by this node
    _value :: a,
    -- | The indices of the predecessors of this node
    _preds :: IntSet,
    -- | The indices of the successors of this node
    _succs :: IntSet
  }
  deriving (Show)

makeLenses ''Node

instance Eq (Node a) where
  (==) = (==) `on` view index

instance Ord (Node a) where
  compare = compare `on` view index

-- | A mapping from every index in the instruction stream to a CFG
-- node containing the instruction at that index
type NodeMap a = IntMap (Node a)

-- | A control flow graph where every node contains exactly one instruction
newtype CFG a = CFG (NodeMap a) deriving (Show)

-- | Insert an edge into the CFG
insertEdge :: Key -> Key -> NodeMap a -> NodeMap a
insertEdge src dst =
  IntMap.adjust (succs %~ IntSet.insert dst) src
    . IntMap.adjust (preds %~ IntSet.insert src) dst

instance IsNode (Node a) where
  isStart Node {_index} = _index == 0

instance IsCFG (CFG a) where
  type NodeOf (CFG a) = Node a
  nodes (CFG g) = IntMap.elems g
  successors Node {_succs} (CFG g) = map (g IntMap.!) $ IntSet.toList _succs
  predecessors Node {_preds} (CFG g) = map (g IntMap.!) $ IntSet.toList _preds

  start (CFG g) = snd <$> IntMap.lookupMin g

instance DynCFG (CFG a) where
  deleteNode Node {_index, _preds, _succs} (CFG cfg) =
    cfg
      & deleteIncoming
      & deleteOutgoing
      & IntMap.delete _index
      & CFG
    where
      deleteIncoming g =
        IntSet.foldl' (flip (IntMap.adjust (succs %~ IntSet.delete _index))) g _preds
      deleteOutgoing g =
        IntSet.foldl' (flip (IntMap.adjust (preds %~ IntSet.delete _index))) g _succs

-- | Look up the index of a label in the instruction stream
labelToIndex :: (ControlFlow a) => [a] -> Label -> Key
labelToIndex instrs = (Map.fromList labels Map.!)
  where
    labels = flip mapMaybe (zip instrs [0 ..]) \(inst, idx) ->
      (,idx) <$> CFG.label inst

-- | @forest instrs@ is a CFG with a node for every instruction in @instrs@ but no edges
forest :: [a] -> NodeMap a
forest instrs =
  IntMap.fromDistinctAscList $
    flip map (zip [0 ..] instrs) \(idx, inst) ->
      let node = Node idx inst IntSet.empty IntSet.empty
       in (idx, node)

-- | Construct a CFG from an instruction stream
fromList :: (ControlFlow a) => [a] -> CFG a
fromList instrs =
  pruneUnreachable $ CFG $ foldl' addEdgesforInstr (forest instrs) (zip [0 ..] instrs)
  where
    lookupLabel = labelToIndex instrs
    addEdgesforInstr g (src, inst)
      | CFG.fallsThrough inst && dst < length instrs =
          -- If control flow falls through from this instruction to the next, add an edge
          insertEdge src dst withEdgesForLabels
      | otherwise = withEdgesForLabels
      where
        dst = succ src
        withEdgesForLabels =
          foldl' (\g' l -> insertEdge src (lookupLabel l) g') g $ CFG.labels inst

modifyValue :: Node a -> (a -> a) -> CFG a -> CFG a
modifyValue Node {_index} f (CFG g) =
  CFG $ IntMap.adjust (value %~ f) _index g

insertPhi :: Var -> Node BasicBlock -> CFG BasicBlock -> CFG BasicBlock
insertPhi x u g = modifyValue u (BB.insertPhi phi) g
  where
    phi = Phi.create x predecessorLabels
    predecessorLabels =
      -- FIXME: add default labels for basic blocks without labels
      mapMaybe (view (value . BB.name)) (predecessors u g)
