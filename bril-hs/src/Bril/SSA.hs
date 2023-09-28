module Bril.SSA where

import Bril.BasicBlock qualified as BB
import Bril.CFG.NodeMap (CFG)
import Bril.CFG.NodeMap qualified as CFG
import Bril.Dominator qualified as Dom
import Bril.Func (BasicBlock, Func)
import Bril.Func qualified as Func
import Bril.Instr (Label)
import Control.Lens (view)
import Data.Foldable (foldl')
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

insertPhis :: Dom.Tree (CFG.Node Label BasicBlock) -> Func -> CFG Label BasicBlock
insertPhis tree func =
  foldl'
    (\cfg x -> go cfg x Set.empty $ Set.toList $ defs ! x)
    (CFG.fromFunc func)
    vars
  where
    defs = Func.defsOf func
    vars = Map.keys defs
    initialCFG = CFG.fromFunc func
    frontierMap = Dom.frontier @(CFG Label BasicBlock) tree initialCFG
    go cfg _ _ [] = cfg
    go cfg x processed (d : ds)
      | d `Set.member` processed = go cfg x processed ds
      | otherwise =
          go cfg' x (Set.insert d processed) (Set.toList frontier ++ ds)
      where
        cfg' = foldl' (flip (CFG.insertPhi x)) cfg frontier
        frontier :: Set Label
        frontier = Set.map (view BB.name . view CFG.value) $ frontierMap ! CFG.findNode d initialCFG
