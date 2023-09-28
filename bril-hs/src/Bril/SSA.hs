module Bril.SSA (runOnProgram) where

import Bril.BasicBlock (start)
import Bril.BasicBlock qualified as BB
import Bril.CFG (IsCFG (successors))
import Bril.CFG.NodeMap (CFG)
import Bril.CFG.NodeMap qualified as CFG
import Bril.Dominator qualified as Dom
import Bril.Expr (Var)
import Bril.Fresh (Fresh)
import Bril.Func (BasicBlock, Func)
import Bril.Func qualified as Func
import Bril.Instr (Instr, Label)
import Bril.Instr qualified as Instr
import Bril.Phi qualified as Phi
import Bril.Program (Program)
import Bril.Program qualified as Program
import Control.Lens (view, (%~), (&), (.~))
import Data.Foldable (foldl')
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.State.Static.Local (State, get)

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

-- | stack[v] is a stack of variable names (for every variable v)
--  |def rename(block):
--  |  for instr in block:
--  |    replace each argument to instr with stack[old name]
--  |    replace instr's destination with a new name
--  |    push that new name onto stack[old name]
--  |  for s in block's successors:
--  |    for p in s's ϕ-nodes:
--  |      Assuming p is for a variable v, make it read from stack[v].
--  |  for b in blocks immediately dominated by block:
--  |    # That is, children in the dominance tree.
--  |    rename(b)
--  |  pop all the names we just pushed onto the stacks
--  |rename(entry)
renameInstrs :: (State (Map Var Var) :> es, Fresh :> es) => [Instr] -> Eff es [Instr]
renameInstrs = mapM \instr -> do
  renames <- get
  let renamedInstr = fmap (renames !) instr
  case Instr.def renamedInstr of
    Nothing -> pure ()
    Just dest -> do
      newDest <- fresh dest
      modify (Map.insert dest newDest)
      pure $ setDef newDest renamedInstr

-- \| replaceUse renames l = args %~ Map.update (`Map.lookup` renames) l

-- | for rename
-- | Func.hs fromBasicBlocks usage of RunFresh example
-- | runFresh Func.vars
-- |   fresh varname


-- | todo
renameBlock :: Dom.Tree -> Map.Map Var Var -> CFG.Node Label BasicBlock -> Label -> CFG.Node Label BasicBlock
renameBlock tree varMap cfg block =
  block
-- |  foldl renameBlock tree varMap cfg (relations (
-- |    cfg' = cfg . successors block .~ renamePhiUse newCFG
-- |)) block) {sdom}
  -- |where
-- |    newCFG = findNode block cfg ^. BB.instrs %~ map renameInstrs varMap
-- |    cfg' = cfg . successors block .~ renamePhiUse newCFG


-- | for a Bril function, for every block, recursively rename variables
renameVariables :: Func -> Func
renameVariables func =
  func & Func.blocks .~ CFG.values (renameBlock tree varMap cfg BasicBlock.start)
  where
    cfg = (CFG.fromFunc func)
    tree = Dom.tree cfg
    varMap = Map.fromList Map.fromSet (\x -> x) Func.vars func

-- | for a Bril function, for every variable in the function, insert phi nodes, accumulate in the CFG with a foldl
runOnFunction :: Func -> Func
runOnFunction func =
  func & Func.blocks .~ CFG.values (insertPhis tree func)
  where
    tree = Dom.tree cfg
    cfg = CFG.fromFunc func

-- | Perform Static Single Assignment rewrite on every function in a program.
runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction

-- | test usage in bril-hs
-- | bril2json < ../benchmarks/core/fact.bril | stack run -- --ssa
