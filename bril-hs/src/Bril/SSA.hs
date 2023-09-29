module Bril.SSA (runOnProgram) where

import Bril.BasicBlock qualified as BB
import Bril.CFG.NodeMap (CFG)
import Bril.CFG.NodeMap qualified as CFG
import Bril.Dominator qualified as Dom
import Bril.Expr (Var)
import Bril.Fresh (Fresh, fresh, runFreshWithPostfix)
import Bril.Func (BasicBlock, Func)
import Bril.Func qualified as Func
import Bril.Instr (Instr, Label, setDef)
import Bril.Instr qualified as Instr
import Bril.Phi qualified as Phi
import Bril.Program (Program)
import Bril.Program qualified as Program
import Control.Lens (view, views, (%~), (&), (.~), (^.))
import Control.Monad (foldM, forM)
import Data.Foldable (foldl')
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.State.Static.Local (State, get, modify, runState)

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
    Nothing -> pure renamedInstr
    Just dest -> do
      newDest <- fresh dest
      modify (Map.insert dest newDest)
      pure $ setDef newDest renamedInstr

renamePhiDest :: (State (Map Var Var) :> es, Fresh :> es) => BasicBlock -> Eff es (Map Var Phi.Node)
renamePhiDest bb =
  Map.fromList <$> forM (views BB.phiNodes Map.toList bb) \(dest, phi) -> do
    newDest <- fresh dest
    modify (Map.insert dest newDest)
    pure (newDest, phi & Phi.dest .~ newDest)

rename :: (Fresh :> es) => Map Var Var -> CFG Label BasicBlock -> Dom.Node (CFG.Node Label BasicBlock) -> Eff es (CFG Label BasicBlock)
rename renames cfg Dom.Node {node, children} = do
  (block, renames') <- runState renames renameBlock
  let withRewrittenBlock = CFG.setValue label block cfg
      succs = cfgNode ^. CFG.succs
      withRewrittenSuccs = foldl' (\acc dst -> CFG.modifyValue dst (BB.renamePhiUses renames' label) acc) withRewrittenBlock succs
  foldM (rename renames') withRewrittenSuccs children
  where
    label = node ^. CFG.index
    cfgNode = CFG.findNode label cfg
    renameBlock :: (State (Map Var Var) :> es, Fresh :> es) => Eff es BasicBlock
    renameBlock = do
      let block = cfgNode ^. CFG.value
      phiNodes <- renamePhiDest block
      instrs <- renameInstrs (block ^. BB.instrs)
      pure (block & (BB.phiNodes .~ phiNodes) . (BB.instrs .~ instrs))

-- \| replaceUse renames l = args %~ Map.update (`Map.lookup` renames) l

-- | for rename
-- | Func.hs fromBasicBlocks usage of RunFresh example
-- | runFresh Func.vars
-- |   fresh varname

-- | todo
-- renameBlock :: Dom.Tree -> Map.Map Var Var -> CFG.Node Label BasicBlock -> Label -> CFG.Node Label BasicBlock
-- renameBlock tree varMap cfg block =
--   block

-- |  foldl renameBlock tree varMap cfg (relations (
-- |    cfg' = cfg . successors block .~ renamePhiUse newCFG
-- |)) block) {sdom}
-- |where
-- |    newCFG = findNode block cfg ^. BB.instrs %~ map renameInstrs varMap
-- |    cfg' = cfg . successors block .~ renamePhiUse newCFG

-- | for a Bril function, for every block, recursively rename variables
-- renameVariables :: Func -> Func
-- renameVariables func =
--   func & Func.blocks .~ CFG.values (renameBlock tree varMap cfg BasicBlock.start)
--   where
--     cfg = (CFG.fromFunc func)
--     tree = Dom.tree cfg
--     varMap = Map.fromList Map.fromSet (\x -> x) Func.vars func

-- | for a Bril function, for every variable in the function, insert phi nodes, accumulate in the CFG with a foldl
runOnFunction :: Func -> Func
runOnFunction func =
  func & Func.blocks .~ CFG.values (CFG.removePhis $ renameCFG $ insertPhis tree func)
  where
    tree = Dom.tree initialCFG
    initialCFG = CFG.fromFunc func
    paramRenames = Map.fromList $ map (\arg -> (arg, arg)) $ Func.argNames func
    renameCFG cfg =
      case tree of
        Dom.Empty -> cfg
        Dom.Root node ->
          runPureEff $
            runFreshWithPostfix "." (Func.defs func) $
              rename paramRenames cfg node

-- | Perform Static Single Assignment rewrite on every function in a program.
runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction

-- | test usage in bril-hs
-- | bril2json < ../benchmarks/core/fact.bril | stack run -- --ssa
