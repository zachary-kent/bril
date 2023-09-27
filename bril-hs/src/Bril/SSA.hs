module Bril.SSA where

import Bril.CFG (IsCFG (..), IsNode (..))
import Bril.CFG.NodeMap (CFG)
import Bril.CFG.NodeMap qualified as CFG

import Bril.Dominator (dominators, frontier, tree)
import Bril.Func (vars)
import Data.Foldable (foldl')
import Debug.Trace(traceShow)
import Bril.Program qualified as Program
import Bril.Program (Program)
import Bril.Func qualified as Func
import Bril.Func (Func)
import Data.Set (Set)
import Data.Set qualified as Set


-- | for v in vars, each variable needs phi nodes inserted in the cfg
-- |   input CFG and set of variables to work on
-- |   output updated CFG
-- | processVars :: IsCFG g -> Set (Var v) -> DynCFG g
-- | processVars g =
-- |   foldl processDef vars cfg

-- | map from variables to blocks where they are assigned, must keep it updated

-- | for d in DEFS[var], in blocks where a variable is assigned insert phi nodes in the dominance frontier blocks
-- |   read from mapping of variables to blocks
-- | processDef :: IsCFG g -> (IsNode d,  ) -> IsCFG g
-- | processDef g = go initialDefs (var cfg)
-- |    where
-- |      initialDefs =
-- |
-- |  let df = dominators d in


-- | todo implement
procVar :: CFG -> var -> CFG
procVar g v = do
    traceShow(v)
    return g


runOnFunction :: Func -> Func
runOnFunction func = foldl procVar cfg Func.vars
  where
    instrs = Func.instrs func
    cfg = CFG.fromList instrs

-- | Perform Static Single Assignment rewrite on every function in a program.
-- | todo complete the loop
runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction
