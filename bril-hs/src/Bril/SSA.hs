module Bril.SSA where

import Bril.CFG ( IsCFG(..), IsNode(..), IsCFG(nodes) )
import Bril.CFG.NodeMap (CFG)
import Bril.CFG.NodeMap qualified as CFG
import Bril.Dominator (dominators, frontier, tree)
import Bril.Func ( vars, Func )
import Data.Foldable (foldl')
import Debug.Trace(traceShow)
import Bril.Program qualified as Program
import Bril.Program (Program)
import Bril.Func qualified as Func
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Lens ( (%~), (.~), (&), view )

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

-- | for rename
-- | Func.hs fromBasicBlocks usage of RunFresh example
-- | runFresh Func.vars
-- |   fresh varname

-- | todo implement
procVar :: (Show var) => CFG a -> var -> CFG a
procVar g v = traceShow ("Processing variable " ++ show v) g

-- | for every Bril function, for every variable in the function, insert phi nodes, accumulate in the CFG with a foldl
runOnFunction :: Func -> Func
runOnFunction func =
  let
    instrs = Func.instrs func
    cfg = CFG.fromList instrs
    cfg' = foldl' procVar cfg (Set.toList $ vars func)
  in
    -- | func & Func.blocks .~ Func.formBasicBlocks (map CFG.value (nodes cfg'))
    func & Func.blocks .~ Func.formBasicBlocks (map (view CFG.value) (nodes cfg'))
    -- | func & Func.blocks .~ Func.formBasicBlocks (map instr (nodes cfg'))

-- | Perform Static Single Assignment rewrite on every function in a program.
-- | todo complete the loop
runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction

-- | test usage in bril-hs
-- | bril2json < ../benchmarks/core/fact.bril | stack run -- --ssa
