module Bril.SSA where

import Bril.CFG (IsCFG (..), IsNode (..))
import Bril.Dominator (dominators, frontier, tree)
import Bril.Func (vars)
import Data.Foldable (foldl')

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
