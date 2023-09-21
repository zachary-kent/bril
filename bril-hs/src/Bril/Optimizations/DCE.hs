module Bril.Optimizations.DCE (runOnProgram) where

import Bril.CFG (IsCFG (nodes))
import Bril.CFG.ByInstr (CFG)
import Bril.CFG.ByInstr qualified as CFG
import Bril.Dataflow (Union (..))
import Bril.Dataflow qualified as Dataflow
import Bril.Expr (Var)
import Bril.Func (Func)
import Bril.Func qualified as Func
import Bril.Instr (Instr)
import Bril.Instr qualified as Instr
import Bril.Program (Program)
import Bril.Program qualified as Program
import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

-- | An uninhabited type used as a tag for liveness analysis
data Liveness

instance Dataflow.Params Liveness CFG where
  type Facts Liveness = Dataflow.Union Var
  dir = Dataflow.Backward
  transfer (Union liveOut) node =
    Union $ case Instr.def instr of
      Just x ->
        if x `Set.member` liveOut || not (Instr.isPure instr)
          then {- If the variable defined by this instruction is live-out
               or this instruction is impure, this instruction cannot
               (yet) be deleted. So, all its uses are live in.
               Further, all the variables that are live out, minus the
               one defined by this instruction, are also live in -}
            uses `Set.union` Set.delete x liveOut
          else -- This instruction will be deleted, so its uses are not live in
            liveOut
      Nothing -> uses `Set.union` liveOut
    where
      instr = node ^. CFG.instr
      uses = Set.fromList (Instr.uses instr)

-- | @isLive liveOut instr@ is @True@ iff @instr@
-- cannot be deleted; that is, it is live given
-- the set `liveOut` of variables live out at `instr`
isLive :: Union Var -> Instr -> Bool
isLive (Union liveOut) instr
  | not (Instr.isPure instr) =
      -- We cannot delete this instruction if it has side effects
      True
  | otherwise =
      case Instr.def instr of
        Nothing -> False
        Just x -> x `Set.member` liveOut

runOnFunction :: Func -> Func
runOnFunction func = func & Func.blocks .~ blocks
  where
    blocks = Func.formBasicBlock $ mapMaybe removeDeadInstr $ nodes cfg
    removeDeadInstr :: CFG.Node -> Maybe Instr
    removeDeadInstr node = do
      let (liveOut, _) = facts node
          instr = node ^. CFG.instr
      guard $ isLive liveOut instr
      pure instr
    instrs = Func.instrs func
    cfg = CFG.fromList instrs
    facts = Dataflow.analyze @Liveness cfg

runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction
