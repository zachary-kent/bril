module Bril.Optimizations.DCE.Trivial.Global (runOnFunction) where

import Bril.Syntax.Func (BasicBlock (..), Func (..))
import Bril.Syntax.Func qualified as Func
import Bril.Syntax.Instr qualified as Instr
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

runOnBasicBlock :: Set Text -> BasicBlock -> BasicBlock
runOnBasicBlock uses bb@(BasicBlock _ instrs) = bb {instrs = removeDeadInstrs instrs}
  where
    removeDeadInstrs = filter \i ->
      case Instr.def i of
        Just x -> not (Instr.isPure i) || Set.member x uses
        Nothing -> True

runOnFunction :: Func -> Func
runOnFunction func@Func {blocks} =
  if Func.size func' < Func.size func then runOnFunction func' else func
  where
    func' = func {blocks = map (runOnBasicBlock uses) blocks}
    uses = Func.uses func
