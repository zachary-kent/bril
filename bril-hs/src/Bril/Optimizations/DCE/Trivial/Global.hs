module Bril.Optimizations.DCE.Trivial.Global (runOnFunction) where

import Bril.BasicBlock qualified as BB
import Bril.Func (BasicBlock (..), Func (..))
import Bril.Func qualified as Func
import Bril.Instr qualified as Instr
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Lens.Micro.Platform ((%~), (&))

runOnBasicBlock :: Set Text -> BasicBlock -> BasicBlock
runOnBasicBlock uses = BB.instrs %~ removeDeadInstrs
  where
    removeDeadInstrs = filter \i ->
      case Instr.def i of
        Just x -> not (Instr.isPure i) || Set.member x uses
        Nothing -> True

runOnFunction :: Func -> Func
runOnFunction func =
  if Func.size func' < Func.size func then runOnFunction func' else func
  where
    func' = func & Func.blocks %~ map (runOnBasicBlock uses)
    uses = Func.uses func
