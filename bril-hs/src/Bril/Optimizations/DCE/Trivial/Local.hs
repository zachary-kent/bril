module Bril.Optimizations.DCE.Trivial.Local (runOnFunction) where

import Bril.Syntax.Func (BasicBlock (..), Func (..))
import Bril.Syntax.Instr (Instr)
import Bril.Syntax.Instr qualified as Instr
import Data.Text (Text)

defReachesUse :: Text -> [Instr] -> Bool
defReachesUse _ [] = True
defReachesUse def (i : is)
  | def `elem` Instr.uses i = True
  | Instr.def i == Just def = False
  | otherwise = defReachesUse def is

runOnBasicBlock :: BasicBlock -> BasicBlock
runOnBasicBlock bb@(BasicBlock _ instrs) = bb {instrs = iterateToConvergence instrs}
  where
    iterateToConvergence is =
      let is' = removeDeadInstrs is
       in if length is' < length is then iterateToConvergence is' else is'
    removeDeadInstrs [] = []
    removeDeadInstrs (i : is) =
      case Instr.def i of
        Just def ->
          if not (Instr.isPure i) || defReachesUse def is
            then i : removeDeadInstrs is
            else removeDeadInstrs is
        Nothing -> i : removeDeadInstrs is

runOnFunction :: Func -> Func
runOnFunction func@Func {blocks} = func {blocks = map runOnBasicBlock blocks}
