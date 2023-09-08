module Bril.Optimizations.DCE.Trivial (runOnProgram) where

import Bril.Syntax.Func (BasicBlock (..), Func (..))
import Bril.Syntax.Instr (Instr)
import Bril.Syntax.Instr qualified as Instr
import Bril.Syntax.Program (Program (..))
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
      let (is', changed) = removeDeadInstrs [] False is
       in if changed then iterateToConvergence is' else is'
    removeDeadInstrs acc changed [] = (reverse acc, changed)
    removeDeadInstrs acc changed (i : is) =
      case Instr.def i of
        Just def ->
          if not (Instr.isPure i) || defReachesUse def is
            then removeDeadInstrs (i : acc) changed is
            else removeDeadInstrs acc True is
        Nothing -> removeDeadInstrs (i : acc) changed is

runOnFunction :: Func -> Func
runOnFunction func@Func {blocks} = func {blocks = map runOnBasicBlock blocks}

runOnProgram :: Program -> Program
runOnProgram prog@Program {functions} = prog {functions = map runOnFunction functions}