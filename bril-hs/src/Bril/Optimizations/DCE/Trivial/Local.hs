module Bril.Optimizations.DCE.Trivial.Local (runOnFunction) where

import Bril.BasicBlock qualified as BB
import Bril.Func (BasicBlock (..), Func (..))
import Bril.Func qualified as Func
import Bril.Instr (Instr)
import Bril.Instr qualified as Instr
import Data.Text (Text)
import Lens.Micro.Platform ((%~))

defReachesUse :: Text -> [Instr] -> Bool
defReachesUse _ [] = True
defReachesUse def (i : is)
  | def `elem` Instr.uses i = True
  | Instr.def i == Just def = False
  | otherwise = defReachesUse def is

runOnBasicBlock :: BasicBlock -> BasicBlock
runOnBasicBlock = BB.instrs %~ iterateToConvergence
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
runOnFunction = Func.blocks %~ map runOnBasicBlock
