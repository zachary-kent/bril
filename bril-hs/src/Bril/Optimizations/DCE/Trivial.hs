module Bril.Optimizations.DCE.Trivial (runOnProgram) where

import Bril.Func (Func)
import Bril.Func qualified as Func
import Bril.Optimizations.DCE.Trivial.Global qualified as Global
import Bril.Optimizations.DCE.Trivial.Local qualified as Local
import Bril.Program (Program (..))
import Bril.Program qualified as Program
import Lens.Micro.Platform ((%~))

runOnFunction :: Func -> Func
runOnFunction func =
  if Func.size func' < Func.size func
    then runOnFunction func'
    else func'
  where
    func' = Local.runOnFunction $ Global.runOnFunction func

runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction
