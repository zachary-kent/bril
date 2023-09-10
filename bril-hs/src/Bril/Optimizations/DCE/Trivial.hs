module Bril.Optimizations.DCE.Trivial (runOnProgram) where

import Bril.Optimizations.DCE.Trivial.Global qualified as Global
import Bril.Optimizations.DCE.Trivial.Local qualified as Local
import Bril.Syntax.Func (Func)
import Bril.Syntax.Func qualified as Func
import Bril.Syntax.Program (Program (..))

runOnFunction :: Func -> Func
runOnFunction func =
  if Func.size func' < Func.size func
    then runOnFunction func'
    else func'
  where
    func' = Local.runOnFunction $ Global.runOnFunction func

runOnProgram :: Program -> Program
runOnProgram prog@Program {functions} = prog {functions = map runOnFunction functions}
