module Bril.Optimizations.LVN (runOnProgram) where

import Bril.Optimizations.LVN.RenameTable qualified as RenameTable
import Bril.Syntax.Func (BasicBlock (..), Func (..))
import Bril.Syntax.Instr (Instr' (..))
import Bril.Syntax.Instr qualified as Instr
import Bril.Syntax.Program (Program (..))
import Data.Text (Text)

replaceUses :: (Functor f) => RenameTable.RenameTable -> f Text -> f Text
replaceUses renames = fmap (`RenameTable.find` renames)

definedBy :: (Foldable t) => Text -> t (Instr' a) -> Bool
definedBy x = any \i -> Instr.def i == Just x

renameVariables :: BasicBlock -> BasicBlock
renameVariables block@BasicBlock {instrs} =
  block {instrs = go (RenameTable.fromBasicBlock instrs) instrs}
  where
    go _ [] = []
    go renames (i@(Assign x ty e) : is)
      | definedBy x is =
          let e' = replaceUses renames e
              (x', renames') = RenameTable.rename x renames
           in Assign x' ty e' : go renames' is
      | otherwise =
          replaceUses renames i : go (RenameTable.delete x renames) is
    go renames (i : is) = replaceUses renames i : go renames is

runOnFunction :: Func -> Func
runOnFunction func@Func {blocks} = func {blocks = map renameVariables blocks}

runOnProgram :: Program -> Program
runOnProgram prog@Program {functions} = prog {functions = map runOnFunction functions}

-- runOnInstrs :: (LVN :> es) => [Instr] -> Eff es [Instr]
-- runOnInstrs [] = pure []
-- runOnInstrs (Assign x ty e : instrs) = do
--   value <- toValueTuple e
--   lookupValue value >>= \case
--     Just (_, var) ->
--       let instr' = Assign x ty (Id var)
--        in (instr' :) <$> runOnInstrs instrs
--     Nothing -> do
--       num <- freshNum
--       error ""
-- runOnInstrs (instr : instrs) = (instr :) <$> runOnInstrs instrs
