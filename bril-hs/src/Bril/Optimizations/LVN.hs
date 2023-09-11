module Bril.Optimizations.LVN (runOnProgram) where

import Bril.Optimizations.LVN.RenameTable (RenameTable)
import Bril.Optimizations.LVN.RenameTable qualified as RenameTable
import Bril.Optimizations.LVN.Table (Table)
import Bril.Optimizations.LVN.Table qualified as Table
import Bril.Syntax.Func (BasicBlock (..), Func (..))
import Bril.Syntax.Instr (Instr, Instr' (..))
import Bril.Syntax.Instr qualified as Instr
import Bril.Syntax.Program (Program (..))
import Data.Text (Text)
import Effectful
import Effectful.State.Static.Local

renameUses :: (Functor f) => RenameTable -> f Text -> f Text
renameUses renames = fmap (`RenameTable.find` renames)

definedBy :: (Foldable t) => Text -> t (Instr' a) -> Bool
definedBy x = any \i -> Instr.def i == Just x

renameVariables :: [Instr] -> [Instr]
renameVariables instrs = go (RenameTable.fromBasicBlock instrs) instrs
  where
    go _ [] = []
    go renames (i@(Assign x ty e) : is)
      | definedBy x is =
          let e' = renameUses renames e
              (x', renames') = RenameTable.rename x renames
           in Assign x' ty e' : go renames' is
      | otherwise =
          renameUses renames i : go (RenameTable.delete x renames) is
    go renames (i : is) = renameUses renames i : go renames is

canonicalizeUses :: (State Table :> es) => Instr -> Eff es Instr
canonicalizeUses = traverse Table.canonicalHome

runOnInstrs :: (State Table :> es) => [Instr] -> Eff es [Instr]
runOnInstrs [] = pure []
runOnInstrs (Assign x ty e : instrs) = do
  value <- Table.valueFromExpr e
  gets (Table.lookupValue value) >>= \case
    Just (num, canonical) -> do
      Table.setVNForVar x num
      (Assign x ty canonical :) <$> runOnInstrs instrs
    Nothing -> do
      e' <- gets (Table.exprFromValue value)
      Table.insertValue x value
      (Assign x ty e' :) <$> runOnInstrs instrs
runOnInstrs (instr : instrs) = (:) <$> canonicalizeUses instr <*> runOnInstrs instrs

runOnBasicBlock :: BasicBlock -> BasicBlock
runOnBasicBlock bb@BasicBlock {instrs} =
  bb {instrs = runPureEff $ evalState Table.empty $ runOnInstrs $ renameVariables instrs}

runOnFunction :: Func -> Func
runOnFunction func@Func {blocks} = func {blocks = map runOnBasicBlock blocks}

runOnProgram :: Program -> Program
runOnProgram prog@Program {functions} = prog {functions = map runOnFunction functions}