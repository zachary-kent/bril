module Bril.Optimizations.LVN (runOnProgram) where

import Bril.BasicBlock qualified as BB
import Bril.Func (BasicBlock (..), Func (..))
import Bril.Func qualified as Func
import Bril.Instr (Instr, Instr' (..))
import Bril.Instr qualified as Instr
import Bril.Optimizations.LVN.RenameTable (RenameTable)
import Bril.Optimizations.LVN.RenameTable qualified as RenameTable
import Bril.Optimizations.LVN.Table (Table)
import Bril.Optimizations.LVN.Table qualified as Table
import Bril.Program (Program (..))
import Bril.Program qualified as Program
import Data.Text (Text)
import Effectful
import Effectful.State.Static.Local
import Lens.Micro.Platform

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
runOnBasicBlock = BB.instrs %~ runPureEff . evalState Table.empty . runOnInstrs . renameVariables

runOnFunction :: Func -> Func
runOnFunction = Func.blocks %~ map runOnBasicBlock

runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction
