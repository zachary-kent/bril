module Bril.Optimizations.DCE (runOnProgram) where

import Algebra.Lattice
import Bril.CFG (IsCFG (nodes))
import Bril.CFG.ByInstr qualified as CFG
import Bril.Dataflow qualified as Dataflow
import Bril.Expr (Var)
import Bril.Func (Func)
import Bril.Func qualified as Func
import Bril.Instr (SurfaceInstr (..))
import Bril.Instr qualified as Instr
import Bril.Program (Program)
import Bril.Program qualified as Program
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro.Platform

newtype LiveVars = LiveVars (Set Var)
  deriving (Semigroup, Monoid, Eq, Show)

instance Lattice LiveVars where
  LiveVars xs \/ LiveVars ys = LiveVars $ xs `Set.intersection` ys
  LiveVars xs /\ LiveVars ys = LiveVars $ xs `Set.union` ys

instance BoundedMeetSemiLattice LiveVars where
  top = LiveVars mempty

data DCE

instance Dataflow.Params DCE CFG.Node where
  type Facts DCE = LiveVars
  dir = Dataflow.Backward
  transfer (LiveVars liveOut) node =
    LiveVars $
      case node ^. CFG.instr of
        Label _ -> liveOut
        Instr instr ->
          let uses = Set.fromList (Instr.uses instr)
           in case Instr.def instr of
                Just x ->
                  if x `Set.member` liveOut || not (Instr.isPure instr)
                    then uses `Set.union` Set.delete x liveOut
                    else liveOut
                Nothing -> uses `Set.union` liveOut

isLive :: LiveVars -> SurfaceInstr -> Bool
isLive _ (Label _) = True
isLive (LiveVars liveOut) (Instr instr)
  | not (Instr.isPure instr) = True
  | otherwise =
      case Instr.def instr of
        Nothing -> False
        Just x -> x `Set.member` liveOut

runOnFunction :: Func -> Func
runOnFunction func = func & Func.blocks .~ blocks
  where
    blocks = Func.formBasicBlock $ mapMaybe removeDeadInstr $ nodes cfg
    removeDeadInstr :: CFG.Node -> Maybe SurfaceInstr
    removeDeadInstr node = do
      let (liveOut, _) = facts node
          instr = node ^. CFG.instr
      guard $ isLive liveOut instr
      pure instr
    instrs = Func.flatten func
    cfg = CFG.fromList instrs
    facts = Dataflow.analyze @DCE cfg

runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction
