module Bril.BasicBlock
  ( BasicBlock (..),
    name,
    phiNodes,
    instrs,
    insertPhi,
  )
where

import Bril.CFG (ControlFlow (..))
import Bril.Instr (Instr, Label)
import Bril.Phi qualified as Phi
import Control.Lens (makeLenses, view, (%~), (^.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

-- | Represents a basic block in a Bril program;
-- that is, a sequence of instructions that is executed atomically.
data BasicBlock = BasicBlock
  { -- | The name of the basic block is the name of the label, if any.
    _name :: Maybe Text,
    -- | The phi nodes at the beginning of this basic block
    _phiNodes :: Map Label Phi.Node,
    -- | The instrs in the basic block
    _instrs :: [Instr]
  }
  deriving (Show)

makeLenses ''BasicBlock

instance ControlFlow BasicBlock where
  label = view name
  fallsThrough BasicBlock {_instrs} = null _instrs || fallsThrough (last _instrs)
  labels BasicBlock {_instrs}
    | null _instrs = []
    | otherwise = labels (last _instrs)

insertPhi :: Phi.Node -> BasicBlock -> BasicBlock
insertPhi phi = phiNodes %~ Map.insert (phi ^. Phi.dest) phi
