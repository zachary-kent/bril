module Bril.BasicBlock
  ( BasicBlock (..),
    name,
    phiNodes,
    instrs,
    insertPhi,
    defs,
  )
where

import Bril.CFG (ControlFlow (..))
import Bril.Expr (Var)
import Bril.Instr (Instr, Label)
import Bril.Instr qualified as Instr
import Bril.Phi qualified as Phi
import Control.Lens (makeLenses, view, (%~), (^.))
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | Represents a basic block in a Bril program;
-- that is, a sequence of instructions that is executed atomically.
data BasicBlock = BasicBlock
  { -- | The name of the basic block is the name of the label
    _name :: Text,
    -- | The phi nodes at the beginning of this basic block
    _phiNodes :: Map Label Phi.Node,
    -- | The instrs in the basic block
    _instrs :: [Instr]
  }
  deriving (Show)

makeLenses ''BasicBlock

instance Eq BasicBlock where
  (==) = (==) `on` view name

instance Ord BasicBlock where
  compare = compare `on` view name

instance ControlFlow BasicBlock where
  label = Just . view name
  fallsThrough BasicBlock {_instrs} = null _instrs || fallsThrough (last _instrs)
  labels BasicBlock {_instrs}
    | null _instrs = []
    | otherwise = labels (last _instrs)

insertPhi :: Phi.Node -> BasicBlock -> BasicBlock
insertPhi phi = phiNodes %~ Map.insert (phi ^. Phi.dest) phi

defs :: BasicBlock -> Set Var
defs = Set.fromList . mapMaybe Instr.def . view instrs
