module Bril.BasicBlock
  ( BasicBlock (..),
    name,
    phiNodes,
    instrs,
    insertPhi,
    renamePhiUses,
    defs,
    start,
  )
where

import Bril.CFG (ControlFlow (..))
import Bril.Expr (Var)
import Bril.Instr (Instr, Label, Instr' (Label))
import Bril.Instr qualified as Instr
import Bril.Phi qualified as Phi
import Control.Lens (makeLenses, view, (%~), (^.), views)
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Effectful.State.Static.Local
import Effectful
import Bril.Fresh


-- | Represents a basic block in a Bril program;
-- that is, a sequence of instructions that is executed atomically.
data BasicBlock = BasicBlock
  { -- | The name of the basic block is the name of the label
    _name :: Text,
    -- | The phi nodes at the beginning of this basic block
    _phiNodes :: Map Var Phi.Node,
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

start :: BasicBlock
start = BasicBlock "__start" Map.empty [Label ".__start"]

insertPhi :: Phi.Node -> BasicBlock -> BasicBlock
insertPhi phi = phiNodes %~ Map.insert (phi ^. Phi.dest) phi

renamePhiUses :: Map Var Var -> Label -> BasicBlock -> BasicBlock
renamePhiUses renamings predLabel = phiNodes %~ Map.map (Phi.replaceUse renamings predLabel)

renamePhiDest :: (State (Map Var Var) :> es, Fresh :> es) => BasicBlock -> Eff es BasicBlock
renamePhiDest bb = do
  _phiNodes <- mapM (views phiNodes  Map.toList bb) \(dest, phi) -> do
    newDest <- fresh dest
    modify (Map.insert dest newDest)
  error ""


defs :: BasicBlock -> Set Var
defs = Set.fromList . mapMaybe Instr.def . view instrs
