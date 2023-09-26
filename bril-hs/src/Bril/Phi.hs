module Bril.Phi (Edge (..), Node (..)) where

import Bril.Expr (Var)
import Bril.Instr (Label)

-- | Represents an argument to a phi node
data Edge = Edge
  { -- | The label of the basic block
    label :: Label,
    -- | The name of the variable
    var :: Var
  }
  deriving (Show)

-- | A phi node
data Node = Node
  { -- | The variable this Phi node writes to
    dest :: Var,
    -- | The arguments to this Phi node
    args :: [Edge]
  }
  deriving (Show)