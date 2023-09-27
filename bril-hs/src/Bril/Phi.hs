module Bril.Phi
  ( Edge (..),
    Node (..),
    create,
    label,
    var,
    dest,
    args,
  )
where

import Bril.Expr (Var)
import Bril.Instr (Label)
import Control.Lens hiding ((.=))
import Data.Aeson

-- | Represents an argument to a phi node
data Edge = Edge
  { -- | The label of the basic block
    _label :: Label,
    -- | The name of the variable
    _var :: Var
  }
  deriving (Show)

makeLenses ''Edge

-- | A phi node
data Node = Node
  { -- | The variable this Phi node writes to
    _dest :: Var,
    -- | The arguments to this Phi node
    _args :: [Edge]
  }
  deriving (Show)

makeLenses ''Node

instance ToJSON Node where
  toJSON Node {_dest, _args} =
    object
      [ "dest" .= _dest,
        "labels" .= map (view label) _args,
        "args" .= map (view var) _args
      ]

create :: Var -> [Label] -> Node
create x = Node x . map (`Edge` x)
