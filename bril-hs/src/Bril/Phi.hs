module Bril.Phi
  ( Edge (..),
    Node (..),
    create,
    label,
    var,
    dest,
    ty,
    args,
  )
where

import Bril.Expr (Var)
import Bril.Instr (Label)
import Bril.Type (Type)
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
    -- | The type of the variable this Phi node writes to
    _ty :: Type,
    -- | The arguments to this Phi node
    _args :: [Edge]
  }
  deriving (Show)

makeLenses ''Node

instance ToJSON Node where
  toJSON Node {_dest, _ty, _args} =
    object
      [ "dest" .= _dest,
        "type" .= _ty,
        "labels" .= map (view label) _args,
        "args" .= map (view var) _args
      ]

create :: Var -> Type -> [Label] -> Node
create x t = Node x t . map (`Edge` x)
