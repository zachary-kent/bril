module Bril.BasicBlock
  ( BasicBlock (..),
    name,
    instrs,
    flatten,
  )
where

import Bril.Instr (Instr, SurfaceInstr (..))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Lens.Micro.Platform (makeLenses)

data BasicBlock = BasicBlock
  { _name :: Maybe Text,
    _instrs :: [Instr]
  }
  deriving (Show)

makeLenses ''BasicBlock

flatten :: BasicBlock -> [SurfaceInstr]
flatten BasicBlock {_name, _instrs} =
  maybeToList (Label <$> _name) ++ map Instr _instrs
