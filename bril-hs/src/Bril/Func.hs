module Bril.Func
  ( BasicBlock (..),
    Func (..),
    name,
    args,
    ty,
    blocks,
    instrs,
    formBasicBlock,
    Arg (..),
    uses,
    size,
  )
where

import Bril.BasicBlock (BasicBlock (..))
import Bril.BasicBlock qualified as BB
import Bril.Expr (Var)
import Bril.Instr (Instr, Instr' (..), isTerminator)
import Bril.Instr qualified as Instr
import Bril.Type (Type (..))
import Control.Arrow ((>>>))
import Control.Lens (makeLenses, view)
import Data.Aeson
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | Split an instruction stream into basic blocks
splitAtTerminators :: [Instr] -> [[Instr]]
splitAtTerminators = filter (not . null) . go []
  where
    go curr [] = [reverse curr]
    go curr (lbl@(Label _) : is) = reverse curr : go [lbl] is
    go curr (instr : is)
      | isTerminator instr = reverse (instr : curr) : go [] is
      | otherwise = go (instr : curr) is

-- | Form a sequence of basic blocks from a sequence of instructions
formBasicBlock :: [Instr] -> [BasicBlock]
formBasicBlock =
  splitAtTerminators >>> map \case
    is@(Label l : _) -> BasicBlock (Just l) is
    is -> BasicBlock Nothing is

-- | An argument to a function
data Arg = Arg
  { name :: Text,
    ty :: Type
  }
  deriving (Show)

instance ToJSON Arg where
  toJSON (Arg name ty) =
    object ["name" .= name, "type" .= ty]

instance FromJSON Arg where
  parseJSON = withObject "arg" \obj ->
    Arg <$> obj .: "name" <*> obj .: "type"

-- | A Bril function
data Func = Func
  { _name :: Text,
    _args :: [Arg],
    _ty :: Maybe Type,
    _blocks :: [BasicBlock]
  }
  deriving (Show)

makeLenses ''Func

-- | The instructions in a Bril function
instrs :: Func -> [Instr]
instrs = concatMap (view BB.instrs) . view blocks

-- | The number of instructions in a Bril function
size :: Func -> Int
size = length . instrs

-- | All variables used by some instruction in a Bril function
uses :: Func -> Set Var
uses = Set.fromList . concatMap Instr.uses . instrs

instance FromJSON Func where
  parseJSON =
    withObject "function" \obj ->
      Func
        <$> obj .: "name"
        <*> obj .:? "args" .!= []
        <*> obj .:? "type"
        <*> (formBasicBlock <$> obj .: "instrs")

instance ToJSON Func where
  toJSON Func {_name, _args, _ty, _blocks} =
    object $
      maybeToList (("type" .=) <$> _ty)
        ++ [ "name" .= _name,
             "args" .= _args,
             "instrs" .= concatMap (map toJSON . view BB.instrs) _blocks
           ]
