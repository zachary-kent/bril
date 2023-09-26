module Bril.Func
  ( BasicBlock (..),
    Func (..),
    name,
    args,
    ty,
    blocks,
    instrs,
    formBasicBlocks,
    Arg (..),
    uses,
    vars,
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
import Data.Map qualified as Map
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
formBasicBlocks :: [Instr] -> [BasicBlock]
formBasicBlocks =
  splitAtTerminators >>> map \case
    is@(Label l : _) -> BasicBlock (Just l) Map.empty is
    is -> BasicBlock Nothing Map.empty is

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

-- | All variables defined in a function
defs :: Func -> Set Var
defs = Set.fromList . mapMaybe Instr.def . instrs

-- | All variables referenced in a function
vars :: Func -> Set Var
vars func = Set.union (uses func) (defs func)

instance FromJSON Func where
  parseJSON =
    withObject "function" \obj ->
      Func
        <$> obj .: "name"
        <*> obj .:? "args" .!= []
        <*> obj .:? "type"
        <*> (formBasicBlocks <$> obj .: "instrs")

instance ToJSON Func where
  toJSON Func {_name, _args, _ty, _blocks} =
    object $
      maybeToList (("type" .=) <$> _ty)
        ++ [ "name" .= _name,
             "args" .= _args,
             "instrs" .= concatMap blockToInstrs _blocks
           ]
    where
      blockToInstrs bb =
        map toJSON (Map.elems $ view BB.phiNodes bb) ++ map toJSON (view BB.instrs bb)
