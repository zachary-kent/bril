module Bril.Func
  ( BasicBlock (..),
    Func (..),
    name,
    args,
    ty,
    blocks,
    formBasicBlock,
    Arg (..),
    flatten,
    uses,
    size,
  )
where

import Bril.BasicBlock (BasicBlock (..))
import Bril.BasicBlock qualified as BB
import Bril.Expr (Var)
import Bril.Instr (Instr, SurfaceInstr (..), isTerminator)
import Bril.Instr qualified as Instr
import Bril.Type (Type (..))
import Control.Arrow ((>>>))
import Data.Aeson
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Lens.Micro.Platform (makeLenses, view)

splitAtTerminators :: [SurfaceInstr] -> [[SurfaceInstr]]
splitAtTerminators = filter (not . null) . go []
  where
    go curr [] = [reverse curr]
    go curr (lab@(Label _) : is) = reverse curr : go [lab] is
    go curr (i@(Instr inst) : is)
      | isTerminator inst = reverse (i : curr) : go [] is
      | otherwise = go (i : curr) is

formBasicBlock :: [SurfaceInstr] -> [BasicBlock]
formBasicBlock =
  splitAtTerminators >>> map \case
    Label l : is -> BasicBlock (Just l) $ map concreteInstr is
    is -> BasicBlock Nothing $ map concreteInstr is
  where
    concreteInstr (Instr i) = i
    concreteInstr (Label _) = error "Impossible: label after beginning of basic block"

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

data Func = Func
  { _name :: Text,
    _args :: [Arg],
    _ty :: Maybe Type,
    _blocks :: [BasicBlock]
  }
  deriving (Show)

makeLenses ''Func

instrs :: Func -> [Instr]
instrs = concatMap (view BB.instrs) . view blocks

flatten :: Func -> [SurfaceInstr]
flatten = concatMap BB.flatten . view blocks

size :: Func -> Int
size = length . instrs

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
             "instrs" .= blocksToInstrs _blocks
           ]
    where
      blocksToInstrs =
        concatMap \BasicBlock {_name, _instrs} ->
          case _name of
            Just block -> object ["label" .= block] : map toJSON _instrs
            Nothing -> map toJSON _instrs
