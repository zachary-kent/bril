module Bril.Syntax.Func (BasicBlock (..), Func (..), Arg (..)) where

import Bril.Syntax.Instr (Instr (..), isTerminator)
import Bril.Syntax.Type (Type (..))
import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Aeson
import Data.Maybe
import Data.Text (Text)

data SurfaceInstr = Instr Instr | Label Text

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

instance FromJSON SurfaceInstr where
  parseJSON j = parseLabel j <|> Instr <$> parseJSON j
    where
      parseLabel = withObject "Label" \obj -> Label <$> obj .: "label"

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

data BasicBlock = BasicBlock
  { name :: Maybe Text,
    instrs :: [Instr]
  }
  deriving (Show)

data Func = Func
  { name :: Text,
    args :: [Arg],
    ty :: Maybe Type,
    blocks :: [BasicBlock]
  }
  deriving (Show)

instance FromJSON Func where
  parseJSON =
    withObject "function" \obj ->
      Func
        <$> obj .: "name"
        <*> obj .:? "args" .!= []
        <*> obj .:? "type"
        <*> (formBasicBlock <$> obj .: "instrs")

instance ToJSON Func where
  toJSON (Func name args ty blocks) =
    object $
      maybeToList (("type" .=) <$> ty)
        ++ [ "name" .= name,
             "args" .= args,
             "instrs" .= blocksToInstrs blocks
           ]
    where
      blocksToInstrs =
        concatMap \(BasicBlock bb instrs) ->
          case bb of
            Just block -> object ["label" .= block] : map toJSON instrs
            Nothing -> map toJSON instrs
