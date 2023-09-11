module Bril.Syntax.Literal (Literal (..), parseForType) where

import Bril.Syntax.Type (Type)
import Bril.Syntax.Type qualified as Type
import Data.Aeson hiding (Bool)
import Data.Aeson.Types (Parser)
import Data.Int (Int64)

data Literal
  = Int Int64
  | Float Double
  | Bool Bool
  deriving (Show)

parseForType :: Type -> Value -> Parser Literal
parseForType Type.Int j = Int <$> parseJSON j
parseForType Type.Bool j = Bool <$> parseJSON j
parseForType Type.Float j = Float <$> parseJSON j
parseForType (Type.Ptr _) _ = error "Cannot parse pointer literal"

instance ToJSON Literal where
  toJSON (Int i) = toJSON i
  toJSON (Float f) = toJSON f
  toJSON (Bool b) = toJSON b