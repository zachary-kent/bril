module Bril.Syntax.Literal (Literal (..)) where

import Control.Applicative
import Data.Aeson hiding (Bool)
import Data.Int (Int64)

data Literal
  = Int Int64
  | Float Double
  | Bool Bool
  deriving (Show)

instance FromJSON Literal where
  parseJSON j =
    (Int <$> parseJSON j)
      <|> (Float <$> parseJSON j)
      <|> (Bool <$> parseJSON j)

instance ToJSON Literal where
  toJSON (Int i) = toJSON i
  toJSON (Float f) = toJSON f
  toJSON (Bool b) = toJSON b