module Bril.Syntax.Program (Program (..)) where

import Bril.Syntax.Func (Func)
import Data.Aeson

newtype Program = Program
  { functions :: [Func]
  }
  deriving (Show)

instance FromJSON Program where
  parseJSON = withObject "program" \obj ->
    Program <$> obj .: "functions"

instance ToJSON Program where
  toJSON (Program funcs) = object ["functions" .= funcs]
