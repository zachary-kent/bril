module Main (main) where

import Bril.Optimizations.DCE.Trivial qualified as DCE
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy qualified as LBS
import System.Exit
import System.IO

main :: IO ()
main = do
  contents <- LBS.getContents
  case eitherDecode contents of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right prog -> LBS.putStr $ encodePretty $ DCE.runOnProgram prog
