module Main (main) where

import Bril.Optimizations.DCE.Trivial qualified as TDCE
import Bril.Optimizations.LVN qualified as LVN
import Bril.Syntax.Program (Program)
import Control.Monad (guard)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (foldl')
import Data.Function
import Data.Functor
import Data.Maybe (catMaybes)
import Options.Applicative
import System.Exit
import System.IO

data Options = Options
  { tdce :: Bool,
    lvn :: Bool
  }

options :: Parser Options
options =
  Options
    <$> switch (long "tdce" <> help "Perform trivial dead code elimination")
    <*> switch (long "lvn" <> help "Perform local value numbering")

type Optimization = Program -> Program

requestedOptimizations :: Options -> [Optimization]
requestedOptimizations Options {tdce, lvn} =
  catMaybes
    [ guard lvn $> LVN.runOnProgram,
      guard tdce $> TDCE.runOnProgram
    ]

parseOptions :: IO Options
parseOptions = execParser parserInfo
  where
    parserInfo =
      info
        (options <**> helper)
        (fullDesc <> progDesc "Optimize Bril programs")

decodeProgram :: IO Program
decodeProgram = do
  contents <- LBS.getContents
  case eitherDecode contents of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right prog -> pure prog

runOptimizations :: [Optimization] -> Program -> Program
runOptimizations opts prog = foldl' (&) prog opts

main :: IO ()
main = do
  prog <- decodeProgram
  opts <- parseOptions
  let optimizations = requestedOptimizations opts
  LBS.putStr $ encodePretty $ runOptimizations optimizations prog
