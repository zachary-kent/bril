{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Bril.Optimizations.LVN.Table
  ( Table,
    Value,
    canonicalHome,
    lookupVN,
    setVNForVar,
    lookupValue,
    insertValue,
    empty,
  )
where

import Bril.Syntax.Expr (Expr')
import Control.Monad (guard)
import Data.Functor
import Data.List (find, genericLength)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Effectful
import Effectful.State.Static.Local
import Lens.Micro.Platform

type Value = Expr' Integer

data Entry = Entry
  { _number :: Integer,
    _value :: Maybe Value,
    _var :: Text
  }

makeLenses ''Entry

data Table = Table
  { _entries :: [Entry],
    _var2num :: Map Text Integer
  }

makeLenses ''Table

freshVN :: [Entry] -> Integer
freshVN = succ . genericLength

lookupEntryForVN :: Integer -> [Entry] -> Entry
lookupEntryForVN num = fromJust . find \entry -> num == entry ^. number

insertMaybeValue :: (State Table :> es) => Text -> Maybe Value -> Eff es Entry
insertMaybeValue x v = do
  tbl <- gets $ view entries
  let vn = freshVN tbl
      entry = Entry vn v x
  modify $ entries %~ (entry :)
  modify $ var2num %~ Map.insert x vn
  pure entry

lookupEntryForVar :: (State Table :> es) => Text -> Eff es Entry
lookupEntryForVar x = do
  Table {_entries, _var2num} <- get
  case Map.lookup x _var2num of
    Just num -> pure $ lookupEntryForVN num _entries
    Nothing -> insertMaybeValue x Nothing

canonicalHome :: (State Table :> es) => Text -> Eff es Text
canonicalHome x = view var <$> lookupEntryForVar x

lookupVN :: (State Table :> es) => Text -> Eff es Integer
lookupVN x = view number <$> lookupEntryForVar x

lookupValue :: Value -> Table -> Maybe (Integer, Text)
lookupValue e Table {_entries} =
  listToMaybe $
    flip mapMaybe _entries \Entry {_number, _value, _var} -> do
      val <- _value
      guard $ e == val
      pure (_number, _var)

insertValue :: (State Table :> es) => Text -> Value -> Eff es ()
insertValue x = void . insertMaybeValue x . Just

setVNForVar :: (State Table :> es) => Text -> Integer -> Eff es ()
setVNForVar x num = modify $ var2num %~ Map.insert x num

empty :: Table
empty = Table [] Map.empty
