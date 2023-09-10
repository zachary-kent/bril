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

import Bril.Syntax.Expr (Expr' (..))
import Control.Monad (guard)
import Data.Functor
import Data.List (find)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import Effectful
import Effectful.State.Static.Local
import Lens.Micro.Platform

type Value = Expr' Integer

data Entry = Entry
  { _number :: Integer,
    _value :: Maybe Value,
    _var :: Text
  }

hasVN :: Integer -> Entry -> Bool
hasVN vn Entry {_number} = vn == _number

makeLenses ''Entry

data Table = Table
  { _entries :: [Entry],
    _var2num :: [(Text, Integer)],
    _nextNum :: Integer
  }

makeLenses ''Table

freshVN :: (State Table :> es) => Eff es Integer
freshVN = do
  vn <- gets (view nextNum)
  modify (nextNum +~ 1)
  pure vn

lookupEntryForVN :: Integer -> [Entry] -> Entry
lookupEntryForVN num = fromJust . find \entry -> num == entry ^. number

setVNForVar :: (State Table :> es) => Text -> Integer -> Eff es ()
setVNForVar x vn = do
  tbl <- get
  let maybePrevEntry = lookupEntryForVar x tbl
  modify $ var2num %~ insertAssoc x vn
  case maybePrevEntry of
    Just prevEntry -> do
      v2n <- gets (view var2num)
      let prevVN = view number prevEntry
      case lookup prevVN (map swap v2n) of
        Just newHome -> modify $ entries %~ (set var newHome prevEntry :)
        Nothing -> pure ()
    Nothing -> pure ()

insertMaybeValue :: (State Table :> es) => Text -> Maybe Value -> Eff es Entry
insertMaybeValue x v = do
  vn <- freshVN
  let entry = Entry vn v x
  modify $ entries %~ (entry :)
  setVNForVar x vn
  pure entry

lookupEntryForVar :: Text -> Table -> Maybe Entry
lookupEntryForVar x Table {_entries, _var2num} = do
  num <- lookup x _var2num
  pure $ lookupEntryForVN num _entries

lookupOrCreateEntryForVar :: (State Table :> es) => Text -> Eff es Entry
lookupOrCreateEntryForVar x = do
  tbl <- get
  case lookupEntryForVar x tbl of
    Just entry -> pure entry
    Nothing -> insertMaybeValue x Nothing

canonicalHome :: (State Table :> es) => Text -> Eff es Text
canonicalHome x = view var <$> lookupOrCreateEntryForVar x

lookupVN :: (State Table :> es) => Text -> Eff es Integer
lookupVN x = view number <$> lookupOrCreateEntryForVar x

lookupValue :: Value -> Table -> Maybe (Integer, Text)
lookupValue (Id x) Table {_var2num, _entries} =
  pure (x, view var $ lookupEntryForVN x _entries)
lookupValue e Table {_entries} =
  listToMaybe $
    flip mapMaybe _entries \Entry {_number, _value, _var} -> do
      val <- _value
      guard $ e == val
      pure (_number, _var)

removeAssoc :: (Eq a) => a -> [(a, b)] -> [(a, b)]
removeAssoc k = filter $ (/= k) . fst

insertAssoc :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
insertAssoc k v m = (k, v) : removeAssoc k m

insertValue :: (State Table :> es) => Text -> Value -> Eff es ()
insertValue x = void . insertMaybeValue x . Just

empty :: Table
empty = Table [] [] 1
