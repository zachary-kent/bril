module Bril.Optimizations.LVN.RenameTable
  ( RenameTable,
    fromBasicBlock,
    rename,
    delete,
    find,
  )
where

import Bril.Syntax.Instr (Instr)
import Bril.Syntax.Instr qualified as Instr
import Data.Foldable qualified as Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

data RenameTable = RenameTable
  { renamings :: Map Text Text,
    unavailable :: Set Text
  }

fromBasicBlock :: [Instr] -> RenameTable
fromBasicBlock block =
  RenameTable
    { unavailable = Set.fromList $ concatMap Instr.uses block ++ mapMaybe Instr.def block,
      renamings = Map.empty
    }

rename :: Text -> RenameTable -> (Text, RenameTable)
rename x RenameTable {renamings, unavailable} = (x', table)
  where
    x' =
      fromJust $
        Foldable.find (\newName -> not $ Set.member newName unavailable) $
          map (\(n :: Integer) -> x <> Text.pack (show n)) [1 ..]
    table =
      RenameTable
        { renamings = Map.insert x x' renamings,
          unavailable = Set.insert x' unavailable
        }

delete :: Text -> RenameTable -> RenameTable
delete x table@RenameTable {renamings} =
  table {renamings = Map.delete x renamings}

find :: Text -> RenameTable -> Text
find x RenameTable {renamings} = Map.findWithDefault x x renamings
