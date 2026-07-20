{-
  Lib.Finance.Reconciliation.StatusMap

  Per-entity recon status tracking. Every source entity (booking /
  subscription / driver_fee / whatever) that participates in more than
  one recon carries a JSON map on its own row of
      { specKey -> ReconciliationStatus }
  so downstream readers (dashboard badges, filters) can answer
  "what's the recon status of this booking for the DSR-vs-Ledger check?"
  in one indexed lookup.

  The framework produces the map; the recipe writes it back to the
  source table via its 'syncSourceStatus' hook (see Recipe.hs).
-}
module Lib.Finance.Reconciliation.StatusMap
  ( ReconciliationStatusMap (..),
    empty,
    fromJsonValue,
    toJsonValue,
    lookupBySpec,
    upsertBySpec,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import Lib.Finance.Reconciliation.Types (ReconciliationSpec, ReconciliationStatus, specKey)

-- | Persisted as JSON on the source entity's @reconciliationStatus@ column.
--   Keyed by the recon spec (see 'specKey') so a single entity can carry
--   multiple recon statuses without column bloat.
newtype ReconciliationStatusMap = ReconciliationStatusMap
  { unReconciliationStatusMap :: HM.HashMap Text ReconciliationStatus
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReconciliationStatusMap where
  toJSON (ReconciliationStatusMap m) = A.Object $ KeyMap.fromHashMapText $ HM.map toJSON m

instance FromJSON ReconciliationStatusMap where
  parseJSON (A.Object o) =
    ReconciliationStatusMap <$> traverse A.parseJSON (KeyMap.toHashMapText o)
  parseJSON _ = fail "ReconciliationStatusMap must be a JSON object"

empty :: ReconciliationStatusMap
empty = ReconciliationStatusMap HM.empty

-- | Read whatever is currently on the entity, defaulting to an empty map
--   when the column is NULL or holds an unrecognised value.
fromJsonValue :: Maybe A.Value -> ReconciliationStatusMap
fromJsonValue Nothing = empty
fromJsonValue (Just v) = case A.fromJSON v of
  A.Success m -> m
  A.Error _ -> empty

toJsonValue :: ReconciliationStatusMap -> A.Value
toJsonValue = toJSON

lookupBySpec :: ReconciliationSpec -> ReconciliationStatusMap -> Maybe ReconciliationStatus
lookupBySpec s (ReconciliationStatusMap m) = HM.lookup (specKey s) m

upsertBySpec :: ReconciliationSpec -> ReconciliationStatus -> ReconciliationStatusMap -> ReconciliationStatusMap
upsertBySpec s st (ReconciliationStatusMap m) = ReconciliationStatusMap (HM.insert (specKey s) st m)
