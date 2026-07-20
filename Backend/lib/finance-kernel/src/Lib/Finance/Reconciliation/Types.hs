{-
  Lib.Finance.Reconciliation.Types

  Domain-neutral value types used by the reconciliation framework and its
  recipes. Deliberately knows nothing about specific tables — Domain and
  DataSource enums are the only extension point when a new business area
  (postpaid subscription, membership, insurance, etc.) needs a recon.

  Any change here is a schema change downstream: the two Beam enum types
  (reconciliation_domain, reconciliation_data_source) must stay in lockstep
  with the Domain and DataSource constructors below.
-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Finance.Reconciliation.Types
  ( -- * Recipe identity
    Domain (..),
    DataSource (..),
    ReconciliationSpec (..),
    specKey,

    -- * Recon result
    ReconciliationStatus (..),
    ReconResult (..),

    -- * Framework value types
    DateRange (..),
    MerchantScope (..),
    Lifecycle (..),
    SourceRecord (..),
    TargetRecord (..),

    -- * Recipe knobs
    ChunkPlan (..),
    chunkDuration,
    GroupingStrategy (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (nominalDay, secondsToNominalDiffTime)
import Kernel.Prelude
import qualified Kernel.Types.Common as KTC
import qualified Tools.Beam.UtilsTH
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- ─── Recipe identity ───────────────────────────────────────────────────────

-- | A business area that owns its own recons. Extend when a new area
--   (rider-side gift cards, insurance premiums, wallet top-ups, …) needs
--   reconciled — every new constructor also lands as a value in the
--   @reconciliation_domain@ Postgres enum.
data Domain
  = PREPAID_SUBSCRIPTION
  | POSTPAID_SUBSCRIPTION
  | MEMBERSHIP
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

-- | A concrete data source that can appear on either side of a recon. Names
--   are intentionally table-shaped so the meaning is obvious in the summary
--   UI without cross-referencing a doc.
data DataSource
  = -- Postpaid subscription
    DRIVER_FEE
  | -- STCL cooperative
    STCL_MEMBERSHIP
  | -- Payment gateway internal
    PAYMENT_ORDER
  | -- PG settlement report tables
    PG_PAYMENT_SETTLEMENT
  | PG_PAYOUT_SETTLEMENT
  | -- Prepaid subscription DSR / DSSR / ledger stack
    DSR
  | DSSR
  | LEDGER
  | SUBSCRIPTION_PURCHASE
  | PAYOUT_REQUEST
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Domain)
$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DataSource)

-- Servant needs to parse these off the wire for the dashboard's
-- mandatoryQuery params (`domain`, `source`, `target`). Round-trip
-- through the constructor name via Show/Read.
instance FromHttpApiData Domain where
  parseQueryParam = enumFromQueryParam "Domain"
  parseUrlPiece = parseQueryParam
  parseHeader = parseQueryParam . decodeUtf8

instance ToHttpApiData Domain where
  toQueryParam = T.pack . show
  toUrlPiece = toQueryParam
  toHeader = encodeUtf8 . toQueryParam

instance FromHttpApiData DataSource where
  parseQueryParam = enumFromQueryParam "DataSource"
  parseUrlPiece = parseQueryParam
  parseHeader = parseQueryParam . decodeUtf8

instance ToHttpApiData DataSource where
  toQueryParam = T.pack . show
  toUrlPiece = toQueryParam
  toHeader = encodeUtf8 . toQueryParam

enumFromQueryParam :: Read a => Text -> Text -> Either Text a
enumFromQueryParam typeName t = case readMaybe (T.unpack t) of
  Just v -> Right v
  Nothing -> Left $ "Invalid " <> typeName <> ": " <> t

-- | Key of the recipe registry. Never persisted as a nested value — every
--   entry / summary row carries the three fields flat so read paths can hit
--   a single btree index.
data ReconciliationSpec = ReconciliationSpec
  { domain :: Domain,
    source :: DataSource,
    target :: DataSource
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Stable string form of a spec — used in log tags and lock keys.
specKey :: ReconciliationSpec -> Text
specKey s = show s.domain <> "|" <> show s.source <> "|" <> show s.target

-- ─── Recon result ──────────────────────────────────────────────────────────

data ReconciliationStatus
  = MATCHED
  | HIGHER_IN_TARGET
  | LOWER_IN_TARGET
  | MISSING_IN_TARGET
  | MISSING_IN_SOURCE
  | -- | Source is not yet in a terminal state — the target may still arrive
    --   or the source may still transition. Excluded from mismatch counts;
    --   the B2 sweep re-checks these entries after the source settles.
    AWAITING_SETTLEMENT
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReconciliationStatus)

data ReconResult = ReconResult
  { reconStatus :: ReconciliationStatus,
    mismatchReason :: Maybe Text
  }
  deriving (Eq, Show)

-- ─── Framework value types ─────────────────────────────────────────────────

-- | Half-open @[from, to)@ interval. All chunk math assumes this convention;
--   a record whose timestamp equals @to@ belongs to the NEXT chunk.
data DateRange = DateRange
  { from :: UTCTime,
    to :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data MerchantScope = MerchantScope
  { merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Whether a source row's underlying state can still change. The classify
--   step short-circuits to 'AWAITING_SETTLEMENT' when any source in the
--   comparison is 'InFlight', so in-flight rows never appear as breaks in
--   dashboards or mismatch counts. Once terminal ('Settled' or 'Cancelled')
--   the entry is decided by the usual amount comparison.
data Lifecycle
  = InFlight
  | Settled
  | Cancelled
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Lifecycle)

-- | Every source row a recipe emits. The runner consumes these exhaustively
--   for the chunk, so populate every field the persisted entry needs — the
--   recipe is the sole place that knows about the underlying tables.
data SourceRecord = SourceRecord
  { -- | Primary key of the underlying source table row.
    srcId :: Text,
    -- | The "entity" this recon is about. Often the same as @srcId@, but
    --   explicit so recipes that partition a single entity into multiple
    --   sources (rare) can still group correctly.
    srcEntityId :: Maybe Text,
    -- | driver_id / owner_id / whoever this recon is attributed to.
    srcPartyId :: Maybe Text,
    -- | Source-side money value being reconciled.
    srcAmount :: KTC.HighPrecMoney,
    -- | Foreign key used to look up the matching target. When absent, the
    --   entry is emitted as MISSING_IN_TARGET (or AWAITING_SETTLEMENT if the
    --   source is still in-flight).
    srcMatchKey :: Maybe Text,
    -- | Optional finance sub-classifier (e.g. \"PENNY_DROP\").
    srcComponent :: Maybe Text,
    -- | Per-domain extras persisted as JSONB alongside the entry.
    srcMeta :: Maybe A.Value,
    -- | Original event timestamp of the source row.
    srcTimestamp :: UTCTime,
    -- | Whether the source is still in-flight or has reached a terminal
    --   state. Recipes decide this from their underlying status enum
    --   (PENDING\/INITIATED\/PROCESSING → 'InFlight'; SUCCESS\/CREDITED\/etc.
    --   → 'Settled'; FAILED\/CANCELLED → 'Cancelled').
    srcLifecycle :: Lifecycle
  }
  deriving (Show)

-- | Target-side row keyed by whatever the source's @srcMatchKey@ points to.
--   Recipes fetch these in bulk given a set of match keys.
--
--   Two IDs are kept because they can differ: @tgtMatchKey@ is what the
--   framework joins on against @srcMatchKey@; @tgtId@ is the target row's
--   own primary key and is persisted on the entry as @targetRecordId@ for
--   auditing. When they're the same table's own PK (e.g. @payment_order.id@),
--   set both to the same value.
data TargetRecord = TargetRecord
  { -- | Target row's own primary key. Persisted verbatim.
    tgtId :: Text,
    -- | The value the source's @srcMatchKey@ should equal. Often the same
    --   as @tgtId@; different when the join runs through an FK column (e.g.
    --   @ledger.reference_id@) rather than a PK.
    tgtMatchKey :: Text,
    tgtAmount :: KTC.HighPrecMoney,
    tgtMeta :: Maybe A.Value,
    -- | Populated when target = PG_PAYMENT_SETTLEMENT.
    tgtSettlementId :: Maybe Text,
    tgtSettlementDate :: Maybe UTCTime,
    tgtSettlementMode :: Maybe Text,
    tgtRrn :: Maybe Text,
    tgtTransactionDate :: Maybe UTCTime
  }
  deriving (Show)

-- ─── Recipe knobs ──────────────────────────────────────────────────────────

-- | How the runner slices the input date range. Only ByHour / ByDay
--   supported — every recon needs at least an hour of collection time to
--   be worth checking, so sub-hour granularity would just add scheduler
--   overhead without changing detection latency meaningfully.
data ChunkPlan
  = ByHour
  | ByDay
  deriving (Eq, Show)

-- | Wall-clock duration of one chunk under the given plan. Consumed by the
--   scheduler to pick a firing cadence and by 'planChunks' to walk a range.
chunkDuration :: ChunkPlan -> NominalDiffTime
chunkDuration = \case
  ByHour -> secondsToNominalDiffTime 3600
  ByDay -> nominalDay

-- | Whether per-row comparison suffices or the recipe needs a group-level
--   check (n:1 source-to-target).
data GroupingStrategy
  = -- | 1:1 comparison between one source and one target.
    Individual
  | -- | Sum the source amounts sharing a target, compare with target amount.
    --   Every source in the group inherits the group-level recon status; the
    --   group total is captured on each entry via group_source_total /
    --   group_target_amount.
    GroupByTargetKey
  deriving (Eq, Show)
