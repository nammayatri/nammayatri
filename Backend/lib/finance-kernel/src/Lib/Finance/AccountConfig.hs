{-
  Lib.Finance.AccountConfig

  Account-class configuration: every money-movement policy the kernel enforces is
  keyed on the CLASS of an account — (merchantOperatingCityId, accountType,
  counterpartyType, subLedger) — never on a domain concept. A driver wallet is just
  (Liability, DRIVER); the kernel never learns the word "wallet".

  Rows live in finance_account_config with wildcards (city '*', Nothing for
  counterpartyType / subLedger) and are resolved most-specific-first. All reads go
  through 'getAccountConfig', which caches the merchant's full (small) row set in
  Redis and walks the ladder in memory — callers never query the table directly.

  Design: docs/backend/design/finance-kernel-unification-plan.md §3.
-}
module Lib.Finance.AccountConfig
  ( AccountClass (..),
    ResolvedAccountConfig (..),
    defaultAccountConfig,
    wildcardCity,
    classOfAccount,
    getAccountConfig,
    getAccountConfigForAccount,
    clearAccountConfigCache,
    cutoffTimeFor,
  )
where

import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Time as Time
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Account (Account, AccountType, CounterpartyType)
import Lib.Finance.Domain.Types.FinanceAccountConfig (CutoffSpec (..), FeeSpec, FinanceAccountConfig)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.FinanceAccountConfig as QFAC

-- | Sentinel for a merchant-wide config row.
wildcardCity :: Text
wildcardCity = "*"

-- | The config key: what KIND of account this is, not which account.
data AccountClass = AccountClass
  { merchantId :: Text,
    merchantOperatingCityId :: Text,
    accountType :: AccountType,
    counterpartyType :: Maybe CounterpartyType,
    subLedger :: Maybe Text
  }
  deriving (Eq, Show, Generic)

classOfAccount :: Account -> AccountClass
classOfAccount acc =
  AccountClass
    { merchantId = acc.merchantId,
      merchantOperatingCityId = acc.merchantOperatingCityId,
      accountType = acc.accountType,
      counterpartyType = acc.counterpartyType,
      subLedger = acc.subLedger
    }

-- | A fully-resolved view: every field has a value, either from the winning row or
--   from 'defaultAccountConfig'. 'resolvedFrom' is Nothing when defaults applied.
data ResolvedAccountConfig = ResolvedAccountConfig
  { allowNegativeBalance :: Bool,
    minRetainedBalance :: HighPrecMoney,
    redeemableCutoff :: Maybe CutoffSpec,
    transferOutEnabled :: Bool,
    instantTransferOutEnabled :: Bool,
    minTransferOutAmount :: HighPrecMoney,
    maxTransferOutPerDay :: Maybe Int,
    transferOutFee :: Maybe FeeSpec,
    transferInEnabled :: Bool,
    autoTransferOut :: Maybe Text,
    statusCheckIntervalSec :: Maybe Int,
    statusCheckMaxAttempts :: Maybe Int,
    -- | points-denominated classes (loyalty wallets): currency units per point
    pointsConversionRate :: Maybe Double,
    resolvedFrom :: Maybe (Id FinanceAccountConfig)
  }
  deriving (Eq, Show, Generic)

-- | Permissive-read, restrictive-move. allowNegativeBalance defaults True because
--   that is the kernel's historical behaviour — tightening happens per class via
--   config rows, never as a global flip.
defaultAccountConfig :: ResolvedAccountConfig
defaultAccountConfig =
  ResolvedAccountConfig
    { allowNegativeBalance = True,
      minRetainedBalance = 0,
      redeemableCutoff = Nothing,
      transferOutEnabled = False,
      instantTransferOutEnabled = False,
      minTransferOutAmount = 0,
      maxTransferOutPerDay = Nothing,
      transferOutFee = Nothing,
      transferInEnabled = False,
      autoTransferOut = Nothing,
      statusCheckIntervalSec = Nothing,
      statusCheckMaxAttempts = Nothing,
      pointsConversionRate = Nothing,
      resolvedFrom = Nothing
    }

cacheKey :: Text -> Text
cacheKey merchantId = "Finance:AccountConfig:" <> merchantId

cacheTtlSeconds :: Integer
cacheTtlSeconds = 600

-- | Resolve the config for an account class. Never fails: a class with no matching
--   row gets 'defaultAccountConfig'.
getAccountConfig :: (BeamFlow.BeamFlow m r) => AccountClass -> m ResolvedAccountConfig
getAccountConfig cls = do
  rows <- getMerchantRows cls.merchantId
  pure $ maybe defaultAccountConfig toResolved (resolveRow cls rows)

getAccountConfigForAccount :: (BeamFlow.BeamFlow m r) => Account -> m ResolvedAccountConfig
getAccountConfigForAccount = getAccountConfig . classOfAccount

getMerchantRows :: (BeamFlow.BeamFlow m r) => Text -> m [FinanceAccountConfig]
getMerchantRows merchantId =
  Redis.safeGet (cacheKey merchantId) >>= \case
    Just rows -> pure rows
    Nothing -> do
      rows <- QFAC.findAllByMerchantId merchantId
      Redis.setExp (cacheKey merchantId) rows (fromInteger cacheTtlSeconds)
      pure rows

-- | Call after any write to finance_account_config (dashboard upserts, seeds).
clearAccountConfigCache :: (Redis.HedisFlow m r) => Text -> m ()
clearAccountConfigCache merchantId = Redis.del (cacheKey merchantId)

-- | Most-specific-wins ladder. A row is compatible when every non-wildcard field
--   matches the class exactly; among compatible rows, exact city beats '*',
--   a concrete counterpartyType beats wildcard, a concrete subLedger beats wildcard
--   (in that precedence order). updatedAt breaks residual ties deterministically.
resolveRow :: AccountClass -> [FinanceAccountConfig] -> Maybe FinanceAccountConfig
resolveRow cls rows = case filter compatible rows of
  [] -> Nothing
  candidates -> Just $ maximumBy (comparing specificity <> comparing (.updatedAt)) candidates
  where
    compatible r =
      r.merchantId == cls.merchantId
        && (r.merchantOperatingCityId == cls.merchantOperatingCityId || r.merchantOperatingCityId == wildcardCity)
        && r.accountType == cls.accountType
        && maybe True (\cp -> Just cp == cls.counterpartyType) r.counterpartyType
        && maybe True (\sl -> Just sl == cls.subLedger) r.subLedger
    specificity :: FinanceAccountConfig -> Int
    specificity r =
      (if r.merchantOperatingCityId /= wildcardCity then 4 else 0)
        + (if isJust r.counterpartyType then 2 else 0)
        + (if isJust r.subLedger then 1 else 0)

toResolved :: FinanceAccountConfig -> ResolvedAccountConfig
toResolved r =
  ResolvedAccountConfig
    { allowNegativeBalance = r.allowNegativeBalance,
      minRetainedBalance = r.minRetainedBalance,
      redeemableCutoff = r.redeemableCutoff,
      transferOutEnabled = r.transferOutEnabled,
      instantTransferOutEnabled = r.instantTransferOutEnabled,
      minTransferOutAmount = r.minTransferOutAmount,
      maxTransferOutPerDay = r.maxTransferOutPerDay,
      transferOutFee = r.transferOutFee,
      transferInEnabled = r.transferInEnabled,
      autoTransferOut = r.autoTransferOut,
      statusCheckIntervalSec = r.statusCheckIntervalSec,
      statusCheckMaxAttempts = r.statusCheckMaxAttempts,
      pointsConversionRate = r.pointsConversionRate,
      resolvedFrom = Just r.id
    }

-- | The redeemable-cutoff instant for "now": the start of the local day (shifted to
--   timeOfDay "HH:MM" when given) minus 'days'. Entries unsettled before this instant
--   are redeemable. (Same anchor semantics as the driver app's historical
--   payoutCutoffTimeUTC, which this replaced.)
cutoffTimeFor :: CutoffSpec -> UTCTime -> UTCTime
cutoffTimeFor spec now =
  let tz = Time.minutesToTimeZone (spec.tzOffsetSeconds `div` 60)
      localNow = Time.utcToLocalTime tz now
      todaySeconds = fromMaybe 0 (parseTimeOfDaySeconds =<< spec.timeOfDay)
      anchorLocal = Time.LocalTime (Time.localDay localNow) (Time.timeToTimeOfDay (fromInteger todaySeconds))
      anchorUtc = Time.localTimeToUTC tz anchorLocal
   in Time.addUTCTime (negate (fromIntegral spec.days * 86400)) anchorUtc

-- | "HH:MM" -> seconds from midnight; Nothing on malformed input (treated as 00:00).
parseTimeOfDaySeconds :: Text -> Maybe Integer
parseTimeOfDaySeconds t = case T.splitOn ":" t of
  [hh, mm] -> do
    h <- readMaybe (T.unpack hh)
    m <- readMaybe (T.unpack mm)
    guard (h >= 0 && h < 24 && m >= 0 && m < 60)
    pure (h * 3600 + m * 60)
  _ -> Nothing
