{-
  Lib.Finance.Transfer.Scheduled

  Recurring sweeps over an account class. The kernel owns the cadence
  (computeNextRunTime) and the per-target execution (initiateTransferOut AllAvailable);
  WHO gets swept stays domain knowledge, injected as a paged enumeration — the kernel
  never learns "driver" or "fleet owner". One call processes ONE page and returns the
  cursor, so the surrounding job can re-run itself exactly like today's batch jobs.

  rule.minimumAmount is for the DOMAIN's enumeration query (skip accounts below it in
  SQL); the kernel-side minimum remains finance_account_config.minTransferOutAmount.
-}
module Lib.Finance.Transfer.Scheduled
  ( AccountSweepTarget (..),
    SweepOutcome (..),
    runScheduledSweep,
    computeNextRunTime,
  )
where

import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Finance.Balance (TransferAmount (AllAvailable))
import Lib.Finance.Core.Types (HasActorInfo)
import Lib.Finance.Domain.Types.Account (Account)
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.Domain.Types.ScheduledTransferRule
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.Error.Types
import Lib.Finance.FinanceM (FinanceCtx)
import Lib.Finance.Ledger.PayoutSettlement (PayoutLedgerRefs, RunFinance)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import Lib.Finance.Transfer.Outbound
import Lib.Finance.Transfer.Rail (TransferRail)

-- | Everything the kernel needs to sweep one account. Built by the domain's
--   enumeration (which knows eligibility, builds the FinanceCtx, mints referenceId).
data AccountSweepTarget = AccountSweepTarget
  { account :: Account,
    ctx :: FinanceCtx,
    ledgerRefs :: PayoutLedgerRefs,
    referenceType :: Text,
    referenceId :: Text,
    legacyOfferHoldKeys :: [Text],
    holdLedgerMetadata :: Maybe LE.LedgerEntryMetadata,
    metadata :: DT.TransferMetadata
  }
  deriving (Generic)

data SweepOutcome = SweepOutcome
  { attempted :: Int,
    inFlightOrDone :: Int,
    -- | expected non-starts: below minimum, nothing available, class disabled
    skipped :: Int,
    failed :: Int,
    nextCursor :: Maybe Text
  }
  deriving (Eq, Show, Generic)

runScheduledSweep ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  TransferRail m ->
  ScheduledTransferRule ->
  Maybe Text -> -- page cursor (opaque to the kernel)
  (Int -> Maybe Text -> m ([AccountSweepTarget], Maybe Text)) -> -- injected enumeration
  m SweepOutcome
runScheduledSweep runFinance rail rule cursor enumerate = do
  (targets, nextCursor) <- enumerate rule.batchSize cursor
  results <- forM targets $ \tgt -> do
    res <-
      try @_ @SomeException $
        initiateTransferOut
          runFinance
          rail
          OutboundSpec
            { ctx = tgt.ctx,
              account = tgt.account,
              amount = AllAvailable,
              referenceType = tgt.referenceType,
              referenceId = tgt.referenceId,
              ledgerRefs = tgt.ledgerRefs,
              instant = False,
              legacyOfferHoldKeys = tgt.legacyOfferHoldKeys,
              coveredEntryIds = Nothing,
              holdLedgerMetadata = tgt.holdLedgerMetadata,
              metadata = tgt.metadata
            }
    case res of
      Left exc -> do
        logError $ "scheduled sweep: " <> tgt.referenceId <> " threw " <> show exc
        pure SweepFailed
      Right (Left err)
        | isExpectedSkip err -> pure SweepSkipped
        | otherwise -> do
          logError $ "scheduled sweep: " <> tgt.referenceId <> " rejected: " <> show err
          pure SweepFailed
      Right (Right t)
        | t.status `elem` [DT.FAILED, DT.NEEDS_REVIEW] -> pure SweepFailed
        | otherwise -> pure SweepOk
  pure
    SweepOutcome
      { attempted = length results,
        inFlightOrDone = length [() | SweepOk <- results],
        skipped = length [() | SweepSkipped <- results],
        failed = length [() | SweepFailed <- results],
        nextCursor
      }
  where
    isExpectedSkip = \case
      DomainError "TRANSFER_BELOW_MINIMUM" _ -> True
      DomainError "TRANSFER_CONSUMED_BY_FEE" _ -> True
      DomainError "INVALID_TRANSFER_AMOUNT" _ -> True
      DomainError "TRANSFER_OUT_DISABLED" _ -> True
      DomainError "TRANSFER_DAILY_LIMIT_REACHED" _ -> True
      AccountError InsufficientBalance _ -> True
      _ -> False

data SweepResult = SweepOk | SweepSkipped | SweepFailed

-- | Next run instant after 'now' for a rule, in the rule's local time
--   (timeDiffFromUtc). timeOfDay "HH:MM" malformed -> 02:00, matching the batch
--   payout job's historical default.
computeNextRunTime :: ScheduledTransferRule -> UTCTime -> UTCTime
computeNextRunTime rule now =
  let tz = Time.minutesToTimeZone (getSeconds rule.timeDiffFromUtc `div` 60)
      localNow = Time.utcToLocalTime tz now
      today = Time.localDay localNow
      tod = parseTod rule.timeOfDay
      atDay d = Time.localTimeToUTC tz (Time.LocalTime d tod)
      nextDaily =
        let candidate = atDay today
         in if candidate > now then candidate else atDay (Time.addDays 1 today)
   in case rule.frequency of
        DAILY -> nextDaily
        HOURLY -> Time.addUTCTime (fromIntegral (3600 * fromMaybe 1 rule.intervalHours)) now
        EVERY_N_DAYS -> atDay (Time.addDays (fromIntegral (max 1 (fromMaybe 1 rule.intervalDays))) today)
        WEEKLY ->
          let targetDow = fromMaybe 1 rule.dayOfWeek -- 1 = Monday, ISO
              (_, _, todayDow) = toWeekDate today
              daysAhead = (targetDow - todayDow) `mod` 7
              candidate = atDay (Time.addDays (fromIntegral daysAhead) today)
           in if candidate > now then candidate else atDay (Time.addDays (fromIntegral (daysAhead + 7)) today)
        MONTHLY ->
          let targetDom = max 1 (min 28 (fromMaybe 1 rule.dayOfMonth))
              (y, m, _) = Time.toGregorian today
              thisMonth = atDay (Time.fromGregorian y m targetDom)
              (y', m') = if m == 12 then (y + 1, 1) else (y, m + 1)
           in if thisMonth > now then thisMonth else atDay (Time.fromGregorian y' m' targetDom)
  where
    parseTod t = case T.splitOn ":" t of
      [hh, mm]
        | Just h <- readMaybe (T.unpack hh),
          Just m <- readMaybe (T.unpack mm),
          h >= 0 && h < 24 && m >= 0 && m < 60 ->
          Time.TimeOfDay h m 0
      _ -> Time.TimeOfDay 2 0 0
