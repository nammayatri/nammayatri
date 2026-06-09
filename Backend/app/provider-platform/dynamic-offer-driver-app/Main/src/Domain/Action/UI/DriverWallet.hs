{-# OPTIONS_GHC -Wno-orphans #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverWallet
  ( getWalletBalance,
    getWalletTransactions,
    getWalletTransactionHistory,
    postWalletPayout,
    postWalletTopup,
    recordAirportCashRecharge,
    getWalletPayoutHistory,
    -- Exported for scheduled batch payout
    PayoutContext (..),
    loadPayoutContext,
    counterpartyFromRole,
    computePayoutFee,
    initiateWalletPayout,
    makePayoutEntryIdsKey,
    mkDriverWalletFinanceCtx,
  )
where

import qualified API.Types.UI.DriverWallet as DriverWallet
import Data.List (partition, span)
import qualified Data.Map.Strict as Map
import qualified Data.Time
import qualified Data.Time.Calendar as Cal
import Domain.Action.UI.Plan hiding (mkDriverFee)
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import Domain.Types.Extra.Plan
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.WalletTransaction as DWT
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.HideSecrets
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance
  ( Account,
    AccountRole (OwnerLiability, PlatformAsset),
    CounterpartyType,
    FinanceCtx (..),
    InvoiceConfig (..),
    InvoiceLineItem (..),
    ItemType (..),
    LineItemDescription (..),
    findByAccountWithFilters,
    getEntriesByReference,
    invoice,
    runFinance,
    transfer,
  )
import qualified Lib.Finance.Domain.Types.Account as FAccount
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerEntry
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.PayoutItems as PayoutItems
import qualified Lib.Payment.Payout.Request as PayoutRequest
import SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import SharedLogic.Finance.Wallet
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.LedgerEntry as CHLE
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.WalletTransaction as QWalletTransaction
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout

instance Kernel.Types.HideSecrets.HideSecrets DriverWallet.TopUpRequest where
  hideSecrets = Kernel.Prelude.identity

-- | Pick the counterparty type based on the person's role.
counterpartyFromRole :: DP.Role -> CounterpartyType
counterpartyFromRole DP.FLEET_OWNER = counterpartyFleetOwner
counterpartyFromRole DP.FLEET_BUSINESS = counterpartyFleetOwner
counterpartyFromRole _ = counterpartyDriver

--------------------------------------------------------------------------------
-- getWalletBalance
--------------------------------------------------------------------------------

getWalletBalance ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow DriverWallet.WalletBalanceResponse
  )
getWalletBalance (mbPersonId, _merchantId, _mocId) = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let counterparty = counterpartyFromRole person.role
  currentBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty driverId.getId
  pure $ DriverWallet.WalletBalanceResponse {currentBalance}

--------------------------------------------------------------------------------
-- getWalletTransactions
--------------------------------------------------------------------------------

getWalletTransactions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe DriverWallet.AggregationLevel ->
    Environment.Flow DriverWallet.WalletSummaryResponse
  )
getWalletTransactions (mbPersonId, _merchantId, mocId) mbFromDate mbToDate mbAggBy = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbActiveFleetAssoc <- QFDA.findByDriverId driverId True
  let mbAssocFleetOwnerId = (.fleetOwnerId) <$> mbActiveFleetAssoc
  let (counterparty, ownerId, mbConcernedIndividualId) =
        case mbAssocFleetOwnerId of
          Just fleetOwnerId -> (counterpartyFleetOwner, fleetOwnerId, Just driverId.getId)
          Nothing -> (counterpartyFromRole person.role, driverId.getId, Nothing)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) (Just (SCTC.findByMerchantOpCityId mocId Nothing)) >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  now <- getCurrentTime
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      fromDate = fromMaybe (Data.Time.UTCTime (Data.Time.utctDay now) 0) mbFromDate
      toDate = fromMaybe now mbToDate
      aggBy = fromMaybe DriverWallet.Day mbAggBy
      cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
      cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  (mbWalletAcc, mbControlAcc) <- getWalletAndControlAccountsByOwner counterparty ownerId
  case (mbWalletAcc, mbControlAcc) of
    (Nothing, Nothing) -> pure emptyWalletSummary
    _ -> do
      currentBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty ownerId
      let accountIds = catMaybes [(.id) <$> mbWalletAcc, (.id) <$> mbControlAcc]
          useClickhouse = fromMaybe False transporterConfig.driverWalletConfig.fetchWalletTransactionsFromClickhouse
      rows <-
        if useClickhouse
          then fetchWalletRowsFromCH accountIds mbConcernedIndividualId fromDate toDate
          else fetchWalletRowsFromLedger accountIds mbConcernedIndividualId fromDate toDate
      let (additions, deductions, nonRedeemableBalance, netEarningsBalance) =
            aggregateWalletRows accountIds cutoff rows
          redeemableBalance = max 0 (currentBalance - nonRedeemableBalance)
          agg = bucketizeRows accountIds (generateBucketWindows aggBy timeDiff fromDate toDate) rows
      pure $
        DriverWallet.WalletSummaryResponse
          { currentBalance,
            redeemableBalance,
            nonRedeemableBalance,
            netEarningsBalance,
            additions,
            deductions,
            agg
          }

emptyWalletSummary :: DriverWallet.WalletSummaryResponse
emptyWalletSummary =
  DriverWallet.WalletSummaryResponse
    { currentBalance = 0,
      redeemableBalance = 0,
      nonRedeemableBalance = 0,
      netEarningsBalance = 0,
      additions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []},
      deductions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []},
      agg = []
    }

-- | Fetch wallet ledger entries from the primary Postgres ledger store and
--   project them to the minimal row shape consumed by the aggregator.
--   Merges across wallet Liability + Control accounts so cash-mode earnings
--   and online wallet movements show up in the same timeline.
fetchWalletRowsFromLedger ::
  (BeamFlow m r) =>
  [Id Account] ->
  Maybe Text ->
  UTCTime ->
  UTCTime ->
  m [CHLE.WalletEntryRow]
fetchWalletRowsFromLedger accountIds mbConcernedIndividualId fromDate toDate = do
  let allRefs = walletCreditRefs ++ [walletReferencePayout]
  entries <-
    QLedgerEntry.findByAccountsWithConcernedIndividual
      accountIds
      allRefs
      (Just fromDate)
      (Just toDate)
      Nothing
      Nothing
      mbConcernedIndividualId
  pure $ map toWalletRow entries
  where
    toWalletRow e =
      CHLE.WalletEntryRow
        { walletAmount = e.amount,
          walletToAccountId = e.toAccountId,
          walletReferenceType = e.referenceType,
          walletTimestamp = e.timestamp
        }

-- | Fetch the same wallet entries from ClickHouse. Used when the transporter
--   config flag is on so the whole summary response is served from CH.
fetchWalletRowsFromCH ::
  (MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  [Id Account] ->
  Maybe Text ->
  UTCTime ->
  UTCTime ->
  m [CHLE.WalletEntryRow]
fetchWalletRowsFromCH accountIds mbConcernedIndividualId fromDate toDate = do
  let allRefs = walletCreditRefs ++ [walletReferencePayout]
  CHLE.findWalletEntries accountIds mbConcernedIndividualId fromDate toDate allRefs

-- | Aggregate raw entries into the WalletSummary fields:
--   per-reference additions/deductions groups, top-level non-redeemable
--   balance, and net earnings (additions - deductions).
aggregateWalletRows ::
  [Id Account] ->
  UTCTime -> -- payout cutoff time
  [CHLE.WalletEntryRow] ->
  (DriverWallet.WalletItemGroup, DriverWallet.WalletItemGroup, HighPrecMoney, HighPrecMoney)
aggregateWalletRows accountIds cutoff entries =
  let (addEntries, dedEntries) = partition (\e -> e.walletToAccountId `elem` accountIds) entries

      addRefMap = foldl' (\acc e -> Map.insertWith (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) e.walletReferenceType (e.walletAmount, if e.walletTimestamp >= cutoff then e.walletAmount else 0) acc) Map.empty addEntries
      addItems =
        map
          ( \(ref, (total, nonRedeem)) ->
              DriverWallet.WalletItem
                { itemReference = ref,
                  itemName = referenceTypeToItemName ref,
                  itemValue = total,
                  redeemableBalance = max 0 (total - nonRedeem),
                  nonRedeemableBalance = nonRedeem
                }
          )
          (Map.toList addRefMap)
      addTotal = sum (map (\(_, (t, _)) -> t) (Map.toList addRefMap))
      topLevelNonRedeemable = sum (map (\(_, (_, nr)) -> nr) (Map.toList addRefMap))

      dedRefMap = foldl' (\acc e -> Map.insertWith (+) e.walletReferenceType e.walletAmount acc) Map.empty dedEntries
      dedItems =
        map
          ( \(ref, val) ->
              DriverWallet.WalletItem
                { itemReference = ref,
                  itemName = referenceTypeToItemName ref,
                  itemValue = val,
                  redeemableBalance = val,
                  nonRedeemableBalance = 0
                }
          )
          (Map.toList dedRefMap)
      dedTotal = sum (Map.elems dedRefMap)
      netEarnings = addTotal - dedTotal
   in ( DriverWallet.WalletItemGroup {totalAmount = addTotal, items = addItems},
        DriverWallet.WalletItemGroup {totalAmount = dedTotal, items = dedItems},
        topLevelNonRedeemable,
        netEarnings
      )

-- | Group the already-fetched window rows into per-bucket aggregates without
--   touching the database again. Sorts rows by timestamp once and walks
--   buckets/rows together in O(R log R + R + B).
--
--   Splits rows between consecutive buckets at the next bucket's start so the
--   1-second gap between Day buckets (Mon 23:59:59 -> Tue 00:00:00) doesn't
--   drop entries. The last bucket uses an inclusive end since there is no
--   successor.
bucketizeRows ::
  [Id Account] ->
  [(UTCTime, UTCTime)] ->
  [CHLE.WalletEntryRow] ->
  [DriverWallet.WalletAggregateBucket]
bucketizeRows accountIds windows rows =
  let sortedRows = sortOn (.walletTimestamp) rows
      isCredit e = e.walletToAccountId `elem` accountIds
      mkBucket bStart bEnd inBucket =
        let (creds, debs) = partition isCredit inBucket
            earnings = sum (map (.walletAmount) creds)
            deductionsT = sum (map (.walletAmount) debs)
         in DriverWallet.WalletAggregateBucket
              { from = bStart,
                to = bEnd,
                earnings = earnings,
                deductions = deductionsT,
                netEarnings = earnings - deductionsT
              }
      go [] _ = []
      go [(bStart, bEnd)] remaining =
        let valid = dropWhile (\e -> e.walletTimestamp < bStart) remaining
            inBucket = takeWhile (\e -> e.walletTimestamp <= bEnd) valid
         in [mkBucket bStart bEnd inBucket]
      go ((bStart, bEnd) : ws@((nextStart, _) : _)) remaining =
        let valid = dropWhile (\e -> e.walletTimestamp < bStart) remaining
            (inBucket, rest) = span (\e -> e.walletTimestamp < nextStart) valid
         in mkBucket bStart bEnd inBucket : go ws rest
   in go windows sortedRows

-- | Generate inclusive UTC `(from, to)` windows covering [fromDate, toDate]
--   bucketed by the requested aggregation level. Buckets are computed in
--   merchant local TZ (Day / ISO-week / calendar-month) and then clipped to
--   the user's requested window so partial first/last buckets are allowed.
generateBucketWindows ::
  DriverWallet.AggregationLevel ->
  Kernel.Prelude.NominalDiffTime ->
  UTCTime ->
  UTCTime ->
  [(UTCTime, UTCTime)]
generateBucketWindows level timeDiff fromDate toDate =
  let toLocalDay t = Data.Time.utctDay (Data.Time.addUTCTime timeDiff t)
      fromLocalDay = toLocalDay fromDate
      toLocalDay' = toLocalDay toDate
      localBuckets = case level of
        DriverWallet.Day -> [(d, d) | d <- [fromLocalDay .. toLocalDay']]
        DriverWallet.Week -> generateWeekBuckets fromLocalDay toLocalDay'
        DriverWallet.Month -> generateMonthBuckets fromLocalDay toLocalDay'
      toUtcWindow (startDay, endDay) =
        let localStart = Data.Time.UTCTime startDay 0
            localEnd = Data.Time.addUTCTime (-1) (Data.Time.UTCTime (Cal.addDays 1 endDay) 0)
            utcStart = max fromDate (Data.Time.addUTCTime (negate timeDiff) localStart)
            utcEnd = min toDate (Data.Time.addUTCTime (negate timeDiff) localEnd)
         in (utcStart, utcEnd)
   in map toUtcWindow localBuckets

-- | Enumerate ISO weeks (Monday-anchored) overlapping [startDay, endDay].
--   Each entry is (Monday, Sunday) of that week — caller clips to the
--   requested window.
generateWeekBuckets :: Cal.Day -> Cal.Day -> [(Cal.Day, Cal.Day)]
generateWeekBuckets startDay endDay =
  let mondayOf d = Cal.addDays (negate (fromIntegral (fromEnum (Cal.dayOfWeek d) - 1))) d
      go mon
        | mon > endDay = []
        | otherwise = (mon, Cal.addDays 6 mon) : go (Cal.addDays 7 mon)
   in go (mondayOf startDay)

-- | Enumerate calendar months overlapping [startDay, endDay]. Each entry is
--   (1st of month, last day of month) — caller clips to the requested window.
generateMonthBuckets :: Cal.Day -> Cal.Day -> [(Cal.Day, Cal.Day)]
generateMonthBuckets startDay endDay =
  let (sy, sm, _) = Cal.toGregorian startDay
      (ey, em, _) = Cal.toGregorian endDay
      step (y, m)
        | (y, m) > (ey, em) = []
        | otherwise =
          let firstDay = Cal.fromGregorian y m 1
              lastDay = Cal.fromGregorian y m (Cal.gregorianMonthLength y m)
              next = if m == 12 then (y + 1, 1) else (y, m + 1)
           in (firstDay, lastDay) : step next
   in step (sy, sm)

referenceTypeToItemName :: Text -> Text
referenceTypeToItemName ref
  | ref == walletReferenceBaseRide = "Ride Earnings"
  | ref == walletReferenceTollCharges = "Toll Charges"
  | ref == walletReferenceParkingCharges = "Parking Charges"
  | ref == walletReferenceTopup = "Wallet Top-up"
  | ref == walletReferenceGSTOnline = "GST (Online)"
  | ref == walletReferenceGSTCash = "GST (Cash)"
  | ref == walletReferenceVATOnline = "VAT (Online)"
  | ref == walletReferenceVATCash = "VAT (Cash)"
  | ref == walletReferenceTDSDeductionOnline = "TDS (Online)"
  | ref == walletReferenceTDSDeductionCash = "TDS (Cash)"
  | ref == walletReferencePayout = "Withdrawal"
  | ref == walletReferenceAirportCashRecharge = "Airport cash recharge (booth)"
  | ref == walletReferenceDiscountsOnline = "Discounts Incl. Vat (Online)"
  | ref == walletReferenceDiscountsCash = "Discounts Incl. Vat (Cash)"
  | ref == walletReferenceCommissionOnline = "Commission (Online)"
  | ref == walletReferenceCommissionCash = "Commission (Cash)"
  | ref == walletReferenceDeductedAtPaymentByPlatform = "Commission Deducted at Payment"
  | otherwise = ref

--------------------------------------------------------------------------------
-- getWalletTransactionHistory (paginated per-row ledger entries)
--------------------------------------------------------------------------------

getWalletTransactionHistory ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow DriverWallet.WalletTransactionHistoryResponse
  )
getWalletTransactionHistory (mbPersonId, _merchantId, _mocId) mbFromDate mbToDate mbLimit mbOffset = do
  validateInput
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let counterParty = counterpartyFromRole person.role
  mbWalletAcc <- getWalletAccountByOwner counterParty driverId.getId
  case mbWalletAcc of
    Nothing -> pure DriverWallet.WalletTransactionHistoryResponse {items = []}
    Just walletAcc -> do
      entries <- QLedgerEntry.findByAccountsWithOptions [walletAcc.id] [] mbFromDate mbToDate mbLimit mbOffset
      let topupOrderIds = [Id e.referenceId | e <- entries, e.referenceType == walletReferenceTopup]
      walletTxns <- QWalletTransaction.findAllByPaymentOrderIds topupOrderIds
      let walletStatusByOrderId = Map.fromList [(wt.paymentOrderId.getId, wt.status) | wt <- walletTxns]
      let mkItem e =
            let isTopup = e.referenceType == walletReferenceTopup
                paymentOrder =
                  if isTopup
                    then
                      Just
                        DriverWallet.PaymentOrderInfo
                          { id = Id e.referenceId,
                            walletStatus = fromMaybe DWT.SUCCESS (Map.lookup e.referenceId walletStatusByOrderId)
                          }
                    else Nothing
             in DriverWallet.WalletTransactionHistoryItem
                  { itemReference = e.referenceType,
                    itemName = referenceTypeToItemName e.referenceType,
                    itemValue = e.amount,
                    isCredit = e.toAccountId == walletAcc.id,
                    status = e.status,
                    paymentOrder = paymentOrder,
                    date = e.timestamp
                  }
      pure DriverWallet.WalletTransactionHistoryResponse {items = map mkItem entries}
  where
    validateInput =
      unless ((isJust mbFromDate && isJust mbToDate) || (isJust mbLimit && isJust mbOffset)) $
        throwError $ InvalidRequest "Either fromDate+toDate or limit+offset must be provided"

--------------------------------------------------------------------------------
-- getWalletPayoutHistory
--------------------------------------------------------------------------------

getWalletPayoutHistory ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe [PR.PayoutRequestStatus] ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow DriverWallet.PayoutHistoryResponse
  )
getWalletPayoutHistory (mbPersonId, _merchantId, _mocId) mbFrom mbTo mbStatuses mbLimit mbOffset = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  let statuses = fromMaybe [] mbStatuses
      limit = mbLimit
      offset = mbOffset

  items <- PayoutItems.getPayoutItems driverId.getId mbFrom mbTo statuses limit offset

  let apiItems = map toHistoryItem items
      totalPaidOut = sum [fromMaybe 0 i.amount | i <- items, isPaidOut i.status]
      totalPending = sum [fromMaybe 0 i.amount | i <- items, isPending i.status]
      lastPayout = Kernel.Prelude.listToMaybe [toHistoryItem i | i <- items, isPaidOut i.status]

  pure $
    DriverWallet.PayoutHistoryResponse
      { totalPaidOut,
        lastPayout,
        totalPending,
        items = apiItems
      }
  where
    isPaidOut s = s == PR.CREDITED || s == PR.CASH_PAID
    isPending s = s == PR.INITIATED || s == PR.PROCESSING

    toHistoryItem :: PayoutItems.PayoutItem -> DriverWallet.PayoutHistoryItem
    toHistoryItem i =
      DriverWallet.PayoutHistoryItem
        { amount = i.amount,
          payoutFee = i.payoutFee,
          entityName = i.entityName,
          status = i.status,
          timestamp = i.timestamp,
          payoutMethod = i.payoutMethod,
          payoutVpa = i.payoutVpa
        }

--------------------------------------------------------------------------------
-- postWalletPayout
--------------------------------------------------------------------------------

-- | All context needed for payout operations, loaded once at the start.
data PayoutContext = PayoutContext
  { driverId :: Kernel.Types.Id.Id DP.Person,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    mocId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    person :: DP.Person,
    payoutVpa :: Maybe Text,
    transporterConfig :: DTConf.TransporterConfig
  }

loadPayoutContext ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person) ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m PayoutContext
loadPayoutContext mbPersonId merchantId mocId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) (Just (SCTC.findByMerchantOpCityId mocId Nothing)) >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  payoutVpa <- case person.role of
    DP.FLEET_OWNER -> do
      mbFleetInfo <- QFOI.findByPersonIdAndEnabledAndVerified Nothing Nothing driverId
      pure (mbFleetInfo >>= (.payoutVpa))
    DP.FLEET_BUSINESS -> do
      mbFleetInfo <- QFOI.findByPersonIdAndEnabledAndVerified Nothing Nothing driverId
      pure (mbFleetInfo >>= (.payoutVpa))
    _ -> do
      mbDriverInfo <- QDI.findById driverId
      pure (mbDriverInfo >>= (.payoutVpa))
  pure PayoutContext {..}

-- | Validate that wallet payouts are enabled for this merchant.
ensurePayoutsEnabled :: (MonadFlow m) => PayoutContext -> m ()
ensurePayoutsEnabled ctx =
  unless ctx.transporterConfig.driverWalletConfig.enableWalletPayout $
    throwError $ InvalidRequest "Payouts are disabled"

-- | Check that the driver hasn't exceeded the maximum daily payout count.
ensurePayoutLimitNotReached ::
  (BeamFlow m r) =>
  PayoutContext ->
  Maybe (Id Account) ->
  UTCTime ->
  m ()
ensurePayoutLimitNotReached ctx mbAccountId now = do
  let timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
      (utcStartOfDay, utcEndOfDay) = todayRangeUTC timeDiff now
  whenJust ctx.transporterConfig.driverWalletConfig.maxWalletPayoutsPerDay $ \maxPayoutsPerDay -> do
    payoutsToday <- case mbAccountId of
      Nothing -> pure []
      Just accountId -> findByAccountWithFilters accountId (Just utcStartOfDay) (Just utcEndOfDay) Nothing Nothing Nothing (Just [walletReferencePayout])
    when (length payoutsToday >= maxPayoutsPerDay) $
      throwError $ InvalidRequest "Maximum payouts per day reached"

-- | Validate that the payoutable balance meets the minimum threshold.
ensureMinimumPayoutAmount :: (MonadFlow m) => PayoutContext -> HighPrecMoney -> m ()
ensureMinimumPayoutAmount ctx payoutableBalance = do
  let minAmount = ctx.transporterConfig.driverWalletConfig.minimumWalletPayoutAmount
  when (payoutableBalance < minAmount) $
    throwError $ InvalidRequest ("Minimum payout amount is " <> show minAmount)

-- | Extract and validate the payout VPA (works for both drivers and fleet owners).
resolvePayoutVpa :: (MonadFlow m) => PayoutContext -> m Text
resolvePayoutVpa ctx =
  fromMaybeM (InternalError $ "Payout vpa not present for " <> ctx.driverId.getId) ctx.payoutVpa

postWalletPayout ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APISuccess.APISuccess
  )
postWalletPayout (mbPersonId, merchantId, mocId) = do
  ctx <- loadPayoutContext mbPersonId merchantId mocId
  ensurePayoutsEnabled ctx
  let counterparty = counterpartyFromRole ctx.person.role
  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 10 $ do
    now <- getCurrentTime
    mbAccount <- getWalletAccountByOwner counterparty ctx.driverId.getId
    let mbAccountId = (.id) <$> mbAccount
    walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty ctx.driverId.getId
    ensurePayoutLimitNotReached ctx mbAccountId now
    -- Single query: get both non-redeemable balance and redeemable entry IDs
    let timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
        cutOffDays = ctx.transporterConfig.driverWalletConfig.payoutCutOffDays
        cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
    (nonRedeemable, redeemableIds, merchantTransferAmt) <- case mbAccountId of
      Nothing -> pure (0, [], 0)
      Just accountId -> getPayoutEligibilityData accountId cutoff now
    logInfo $ "Payout eligibility for driver " <> ctx.driverId.getId <> ": walletBalance=" <> show walletBalance <> ", nonRedeemable=" <> show nonRedeemable <> ", redeemableEntryIds=" <> show redeemableIds
    let payoutableBalance = walletBalance - nonRedeemable
    ensureMinimumPayoutAmount ctx payoutableBalance
    initiateWalletPayout ctx payoutableBalance PR.INSTANT Nothing (Just cutoff) (map (.getId) redeemableIds) merchantTransferAmt
  pure APISuccess.Success

-- | Compute the payout fee based on the PayoutFeeConfig.
--   Returns 0 if no config is set.
computePayoutFee :: Maybe DTConf.PayoutFeeConfig -> HighPrecMoney -> HighPrecMoney
computePayoutFee Nothing _ = 0
computePayoutFee (Just feeConfig) amount =
  case feeConfig.feeType of
    DTConf.PERCENTAGE -> SPayment.roundToTwoDecimalPlaces $ amount * feeConfig.feeValue / 100
    DTConf.FIXED -> min feeConfig.feeValue amount -- fee cannot exceed the amount

-- | Create a PayoutRequest (INITIATED), then call Juspay createPayoutService (→ PROCESSING).
--   No ledger entry here — that happens in the webhook handler on SUCCESS.
initiateWalletPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    BeamFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r
  ) =>
  PayoutContext ->
  HighPrecMoney -> -- payoutable balance
  PR.PayoutType -> -- INSTANT or SCHEDULED
  Maybe UTCTime -> -- coverageFrom
  Maybe UTCTime -> -- coverageTo
  [Text] -> -- redeemable entry IDs for settlement
  HighPrecMoney -> -- merchant transfer amount (VAT input + discounts)
  m ()
initiateWalletPayout ctx payoutableBalance payoutType coverageFrom coverageTo redeemableEntryIds merchantTransferAmount = do
  phoneNo <- mapM decrypt ctx.person.mobileNumber
  merchantOperatingCity <- CQMOC.findById (Kernel.Types.Id.cast ctx.person.merchantOperatingCityId) >>= fromMaybeM (MerchantOperatingCityNotFound ctx.person.merchantOperatingCityId.getId)
  (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- Payout.getCreatePayoutServiceFlow (Payout.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService ctx.person.clientSdkVersion ctx.person.merchantOperatingCityId ctx.person.id
  vpa <- case payoutServiceFlow of
    IPayout.JuspayFlow -> Just <$> resolvePayoutVpa ctx
    IPayout.StripeFlow -> pure Nothing
  let fee = computePayoutFee ctx.transporterConfig.driverWalletConfig.payoutFee payoutableBalance
      netAmount = SPayment.roundToTwoDecimalPlaces (payoutableBalance - fee)
      submission =
        PayoutRequest.PayoutSubmission
          { beneficiaryId = ctx.driverId.getId,
            entityName = DPayment.DRIVER_WALLET_TRANSACTION,
            entityId = ctx.driverId.getId,
            entityRefId = Nothing,
            amount = netAmount,
            currency = ctx.transporterConfig.currency,
            payoutFee = if fee > 0 then Just fee else Nothing,
            transferAmount = Just merchantTransferAmount,
            merchantId = ctx.merchantId.getId,
            merchantOpCityId = ctx.mocId.getId,
            city = show merchantOperatingCity.city,
            vpa = vpa,
            customerName = Just ctx.person.firstName,
            customerPhone = phoneNo,
            customerEmail = ctx.person.email,
            remark = "Settlement for wallet",
            orderType = "FULFILL_ONLY",
            scheduledAt = Nothing,
            payoutType = Just payoutType,
            coverageFrom = coverageFrom,
            coverageTo = coverageTo,
            ledgerEntryIds = [], -- driver side keeps Redis stash flow unchanged
            payoutServiceFlow
          }
      payoutCall = Payout.createPayoutOrder payoutServiceName ctx.person.merchantOperatingCityId ctx.person.id mbPersonBankAccount

  when (netAmount > 0.0) $ do
    result <- PayoutRequest.submitPayoutRequest submission payoutCall
    case result of
      PayoutRequest.PayoutInitiated pr _ -> do
        -- Stash redeemable entry IDs in Redis for the webhook handler to settle
        unless (null redeemableEntryIds) $
          Redis.setExp (makePayoutEntryIdsKey pr.id.getId) redeemableEntryIds 86400 -- 24h TTL
        Notify.sendNotificationToDriver ctx.person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED "Payout Initiated" ("Your payout of " <> show netAmount <> " has been initiated." <> if fee > 0 then " (Fee: " <> show fee <> ")" else "") ctx.person ctx.person.deviceToken
      PayoutRequest.PayoutFailed _ err ->
        logError $ "Wallet payout failed for driver " <> ctx.driverId.getId <> ": " <> err

-- | Redis key for stashing redeemable entry IDs during payout.
makePayoutEntryIdsKey :: Text -> Text
makePayoutEntryIdsKey payoutRequestId = "payout-entry-ids:" <> payoutRequestId

--------------------------------------------------------------------------------
-- postWalletTopup (finance ledger: platform Asset -> driver RideCredit, reference WalletTopup)
--------------------------------------------------------------------------------

postWalletTopup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    DriverWallet.TopUpRequest ->
    Environment.Flow PlanSubscribeRes
  )
postWalletTopup (mbPersonId, merchantId, mocId) = doWalletTopup mbPersonId merchantId mocId
  where
    doWalletTopup mbP mId mocId0 r =
      do
        driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbP
        driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
        when driverInfo.blocked $ throwError (DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag))
        when (r.amount <= 0) $ throwError $ InvalidRequest "Top-up amount must be greater than zero"
        Redis.whenWithLockRedisAndReturnValue (makeWalletTopupLockKey driverId.getId) 10 $ do
          mbExisting <- QWalletTransaction.findByDriverIdAndStatus driverId DWT.INITIATED
          let mbReuseOrderId = case mbExisting of
                Just e | e.amount == r.amount -> Just e.paymentOrderId
                _ -> Nothing
          (createOrderResp, returnedOrderId) <- SPayment.createWalletTopupOrder (driverId, mId, mocId0) r.amount mbReuseOrderId
          handleWalletTransaction driverId mId mocId0 r.amount mbExisting returnedOrderId
          pure $
            PlanSubscribeRes
              { orderId = returnedOrderId,
                orderResp = createOrderResp
              }
        >>= \case
          Right res -> pure res
          Left _ -> throwError WalletTopupLockFailed

    handleWalletTransaction driverId mId mocId0 amount mbExisting returnedOrderId =
      case mbExisting of
        Just existing
          | existing.paymentOrderId == returnedOrderId ->
            pure ()
        Just existing -> do
          QWalletTransaction.updateStatus DWT.EXPIRED existing.paymentOrderId
          insertWalletTransaction driverId mId mocId0 returnedOrderId amount
        Nothing ->
          insertWalletTransaction driverId mId mocId0 returnedOrderId amount

    insertWalletTransaction driverId mId mocId0 paymentOrderId amount = do
      walletTxnId <- generateGUID
      now <- getCurrentTime
      QWalletTransaction.create $
        DWT.WalletTransaction
          { id = Id walletTxnId,
            driverId = driverId,
            paymentOrderId = paymentOrderId,
            amount = amount,
            status = DWT.INITIATED,
            merchantId = Just mId,
            merchantOperatingCityId = Just mocId0,
            createdAt = now,
            updatedAt = now
          }

    makeWalletTopupLockKey :: Text -> Text
    makeWalletTopupLockKey dId = "wallet-topup-lock:" <> dId

--------------------------------------------------------------------------------
-- recordAirportCashRecharge (booth operator took cash; credit driver wallet, idempotent by referenceId)
--------------------------------------------------------------------------------

-- | Shared helper to build a FinanceCtx for driver wallet recharges (topup/cash).
--   Populates merchant/supplier metadata so invoices and analytics have rich context.
-- TODO: Add supplier name and GSTIN
mkDriverWalletFinanceCtx ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DP.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Currency ->
  Text -> -- referenceId (order id or booth receipt id)
  m FinanceCtx
mkDriverWalletFinanceCtx driverId merchantId mocId currency referenceId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
  let mName = Just merchant.name
      mShortId = Just merchant.shortId.getShortId
      address =
        Just $
          show merchantOpCity.city
            <> ", "
            <> show merchantOpCity.state
            <> ", "
            <> show merchantOpCity.country
      supplierName = Nothing
      supplierGSTIN = Nothing
      supplierId = Nothing
  pure
    FinanceCtx
      { merchantId = merchantId.getId,
        merchantOpCityId = mocId.getId,
        currency = currency,
        isOnline = True,
        counterpartyType = FAccount.DRIVER,
        counterpartyId = driverId.getId,
        concernedIndividualId = Just driverId.getId,
        referenceId = referenceId,
        merchantName = mName,
        merchantShortId = mShortId,
        issuedByAddress = address,
        supplierName = supplierName,
        supplierGSTIN = supplierGSTIN,
        merchantGstin = Nothing,
        supplierVatNumber = Nothing,
        supplierAddress = Nothing,
        merchantVatNumber = Nothing,
        supplierId = supplierId,
        panOfParty = Nothing,
        panType = Nothing,
        tdsRateReason = Nothing,
        emitLedgerEntries = True,
        fromLocationAddress = Nothing,
        issuedToName = Nothing
      }

-- | Record airport booth cash recharge: credit driver wallet (PlatformAsset → OwnerLiability)
--   with referenceType = walletReferenceAirportCashRecharge. Idempotent by referenceId (e.g. booth receipt id).
recordAirportCashRecharge ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  (Id DP.Person, Id Domain.Types.Merchant.Merchant, Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  HighPrecMoney ->
  Text -> -- referenceId for idempotency (e.g. booth receipt id)
  m ()
recordAirportCashRecharge (driverId, merchantId, mocId) amount referenceId = do
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInfo.blocked $ throwError (DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag))
  when (amount <= 0) $ throwError $ InvalidRequest "Cash recharge amount must be greater than zero"
  existing <- getEntriesByReference walletReferenceAirportCashRecharge referenceId
  when (null existing) $ do
    merchantOpCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
    let currency = merchantOpCity.currency
    ctx <- mkDriverWalletFinanceCtx driverId merchantId mocId currency referenceId
    let cashRechargeInvoiceConfig =
          InvoiceConfig
            { invoiceType = SubscriptionPurchase,
              issuedToType = DRIVER,
              issuedToId = driverId.getId,
              issuedToName = Nothing,
              issuedToAddress = Nothing,
              referenceId = Nothing,
              lineItems = [InvoiceLineItem {description = "Airport Cash Recharge", descriptionType = Just AirportCashRecharge, quantity = 1, unitPrice = amount, lineTotal = amount, isExternalCharge = False, groupId = Just "g-airport", itemType = Just Fare}],
              gstBreakdown = Nothing,
              isVat = False,
              issuedToTaxNo = Nothing,
              issuedByTaxNo = Nothing,
              paymentMode = Just "CASH",
              periodStart = Nothing,
              periodEnd = Nothing
            }
    result <- runFinance ctx $ do
      _ <- transfer PlatformAsset OwnerLiability amount walletReferenceAirportCashRecharge
      invoice cashRechargeInvoiceConfig
    void $ fromEitherM (\e -> WalletLedgerEntryFailed ("airport cash recharge: " <> show e)) result
