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
    clearDuesByWalletTopup,
    recordAirportCashRecharge,
    getWalletPayoutHistory,
    counterpartyFromRole,
    mkDriverWalletFinanceCtx,
  )
where

import qualified API.Types.UI.DriverWallet as DriverWallet
import Data.List (partition, span)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time
import qualified Data.Time.Calendar as Cal
import qualified Domain.Action.UI.Payout as UIPayout
import Domain.Action.UI.Plan hiding (mkDriverFee)
import qualified Domain.Types.DriverFee as DDF
import Domain.Types.DriverInformation as DI
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DP
import Domain.Types.Plan (ServiceNames (YATRI_SUBSCRIPTION))
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.WalletTransaction as DWT
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.HideSecrets
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance
  ( Account,
    AccountRole (OwnerLiability, PlatformAsset),
    FinanceCtx (..),
    InvoiceConfig (..),
    InvoiceLineItem (..),
    ItemType (..),
    LedgerEntryMetadata (..),
    LineItemDescription (..),
    getEntriesByReference,
    invoice,
    markEntriesAsClaimed,
    transfer,
  )
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Account as FAccount
import qualified Lib.Finance.Domain.Types.LedgerEntry as FLedgerEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerEntry
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.PayoutItems as PayoutItems
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra
import SharedLogic.Finance.PostActions (runFinance)
import SharedLogic.Finance.Prepaid (counterpartyFleetOwner)
import SharedLogic.Finance.Wallet
import qualified SharedLogic.Finance.WalletAccount as WA
import qualified SharedLogic.Finance.WalletAutopay as WalletAutopay
import SharedLogic.Finance.WalletPayout (WalletPayoutParams (..), computePayoutFee, ensureInstantPayoutAllowed, ensurePayoutsEnabled, instantPayoutAllowedFor, loadPayoutContext)
import qualified SharedLogic.Finance.WalletSubscription as WalletSubscription
import qualified SharedLogic.Payment as SPayment
import SharedLogic.Payout.Bulk.Driver (runInstantPayout)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Clickhouse.LedgerEntry as CHLE
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Mandate as QMandate
import qualified Storage.Queries.Notification as QNotification
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.WalletTransaction as QWalletTransaction
import Tools.Error

instance Kernel.Types.HideSecrets.HideSecrets DriverWallet.TopUpRequest where
  hideSecrets = Kernel.Prelude.identity

-- | Pick the counterparty type based on the person's role.

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
  -- TODO: Stage, minimum balance, Autopay and withdrawable fields assume the subscription/GST wallet
  -- model. Validate against MSIL and International use cases before relying on them there.
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  let dwc = transporterConfig.driverWalletConfig
      baseResponse =
        DriverWallet.WalletBalanceResponse
          { currentBalance,
            walletStage = Nothing,
            minimumBalance = Nothing,
            blockerEnforced = Nothing,
            isSubscriptionWalletManaged = Nothing,
            withdrawableBalance = Nothing,
            autopay = Nothing
          }
  if not dwc.enableDriverWallet
    then pure baseResponse
    else do
      mbVariant <- fmap (.variant) <$> QVehicle.findById driverId
      tiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing
      let mbMinimum = mbVariant >>= WA.tierMinimum tiers
          isLive = WA.isWalletSubscriptionCity dwc
      -- A wallet that isn't charged from yet, in a city where no tier minimum is configured, has nothing
      -- new to show: the response stays what it always was.
      if not isLive && isNothing mbMinimum
        then pure baseResponse
        else do
          withdrawable <- getWithdrawableBalance person transporterConfig counterparty currentBalance
          mbDriverPlan <- QDPlan.findByDriverIdWithServiceName driverId YATRI_SUBSCRIPTION
          mbMandate <- maybe (pure Nothing) QMandate.findById (mbDriverPlan >>= (.mandateId))
          mbRecharge <- WalletAutopay.findOpenAutopayRecharge driverId
          let autopayInfo =
                DriverWallet.WalletAutopayInfo
                  { status = mbDriverPlan >>= (.autoPayStatus),
                    mandateMaxAmount = (.maxAmount) <$> mbMandate,
                    triggerBalance = Just (WA.autopayTriggerBalance dwc tiers mbVariant),
                    nextRechargeAmount = (\recharge -> recharge.platformFee.fee) <$> mbRecharge,
                    nextRechargeAt = (.payBy) <$> mbRecharge
                  }
          if not isLive
            then
              pure
                baseResponse
                  { DriverWallet.walletStage = Just DriverWallet.COMING_SOON,
                    DriverWallet.minimumBalance = mbMinimum,
                    -- The minimum can be enforced before the subscription moves to the wallet.
                    DriverWallet.blockerEnforced = Just (WA.isMinBalanceEnforced dwc),
                    DriverWallet.withdrawableBalance = withdrawable,
                    DriverWallet.autopay = Just autopayInfo
                  }
            else
              pure
                baseResponse
                  { DriverWallet.walletStage = Just DriverWallet.LIVE,
                    DriverWallet.minimumBalance = mbMinimum,
                    DriverWallet.blockerEnforced = Just (WA.isMinBalanceEnforced dwc),
                    DriverWallet.isSubscriptionWalletManaged = Just True,
                    DriverWallet.withdrawableBalance = withdrawable,
                    DriverWallet.autopay = Just autopayInfo
                  }

-- | What the driver could withdraw right now (the same figure the payout uses: redeemable credit minus
--   ride and offer holds). Only computed where the driver can withdraw (wallet payouts on and instant
--   payout allowed for the role); elsewhere the balance response stays cheap and carries no figure.
getWithdrawableBalance ::
  DP.Person ->
  DTConf.TransporterConfig ->
  FAccount.CounterpartyType ->
  HighPrecMoney ->
  Environment.Flow (Maybe HighPrecMoney)
getWithdrawableBalance person transporterConfig counterparty currentBalance
  -- Same gate as the summary's payoutEnabled: payouts on, and instant payout allowed for this role.
  | not (transporterConfig.driverWalletConfig.enableWalletPayout && instantPayoutAllowedFor transporterConfig.driverWalletConfig person.role) = pure Nothing
  | otherwise = do
    now <- getCurrentTime
    mbWalletAcc <- WA.getWalletAccountByOwner counterparty person.id.getId
    case mbWalletAcc of
      Nothing -> pure (Just 0)
      Just walletAcc -> do
        let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
            cutoff = payoutCutoffTimeUTC timeDiff transporterConfig.driverWalletConfig.payoutCutOffDays now
        eligibility <- getPayoutEligibilityData counterparty person.id.getId walletAcc.id currentBalance cutoff now
        (_, redeemableBalance) <- redeemableAfterHolds counterparty person.id.getId eligibility.redeemableBalance
        pure (Just redeemableBalance)

-- | The redeemable balance net of what is still held in the wallet (PENDING ride holds and Redis offer
--   holds both sit inside the balance), with the total held. The one figure the payout, the wallet
--   summary and the balance API all use.
redeemableAfterHolds :: FAccount.CounterpartyType -> Text -> HighPrecMoney -> Environment.Flow (HighPrecMoney, HighPrecMoney)
redeemableAfterHolds counterparty ownerId eligibleRedeemable = do
  dbHoldBalance <- getWalletHoldBalanceByOwner counterparty ownerId
  offerHoldBalance <- getWalletOfferHoldTotal ownerId
  pure (dbHoldBalance + offerHoldBalance, max 0 (eligibleRedeemable - dbHoldBalance - offerHoldBalance))

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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  -- Per city: FLEET_ACCOUNT (default) shows the fleet owner's account filtered to this driver's rides,
  -- OWN_ACCOUNT only the driver's own account, HIDDEN nothing.
  let mbAssocFleetOwnerId = (.fleetOwnerId) <$> mbActiveFleetAssoc
      walletView = fromMaybe DTConf.FLEET_ACCOUNT transporterConfig.driverWalletConfig.fleetLinkedDriverWalletView
      ownAccount = (counterpartyFromRole person.role, driverId.getId, Nothing)
      (counterparty, ownerId, mbConcernedIndividualId) =
        case mbAssocFleetOwnerId of
          Just fleetOwnerId | walletView == DTConf.FLEET_ACCOUNT -> (counterpartyFleetOwner, fleetOwnerId, Just driverId.getId)
          _ -> ownAccount
  let canWithdraw = instantPayoutAllowedFor transporterConfig.driverWalletConfig person.role
  now <- getCurrentTime
  if isJust mbAssocFleetOwnerId && walletView == DTConf.HIDDEN
    then pure emptyWalletSummary {DriverWallet.payoutConfig = buildWalletPayoutConfig canWithdraw transporterConfig.driverWalletConfig 0}
    else walletTransactionsFor canWithdraw counterparty ownerId mbConcernedIndividualId transporterConfig now mbFromDate mbToDate mbAggBy

-- | The wallet read itself, once it has been decided whose account to read.
walletTransactionsFor ::
  Bool -> -- whether the app may offer this person an instant payout
  FAccount.CounterpartyType ->
  Text ->
  Kernel.Prelude.Maybe Text ->
  DTConf.TransporterConfig ->
  Data.Time.UTCTime ->
  Kernel.Prelude.Maybe Data.Time.UTCTime ->
  Kernel.Prelude.Maybe Data.Time.UTCTime ->
  Kernel.Prelude.Maybe DriverWallet.AggregationLevel ->
  Environment.Flow DriverWallet.WalletSummaryResponse
walletTransactionsFor canWithdraw counterparty ownerId mbConcernedIndividualId transporterConfig now mbFromDate mbToDate mbAggBy = do
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      fromDate = fromMaybe (Data.Time.UTCTime (Data.Time.utctDay now) 0) mbFromDate
      toDate = fromMaybe now mbToDate
      aggBy = fromMaybe DriverWallet.Day mbAggBy
      cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
      cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  (mbWalletAcc, mbControlAcc) <- getWalletAndControlAccountsByOwner counterparty ownerId
  case (mbWalletAcc, mbControlAcc) of
    (Nothing, Nothing) -> pure emptyWalletSummary {DriverWallet.payoutConfig = buildWalletPayoutConfig canWithdraw transporterConfig.driverWalletConfig 0}
    _ -> do
      currentBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty ownerId
      let accountIds = catMaybes [(.id) <$> mbWalletAcc, (.id) <$> mbControlAcc]
          useClickhouse = fromMaybe False transporterConfig.driverWalletConfig.fetchWalletTransactionsFromClickhouse
      rows <-
        if useClickhouse
          then fetchWalletRowsFromCH accountIds mbConcernedIndividualId fromDate toDate
          else fetchWalletRowsFromLedger accountIds mbConcernedIndividualId fromDate toDate
      eligibility <- case mbWalletAcc of
        Nothing -> pure emptyWalletPayoutEligibility
        Just walletAcc -> getPayoutEligibilityData counterparty ownerId walletAcc.id currentBalance cutoff now
      (holdBalance, redeemableBalance) <- redeemableAfterHolds counterparty ownerId eligibility.redeemableBalance
      let (additions, deductions, netEarningsBalance) =
            aggregateWalletRows (isVatMerchant transporterConfig) accountIds cutoff rows
          agg = bucketizeRows accountIds (generateBucketWindows aggBy timeDiff fromDate toDate) rows
      pure $
        DriverWallet.WalletSummaryResponse
          { currentBalance,
            redeemableBalance,
            nonRedeemableBalance = eligibility.nonRedeemableBalance,
            processingPayoutBalance = eligibility.processingPayoutBalance,
            holdBalance = Just holdBalance,
            netEarningsBalance,
            payoutConfig = buildWalletPayoutConfig canWithdraw transporterConfig.driverWalletConfig redeemableBalance,
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
      processingPayoutBalance = 0,
      holdBalance = Just 0,
      netEarningsBalance = 0,
      payoutConfig =
        DriverWallet.WalletPayoutConfig
          { payoutEnabled = False,
            payoutCutOffDays = 0,
            minimumPayoutAmount = 0,
            payoutFee = 0
          },
      additions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []},
      deductions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []},
      agg = []
    }

-- | @payoutEnabled@ drives the withdraw button, so it is off for roles 'postWalletPayout' would
--   refuse (@instantPayoutExcludedRoles@).
buildWalletPayoutConfig :: Bool -> DTConf.DriverWalletConfig -> HighPrecMoney -> DriverWallet.WalletPayoutConfig
buildWalletPayoutConfig canWithdraw walletConfig redeemableBalance =
  DriverWallet.WalletPayoutConfig
    { payoutEnabled = walletConfig.enableWalletPayout && canWithdraw,
      payoutCutOffDays = walletConfig.payoutCutOffDays,
      minimumPayoutAmount = walletConfig.minimumWalletPayoutAmount,
      payoutFee = computePayoutFee (mfilter (\cfg -> cfg.feeBearer == Just DTConf.DRIVER_BEARER) walletConfig.payoutFee) redeemableBalance
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
  -- Payouts are a settlement of already-earned money, not a deduction from earnings,
  -- so they are excluded from the transaction breakdown entirely.
  let allRefs = walletCreditRefs ++ [walletReferenceAirportCashWithdrawal, walletReferenceGateDriverFee]
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
  -- Payouts are a settlement of already-earned money, not a deduction from earnings,
  -- so they are excluded from the transaction breakdown entirely.
  let allRefs = walletCreditRefs ++ [walletReferenceAirportCashWithdrawal, walletReferenceGateDriverFee]
  CHLE.findWalletEntries accountIds mbConcernedIndividualId fromDate toDate allRefs

-- | Per-reference additions/deductions groups and the net earnings for the window.
--   Items still carry the payout-cutoff split, but the response's own
--   redeemable/non-redeemable pair is NOT derived from these: that pair splits the
--   current balance and is computed by the caller, so the two answer different
--   questions and are not expected to agree.
aggregateWalletRows ::
  Bool -> -- merchant charges VAT rather than GST
  [Id Account] ->
  UTCTime -> -- payout cutoff time
  [CHLE.WalletEntryRow] ->
  (DriverWallet.WalletItemGroup, DriverWallet.WalletItemGroup, HighPrecMoney)
aggregateWalletRows isVat accountIds cutoff entries =
  let (addEntries, dedEntries) = partition (\e -> e.walletToAccountId `elem` accountIds) entries

      addRefMap = foldl' (\acc e -> Map.insertWith (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) e.walletReferenceType (e.walletAmount, if e.walletTimestamp >= cutoff then e.walletAmount else 0) acc) Map.empty addEntries
      addItems =
        map
          ( \(ref, (total, nonRedeem)) ->
              DriverWallet.WalletItem
                { itemReference = ref,
                  itemName = referenceTypeToItemName isVat ref,
                  itemValue = total,
                  redeemableBalance = max 0 (total - nonRedeem),
                  nonRedeemableBalance = nonRedeem
                }
          )
          (Map.toList addRefMap)
      addTotal = sum (map (\(_, (t, _)) -> t) (Map.toList addRefMap))

      dedRefMap = foldl' (\acc e -> Map.insertWith (+) e.walletReferenceType e.walletAmount acc) Map.empty dedEntries
      dedItems =
        map
          ( \(ref, val) ->
              DriverWallet.WalletItem
                { itemReference = ref,
                  itemName = referenceTypeToItemName isVat ref,
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

referenceTypeToItemName :: Bool -> Text -> Text
referenceTypeToItemName isVat ref
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
  | ref == walletReferenceTDSDeductionCancellation = "TDS (Cancellation)"
  | ref == walletReferencePayout = "Withdrawal"
  | ref == walletReferenceGateDriverFee = "Gate Fee"
  | ref == walletReferenceAirportCashRecharge = "Airport cash recharge (booth)"
  | ref == walletReferenceAirportCashWithdrawal = "Airport cash withdrawal (booth)"
  | ref == walletReferenceDiscountsOnline = "Discounts Incl. " <> taxLabel <> " (Online)"
  | ref == walletReferenceDiscountsCash = "Discounts Incl. " <> taxLabel <> " (Cash)"
  | ref == walletReferenceCustomerCancellationCharges = "Cancellation Fee"
  | ref == walletReferenceCustomerCancellationGST = "Cancellation Fee " <> taxLabel
  | ref == walletReferenceCommissionOnline = "Commission (Online)"
  | ref == walletReferenceCommissionCash = "Commission (Cash)"
  | ref == walletReferenceCommissionVATOnline = "Commission " <> taxLabel <> " (Online)"
  | ref == walletReferenceCommissionVATCash = "Commission " <> taxLabel <> " (Cash)"
  | ref == walletReferenceCancellationCommission = "Cancellation Commission"
  | ref == walletReferenceCancellationCommissionVAT = "Cancellation Commission " <> taxLabel
  | ref == walletReferenceDeductedAtPaymentByPlatform = "Commission Deducted at Payment"
  | ref == walletReferencePaymentChargePaidByCustomer = "Payment Charge Paid by Customer"
  | ref == walletReferencePaymentChargeVatPaidByCustomer = "Payment Charge VAT Paid by Customer"
  | ref == walletReferencePGPaymentCharges = "Payment Charge"
  | ref == walletReferencePGPaymentChargesVAT = "Payment Charge VAT"
  | ref == walletReferencePGPayoutCharges = "Payout Charge"
  | ref == walletReferenceConnectAccountCharges = "Connect Account Charge"
  | ref == WalletSubscription.walletReferenceSubscriptionFee = "Subscription Fee"
  | ref == WalletSubscription.walletReferenceSubscriptionGST = "Subscription Fee " <> taxLabel
  | ref == WalletSubscription.walletReferenceSubscriptionPenalty = "Cancellation Penalty"
  | ref == WalletSubscription.walletReferenceMigratedDues = "Pending Dues"
  | ref == WalletSubscription.walletReferenceMigratedDuesGST = "Pending Dues " <> taxLabel
  | ref == WalletSubscription.walletReferenceMigratedDuesPenalty = "Pending Cancellation Penalty"
  | ref == WalletSubscription.walletReferenceAutopayRecharge = "Wallet Recharge (Autopay)"
  | ref == WalletSubscription.walletReferenceLatePaymentCredit = "Late Payment Credited"
  | ref == WalletSubscription.walletReferenceBehaviourPenalty = "Penalty"
  | otherwise = ref
  where
    taxLabel = if isVat then "VAT" else "GST"

isVatMerchant :: DTConf.TransporterConfig -> Bool
isVatMerchant transporterConfig = isJust transporterConfig.taxConfig.serviceVatPercentage

-- | Per-row item name. A gate DriverFeeItem is booked under one shared reference
--   type but is configured per gate, so its own name is carried on the ledger entry
--   metadata and shown here in place of the generic bucket label. Every other
--   reference type has a fixed name. The wallet summary still aggregates gate fees
--   under the bucket label -- it groups by reference type and has no metadata.
walletRowItemName :: Bool -> Text -> Maybe Text -> Text
walletRowItemName isVat ref mbReason
  | ref == walletReferenceGateDriverFee,
    Just reason <- mbReason,
    not (T.null reason) =
    reason
  | otherwise = referenceTypeToItemName isVat ref

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
getWalletTransactionHistory (mbPersonId, _merchantId, mocId) mbFromDate mbToDate mbLimit mbOffset = do
  validateInput
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  let isVat = isVatMerchant transporterConfig
  let counterParty = counterpartyFromRole person.role
  mbWalletAcc <- getWalletAccountByOwner counterParty driverId.getId
  case mbWalletAcc of
    Nothing -> pure DriverWallet.WalletTransactionHistoryResponse {items = []}
    Just walletAcc -> do
      entries <- QLedgerEntry.findByAccountsWithOptions [walletAcc.id] [] mbFromDate mbToDate mbLimit mbOffset
      let referenceIdsOf refType = [e.referenceId | e <- entries, e.referenceType == refType]
      walletTxns <- QWalletTransaction.findAllByPaymentOrderIds (map Id (referenceIdsOf walletReferenceTopup))
      let autopayFeeIds = map Id (referenceIdsOf WalletSubscription.walletReferenceAutopayRecharge)
      autopayFees <- QDF.findAllByIds autopayFeeIds
      autopayNotifications <- QNotification.findAllByDriverFeeIds autopayFeeIds
      payoutRequests <- QPayoutRequestExtra.findByIds (referenceIdsOf walletReferencePayout)
      let walletTxnByOrderId = Map.fromList [(wt.paymentOrderId.getId, wt) | wt <- walletTxns]
          walletStatusByOrderId = (.status) <$> walletTxnByOrderId
          autopayFeeById = Map.fromList [(fee.id.getId, fee) | fee <- autopayFees]
          executionAtByFeeId = Map.fromListWith max [(n.driverFeeId.getId, Data.Time.addUTCTime pdnNoticePeriod n.createdAt) | n <- autopayNotifications]
          payoutRequestById = Map.fromList [(pr.id.getId, pr) | pr <- payoutRequests]
      let mkItem e =
            let isTopup = e.referenceType == walletReferenceTopup
                mbReason = e.metadataV2 >>= (.reason)
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
                    itemName = walletRowItemName isVat e.referenceType mbReason,
                    itemValue = e.amount,
                    isCredit = e.toAccountId == walletAcc.id,
                    status = e.status,
                    paymentOrder = paymentOrder,
                    payment = paymentInfoFor walletTxnByOrderId autopayFeeById executionAtByFeeId payoutRequestById e,
                    date = e.timestamp,
                    reason = mbReason
                  }
      pure DriverWallet.WalletTransactionHistoryResponse {items = map mkItem entries}
  where
    validateInput =
      unless ((isJust mbFromDate && isJust mbToDate) || (isJust mbLimit && isJust mbOffset)) $
        throwError $ InvalidRequest "Either fromDate+toDate or limit+offset must be provided"

paymentInfoFor ::
  Map.Map Text DWT.WalletTransaction ->
  Map.Map Text DDF.DriverFee ->
  Map.Map Text Data.Time.UTCTime ->
  Map.Map Text PR.PayoutRequest ->
  FLedgerEntry.LedgerEntry ->
  Maybe DriverWallet.WalletPaymentInfo
paymentInfoFor walletTxnByOrderId autopayFeeById executionAtByFeeId payoutRequestById e
  | e.referenceType == walletReferenceTopup =
    let mbWalletTxn = Map.lookup e.referenceId walletTxnByOrderId
        stage = maybe DriverWallet.SUCCESS (topupStage . (.status)) mbWalletTxn
     in Just
          (emptyPaymentInfo DriverWallet.TOPUP stage)
            { DriverWallet.initiatedAt = (.createdAt) <$> mbWalletTxn,
              DriverWallet.completedAt = if stage == DriverWallet.SUCCESS then Just e.timestamp else Nothing
            }
  | e.referenceType == WalletSubscription.walletReferenceAutopayRecharge =
    let mbFee = Map.lookup e.referenceId autopayFeeById
     in Just
          (emptyPaymentInfo DriverWallet.AUTOPAY DriverWallet.SUCCESS)
            { DriverWallet.initiatedAt = (.createdAt) <$> mbFee,
              DriverWallet.scheduledAt = Map.lookup e.referenceId executionAtByFeeId,
              DriverWallet.completedAt = Just e.timestamp
            }
  | e.referenceType == walletReferencePayout =
    let mbPayout = Map.lookup e.referenceId payoutRequestById
        stage = maybe (if e.status == FLedgerEntry.VOIDED then DriverWallet.FAILED else DriverWallet.PROCESSING) (payoutStage . (.status)) mbPayout
     in Just
          (emptyPaymentInfo DriverWallet.PAYOUT stage)
            { DriverWallet.initiatedAt = ((.createdAt) <$> mbPayout) <|> Just e.timestamp,
              DriverWallet.scheduledAt = mbPayout >>= (.expectedCreditTime),
              DriverWallet.completedAt = if stage == DriverWallet.SUCCESS then (.updatedAt) <$> mbPayout else Nothing,
              DriverWallet.payoutVpa = mbPayout >>= (.customerVpa),
              DriverWallet.transactionId = mbPayout >>= (.payoutTransactionId)
            }
  | otherwise = Nothing
  where
    emptyPaymentInfo kind stage =
      DriverWallet.WalletPaymentInfo
        { kind,
          stage,
          referenceId = e.referenceId,
          initiatedAt = Nothing,
          scheduledAt = Nothing,
          completedAt = Nothing,
          payoutVpa = Nothing,
          transactionId = Nothing
        }

pdnNoticePeriod :: Kernel.Prelude.NominalDiffTime
pdnNoticePeriod = 24 * 3600

topupStage :: DWT.WalletTransactionStatus -> DriverWallet.WalletPaymentStage
topupStage = \case
  DWT.INITIATED -> DriverWallet.INITIATED
  DWT.PROCESSING -> DriverWallet.PROCESSING
  DWT.SUCCESS -> DriverWallet.SUCCESS
  _ -> DriverWallet.FAILED

payoutStage :: PR.PayoutRequestStatus -> DriverWallet.WalletPaymentStage
payoutStage = \case
  PR.INITIATED -> DriverWallet.INITIATED
  PR.PROCESSING -> DriverWallet.PROCESSING
  PR.RETRYING -> DriverWallet.PROCESSING
  PR.CASH_PENDING -> DriverWallet.PROCESSING
  PR.CREDITED -> DriverWallet.SUCCESS
  PR.CASH_PAID -> DriverWallet.SUCCESS
  _ -> DriverWallet.FAILED

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

  refreshNonTerminalWalletPayouts driverId
  -- EXCLUDED rows are for admins (the driver gets a push); dropped in the query so limit/offset stay right.
  items <- PayoutItems.getPayoutItems driverId.getId mbFrom mbTo statuses [PR.EXCLUDED] limit offset

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
    refreshNonTerminalWalletPayouts driverId = do
      pending <- QPayoutRequestExtra.findByBeneficiaryWithFilters driverId.getId Nothing Nothing [PR.INITIATED, PR.PROCESSING] [] Nothing Nothing
      forM_ pending $ \pr ->
        whenJust pr.payoutTransactionId $ \payoutOrderId -> do
          mbOrder <- QPayoutOrder.findById (Id payoutOrderId)
          whenJust mbOrder $ \order ->
            void $ withTryCatch ("refreshWalletPayout:" <> pr.id.getId) (UIPayout.refreshPayoutOrderWithSettlement order)

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
          payoutVpa = i.payoutVpa,
          bankName = i.bankName,
          bankAccountLast4 = i.bankAccountLast4
        }

--------------------------------------------------------------------------------
-- postWalletPayout
--------------------------------------------------------------------------------

-- | All context needed for payout operations, loaded once at the start.
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
  ensureInstantPayoutAllowed ctx
  let params =
        WalletPayoutParams
          { payoutType = PR.INSTANT,
            minimumPayoutAmount = ctx.transporterConfig.driverWalletConfig.minimumWalletPayoutAmount,
            enforceDailyLimit = True,
            throwOnBelowMinimum = True
          }
  runInstantPayout ctx params
  pure APISuccess.Success

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
        when driverInfo.blocked $ throwError (DriverAccountBlocked (mkBlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag driverInfo.blockedReason))
        when (r.amount <= 0) $ throwError $ InvalidRequest "Top-up amount must be greater than zero"
        transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId0.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound mocId0.getId)
        -- Only enforced in wallet-subscription cities: elsewhere the flag was never read and defaults
        -- to off, so enforcing it everywhere would break existing top-ups.
        let dwc = transporterConfig.driverWalletConfig
        when (WA.isWalletSubscriptionCity dwc && not dwc.enableWalletTopup) $ throwError $ InvalidRequest "Wallet top-up is not enabled in this city"
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

-- | "Clear dues" from the driver app in a wallet-subscription city: a wallet top-up of what the driver
--   is short of their tier's minimum balance, so one payment brings them back to the minimum and lets
--   them go online again (0 when the tier has no minimum). Nothing (the caller keeps the payment-link
--   flow) outside wallet cities, and while the driver still has overdue fees the dues migration has not
--   moved into the wallet yet.
clearDuesByWalletTopup ::
  ( Kernel.Types.Id.Id DP.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow (Maybe PlanSubscribeRes)
clearDuesByWalletTopup (driverId, merchantId, mocId) = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  if not (WA.isWalletSubscriptionCity transporterConfig.driverWalletConfig)
    then pure Nothing
    else do
      unmigratedDues <- QDF.findAllByStatusAndDriverIdWithServiceName driverId [DDF.PAYMENT_OVERDUE] Nothing YATRI_SUBSCRIPTION
      if not (null unmigratedDues)
        then pure Nothing
        else do
          (ownerCounterparty, ownerId) <- WalletSubscription.resolveWalletOwner driverId
          balance <- fromMaybe 0 <$> WA.getWalletAvailableBalanceByOwner ownerCounterparty ownerId
          mbVariant <- fmap (.variant) <$> QVehicle.findById driverId
          tiers <- CQVST.findAllByMerchantOpCityId mocId Nothing
          let minimumBalance = fromMaybe 0 (mbVariant >>= WA.tierMinimum tiers)
              shortfall = minimumBalance - balance
          when (shortfall <= 0) $ throwError $ InvalidRequest "No dues to clear"
          Just <$> postWalletTopup (Just driverId, merchantId, mocId) DriverWallet.TopUpRequest {amount = shortfall, purpose = Nothing}

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
  DTConf.DriverWalletConfig -> -- the city's wallet config (wallet-tier recheck and pool balance sync flags)
  m FinanceCtx
mkDriverWalletFinanceCtx driverId merchantId mocId currency referenceId walletConfig = do
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
        entityReferenceId = Nothing,
        entityReferenceType = Nothing,
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
        issuedToName = Nothing,
        enableWalletGatedTierCheck = fromMaybe False walletConfig.enableWalletGatedTierCheck,
        syncDriverWalletBalance = walletConfig.enableDriverWallet,
        buyerCounterpartyId = Nothing
      }

-- | Record airport booth cash recharge: credit driver wallet (PlatformAsset → OwnerLiability)
--   with referenceType = walletReferenceAirportCashRecharge. Idempotent by referenceId (e.g. booth receipt id).
--   A negative amount performs a reversal: the accounts are swapped (OwnerLiability → PlatformAsset)
--   for the absolute amount and no invoice is generated.
recordAirportCashRecharge ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisLTSFlowEnv r,
    Redis.HedisFlow m r,
    Finance.HasActorInfo m r
  ) =>
  (Id DP.Person, Id Domain.Types.Merchant.Merchant, Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  HighPrecMoney ->
  Text -> -- referenceId for idempotency (e.g. booth receipt id)
  Maybe Text -> -- reason for the booth payment (stored on the ledger entry metadata)
  m ()
recordAirportCashRecharge (driverId, merchantId, mocId) amount referenceId mbReason = do
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInfo.blocked $ throwError (DriverAccountBlocked (mkBlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag driverInfo.blockedReason))
  now <- getCurrentTime
  effectiveAirport <- QDI.resolveAirportRestriction now driverInfo
  when (effectiveAirport == DI.BLOCKED) $ throwError DriverNotEnabledForAirport
  when (amount == 0) $ throwError $ InvalidRequest "Cash recharge amount must not be zero"
  let isReversal = amount < 0
      absAmount = abs amount
      referenceType = if isReversal then walletReferenceAirportCashWithdrawal else walletReferenceAirportCashRecharge
  -- Under the wallet lock so this credit can't interleave with another posting on the wallet.
  Redis.withWaitAndLockRedis (makeWalletRunningBalanceLockKey driverId.getId) 10 1000 $ do
    existing <- getEntriesByReference referenceType referenceId
    when (null existing) $ do
      merchantOpCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
      let currency = merchantOpCity.currency
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
      ctx <- mkDriverWalletFinanceCtx driverId merchantId mocId currency referenceId transporterConfig.driverWalletConfig
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
      let mbLedgerMetadata =
            mbReason <&> \reason ->
              LedgerEntryMetadata
                { d2cReferralEarnings = Nothing,
                  d2dReferralEarnings = Nothing,
                  dailyStatsId = Nothing,
                  driverPayable = Nothing,
                  payoutOrderId = Nothing,
                  reason = Just reason,
                  subscriptionAllocations = Nothing
                }
      result <-
        runFinance ctx $
          if isReversal
            then void $ transfer OwnerLiability PlatformAsset absAmount referenceType mbLedgerMetadata
            else do
              _ <- transfer PlatformAsset OwnerLiability amount referenceType mbLedgerMetadata
              void $ invoice cashRechargeInvoiceConfig
      (_, entryIds) <- fromEitherM (\e -> WalletLedgerEntryFailed ("airport cash recharge: " <> show e)) result
      when isReversal $ markEntriesAsClaimed entryIds
