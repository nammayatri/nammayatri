module SharedLogic.Finance.CashbackPayout (runCashbackPayout) where

import Domain.Types.PayoutConfig (PayoutConfig)
import Domain.Types.Person (Person)
import Domain.Types.VehicleCategory as DV
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Balance as FinanceBalance
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Account as DA
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.FinanceM (runFinance)
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.Finance.Transfer.Outbound (OutboundSpec (..), initiateTransferOut)
import qualified Lib.Payment.Domain.Types.Common as DLP
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified SharedLogic.Finance.Rails as Rails
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as TP

type CashbackPayoutFlow m r c =
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    ServiceFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r,
    HasField "blackListedJobs" r [Text]
  )

-- | One cashback payout attempt for a rider, guarded by a per-person lock: eligibility, payout
--   request, and on PayoutInitiated the OwnerPayoutLiability hold.
runCashbackPayout :: (CashbackPayoutFlow m r c) => Id Person -> m ()
runCashbackPayout personId =
  PayoutRequest.runPayoutUnderLock ("CashRideCashbackPayoutJob:" <> personId.getId) 120 (findCashbackPayoutAmount personId) submitCashbackPayout

data CashbackPayoutPlan = CashbackPayoutPlan
  { person :: Person,
    payoutVpa :: Text,
    payoutConfig :: PayoutConfig,
    cashbackEntries :: [LE.LedgerEntry],
    totalAmount :: HighPrecMoney
  }

findCashbackPayoutAmount :: (CashbackPayoutFlow m r c) => Id Person -> m (Maybe CashbackPayoutPlan)
findCashbackPayoutAmount personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  case person.payoutVpa of
    Nothing -> do
      logError $ "Skipping cashback payout — missing payout VPA for person: " <> person.id.getId
      pure Nothing
    Just payoutVpa -> do
      (_walletBalance, unsettledWithNet) <- RidePaymentFinance.getPayoutEligibilityData DA.RIDER personId
      let cashbackEntries =
            filter
              (\(e, _) -> e.referenceType == RidePaymentFinance.ridePaymentRefCashbackPayout)
              unsettledWithNet
          totalAmount = sum (map snd cashbackEntries)
      if null cashbackEntries
        then do
          logInfo $ "No eligible cashback entries for person=" <> personId.getId
          pure Nothing
        else
          if totalAmount <= 0
            then do
              logInfo $ "Cashback net total non-positive (" <> show totalAmount <> ") for person=" <> personId.getId <> " — skipping"
              pure Nothing
            else do
              mbPayoutConfig <- getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, vehicleCategory = Just DV.CAR, isPayoutEnabled = Nothing, payoutEntity = Nothing}) Nothing
              case mbPayoutConfig of
                Nothing -> do
                  logError $ "PayoutConfig not found for city=" <> person.merchantOperatingCityId.getId <> " — skipping payout for person=" <> personId.getId
                  pure Nothing
                Just payoutConfig ->
                  pure $ Just CashbackPayoutPlan {person, payoutVpa, payoutConfig, cashbackEntries = map fst cashbackEntries, totalAmount}

submitCashbackPayout :: (CashbackPayoutFlow m r c) => CashbackPayoutPlan -> m ()
submitCashbackPayout CashbackPayoutPlan {..} = do
  merchantOperatingCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  phoneNo <- mapM decrypt person.mobileNumber
  emailId <- mapM decrypt person.email
  let originalEntryIds = map (.id) cashbackEntries
      payoutCall = TP.createPayoutOrder person.clientSdkVersion person.merchantId person.merchantOperatingCityId (Just person.id.getId)
      submission =
        PayoutRequest.PayoutSubmission
          { -- rider cashback is never paid in a bulk batch
            batchId = Nothing,
            beneficiaryId = person.id.getId,
            entityName = DLP.RIDE_OFFER_CASHBACK,
            entityId = person.id.getId,
            entityRefId = Nothing,
            amount = totalAmount,
            currency = payoutConfig.currency,
            payoutFee = Nothing,
            transferAmount = Nothing,
            merchantId = person.merchantId.getId,
            merchantOpCityId = person.merchantOperatingCityId.getId,
            city = show merchantOperatingCity.city,
            vpa = Just payoutVpa,
            -- VPA payout: no bank account involved.
            bankName = Nothing,
            bankAccountLast4 = Nothing,
            customerName = person.firstName,
            customerPhone = phoneNo,
            customerEmail = emailId,
            remark = payoutConfig.remark,
            orderType = payoutConfig.orderType,
            scheduledAt = Nothing,
            payoutType = Just DPR.INSTANT,
            coverageFrom = Nothing,
            coverageTo = Nothing,
            ledgerEntryIds = map (.getId) originalEntryIds, -- TODO :: Can be made empty in next release `[]` as now using Redis for storing ids for not bloating DB rows with ids in a row.
            payoutServiceFlow = Payout.JuspayFlow -- StripeFlow not supported currently in rider-app
          }
  -- Create the PayoutRequest first (its id anchors every ledger leg and the kernel
  -- transfer), then hand the whole reserve/hold/submit/compensate dance to
  -- initiateTransferOut: the kernel marks the covered entries PROCESSING, posts the
  -- OwnerPayoutLiability hold, and records the outcome on the finance_transfer row.
  payoutRequest <- PayoutRequest.buildPayoutRequest submission
  PayoutRequest.createPayoutRequest payoutRequest
  mbAccount <- RidePaymentFinance.getWalletAccountByOwner DA.RIDER person.id.getId
  case mbAccount of
    Nothing ->
      -- cashbackEntries exist, so the wallet account must too; never submit unledgered
      logError $ "Cashback payout skipped — no wallet account for person=" <> person.id.getId <> " payoutRequestId=" <> payoutRequest.id.getId
    Just account -> do
      let ctx = RidePaymentFinance.buildRiderFinanceCtx person.merchantId.getId person.merchantOperatingCityId.getId payoutConfig.currency True person.id.getId payoutRequest.id.getId Nothing Nothing Nothing
          rail = Rails.cashbackSubmitRail submission payoutRequest payoutCall afterPayoutOrderCreated
      res <-
        initiateTransferOut
          runFinance
          rail
          OutboundSpec
            { ctx = ctx,
              account = account,
              amount = FinanceBalance.Exact totalAmount,
              referenceType = RidePaymentFinance.ridePaymentRefCashbackPayoutTransfer,
              referenceId = payoutRequest.id.getId,
              ledgerRefs = RidePaymentFinance.cashbackPayoutLedgerRefs,
              instant = True,
              legacyOfferHoldKeys = [],
              alwaysRedeemableRefs = [],
              coveredEntryIds = Just originalEntryIds,
              holdLedgerMetadata = Nothing,
              metadata =
                DT.emptyTransferMetadata
                  { DT.coveredRefs = Just [DT.CoveredRef {refType = RidePaymentFinance.ridePaymentRefCashbackPayout, amount = totalAmount}]
                  }
            }
      case res of
        Left err ->
          logError $ "Cashback payout rejected before submission for person=" <> person.id.getId <> " payoutRequestId=" <> payoutRequest.id.getId <> ": " <> show err
        Right transfer -> case transfer.status of
          DT.FAILED -> do
            logError $ "Cashback payout failed for person=" <> person.id.getId <> " payoutRequestId=" <> payoutRequest.id.getId <> ": " <> fromMaybe "-" transfer.lastError
            Notify.notifyRiderPayoutStatus person "OFFER_CASHBACK_FAILED" totalAmount
          DT.NEEDS_REVIEW ->
            logError $ "Cashback payout outcome unknown for person=" <> person.id.getId <> " payoutRequestId=" <> payoutRequest.id.getId <> ": " <> fromMaybe "-" transfer.lastError <> " -- hold kept, needs manual review"
          _ -> do
            logInfo $
              "Cashback payout initiated person="
                <> person.id.getId
                <> " payoutRequestId="
                <> payoutRequest.id.getId
                <> " transferId="
                <> transfer.id.getId
                <> " entries="
                <> show (length originalEntryIds)
                <> " amount="
                <> show totalAmount
            -- notify only on a genuinely fresh submission (IN_FLIGHT = rail accepted);
            -- SUBMITTED here means an idempotent re-entry (NotExecutable), which the
            -- pre-kernel code logged without pushing.
            when (transfer.status == DT.IN_FLIGHT) $
              Notify.notifyRiderPayoutStatus person "OFFER_CASHBACK_INITIATED" totalAmount
