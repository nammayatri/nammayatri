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
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Account as DA
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.Ledger.PayoutSettlement (PayoutOutcome (..))
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Types.Common as DLP
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Request as PayoutRequest
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
--   request, the OwnerPayoutLiability hold, then the Juspay call.
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
  -- Reserve the entries before calling Juspay, so no other run picks them while the order is in flight.
  -- They become PAID_OUT on a success status and UNSETTLED again on a failure status.
  RidePaymentFinance.reserveCashbackEntriesForPayout originalEntryIds Nothing
  pr <-
    ( do
        payoutRequest <- PayoutRequest.buildPayoutRequest submission
        PayoutRequest.createPayoutRequest payoutRequest
        pure payoutRequest
      )
      `catch` \(e :: SomeException) -> do
        RidePaymentFinance.releaseCashbackEntriesReservation originalEntryIds
        throwM e
  RidePaymentFinance.reserveCashbackEntriesForPayout originalEntryIds (Just pr.id.getId)
  let ownerPayoutCtx = RidePaymentFinance.buildRiderFinanceCtx person.merchantId.getId person.merchantOperatingCityId.getId payoutConfig.currency True person.id.getId pr.id.getId Nothing Nothing Nothing
      -- nothing reached Juspay: give back the hold, release the entries, fail the request if still INITIATED
      stopNotSent failStillInitiated reason = do
        logError $ "Cashback payout not sent for person=" <> person.id.getId <> " payoutRequestId=" <> pr.id.getId <> ": " <> reason
        RidePaymentFinance.withPayoutSettlementLock pr.id.getId (RidePaymentFinance.settleCashbackPayoutLedger ownerPayoutCtx totalAmount originalEntryIds (PayoutFailed ("Payout not sent: " <> reason)))
          >>= either (\err -> logError $ "Failed to give back cashback payout hold for payoutRequest " <> pr.id.getId <> ": " <> show err) (const (pure ()))
        RidePaymentFinance.releaseCashbackEntriesReservation originalEntryIds
        PayoutRequest.clearPayoutLedgerEntryIds pr.id.getId
        when failStillInitiated $ do
          current <- fromMaybe pr <$> PayoutRequest.getPayoutRequestById pr.id
          when (current.status == DPR.INITIATED) $
            PayoutRequest.updateStatusWithHistoryById DPR.AUTO_PAY_FAILED (Just $ "Payout not sent: " <> reason) current
        Notify.notifyRiderPayoutStatus person "OFFER_CASHBACK_FAILED" totalAmount
  -- hold before the Juspay call, so a webhook always finds it and only settles it
  holdResult <- try (RidePaymentFinance.withPayoutSettlementLock pr.id.getId $ RidePaymentFinance.postCashbackOwnerPayoutLiability ownerPayoutCtx totalAmount)
  case holdResult of
    Left (err :: SomeException) -> stopNotSent True ("hold failed: " <> show err)
    Right (Left err) -> stopNotSent True ("hold failed: " <> show err)
    Right (Right _) -> do
      stashResult <- try (PayoutRequest.stashPayoutLedgerEntryIds pr.id.getId (map (.getId) originalEntryIds))
      case stashResult of
        Left (err :: SomeException) -> stopNotSent True ("recording the hold failed: " <> show err)
        Right () -> do
          sendResult <- try (PayoutRequest.executePayoutRequestWithOutcome submission.transferAmount submission.currency submission.payoutServiceFlow pr payoutCall afterPayoutOrderCreated)
          case sendResult of
            Right (PayoutRequest.Executed _) -> do
              logInfo $
                "Cashback payout initiated person="
                  <> person.id.getId
                  <> " payoutRequestId="
                  <> pr.id.getId
                  <> " entries="
                  <> show (length originalEntryIds)
                  <> " amount="
                  <> show totalAmount
              Notify.notifyRiderPayoutStatus person "OFFER_CASHBACK_INITIATED" totalAmount
            -- request already AUTO_PAY_FAILED
            Right (PayoutRequest.ConfirmedFailure err) -> stopNotSent False err
            -- outcome unknown or already settled by a webhook: leave the hold for settlement
            Right (PayoutRequest.NotExecutable status) ->
              logInfo $
                "Cashback payout already in flight. person="
                  <> person.id.getId
                  <> " payoutRequestId="
                  <> pr.id.getId
                  <> " status="
                  <> show status
            -- bulk only
            Right (PayoutRequest.AmbiguousFailure err) ->
              logError $
                "Cashback payout outcome unknown for person="
                  <> person.id.getId
                  <> " payoutRequestId="
                  <> pr.id.getId
                  <> ": "
                  <> err
                  <> " -- hold left in place, needs manual reconciliation"
            -- still INITIATED means nothing was sent
            Left (err :: SomeException) -> do
              current <- fromMaybe pr <$> PayoutRequest.getPayoutRequestById pr.id
              if current.status == DPR.INITIATED
                then stopNotSent True ("send failed: " <> show err)
                else logError $ "Cashback payout send errored after the request moved to " <> show current.status <> " for payoutRequestId=" <> pr.id.getId <> ": " <> show err
