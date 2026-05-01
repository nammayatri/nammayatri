module SharedLogic.Scheduler.Jobs.ExecuteCashRideCashbackPayout where

import Domain.Types.PayoutConfig (PayoutConfig)
import Domain.Types.Person (Person)
import Domain.Types.VehicleCategory as DV
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Account as DA
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Types.Common as DLP
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Request as PayoutRequest
import Lib.Scheduler
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.JobScheduler
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.PayoutConfig (PayoutDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Payout as TP

executeCashRideCashbackPayoutJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Job 'ExecuteCashRideCashbackPayout ->
  m ExecutionResult
executeCashRideCashbackPayoutJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
      lockKey = "CashRideCashbackPayoutJob:" <> personId.getId
  -- Single in-flight job per person; if a parallel job is already
  -- executing we silently skip — its outcome will cover this run too.
  Redis.whenWithLockRedis lockKey 120 $ runPayoutForPerson personId
  pure Complete

runPayoutForPerson ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Id Person ->
  m ()
runPayoutForPerson personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  case person.payoutVpa of
    Nothing -> logError $ "Skipping cashback payout — missing payout VPA for person: " <> person.id.getId
    Just payoutVpa -> do
      -- Pull all unsettled (DUE/SETTLED) wallet entries with their net
      -- balance impact; PROCESSING entries are naturally excluded by
      -- the underlying findUnsettledByAccountBeforeTime* queries.
      (_walletBalance, unsettledWithNet) <- RidePaymentFinance.getPayoutEligibilityData DA.RIDER personId
      let cashbackEntries =
            filter
              (\(e, _) -> e.referenceType == RidePaymentFinance.ridePaymentRefCashbackPayout)
              unsettledWithNet
          totalAmount = sum (map snd cashbackEntries)
      if null cashbackEntries
        then logInfo $ "No eligible cashback entries for person=" <> personId.getId
        else
          if totalAmount <= 0
            then logInfo $ "Cashback net total non-positive (" <> show totalAmount <> ") for person=" <> personId.getId <> " — skipping"
            else do
              mbPayoutConfig <- getOneConfig (PayoutDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, vehicleCategory = Just DV.CAR, isPayoutEnabled = Nothing, payoutEntity = Nothing})
              case mbPayoutConfig of
                Nothing ->
                  logError $ "PayoutConfig not found for city=" <> person.merchantOperatingCityId.getId <> " — skipping payout for person=" <> personId.getId
                Just payoutConfig ->
                  submitCashbackPayout person payoutVpa payoutConfig (map fst cashbackEntries) totalAmount

submitCashbackPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Person ->
  Text -> -- VPA
  PayoutConfig ->
  [LE.LedgerEntry] ->
  HighPrecMoney ->
  m ()
submitCashbackPayout person payoutVpa payoutConfig cashbackEntries totalAmount = do
  merchantOperatingCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  phoneNo <- mapM decrypt person.mobileNumber
  emailId <- mapM decrypt person.email
  let originalEntryIds = map (.id) cashbackEntries
      payoutCall = TP.createPayoutOrder person.clientSdkVersion person.merchantId person.merchantOperatingCityId (Just person.id.getId)
      submission =
        PayoutRequest.PayoutSubmission
          { beneficiaryId = person.id.getId,
            entityName = DLP.RIDE_OFFER_CASHBACK,
            entityId = person.id.getId,
            entityRefId = Nothing,
            amount = totalAmount,
            currency = payoutConfig.currency,
            payoutFee = Nothing,
            merchantId = person.merchantId.getId,
            merchantOpCityId = person.merchantOperatingCityId.getId,
            city = show merchantOperatingCity.city,
            vpa = Just payoutVpa,
            customerName = person.firstName,
            customerPhone = phoneNo,
            customerEmail = emailId,
            remark = payoutConfig.remark,
            orderType = payoutConfig.orderType,
            scheduledAt = Nothing,
            payoutType = Just DPR.INSTANT,
            coverageFrom = Nothing,
            coverageTo = Nothing,
            ledgerEntryIds = map (.getId) originalEntryIds,
            payoutServiceFlow = Payout.JuspayFlow -- StripeFlow not supported currently in rider-app
          }
  -- DB-level reservation: flip entries UNSETTLED → PROCESSING BEFORE
  -- submitting so a parallel job (or a delayed webhook) can't re-pick
  -- the same entries while this Juspay call is in flight.
  RidePaymentFinance.reserveCashbackEntriesForPayout originalEntryIds Nothing
  result <-
    PayoutRequest.submitPayoutRequest submission payoutCall
      `catch` \(e :: SomeException) -> do
        RidePaymentFinance.releaseCashbackEntriesReservation originalEntryIds
        throwM e
  case result of
    PayoutRequest.PayoutInitiated pr _ -> do
      -- Stamp the PayoutRequest id onto the PROCESSING entries. The
      -- entries stay PROCESSING until the Juspay webhook resolves them
      -- (success → PAID_OUT via markCashbackEntriesAsPaidOut, failure →
      -- back to UNSETTLED via releaseCashbackEntriesReservation).
      RidePaymentFinance.reserveCashbackEntriesForPayout originalEntryIds (Just pr.id.getId)
      logInfo $
        "Cashback payout initiated person="
          <> person.id.getId
          <> " payoutRequestId="
          <> pr.id.getId
          <> " entries="
          <> show (length originalEntryIds)
          <> " amount="
          <> show totalAmount
    PayoutRequest.PayoutFailed _ err -> do
      RidePaymentFinance.releaseCashbackEntriesReservation originalEntryIds
      RidePaymentFinance.markCashbackEntriesAsDue originalEntryIds
      logError $ "Cashback payout submission failed for person=" <> person.id.getId <> ": " <> err
