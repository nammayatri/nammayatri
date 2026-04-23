module SharedLogic.Scheduler.Jobs.ExecuteCashRideCashbackPayout where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.VehicleVariant as DV
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
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
import Storage.ConfigPilot.Interface.Types (getConfig)
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
  case NE.nonEmpty jobData.vehicleReferences of
    Nothing -> do
      logInfo $ "Skipping cashback payout — empty refs for person: " <> jobData.personId.getId
      pure Complete
    Just refsNE -> do
      person <- runInReplica $ QPerson.findById jobData.personId >>= fromMaybeM (PersonNotFound jobData.personId.getId)
      case person.payoutVpa of
        Nothing -> do
          logError $ "Skipping cashback payout — missing payout VPA for person: " <> person.id.getId
          pure Complete
        Just payoutVpa -> do
          merchantOperatingCity <-
            CQMOC.findById person.merchantOperatingCityId
              >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
          phoneNo <- mapM decrypt person.mobileNumber
          emailId <- mapM decrypt person.email
          payoutServiceName <- TP.decidePayoutService (DEMSC.PayoutService PT.Juspay) person.clientSdkVersion
          let groups = NE.groupAllWith (.vehicleVariant) (NE.toList refsNE)
              payoutCall = TP.createPayoutOrder person.merchantId person.merchantOperatingCityId payoutServiceName (Just person.id.getId)
          forM_ groups $ \group -> do
            let variant = (NE.head group).vehicleVariant
                vehicleCategory = DV.castVehicleVariantToVehicleCategory variant
                groupRefIds = map (.referenceId) (NE.toList group)
                groupAmount = sum (map (.amount) (NE.toList group))
            if groupAmount <= 0
              then logInfo $ "Skipping variant=" <> show vehicleCategory <> " (zero amount) refs=" <> show groupRefIds
              else do
                payoutConfigs <- getConfig (PayoutDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Just True, payoutEntity = Nothing})
                case listToMaybe payoutConfigs of
                  Nothing ->
                    logError $ "PayoutConfig not found for variant=" <> show vehicleCategory <> " city=" <> person.merchantOperatingCityId.getId <> "; skipping refs=" <> show groupRefIds
                  Just payoutConfig -> do
                    unsettled <- RidePaymentFinance.findUnsettledCashbackEntriesForRefs groupRefIds
                    if null unsettled
                      then logInfo $ "No UNSETTLED cashback entries for refs=" <> show groupRefIds <> " — skipping payout (already paid out or never created)"
                      else do
                        let entryIds = map (\e -> e.id.getId) unsettled
                            submission =
                              PayoutRequest.PayoutSubmission
                                { beneficiaryId = person.id.getId,
                                  entityName = DLP.RIDE_OFFER_CASHBACK,
                                  entityId = person.id.getId,
                                  entityRefId = Nothing,
                                  amount = groupAmount,
                                  payoutFee = Nothing,
                                  merchantId = person.merchantId.getId,
                                  merchantOpCityId = person.merchantOperatingCityId.getId,
                                  city = show merchantOperatingCity.city,
                                  vpa = payoutVpa,
                                  customerName = person.firstName,
                                  customerPhone = phoneNo,
                                  customerEmail = emailId,
                                  remark = payoutConfig.remark,
                                  orderType = payoutConfig.orderType,
                                  scheduledAt = Nothing,
                                  payoutType = Just DPR.INSTANT,
                                  coverageFrom = Nothing,
                                  coverageTo = Nothing,
                                  ledgerEntryIds = entryIds
                                }
                        result <- PayoutRequest.submitPayoutRequest submission payoutCall
                        case result of
                          PayoutRequest.PayoutInitiated pr _ ->
                            logInfo $
                              "Cashback payout initiated for variant=" <> show vehicleCategory
                                <> " person=" <> person.id.getId
                                <> " payoutRequestId=" <> pr.id.getId
                                <> " entries=" <> show (length entryIds)
                                <> " amount=" <> show groupAmount
                          PayoutRequest.PayoutFailed _ err ->
                            logError $ "Cashback payout submission failed for variant=" <> show vehicleCategory <> " refs=" <> show groupRefIds <> ": " <> err
          pure Complete
