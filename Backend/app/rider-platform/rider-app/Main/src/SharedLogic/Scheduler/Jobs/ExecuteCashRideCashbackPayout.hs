module SharedLogic.Scheduler.Jobs.ExecuteCashRideCashbackPayout where

import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.VehicleVariant as DV
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Action as Payment
import qualified Lib.Payment.Domain.Types.Common as DLP
import Lib.Scheduler
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.JobScheduler
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.PayoutConfig (PayoutDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
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
  if jobData.cashbackAmount <= 0
    then do
      logInfo $ "Skipping cashback payout job for non-positive amount ride: " <> jobData.rideId.getId
      pure Complete
    else do
      person <- runInReplica $ QPerson.findById jobData.personId >>= fromMaybeM (PersonNotFound jobData.personId.getId)
      ride <- runInReplica $ QRide.findById jobData.rideId >>= fromMaybeM (RideNotFound jobData.rideId.getId)
      case person.payoutVpa of
        Nothing -> do
          logError $ "Skipping cashback payout due to missing payout VPA for person: " <> person.id.getId
          pure Complete
        Just payoutVpa -> do
          let vehicleCategory = DV.castVehicleVariantToVehicleCategory ride.vehicleVariant
          payoutConfigs <- getConfig (PayoutDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Just True, payoutEntity = Nothing})
          payoutConfig <- listToMaybe payoutConfigs & fromMaybeM (PayoutConfigNotFound (show vehicleCategory) person.merchantOperatingCityId.getId)
          merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
          phoneNo <- mapM decrypt person.mobileNumber
          emailId <- mapM decrypt person.email
          uid <- generateGUID
          let createPayoutOrderReq = Payment.mkCreatePayoutOrderReq uid jobData.cashbackAmount phoneNo emailId person.id.getId payoutConfig.remark person.firstName payoutVpa payoutConfig.orderType True
              entityName = DLP.RIDE_OFFER_CASHBACK
          payoutServiceName <- TP.decidePayoutService (DEMSC.PayoutService PT.Juspay) person.clientSdkVersion
          let createPayoutOrderCall = TP.createPayoutOrder person.merchantId person.merchantOperatingCityId payoutServiceName (Just person.id.getId)
          payoutResp <- withTryCatch "createPayoutService:cashRideCashback" $ Payment.createPayoutService (cast person.merchantId) (Just $ cast person.merchantOperatingCityId) (cast person.id) (Just [ride.id.getId]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall Nothing
          case payoutResp of
            Left err -> logError $ "Failed cash ride cashback payout for ride " <> ride.id.getId <> ": " <> show err
            Right _ -> do
              let ledgerCtx =
                    RidePaymentFinance.buildRiderFinanceCtx
                      person.merchantId.getId
                      person.merchantOperatingCityId.getId
                      jobData.currency
                      person.id.getId
                      ride.id.getId
                      Nothing
                      Nothing
              ledgerResult <- RidePaymentFinance.createCashbackPayoutLedger ledgerCtx jobData.cashbackAmount
              case ledgerResult of
                Left err -> logError $ "Cashback payout succeeded but payout ledger creation failed for ride " <> ride.id.getId <> ": " <> show err
                Right () -> logInfo $ "Processed cashback payout and payout ledger for ride: " <> ride.id.getId
          pure Complete
