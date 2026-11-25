module SharedLogic.DriverPayout
  ( dispatchPayoutToDriver,
  )
where

import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Lib.Scheduler
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import qualified SharedLogic.Ride as SharedRide
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Payout as TP

dispatchPayoutToDriver ::
  ( EncFlow m r,
    Finance.HasActorInfo m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DPR.PayoutRequest ->
  m ExecutionResult
dispatchPayoutToDriver payoutRequest = do
  let driverId = Id payoutRequest.beneficiaryId
  let merchantOpCityId = Id payoutRequest.merchantOperatingCityId
  (person :: DP.Person) <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  (merchantOperatingCity :: DMOC.MerchantOperatingCity) <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  (ridePayoutFlow, _) <- TP.getPayoutServiceFlowForMerchant (.createPayoutOrder) TP.MerchantServiceUsageConfigOption DEMSC.RidePayoutService merchantOpCityId
  if ridePayoutFlow == IPayout.BulkFlow
    then do
      let reason = "Payouts are not supported on the bulk payout rail"
      logError $ reason <> " | payoutRequest " <> payoutRequest.id.getId
      PayoutRequest.updateStatusWithHistoryById DPR.AUTO_PAY_FAILED (Just reason) payoutRequest
      QPR.updateStatusWithReasonById DPR.AUTO_PAY_FAILED (Just reason) payoutRequest.id
      SharedRide.safeRevertVehicleBalanceForPayout payoutRequest
    else do
      (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- TP.getCreatePayoutServiceFlow TP.MerchantServiceUsageConfigOption DEMSC.RidePayoutService person.clientSdkVersion merchantOpCityId person.id
      let payoutCall = TP.createPayoutOrder payoutServiceName merchantOpCityId person.id mbPersonBankAccount
      mbPayoutOrder <- PayoutRequest.executePayoutRequest merchantOperatingCity.currency payoutServiceFlow payoutRequest payoutCall afterPayoutOrderCreated
      case mbPayoutOrder of
        Just (payoutOrder :: DPayoutOrder.PayoutOrder) ->
          logInfo $ "Payout request submitted for id: " <> show payoutRequest.id <> " | orderId: " <> payoutOrder.id.getId
        Nothing -> do
          mbUpdatedPayoutReq <- QPR.findById payoutRequest.id
          whenJust mbUpdatedPayoutReq $ \updatedPayoutReq ->
            when (updatedPayoutReq.status `elem` [DPR.CANCELLED, DPR.AUTO_PAY_FAILED, DPR.FAILED]) $
              SharedRide.safeRevertVehicleBalanceForPayout updatedPayoutReq
  pure Complete
