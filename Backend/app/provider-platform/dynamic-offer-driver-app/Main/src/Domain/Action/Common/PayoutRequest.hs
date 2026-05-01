module Domain.Action.Common.PayoutRequest
  ( refreshPayoutRequestStatus,
    executeSpecialZonePayoutRequest,
  )
where

import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Payout.Interface.Types as IPayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Types.Id (Id (..), cast)
import Kernel.Utils.Common
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.LocationUpdates (LocationUpdateFlow)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DCommon
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Order as PayoutOrder
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Payout.Status as PayoutStatus
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified SharedLogic.Allocator.Jobs.Payout.SpecialZonePayout as SpecialZonePayout
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Tools.Payout as Payout

refreshPayoutRequestStatus ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    HasField "kafkaProducerTools" r KafkaProducerTools,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  DPR.PayoutRequest ->
  m DPR.PayoutRequest
refreshPayoutRequestStatus payoutRequest = do
  case payoutRequest.entityName of
    Just DCommon.SPECIAL_ZONE_PAYOUT -> refreshSpecialZone payoutRequest
    _ -> pure payoutRequest
  where
    refreshSpecialZone ::
      ( EncFlow m r,
        EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        HasField "kafkaProducerTools" r KafkaProducerTools,
        PaymentBeamFlow.BeamFlow m r,
        FinanceBeamFlow.BeamFlow m r
      ) =>
      DPR.PayoutRequest ->
      m DPR.PayoutRequest
    refreshSpecialZone pr = do
      mbPayoutOrder <- PayoutOrder.findLatestPayoutOrderByEntityId DCommon.SPECIAL_ZONE_PAYOUT pr.id.getId
      case mbPayoutOrder of
        Nothing -> pure pr
        Just payoutOrder -> do
          let clientSdkVersion = Nothing
              opCityId = Id pr.merchantOperatingCityId
          (_payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- Payout.getPayoutStatusServiceFlow Payout.MerchantServiceUsageConfigOption DEMSC.RidePayoutService clientSdkVersion opCityId (Id @DP.Person pr.beneficiaryId) -- TODO check that pr.beneficiaryId = personId.getId
          let merchantId = Id pr.merchantId
              createPayoutOrderStatusReq = DPayment.PayoutStatusServiceReq {orderId = payoutOrder.orderId, mbExpand = Nothing}
              createPayoutOrderStatusCall = Payout.payoutOrderStatus payoutServiceName opCityId (Id @DP.Person pr.beneficiaryId) mbPersonBankAccount -- TODO check that pr.beneficiaryId = personId.getId
              shouldUpdate current new = current /= new && current `notElem` [DPR.CREDITED, DPR.CASH_PAID, DPR.CASH_PENDING]
              onUpdate newStatus rawStatus = do
                let statusMsg = "Order Status Updated: " <> show rawStatus
                PayoutRequest.updateStatusWithHistoryById newStatus (Just statusMsg) pr
          newStatus <-
            PayoutStatus.refreshPayoutStatus
              (cast merchantId)
              (Id pr.beneficiaryId)
              createPayoutOrderStatusReq
              createPayoutOrderStatusCall
              pr.status
              castPayoutOrderStatusToPayoutRequestStatus
              shouldUpdate
              onUpdate
          pure pr {DPR.status = newStatus}

executeSpecialZonePayoutRequest ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    RideEnd.EndRideFlow m r,
    LocationUpdateFlow m r c
  ) =>
  DPR.PayoutRequest ->
  m ()
executeSpecialZonePayoutRequest payoutRequest = do
  _ <- SpecialZonePayout.executeSpecialZonePayout payoutRequest
  pure ()

castPayoutOrderStatusToPayoutRequestStatus :: IPayout.PayoutOrderStatus -> DPR.PayoutRequestStatus
castPayoutOrderStatusToPayoutRequestStatus payoutOrderStatus =
  case payoutOrderStatus of
    IPayout.SUCCESS -> DPR.CREDITED
    IPayout.FULFILLMENTS_SUCCESSFUL -> DPR.CREDITED
    IPayout.ERROR -> DPR.AUTO_PAY_FAILED
    IPayout.FAILURE -> DPR.AUTO_PAY_FAILED
    IPayout.FULFILLMENTS_FAILURE -> DPR.AUTO_PAY_FAILED
    IPayout.CANCELLED -> DPR.CANCELLED
    IPayout.FULFILLMENTS_CANCELLED -> DPR.CANCELLED
    IPayout.FULFILLMENTS_MANUAL_REVIEW -> DPR.PROCESSING
    _ -> DPR.PROCESSING
