{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payout
  ( juspayPayoutWebhookHandler,
    castPayoutOrderStatus,
    payoutProcessingLockKey,
    processPreviousPayoutAmount,
  )
where

import Data.Time (utctDay)
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Plan as DP
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import Kernel.Beam.Functions as B (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payout.Types as TPayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Servant (BasicAuthData)
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.Merchant
import Storage.Beam.Payment ()
import Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Tools.Error
import qualified Tools.Payout as Payout
import Utils.Common.Cac.KeyNameConstants

-- webhook ----------------------------------------------------------

juspayPayoutWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe DP.ServiceNames ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayPayoutWebhookHandler merchantShortId mbOpCity mbServiceName authData value = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant mbOpCity
  let merchantId = merchant.id
  payoutServiceName' <- case mbServiceName of
    Just serviceName -> do
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchanOperatingCityId serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService merchanOperatingCityId.getId $ show serviceName)
      return $ fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName
    Nothing -> return $ DEMSC.PayoutService TPayout.Juspay
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity payoutServiceName' merchanOperatingCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show TPayout.Juspay))
  psc <- case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig psc' -> pure psc'
    DMSC.RentalPayoutServiceConfig psc' -> pure psc'
    _ -> throwError $ InternalError "Unknown Service Config"
  orderStatusResp <- Juspay.payoutOrderStatusWebhook psc authData value
  osr <- case orderStatusResp of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  logDebug $ "Webhook Payout Resp: " <> show osr
  case osr of
    IPayout.OrderStatusPayoutResp {..} -> do
      payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
      unless (isSuccessStatus payoutOrder.status) do
        mbVehicle <- QV.findById (Id payoutOrder.customerId)
        let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
        payoutConfig <- CPC.findByPrimaryKey merchanOperatingCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchanOperatingCityId.getId)
        when (isSuccessStatus payoutStatus) do
          driverStats <- QDriverStats.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
          QDriverStats.updateTotalPayoutAmountPaid (driverStats.totalPayoutAmountPaid <&> (+ amount)) (Id payoutOrder.customerId)
          updateDFeeStatusForPayoutRegistrationRefund payoutOrder.customerId
        case payoutOrder.entityName of
          Just DPayment.DRIVER_DAILY_STATS -> do
            forM_ (listToMaybe =<< payoutOrder.entityIds) $ \dailyStatsId -> do
              dailyStats <- QDailyStats.findByPrimaryKey dailyStatsId >>= fromMaybeM (InternalError "DailyStats Not Found")
              Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
                let dPayoutStatus = castPayoutOrderStatus payoutStatus
                when (dailyStats.payoutStatus /= DS.Success) $ do
                  QDailyStats.updatePayoutStatusById dPayoutStatus dailyStatsId
              fork "Update Payout Status and Transactions for DailyStats" $ do
                callPayoutService dailyStats.driverId payoutConfig payoutOrderId
          Just DPayment.MANUAL -> do
            forM_ (listToMaybe =<< payoutOrder.entityIds) $ \driverId -> do
              fork "Update Payout Status and Transactions for Manual Payout" $ do
                callPayoutService (Id driverId) payoutConfig payoutOrderId
          Just DPayment.BACKLOG -> do
            whenJust payoutOrder.entityIds $ \entityIds -> do
              fork "Update Payout Status for Backlog" $ do
                mapM_ (updateStatsWithLock merchantId merchanOperatingCityId payoutStatus payoutOrderId payoutConfig) entityIds
          Just DPayment.RETRY_VIA_DASHBOARD -> do
            whenJust payoutOrder.entityIds $ \entityIds -> do
              fork "Update Payout Status for Retried Orders" $ do
                mapM_ (updateStatsWithLock merchantId merchanOperatingCityId payoutStatus payoutOrderId payoutConfig) entityIds
          Just DPayment.DAILY_STATS_VIA_DASHBOARD -> do
            forM_ (listToMaybe =<< payoutOrder.entityIds) $ \dailyStatsId -> do
              dailyStats <- QDailyStats.findByPrimaryKey dailyStatsId >>= fromMaybeM (InternalError "DailyStats Not Found")
              Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
                let dPayoutStatus = castPayoutOrderStatus payoutStatus
                when (dailyStats.payoutStatus /= DS.Success) $ QDailyStats.updatePayoutStatusById dPayoutStatus dailyStatsId
              fork "Update Payout Status For DailyStats Via Dashboard" $ do
                callPayoutService dailyStats.driverId payoutConfig payoutOrderId
          Just DPayment.DRIVER_FEE -> do
            let dPayoutStatus = casPayoutOrderStatusToDFeeStatus payoutStatus
            driverIdsWithServiceName <- do
              forM (fromMaybe [] payoutOrder.entityIds) $ \driverFeeId -> do
                driverFee <- QDF.findById (Id driverFeeId) >>= fromMaybeM (InternalError "DriverFee Not Found")
                Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey driverFee.driverId.getId) 3 3 $ do
                  let refundData =
                        DDF.RefundInfo
                          { status = Just dPayoutStatus,
                            refundEntityId = Nothing,
                            refundedAmount = Nothing,
                            refundedAt = Nothing,
                            refundedBy = Nothing
                          }
                  when (driverFee.status == DDF.REFUND_PENDING) $ QDF.updateRefundData (Id driverFeeId) refundData
                fork "Update Payout Status and Transactions for DriverFee" $ do
                  callPayoutService driverFee.driverId payoutConfig payoutOrderId
                return (driverFee.driverId, driverFee.serviceName)
            let mbDriverIdAndServiceName = listToMaybe driverIdsWithServiceName
            whenJust mbDriverIdAndServiceName $ \(driverId, serviceName) -> do
              when (dPayoutStatus == DDF.REFUNDED) $ do
                dueDriverFees <- QDF.findAllByStatusAndDriverIdWithServiceName driverId [DDF.PAYMENT_OVERDUE] Nothing serviceName
                SLDriverFee.adjustDues dueDriverFees
          _ -> pure ()
      pure ()
    IPayout.BadStatusResp -> pure ()
  pure Ack
  where
    isSuccessStatus payoutStatus = payoutStatus `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]

    updateDFeeStatusForPayoutRegistrationRefund driverId = do
      mbDriverFee <- QDF.findLatestByFeeTypeAndStatusWithServiceName DDF.PAYOUT_REGISTRATION [DDF.REFUND_PENDING] (Id driverId) DP.YATRI_SUBSCRIPTION
      whenJust mbDriverFee $ \driverFee -> do
        now <- getCurrentTime
        QDF.updateStatus DDF.REFUNDED driverFee.id now

    callPayoutService driverId payoutConfig payoutOrderId = do
      driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrderId, mbExpand = payoutConfig.expand}
          serviceName = DEMSC.PayoutService TPayout.Juspay
          createPayoutOrderStatusCall = Payout.payoutOrderStatus driver.merchantId driver.merchantOperatingCityId serviceName
      void $ DPayment.payoutStatusService (cast driver.merchantId) (cast driver.id) createPayoutOrderStatusReq createPayoutOrderStatusCall

    updateStatsWithLock merchantId merchanOperatingCityId payoutStatus payoutOrderId payoutConfig dStatsId = do
      let dPayoutStatus = castPayoutOrderStatus payoutStatus
      dailyStats <- QDailyStats.findByPrimaryKey dStatsId >>= fromMaybeM (InternalError "DailyStats Not Found")
      Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
        when (dailyStats.payoutStatus /= DS.Success) $ QDailyStats.updatePayoutStatusById dPayoutStatus dStatsId
      let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrderId, mbExpand = payoutConfig.expand}
          serviceName = DEMSC.PayoutService TPayout.Juspay
          createPayoutOrderStatusCall = Payout.payoutOrderStatus merchantId merchanOperatingCityId serviceName
      void $ DPayment.payoutStatusService (cast merchantId) (cast dailyStats.driverId) createPayoutOrderStatusReq createPayoutOrderStatusCall

castPayoutOrderStatus :: Payout.PayoutOrderStatus -> DS.PayoutStatus
castPayoutOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DS.Success
    Payout.FULFILLMENTS_SUCCESSFUL -> DS.Success
    Payout.ERROR -> DS.Failed
    Payout.FAILURE -> DS.Failed
    Payout.FULFILLMENTS_FAILURE -> DS.Failed
    Payout.CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DS.ManualReview
    _ -> DS.Processing

casPayoutOrderStatusToDFeeStatus :: Payout.PayoutOrderStatus -> DDF.DriverFeeStatus
casPayoutOrderStatusToDFeeStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DDF.REFUNDED
    Payout.FULFILLMENTS_SUCCESSFUL -> DDF.REFUNDED
    Payout.ERROR -> DDF.REFUND_FAILED
    Payout.FAILURE -> DDF.REFUND_FAILED
    Payout.FULFILLMENTS_FAILURE -> DDF.REFUND_FAILED
    Payout.CANCELLED -> DDF.REFUND_MANUAL_REVIEW_REQUIRED
    Payout.FULFILLMENTS_CANCELLED -> DDF.REFUND_MANUAL_REVIEW_REQUIRED
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DDF.REFUND_MANUAL_REVIEW_REQUIRED
    _ -> DDF.REFUND_PENDING

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId

processPreviousPayoutAmount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> Maybe Text -> Id DMOC.MerchantOperatingCity -> m ()
processPreviousPayoutAmount personId mbVpa merchOpCity = do
  mbVehicle <- QV.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchOpCity vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchOpCity.getId)
  redisLockDriverId <- Redis.tryLockRedis lockKey 10800
  dInfo <- QDI.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (payoutConfig.isPayoutEnabled && redisLockDriverId && dInfo.isBlockedForReferralPayout /= Just True) do
    dailyStats_ <- QDailyStats.findAllByPayoutStatusAndReferralEarningsAndDriver DS.PendingForVpa personId
    transporterConfig <- SCTC.findByMerchantOpCityId merchOpCity (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchOpCity.getId)
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    let dailyStats = filter (\ds -> (ds.activatedValidRides <= transporterConfig.maxPayoutReferralForADay) && ds.merchantLocalDate /= (utctDay localTime)) dailyStats_ -- filter out the flagged payouts and current day payout earning
    when (length dailyStats > 0) $ do
      person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let statsIds = map (.id) dailyStats
          pendingAmount = sum $ map (.referralEarnings) dailyStats
      case (mbVpa, pendingAmount <= payoutConfig.thresholdPayoutAmountPerPerson) of
        (Just vpa, True) -> do
          uid <- generateGUID
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey personId.getId) 3 3 $ do
            mapM_ (QDailyStats.updatePayoutStatusById DS.Processing) statsIds
            mapM_ (QDailyStats.updatePayoutOrderId (Just uid)) statsIds
          phoneNo <- mapM decrypt person.mobileNumber
          let createPayoutOrderReq = DPayment.mkCreatePayoutOrderReq uid pendingAmount phoneNo person.email personId.getId payoutConfig.remark (Just person.firstName) vpa payoutConfig.orderType
          let serviceName = DEMSC.PayoutService TPayout.Juspay
          let entityName = DPayment.BACKLOG
              createPayoutOrderCall = Payout.createPayoutOrder person.merchantId merchOpCity serviceName
          merchantOperatingCity <- CQMOC.findById (cast merchOpCity) >>= fromMaybeM (MerchantOperatingCityNotFound merchOpCity.getId)
          logDebug $ "calling create payoutOrder with driverId: " <> personId.getId <> " | amount: " <> show pendingAmount <> " | orderId: " <> show uid
          void $ DPayment.createPayoutService (cast person.merchantId) (Just $ cast merchOpCity) (cast personId) (Just statsIds) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
        (_, False) -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey personId.getId) 3 3 $ do
            mapM_ (QDailyStats.updatePayoutStatusById DS.ManualReview) statsIds -- don't pay if amount is greater than threshold amount
        _ -> pure ()
  where
    lockKey = "ProcessBacklogPayout:DriverId-" <> personId.getId
