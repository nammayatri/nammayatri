{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Domain.Action.UI.Payout
  ( juspayPayoutWebhookHandler,
    castPayoutOrderStatus,
    payoutProcessingLockKey,
  )
where

import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Plan as DP
import Environment
import Kernel.Beam.Functions as B (runInReplica)
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
import qualified Lib.Payment.Storage.Queries.PayoutOrders as QPayoutOrder
import Servant (BasicAuthData)
import SharedLogic.Merchant
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Payout as Payout

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
      serviceName' = fromMaybe DP.YATRI_SUBSCRIPTION mbServiceName
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchanOperatingCityId serviceName'
      >>= fromMaybeM (NoSubscriptionConfigForService merchanOperatingCityId.getId $ show serviceName')
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndServiceWithCity merchantId (subscriptionConfig.paymentServiceName) merchanOperatingCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show TPayout.Juspay))
  psc <- case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig psc' -> pure psc'
    _ -> throwError $ InternalError "Unknown Service Config"
  orderStatusResp <- Juspay.payoutOrderStatusWebhook psc authData value
  osr <- case orderStatusResp of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  case osr of
    IPayout.OrderStatusPayoutResp {..} -> do
      payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (InternalError "PayoutOrder Not Found")
      case payoutOrder.entityName of
        Just DPayment.DRIVER_DAILY_STATS -> do
          whenJust payoutOrder.entityId $ \driverStatsId -> do
            dailyStats <- QDailyStats.findByPrimaryKey driverStatsId >>= fromMaybeM (InternalError "DriverStats Not Found")
            Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
              let dPayoutStatus = castPayoutOrderStatus payoutStatus
              when (dailyStats.payoutStatus /= DS.Success) $ QDailyStats.updatePayoutStatusById dPayoutStatus driverStatsId
            fork "Update Payout Status and Transactions for DailyStats" $ do
              callPayoutService dailyStats.driverId payoutOrderId
        Just DPayment.MANUAL -> do
          whenJust payoutOrder.entityId $ \driverId -> do
            fork "Update Payout Status and Transactions for Manual Payout" $ do
              callPayoutService (Id driverId) payoutOrderId
        _ -> pure ()
      pure ()
    IPayout.BadStatusResp -> pure ()
  pure Ack
  where
    callPayoutService driverId payoutOrderId = do
      driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrderId}
          serviceName = DEMSC.PayoutService TPayout.Juspay
          createPayoutOrderStatusCall = Payout.payoutOrderStatus driver.merchantId driver.merchantOperatingCityId serviceName
      void $ DPayment.payoutStatusService (cast driver.merchantId) (cast driver.id) createPayoutOrderStatusReq createPayoutOrderStatusCall

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

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId
