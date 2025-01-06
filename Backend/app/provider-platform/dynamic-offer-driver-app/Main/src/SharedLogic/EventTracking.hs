module SharedLogic.EventTracking where

import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverPlan as DDPlan
import EulerHS.Prelude hiding (id, state)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPO
import Lib.SessionizerMetrics.Types.Event
import Tools.Event as TE

trackFromAutoPayToManual :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EventStreamFlow m r) => DDF.DriverFee -> m ()
trackFromAutoPayToManual driverFee = do
  id <- generateGUIDText
  now <- getCurrentTime
  let eventData =
        TE.EventTrackerData
          { eventName = DRIVER_FEE_AUTO_PAY_TO_MANUAL,
            id = id,
            createdAt = now,
            entity = "DriverFee",
            entityFieldName = "FeeType",
            entityPrimaryId = driverFee.id.getId,
            fromState = Just $ show DDF.RECURRING_EXECUTION_INVOICE,
            reason = Nothing,
            toState = Just $ show DDF.RECURRING_INVOICE,
            merchantId = Just driverFee.merchantId,
            merchantOperatingCityId = Nothing,
            updatedAt = now,
            subscriptionServiceName = Just $ show driverFee.serviceName
          }
  TE.triggerEventTrackerEvent eventData

trackServiceUsageChargeToggle :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EventStreamFlow m r) => DDPlan.DriverPlan -> Maybe Text -> m ()
trackServiceUsageChargeToggle driverPlan mbReason = do
  id <- generateGUIDText
  now <- getCurrentTime
  let eventData =
        TE.EventTrackerData
          { eventName = SERVICE_USAGE_CHARGE_TOGGLE,
            id = id,
            createdAt = now,
            entity = "DriverPlan",
            entityFieldName = "ServiceUsageCharge",
            entityPrimaryId = driverPlan.driverId.getId,
            fromState = Just $ show driverPlan.enableServiceUsageCharge,
            reason = mbReason,
            toState = Just $ show (not driverPlan.enableServiceUsageCharge),
            merchantId = Just driverPlan.merchantId,
            merchantOperatingCityId = Nothing,
            updatedAt = now,
            subscriptionServiceName = Just $ show driverPlan.serviceName
          }
  TE.triggerEventTrackerEvent eventData

trackAutoPayStatusChange :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EventStreamFlow m r) => DDPlan.DriverPlan -> Text -> m ()
trackAutoPayStatusChange driverPlan toStatus = do
  id <- generateGUIDText
  now <- getCurrentTime
  let eventData =
        TE.EventTrackerData
          { eventName = AUTO_PAY_STATUS_TOGGLE,
            id = id,
            createdAt = now,
            entity = "DriverPlan",
            entityFieldName = "AutoPayStatus",
            entityPrimaryId = driverPlan.driverId.getId,
            fromState = Just $ show driverPlan.autoPayStatus,
            reason = Nothing,
            toState = Just toStatus,
            merchantId = Just driverPlan.merchantId,
            merchantOperatingCityId = Nothing,
            updatedAt = now,
            subscriptionServiceName = Just $ show driverPlan.serviceName
          }
  TE.triggerEventTrackerEvent eventData

trackRefundSegregation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EventStreamFlow m r) => DPO.PayoutOrder -> Text -> Text -> m ()
trackRefundSegregation payoutOrder segregation serviceName = do
  id <- generateGUIDText
  now <- getCurrentTime
  let eventData =
        TE.EventTrackerData
          { eventName = REFUND_SECURITY_DEPOSIT,
            id = id,
            createdAt = now,
            entity = "PayoutOrder",
            entityFieldName = "refundSegregation",
            entityPrimaryId = payoutOrder.id.getId,
            fromState = Nothing,
            reason = Just "refund intiated against security deposit",
            toState = Just segregation,
            merchantId = Just $ Id payoutOrder.merchantId,
            merchantOperatingCityId = Nothing,
            updatedAt = now,
            subscriptionServiceName = Just serviceName
          }
  TE.triggerEventTrackerEvent eventData
