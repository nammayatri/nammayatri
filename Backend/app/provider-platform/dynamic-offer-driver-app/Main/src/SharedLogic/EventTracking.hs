{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module SharedLogic.EventTracking where

import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverPlan as DDPlan
import Domain.Types.EventTracker
import EulerHS.Prelude hiding (id, state)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.EventTracker

trackFromAutoPayToManual :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DDF.DriverFee -> m ()
trackFromAutoPayToManual driverFee = do
  id <- generateGUID
  now <- getCurrentTime
  let eventData =
        EventTracker
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
  create eventData

trackServiceUsageChargeToggle :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DDPlan.DriverPlan -> Maybe Text -> m ()
trackServiceUsageChargeToggle driverPlan mbReason = do
  id <- generateGUID
  now <- getCurrentTime
  let eventData =
        EventTracker
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
  create eventData

trackAutoPayStatusChange :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DDPlan.DriverPlan -> Text -> m ()
trackAutoPayStatusChange driverPlan fromStatus = do
  id <- generateGUID
  now <- getCurrentTime
  let eventData =
        EventTracker
          { eventName = AUTO_PAY_STATUS_TOGGLE,
            id = id,
            createdAt = now,
            entity = "DriverPlan",
            entityFieldName = "AutoPayStatus",
            entityPrimaryId = driverPlan.driverId.getId,
            fromState = Just fromStatus,
            reason = Nothing,
            toState = Just $ show driverPlan.autoPayStatus,
            merchantId = Just driverPlan.merchantId,
            merchantOperatingCityId = Nothing,
            updatedAt = now,
            subscriptionServiceName = Just $ show driverPlan.serviceName
          }
  create eventData
