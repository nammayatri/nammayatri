{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SubscriptionConfig where

import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantMessage
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.Plan
import qualified Domain.Types.SubscriptionConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SubscriptionConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.SubscriptionConfig.SubscriptionConfig -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.SubscriptionConfig.SubscriptionConfig] -> m ()
createMany = traverse_ create

findSubscriptionConfigsByMerchantOpCityIdAndServiceName :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> Domain.Types.Plan.ServiceNames -> m (Maybe (Domain.Types.SubscriptionConfig.SubscriptionConfig))
findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> m (Maybe (Domain.Types.SubscriptionConfig.SubscriptionConfig))
findByPrimaryKey serviceName merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.SubscriptionConfig.SubscriptionConfig -> m ()
updateByPrimaryKey Domain.Types.SubscriptionConfig.SubscriptionConfig {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowDriverFeeCalcSchedule allowDriverFeeCalcSchedule,
      Se.Set Beam.allowDueAddition allowDueAddition,
      Se.Set Beam.allowManualPaymentLinks allowManualPaymentLinks,
      Se.Set Beam.deepLinkExpiryTimeInMinutes deepLinkExpiryTimeInMinutes,
      Se.Set Beam.genericBatchSizeForJobs genericBatchSizeForJobs,
      Se.Set Beam.genericJobRescheduleTime $ Kernel.Utils.Common.nominalDiffTimeToSeconds genericJobRescheduleTime,
      Se.Set Beam.isTriggeredAtEndRide isTriggeredAtEndRide,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.paymentLinkChannel paymentLinkChannel,
      Se.Set Beam.paymentLinkJobTime $ Kernel.Utils.Common.nominalDiffTimeToSeconds paymentLinkJobTime,
      Se.Set Beam.paymentServiceName paymentServiceName,
      Se.Set Beam.sendDeepLink sendDeepLink,
      Se.Set Beam.sendInAppFcmNotifications sendInAppFcmNotifications,
      Se.Set Beam.useOverlayService useOverlayService,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

instance FromTType' Beam.SubscriptionConfig Domain.Types.SubscriptionConfig.SubscriptionConfig where
  fromTType' Beam.SubscriptionConfigT {..} = do
    pure $
      Just
        Domain.Types.SubscriptionConfig.SubscriptionConfig
          { allowDriverFeeCalcSchedule = allowDriverFeeCalcSchedule,
            allowDueAddition = allowDueAddition,
            allowManualPaymentLinks = allowManualPaymentLinks,
            deepLinkExpiryTimeInMinutes = deepLinkExpiryTimeInMinutes,
            genericBatchSizeForJobs = genericBatchSizeForJobs,
            genericJobRescheduleTime = Kernel.Utils.Common.secondsToNominalDiffTime genericJobRescheduleTime,
            isTriggeredAtEndRide = isTriggeredAtEndRide,
            maxRetryCount = maxRetryCount,
            paymentLinkChannel = paymentLinkChannel,
            paymentLinkJobTime = Kernel.Utils.Common.secondsToNominalDiffTime paymentLinkJobTime,
            paymentServiceName = paymentServiceName,
            sendDeepLink = sendDeepLink,
            sendInAppFcmNotifications = sendInAppFcmNotifications,
            serviceName = serviceName,
            useOverlayService = useOverlayService,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SubscriptionConfig Domain.Types.SubscriptionConfig.SubscriptionConfig where
  toTType' Domain.Types.SubscriptionConfig.SubscriptionConfig {..} = do
    Beam.SubscriptionConfigT
      { Beam.allowDriverFeeCalcSchedule = allowDriverFeeCalcSchedule,
        Beam.allowDueAddition = allowDueAddition,
        Beam.allowManualPaymentLinks = allowManualPaymentLinks,
        Beam.deepLinkExpiryTimeInMinutes = deepLinkExpiryTimeInMinutes,
        Beam.genericBatchSizeForJobs = genericBatchSizeForJobs,
        Beam.genericJobRescheduleTime = Kernel.Utils.Common.nominalDiffTimeToSeconds genericJobRescheduleTime,
        Beam.isTriggeredAtEndRide = isTriggeredAtEndRide,
        Beam.maxRetryCount = maxRetryCount,
        Beam.paymentLinkChannel = paymentLinkChannel,
        Beam.paymentLinkJobTime = Kernel.Utils.Common.nominalDiffTimeToSeconds paymentLinkJobTime,
        Beam.paymentServiceName = paymentServiceName,
        Beam.sendDeepLink = sendDeepLink,
        Beam.sendInAppFcmNotifications = sendInAppFcmNotifications,
        Beam.serviceName = serviceName,
        Beam.useOverlayService = useOverlayService,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
