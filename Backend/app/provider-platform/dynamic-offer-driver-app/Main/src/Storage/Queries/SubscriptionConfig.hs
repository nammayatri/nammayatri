{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SubscriptionConfig where

import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Domain.Types.SubscriptionConfig
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SubscriptionConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.SubscriptionConfig.SubscriptionConfig -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.SubscriptionConfig.SubscriptionConfig] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.SubscriptionConfig.SubscriptionConfig -> m (Maybe Domain.Types.SubscriptionConfig.SubscriptionConfig)
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findSubscriptionConfigsByMerchantOpCityIdAndServiceName :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> m (Maybe Domain.Types.SubscriptionConfig.SubscriptionConfig)
findSubscriptionConfigsByMerchantOpCityIdAndServiceName serviceName merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.serviceName $ Se.Eq serviceName,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.SubscriptionConfig.SubscriptionConfig -> m (Maybe (Domain.Types.SubscriptionConfig.SubscriptionConfig))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.SubscriptionConfig.SubscriptionConfig -> m ()
updateByPrimaryKey Domain.Types.SubscriptionConfig.SubscriptionConfig {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowManualPaymentLinks allowManualPaymentLinks,
      Se.Set Beam.serviceName serviceName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.SubscriptionConfig Domain.Types.SubscriptionConfig.SubscriptionConfig where
  fromTType' Beam.SubscriptionConfigT {..} = do
    pure $
      Just
        Domain.Types.SubscriptionConfig.SubscriptionConfig
          { allowManualPaymentLinks = allowManualPaymentLinks,
            id = Kernel.Types.Id.Id id,
            serviceName = serviceName,
            useOverlayService = useOverlayService,
            paymentLinkChannel = paymentLinkChannel,
            paymentLinkJobTime = secondsToNominalDiffTime paymentLinkJobTime,
            genericJobRescheduleTime = secondsToNominalDiffTime genericJobRescheduleTime,
            sendInAppFcmNotifications = sendInAppFcmNotifications,
            paymentServiceName = paymentServiceName,
            sendDeepLink = sendDeepLink,
            allowDueAddition = allowDueAddition,
            genericBatchSizeForJobs = genericBatchSizeForJobs,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            maxRetryCount = maxRetryCount,
            deepLinkExpiryTimeInMinutes = deepLinkExpiryTimeInMinutes,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SubscriptionConfig Domain.Types.SubscriptionConfig.SubscriptionConfig where
  toTType' Domain.Types.SubscriptionConfig.SubscriptionConfig {..} = do
    Beam.SubscriptionConfigT
      { Beam.allowManualPaymentLinks = allowManualPaymentLinks,
        Beam.useOverlayService = useOverlayService,
        Beam.paymentLinkChannel = paymentLinkChannel,
        Beam.paymentLinkJobTime = nominalDiffTimeToSeconds paymentLinkJobTime,
        Beam.genericJobRescheduleTime = nominalDiffTimeToSeconds genericJobRescheduleTime,
        Beam.genericBatchSizeForJobs = genericBatchSizeForJobs,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.sendInAppFcmNotifications = sendInAppFcmNotifications,
        Beam.paymentServiceName = paymentServiceName,
        Beam.allowDueAddition = allowDueAddition,
        Beam.serviceName = serviceName,
        Beam.sendDeepLink = sendDeepLink,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.deepLinkExpiryTimeInMinutes = deepLinkExpiryTimeInMinutes,
        Beam.maxRetryCount = maxRetryCount,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
