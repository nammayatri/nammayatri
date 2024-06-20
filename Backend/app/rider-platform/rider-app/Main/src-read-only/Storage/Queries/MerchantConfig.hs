{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantConfig where

import qualified Domain.Types.MerchantConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantConfig.MerchantConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantConfig.MerchantConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.MerchantConfig.MerchantConfig])
findAllByMerchantOperatingCityId merchantOperatingCityId enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantConfig.MerchantConfig -> m (Maybe Domain.Types.MerchantConfig.MerchantConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantConfig.MerchantConfig -> m ())
updateByPrimaryKey (Domain.Types.MerchantConfig.MerchantConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.fraudBookingCancellationCountThreshold fraudBookingCancellationCountThreshold,
      Se.Set Beam.fraudBookingCancellationCountWindow fraudBookingCancellationCountWindow,
      Se.Set Beam.fraudBookingTotalCountThreshold fraudBookingTotalCountThreshold,
      Se.Set Beam.fraudBookingCancelledByDriverCountThreshold fraudBookingCancelledByDriverCountThreshold,
      Se.Set Beam.fraudBookingCancelledByDriverCountWindow fraudBookingCancelledByDriverCountWindow,
      Se.Set Beam.fraudSearchCountThreshold fraudSearchCountThreshold,
      Se.Set Beam.fraudSearchCountWindow fraudSearchCountWindow,
      Se.Set Beam.fraudRideCountThreshold fraudRideCountThreshold,
      Se.Set Beam.fraudRideCountWindow fraudRideCountWindow,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt (Just _now),
      Se.Set Beam.enabled enabled
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantConfig Domain.Types.MerchantConfig.MerchantConfig where
  fromTType' (Beam.MerchantConfigT {..}) = do
    pure $
      Just
        Domain.Types.MerchantConfig.MerchantConfig
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            fraudBookingCancellationCountThreshold = fraudBookingCancellationCountThreshold,
            fraudBookingCancellationCountWindow = fraudBookingCancellationCountWindow,
            fraudBookingTotalCountThreshold = fraudBookingTotalCountThreshold,
            fraudBookingCancelledByDriverCountThreshold = fraudBookingCancelledByDriverCountThreshold,
            fraudBookingCancelledByDriverCountWindow = fraudBookingCancelledByDriverCountWindow,
            fraudSearchCountThreshold = fraudSearchCountThreshold,
            fraudSearchCountWindow = fraudSearchCountWindow,
            fraudRideCountThreshold = fraudRideCountThreshold,
            fraudRideCountWindow = fraudRideCountWindow,
            createdAt = createdAt,
            updatedAt = updatedAt,
            enabled = enabled
          }

instance ToTType' Beam.MerchantConfig Domain.Types.MerchantConfig.MerchantConfig where
  toTType' (Domain.Types.MerchantConfig.MerchantConfig {..}) = do
    Beam.MerchantConfigT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.fraudBookingCancellationCountThreshold = fraudBookingCancellationCountThreshold,
        Beam.fraudBookingCancellationCountWindow = fraudBookingCancellationCountWindow,
        Beam.fraudBookingTotalCountThreshold = fraudBookingTotalCountThreshold,
        Beam.fraudBookingCancelledByDriverCountThreshold = fraudBookingCancelledByDriverCountThreshold,
        Beam.fraudBookingCancelledByDriverCountWindow = fraudBookingCancelledByDriverCountWindow,
        Beam.fraudSearchCountThreshold = fraudSearchCountThreshold,
        Beam.fraudSearchCountWindow = fraudSearchCountWindow,
        Beam.fraudRideCountThreshold = fraudRideCountThreshold,
        Beam.fraudRideCountWindow = fraudRideCountWindow,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.enabled = enabled
      }
