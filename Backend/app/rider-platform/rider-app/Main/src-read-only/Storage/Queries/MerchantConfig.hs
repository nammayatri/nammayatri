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
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.fraudAuthCountThreshold fraudAuthCountThreshold,
      Se.Set Beam.fraudAuthCountWindow fraudAuthCountWindow,
      Se.Set Beam.fraudBookingCancellationCountThreshold fraudBookingCancellationCountThreshold,
      Se.Set Beam.fraudBookingCancellationCountWindow fraudBookingCancellationCountWindow,
      Se.Set Beam.fraudBookingCancelledByDriverCountThreshold fraudBookingCancelledByDriverCountThreshold,
      Se.Set Beam.fraudBookingCancelledByDriverCountWindow fraudBookingCancelledByDriverCountWindow,
      Se.Set Beam.fraudBookingTotalCountThreshold fraudBookingTotalCountThreshold,
      Se.Set Beam.fraudRideCountThreshold fraudRideCountThreshold,
      Se.Set Beam.fraudRideCountWindow fraudRideCountWindow,
      Se.Set Beam.fraudSearchCountThreshold fraudSearchCountThreshold,
      Se.Set Beam.fraudSearchCountWindow fraudSearchCountWindow,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantConfig Domain.Types.MerchantConfig.MerchantConfig where
  fromTType' (Beam.MerchantConfigT {..}) = do
    pure $
      Just
        Domain.Types.MerchantConfig.MerchantConfig
          { createdAt = createdAt,
            enabled = enabled,
            fraudAuthCountThreshold = fraudAuthCountThreshold,
            fraudAuthCountWindow = fraudAuthCountWindow,
            fraudBookingCancellationCountThreshold = fraudBookingCancellationCountThreshold,
            fraudBookingCancellationCountWindow = fraudBookingCancellationCountWindow,
            fraudBookingCancelledByDriverCountThreshold = fraudBookingCancelledByDriverCountThreshold,
            fraudBookingCancelledByDriverCountWindow = fraudBookingCancelledByDriverCountWindow,
            fraudBookingTotalCountThreshold = fraudBookingTotalCountThreshold,
            fraudRideCountThreshold = fraudRideCountThreshold,
            fraudRideCountWindow = fraudRideCountWindow,
            fraudSearchCountThreshold = fraudSearchCountThreshold,
            fraudSearchCountWindow = fraudSearchCountWindow,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantConfig Domain.Types.MerchantConfig.MerchantConfig where
  toTType' (Domain.Types.MerchantConfig.MerchantConfig {..}) = do
    Beam.MerchantConfigT
      { Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.fraudAuthCountThreshold = fraudAuthCountThreshold,
        Beam.fraudAuthCountWindow = fraudAuthCountWindow,
        Beam.fraudBookingCancellationCountThreshold = fraudBookingCancellationCountThreshold,
        Beam.fraudBookingCancellationCountWindow = fraudBookingCancellationCountWindow,
        Beam.fraudBookingCancelledByDriverCountThreshold = fraudBookingCancelledByDriverCountThreshold,
        Beam.fraudBookingCancelledByDriverCountWindow = fraudBookingCancelledByDriverCountWindow,
        Beam.fraudBookingTotalCountThreshold = fraudBookingTotalCountThreshold,
        Beam.fraudRideCountThreshold = fraudRideCountThreshold,
        Beam.fraudRideCountWindow = fraudRideCountWindow,
        Beam.fraudSearchCountThreshold = fraudSearchCountThreshold,
        Beam.fraudSearchCountWindow = fraudSearchCountWindow,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
