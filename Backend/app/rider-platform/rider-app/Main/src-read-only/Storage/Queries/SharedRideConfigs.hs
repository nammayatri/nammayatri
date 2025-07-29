{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedRideConfigs where

import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SharedRideConfigs
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedRideConfigs as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedRideConfigs.SharedRideConfigs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedRideConfigs.SharedRideConfigs] -> m ())
createMany = traverse_ create

findAllByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.SharedRideConfigs.SharedRideConfigs]))
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.SharedRideConfigs.SharedRideConfigs]))
findByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByMerchantOperatingCityIdAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.SharedRideConfigs.SharedRideConfigs))
findByMerchantOperatingCityIdAndVehicleCategory merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateConfigValues ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.Meters -> Kernel.Prelude.Int -> Kernel.Types.Common.Seconds -> Kernel.Types.Common.Seconds -> Kernel.Prelude.Int -> Kernel.Types.Common.Meters -> Kernel.Types.Common.Meters -> Kernel.Types.Common.Meters -> Kernel.Prelude.Double -> Kernel.Prelude.Int -> Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.SharedRideConfigs.SharedRideConfigs -> m ())
updateConfigValues pickupLocationSearchRadius searchThresholdForSharedEstimate searchRequestExpirySeconds searchExpiryBufferSeconds customerRemainingThresholdForFlowContinuation dropLocationSearchRadius actualPickupDistanceThreshold actualDropDistanceThreshold routeMatchingThreshold geoHashPrecisionForRouteMatching routeOverlapThreshold id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.pickupLocationSearchRadius pickupLocationSearchRadius,
      Se.Set Beam.searchThresholdForSharedEstimate searchThresholdForSharedEstimate,
      Se.Set Beam.searchRequestExpirySeconds searchRequestExpirySeconds,
      Se.Set Beam.searchExpiryBufferSeconds searchExpiryBufferSeconds,
      Se.Set Beam.customerRemainingThresholdForFlowContinuation customerRemainingThresholdForFlowContinuation,
      Se.Set Beam.dropLocationSearchRadius dropLocationSearchRadius,
      Se.Set Beam.actualPickupDistanceThreshold actualPickupDistanceThreshold,
      Se.Set Beam.actualDropDistanceThreshold actualDropDistanceThreshold,
      Se.Set Beam.routeMatchingThreshold routeMatchingThreshold,
      Se.Set Beam.geoHashPrecisionForRouteMatching geoHashPrecisionForRouteMatching,
      Se.Set Beam.routeOverlapThreshold routeOverlapThreshold,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedRideConfigs.SharedRideConfigs -> m (Maybe Domain.Types.SharedRideConfigs.SharedRideConfigs))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedRideConfigs.SharedRideConfigs -> m ())
updateByPrimaryKey (Domain.Types.SharedRideConfigs.SharedRideConfigs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualDropDistanceThreshold actualDropDistanceThreshold,
      Se.Set Beam.actualPickupDistanceThreshold actualPickupDistanceThreshold,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.customerRemainingThresholdForFlowContinuation customerRemainingThresholdForFlowContinuation,
      Se.Set Beam.dropLocationSearchRadius dropLocationSearchRadius,
      Se.Set Beam.geoHashPrecisionForRouteMatching geoHashPrecisionForRouteMatching,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.pickupLocationSearchRadius pickupLocationSearchRadius,
      Se.Set Beam.routeMatchingThreshold routeMatchingThreshold,
      Se.Set Beam.routeOverlapThreshold routeOverlapThreshold,
      Se.Set Beam.searchExpiryBufferSeconds searchExpiryBufferSeconds,
      Se.Set Beam.searchRequestExpirySeconds searchRequestExpirySeconds,
      Se.Set Beam.searchThresholdForSharedEstimate searchThresholdForSharedEstimate,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedRideConfigs Domain.Types.SharedRideConfigs.SharedRideConfigs where
  fromTType' (Beam.SharedRideConfigsT {..}) = do
    pure $
      Just
        Domain.Types.SharedRideConfigs.SharedRideConfigs
          { actualDropDistanceThreshold = actualDropDistanceThreshold,
            actualPickupDistanceThreshold = actualPickupDistanceThreshold,
            createdAt = createdAt,
            customerRemainingThresholdForFlowContinuation = customerRemainingThresholdForFlowContinuation,
            dropLocationSearchRadius = dropLocationSearchRadius,
            geoHashPrecisionForRouteMatching = geoHashPrecisionForRouteMatching,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pickupLocationSearchRadius = pickupLocationSearchRadius,
            routeMatchingThreshold = routeMatchingThreshold,
            routeOverlapThreshold = routeOverlapThreshold,
            searchExpiryBufferSeconds = searchExpiryBufferSeconds,
            searchRequestExpirySeconds = searchRequestExpirySeconds,
            searchThresholdForSharedEstimate = searchThresholdForSharedEstimate,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.SharedRideConfigs Domain.Types.SharedRideConfigs.SharedRideConfigs where
  toTType' (Domain.Types.SharedRideConfigs.SharedRideConfigs {..}) = do
    Beam.SharedRideConfigsT
      { Beam.actualDropDistanceThreshold = actualDropDistanceThreshold,
        Beam.actualPickupDistanceThreshold = actualPickupDistanceThreshold,
        Beam.createdAt = createdAt,
        Beam.customerRemainingThresholdForFlowContinuation = customerRemainingThresholdForFlowContinuation,
        Beam.dropLocationSearchRadius = dropLocationSearchRadius,
        Beam.geoHashPrecisionForRouteMatching = geoHashPrecisionForRouteMatching,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pickupLocationSearchRadius = pickupLocationSearchRadius,
        Beam.routeMatchingThreshold = routeMatchingThreshold,
        Beam.routeOverlapThreshold = routeOverlapThreshold,
        Beam.searchExpiryBufferSeconds = searchExpiryBufferSeconds,
        Beam.searchRequestExpirySeconds = searchRequestExpirySeconds,
        Beam.searchThresholdForSharedEstimate = searchThresholdForSharedEstimate,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
