{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetConfig where

import qualified Domain.Types.FleetConfig
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetConfig.FleetConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetConfig.FleetConfig] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.FleetConfig.FleetConfig))
findByPrimaryKey fleetOwnerId = do findOneWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetConfig.FleetConfig -> m ())
updateByPrimaryKey (Domain.Types.FleetConfig.FleetConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowAutomaticRoundTripAssignment allowAutomaticRoundTripAssignment,
      Se.Set Beam.allowEndingMidRoute allowEndingMidRoute,
      Se.Set Beam.allowStartRideFromQR allowStartRideFromQR,
      Se.Set Beam.blacklistCoinEvents blacklistCoinEvents,
      Se.Set Beam.directlyStartFirstTripAssignment (Just directlyStartFirstTripAssignment),
      Se.Set Beam.endRideDistanceThreshold (Just endRideDistanceThreshold),
      Se.Set Beam.rideEndApproval rideEndApproval,
      Se.Set Beam.unlinkDriverAndVehicleOnTripTermination (Just unlinkDriverAndVehicleOnTripTermination),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId)]]

instance FromTType' Beam.FleetConfig Domain.Types.FleetConfig.FleetConfig where
  fromTType' (Beam.FleetConfigT {..}) = do
    pure $
      Just
        Domain.Types.FleetConfig.FleetConfig
          { allowAutomaticRoundTripAssignment = allowAutomaticRoundTripAssignment,
            allowEndingMidRoute = allowEndingMidRoute,
            allowStartRideFromQR = allowStartRideFromQR,
            blacklistCoinEvents = blacklistCoinEvents,
            directlyStartFirstTripAssignment = fromMaybe True directlyStartFirstTripAssignment,
            endRideDistanceThreshold = fromMaybe 100 endRideDistanceThreshold,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            rideEndApproval = rideEndApproval,
            unlinkDriverAndVehicleOnTripTermination = fromMaybe True unlinkDriverAndVehicleOnTripTermination,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetConfig Domain.Types.FleetConfig.FleetConfig where
  toTType' (Domain.Types.FleetConfig.FleetConfig {..}) = do
    Beam.FleetConfigT
      { Beam.allowAutomaticRoundTripAssignment = allowAutomaticRoundTripAssignment,
        Beam.allowEndingMidRoute = allowEndingMidRoute,
        Beam.allowStartRideFromQR = allowStartRideFromQR,
        Beam.blacklistCoinEvents = blacklistCoinEvents,
        Beam.directlyStartFirstTripAssignment = Just directlyStartFirstTripAssignment,
        Beam.endRideDistanceThreshold = Just endRideDistanceThreshold,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.rideEndApproval = rideEndApproval,
        Beam.unlinkDriverAndVehicleOnTripTermination = Just unlinkDriverAndVehicleOnTripTermination,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
