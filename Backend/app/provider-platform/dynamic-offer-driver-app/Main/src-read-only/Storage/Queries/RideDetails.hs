{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideDetails (module Storage.Queries.RideDetails, module ReExport) where

import qualified Domain.Types.Ride
import qualified Domain.Types.RideDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RideDetails as Beam
import Storage.Queries.RideDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideDetails.RideDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RideDetails.RideDetails] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.RideDetails.RideDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.RideDetails.RideDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideDetails.RideDetails -> m ())
updateByPrimaryKey (Domain.Types.RideDetails.RideDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.defaultServiceTierName defaultServiceTierName,
      Se.Set Beam.driverCountryCode driverCountryCode,
      Se.Set Beam.driverName driverName,
      Se.Set Beam.driverNumberEncrypted (driverNumber <&> unEncrypted . (.encrypted)),
      Se.Set Beam.driverNumberHash (driverNumber <&> (.hash)),
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.rcId rcId,
      Se.Set Beam.vehicleAge vehicleAge,
      Se.Set Beam.vehicleClass vehicleClass,
      Se.Set Beam.vehicleColor vehicleColor,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
