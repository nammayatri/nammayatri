{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RoutePolylines where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RoutePolylines
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RoutePolylines as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RoutePolylines.RoutePolylines -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RoutePolylines.RoutePolylines] -> m ())
createMany = traverse_ create

findByRouteIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RoutePolylines.RoutePolylines))
findByRouteIdAndCity routeId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.routeId $ Se.Eq routeId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByRouteIdAndVehicleType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> BecknV2.FRFS.Enums.VehicleCategory -> m (Maybe Domain.Types.RoutePolylines.RoutePolylines))
findByRouteIdAndVehicleType routeId vehicleType = do findOneWithKV [Se.And [Se.Is Beam.routeId $ Se.Eq routeId, Se.Is Beam.vehicleType $ Se.Eq vehicleType]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RoutePolylines.RoutePolylines -> m (Maybe Domain.Types.RoutePolylines.RoutePolylines))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RoutePolylines.RoutePolylines -> m ())
updateByPrimaryKey (Domain.Types.RoutePolylines.RoutePolylines {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.polyline polyline,
      Se.Set Beam.routeId routeId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleType vehicleType
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RoutePolylines Domain.Types.RoutePolylines.RoutePolylines where
  fromTType' (Beam.RoutePolylinesT {..}) = do
    pure $
      Just
        Domain.Types.RoutePolylines.RoutePolylines
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            polyline = polyline,
            routeId = routeId,
            updatedAt = updatedAt,
            vehicleType = vehicleType
          }

instance ToTType' Beam.RoutePolylines Domain.Types.RoutePolylines.RoutePolylines where
  toTType' (Domain.Types.RoutePolylines.RoutePolylines {..}) = do
    Beam.RoutePolylinesT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.polyline = polyline,
        Beam.routeId = routeId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleType = vehicleType
      }
