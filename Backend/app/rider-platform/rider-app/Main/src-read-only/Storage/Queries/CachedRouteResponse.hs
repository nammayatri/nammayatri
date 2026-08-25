{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CachedRouteResponse (module Storage.Queries.CachedRouteResponse, module ReExport) where

import qualified Domain.Types.CachedRouteResponse
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CachedRouteResponse as Beam
import Storage.Queries.CachedRouteResponseExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CachedRouteResponse.CachedRouteResponse -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CachedRouteResponse.CachedRouteResponse] -> m ())
createMany = traverse_ create

findByRiderIdAndGeohashAndHourAndToll ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> m (Maybe Domain.Types.CachedRouteResponse.CachedRouteResponse))
findByRiderIdAndGeohashAndHourAndToll riderId pickupGeohash dropGeohash hourOfDay avoidToll = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId),
          Se.Is Beam.pickupGeohash $ Se.Eq pickupGeohash,
          Se.Is Beam.dropGeohash $ Se.Eq dropGeohash,
          Se.Is Beam.hourOfDay $ Se.Eq hourOfDay,
          Se.Is Beam.avoidToll $ Se.Eq avoidToll
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CachedRouteResponse.CachedRouteResponse -> m (Maybe Domain.Types.CachedRouteResponse.CachedRouteResponse))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CachedRouteResponse.CachedRouteResponse -> m ())
updateByPrimaryKey (Domain.Types.CachedRouteResponse.CachedRouteResponse {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.avoidToll avoidToll,
      Se.Set Beam.distance distance,
      Se.Set Beam.dropGeohash dropGeohash,
      Se.Set Beam.duration duration,
      Se.Set Beam.hourOfDay hourOfDay,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.pickupGeohash pickupGeohash,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routes (Just $ toJSON routes),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
