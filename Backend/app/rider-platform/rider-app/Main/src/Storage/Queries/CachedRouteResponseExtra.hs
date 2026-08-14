{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CachedRouteResponseExtra where

import Domain.Types.CachedRouteResponse
import qualified Domain.Types.Person as Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.CachedRouteResponse as Beam
import Storage.Queries.OrphanInstances.CachedRouteResponse

deleteByRiderIdAndGeohashAndHourAndToll :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Text -> Text -> Int -> Maybe Bool -> m ()
deleteByRiderIdAndGeohashAndHourAndToll riderId pickupGeohash dropGeohash hourOfDay avoidToll =
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Id.getId riderId),
          Se.Is Beam.pickupGeohash $ Se.Eq pickupGeohash,
          Se.Is Beam.dropGeohash $ Se.Eq dropGeohash,
          Se.Is Beam.hourOfDay $ Se.Eq hourOfDay,
          Se.Is Beam.avoidToll $ Se.Eq avoidToll
        ]
    ]
