{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CachedRouteResponseExtra where

import qualified Data.List as DL
import Domain.Types.CachedRouteResponse
import qualified Domain.Types.Person as Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.CachedRouteResponse as Beam
import Storage.Queries.OrphanInstances.CachedRouteResponse

findByRiderIdAndGeohashAndHourRangeAndToll ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id.Id Person.Person ->
  Text ->
  Text ->
  Int ->
  Maybe Bool ->
  m (Maybe CachedRouteResponse)
findByRiderIdAndGeohashAndHourRangeAndToll riderId pickupGeohash dropGeohash hourOfDay avoidToll = do
  entries <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.riderId $ Se.Eq (Id.getId riderId),
            Se.Is Beam.pickupGeohash $ Se.Eq pickupGeohash,
            Se.Is Beam.dropGeohash $ Se.Eq dropGeohash,
            Se.Is Beam.hourOfDay $ Se.In (adjacentHours hourOfDay),
            Se.Is Beam.avoidToll $ Se.Eq avoidToll
          ]
      ]
  pure $ pickClosestHourEntry hourOfDay entries

-- hourOfDay is 1..24 (todHour + 1); wrap neighbours cyclically.
adjacentHours :: Int -> [Int]
adjacentHours hourOfDay = [wrapHour (hourOfDay - 1), hourOfDay, wrapHour (hourOfDay + 1)]
  where
    wrapHour h
      | h < 1 = 24
      | h > 24 = 1
      | otherwise = h

pickClosestHourEntry :: Int -> [CachedRouteResponse] -> Maybe CachedRouteResponse
pickClosestHourEntry hourOfDay entries =
  case DL.sortOn (cyclicHourDistance hourOfDay . (.hourOfDay)) entries of
    (closest : _) -> Just closest
    [] -> Nothing
  where
    cyclicHourDistance a b = let d = abs (a - b) in min d (24 - d)

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
