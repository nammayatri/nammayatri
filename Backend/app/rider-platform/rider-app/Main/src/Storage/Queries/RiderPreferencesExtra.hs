{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderPreferencesExtra where

import Domain.Types.Extra.RiderPreferences
import Domain.Types.Person (Person)
import Domain.Types.RiderPreferences (RiderPreferences)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderPreferences as Beam
import Storage.Queries.OrphanInstances.RiderPreferences

-- Returns the LOCATION_PICKUP preference whose sourceGeohash matches the given hash.
-- Fetches all LOCATION_PICKUP rows for the rider and filters in Haskell because the
-- geohash lives inside JSONB (preferenceData), which Sequelize cannot filter on directly.
findLocationPickupByGeohash ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Person ->
  Text ->
  m (Maybe RiderPreferences)
findLocationPickupByGeohash riderId geohash = do
  rows <- findAllWithKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (getId riderId),
          Se.Is Beam.preferenceType $ Se.Eq LOCATION_PICKUP
        ]
    ]
  pure $ find matchesGeohash rows
  where
    matchesGeohash rp = case rp.preferenceData of
      LocationPickupPreference d -> d.sourceGeohash == geohash
