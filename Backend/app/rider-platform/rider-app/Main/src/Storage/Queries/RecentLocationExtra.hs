{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RecentLocationExtra where

import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as Person
import Domain.Types.RecentLocation as DRecentLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude hiding (isNothing)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.Functions as F
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.RecentLocation as Beam
import Storage.Queries.OrphanInstances.RecentLocation

findByPersonIdAndRouteCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Id.Id MerchantOperatingCity -> Text -> m [RecentLocation]
findByPersonIdAndRouteCode personId mocId routeCode = do
  findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Id.getId personId), Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId), Se.Is Beam.routeCode $ Se.Eq $ Just routeCode]]

findRecentLocationsByRouteCodes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Id.Id MerchantOperatingCity -> [Text] -> m [RecentLocation]
findRecentLocationsByRouteCodes personId mocId routeCodes = do
  findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Id.getId personId), Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId), Se.Is Beam.routeCode $ Se.In (map Just routeCodes)]]

findRecentLocationsByEntityType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DRecentLocation.EntityType -> Id.Id Person.Person -> Id.Id MerchantOperatingCity -> m [RecentLocation]
findRecentLocationsByEntityType entityType personId mocId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Id.getId personId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId),
          Se.Is Beam.entityType $ Se.Eq entityType
        ]
    ]
    (Se.Desc Beam.frequency)
    (Just 10)
    Nothing

-- Full, uncapped recent-location history across public-transport entity types (BUS/METRO/SUBWAY),
-- used to seed the frontend's local cache on login/periodic sync (fetchAll), not for per-screen quick suggestions.
findAllPublicTransportRecentLocations :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Id.Id MerchantOperatingCity -> m [RecentLocation]
findAllPublicTransportRecentLocations personId mocId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Id.getId personId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId),
          Se.Is Beam.entityType $ Se.In [DRecentLocation.BUS, DRecentLocation.METRO, DRecentLocation.SUBWAY]
        ]
    ]
    (Se.Desc Beam.frequency)
    Nothing
    Nothing

findRecentLocations :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Id.Id MerchantOperatingCity -> m [RecentLocation]
findRecentLocations personId mocId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Id.getId personId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId)
        ]
    ]
    (Se.Desc Beam.frequency)
    (Just 10)
    Nothing

increaceFrequencyById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id RecentLocation -> Kernel.Types.Common.HighPrecMoney -> m ()
increaceFrequencyById id newFare = do
  recentLoc <- findOneWithKV [Se.Is Beam.id $ Se.Eq (Id.getId id)]
  case recentLoc of
    Nothing -> pure ()
    Just rl -> do
      now <- getCurrentTime
      updateOneWithKV
        [ Se.Set Beam.frequency ((rl.frequency :: Int) + 1),
          Se.Set Beam.updatedAt now,
          Se.Set Beam.fare (Just newFare)
        ]
        [Se.Is Beam.id $ Se.Eq (Id.getId id)]

findByRiderIdAndGeohashAndEntityType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Maybe Text -> Maybe Text -> DRecentLocation.EntityType -> m (Maybe RecentLocation)
findByRiderIdAndGeohashAndEntityType riderId toGeohash fromGeohash entityType = do
  findOneWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Id.getId riderId), Se.Is Beam.toGeohash $ Se.Eq $ toGeohash, Se.Is Beam.fromGeohash $ Se.Eq $ fromGeohash, Se.Is Beam.entityType $ Se.Eq entityType]]
