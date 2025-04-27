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
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.RecentLocation as Beam
import Storage.Queries.OrphanInstances.RecentLocation
import qualified Storage.Tabular.RecentLocation as TRL

-- Extra code goes here --
getRecentLocationByLatLon :: Transactionable m => Double -> Double -> Int -> Id.Id Person.Person -> Id.Id MerchantOperatingCity -> m [RecentLocation]
getRecentLocationByLatLon lat lon radius personId merchantOperatingCityId = do
  Esq.findAll $ do
    recentLocation <- from $ table @TRL.RecentLocationT
    where_ $
      F.pointCloseByOrWithin (lon, lat) (val radius)
        &&. recentLocation ^. TRL.RecentLocationMerchantOperatingCityId ==. val (Id.getId merchantOperatingCityId)
        &&. recentLocation ^. TRL.RecentLocationRiderId ==. val (Id.getId personId)
    pure recentLocation

getRecentLocationByLatLonAndEntityType :: Transactionable m => DRecentLocation.EntityType -> Double -> Double -> Int -> Id.Id Person.Person -> Id.Id MerchantOperatingCity -> m [RecentLocation]
getRecentLocationByLatLonAndEntityType entityType lat lon radius personId merchantOperatingCityId = do
  Esq.findAll $ do
    recentLocation <- from $ table @TRL.RecentLocationT
    where_ $
      F.pointCloseByOrWithin (lon, lat) (val radius)
        &&. recentLocation ^. TRL.RecentLocationMerchantOperatingCityId ==. val (Id.getId merchantOperatingCityId)
        &&. recentLocation ^. TRL.RecentLocationRiderId ==. val (Id.getId personId)
        &&. recentLocation ^. TRL.RecentLocationEntityType ==. val entityType
    pure recentLocation

findByPersonIdAndRouteCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Id.Id MerchantOperatingCity -> Text -> m [RecentLocation]
findByPersonIdAndRouteCode personId mocId routeCode = do
  findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Id.getId personId), Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId), Se.Is Beam.routeCode $ Se.Eq $ Just routeCode]]

findRecentLocationsByRouteCodes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Id.Id MerchantOperatingCity -> [Text] -> m [RecentLocation]
findRecentLocationsByRouteCodes personId mocId routeCodes = do
  findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Id.getId personId), Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId), Se.Is Beam.routeCode $ Se.In (map Just routeCodes)]]

findRecentLocations :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DRecentLocation.EntityType -> Id.Id Person.Person -> Id.Id MerchantOperatingCity -> m [RecentLocation]
findRecentLocations entityType personId mocId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Id.getId personId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId),
          Se.Is Beam.entityType $ Se.Eq entityType
        ]
    ]

increaceFrequencyById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id RecentLocation -> m ()
increaceFrequencyById id = do
  recentLoc <- findOneWithKV [Se.Is Beam.id $ Se.Eq (Id.getId id)]
  case recentLoc of
    Nothing -> pure ()
    Just rl ->
      updateOneWithKV
        [Se.Set Beam.frequency ((rl.frequency :: Int) + 1)]
        [Se.Is Beam.id $ Se.Eq (Id.getId id)]
