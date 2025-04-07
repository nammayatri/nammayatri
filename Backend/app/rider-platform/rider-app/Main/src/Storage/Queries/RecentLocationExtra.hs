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
    where_ $ F.pointCloseByOrWithin (lon, lat) (val radius) &&. recentLocation ^. TRL.RecentLocationMerchantOperatingCityId ==. val (Id.getId merchantOperatingCityId) &&. recentLocation ^. TRL.RecentLocationRiderId ==. val (Id.getId personId)
    pure recentLocation

findByPersonIdAndRouteId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Id.Id MerchantOperatingCity -> Text -> m [RecentLocation]
findByPersonIdAndRouteId personId mocId routeId = do
  findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Id.getId personId), Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId mocId), Se.Is Beam.routeId $ Se.Eq $ Just routeId]]

findRecentLocationsByRouteIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> [Text] -> m [RecentLocation]
findRecentLocationsByRouteIds personId routeIds = do
  findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Id.getId personId), Se.Is Beam.routeId $ Se.In (map Just routeIds)]]

increaceFrequencyById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id RecentLocation -> m ()
increaceFrequencyById id = do
  recentLoc <- findOneWithKV [Se.Is Beam.id $ Se.Eq (Id.getId id)]
  case recentLoc of
    Nothing -> pure ()
    Just rl ->
      updateOneWithKV
        [Se.Set Beam.frequency ((rl.frequency :: Int) + 1)]
        [Se.Is Beam.id $ Se.Eq (Id.getId id)]
