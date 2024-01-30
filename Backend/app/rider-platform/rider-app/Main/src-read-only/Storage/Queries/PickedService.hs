{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PickedService where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PickedService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PickedService as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.PickedService.PickedService -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.PickedService.PickedService] -> m ()
createMany = traverse_ createWithKV

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.PickedService.PickedService -> m (Maybe (Domain.Types.PickedService.PickedService))
findByPrimaryKey (Kernel.Types.Id.Id searchRequestId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.searchRequestId $ Se.Eq searchRequestId
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.PickedService.PickedService -> m ()
updateByPrimaryKey Domain.Types.PickedService.PickedService {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.autoComplete $ autoComplete,
      Se.Set Beam.getDistances $ getDistances,
      Se.Set Beam.getDistancesForCancelRide $ getDistancesForCancelRide,
      Se.Set Beam.getPickupRoutes $ getPickupRoutes,
      Se.Set Beam.getPlaceDetails $ getPlaceDetails,
      Se.Set Beam.getPlaceName $ getPlaceName,
      Se.Set Beam.getRoutes $ getRoutes,
      Se.Set Beam.getTripRoutes $ getTripRoutes,
      Se.Set Beam.snapToRoad $ snapToRoad,
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)
        ]
    ]

instance FromTType' Beam.PickedService Domain.Types.PickedService.PickedService where
  fromTType' Beam.PickedServiceT {..} = do
    pure $
      Just
        Domain.Types.PickedService.PickedService
          { autoComplete = autoComplete,
            getDistances = getDistances,
            getDistancesForCancelRide = getDistancesForCancelRide,
            getPickupRoutes = getPickupRoutes,
            getPlaceDetails = getPlaceDetails,
            getPlaceName = getPlaceName,
            getRoutes = getRoutes,
            getTripRoutes = getTripRoutes,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            snapToRoad = snapToRoad,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PickedService Domain.Types.PickedService.PickedService where
  toTType' Domain.Types.PickedService.PickedService {..} = do
    Beam.PickedServiceT
      { Beam.autoComplete = autoComplete,
        Beam.getDistances = getDistances,
        Beam.getDistancesForCancelRide = getDistancesForCancelRide,
        Beam.getPickupRoutes = getPickupRoutes,
        Beam.getPlaceDetails = getPlaceDetails,
        Beam.getPlaceName = getPlaceName,
        Beam.getRoutes = getRoutes,
        Beam.getTripRoutes = getTripRoutes,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.snapToRoad = snapToRoad,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
