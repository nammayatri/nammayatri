{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PickedServices where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PickedServices
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PickedServices as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.PickedServices.PickedServices -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.PickedServices.PickedServices] -> m ()
createMany = traverse_ createWithKV

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.PickedServices.PickedServices -> m (Maybe (Domain.Types.PickedServices.PickedServices))
findByPrimaryKey (Kernel.Types.Id.Id searchRequestId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.searchRequestId $ Se.Eq searchRequestId
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.PickedServices.PickedServices -> m ()
updateByPrimaryKey Domain.Types.PickedServices.PickedServices {..} = do
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
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.snapToRoad $ snapToRoad,
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)
        ]
    ]

instance FromTType' Beam.PickedServices Domain.Types.PickedServices.PickedServices where
  fromTType' Beam.PickedServicesT {..} = do
    pure $
      Just
        Domain.Types.PickedServices.PickedServices
          { autoComplete = autoComplete,
            getDistances = getDistances,
            getDistancesForCancelRide = getDistancesForCancelRide,
            getPickupRoutes = getPickupRoutes,
            getPlaceDetails = getPlaceDetails,
            getPlaceName = getPlaceName,
            getRoutes = getRoutes,
            getTripRoutes = getTripRoutes,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            snapToRoad = snapToRoad,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PickedServices Domain.Types.PickedServices.PickedServices where
  toTType' Domain.Types.PickedServices.PickedServices {..} = do
    Beam.PickedServicesT
      { Beam.autoComplete = autoComplete,
        Beam.getDistances = getDistances,
        Beam.getDistancesForCancelRide = getDistancesForCancelRide,
        Beam.getPickupRoutes = getPickupRoutes,
        Beam.getPlaceDetails = getPlaceDetails,
        Beam.getPlaceName = getPlaceName,
        Beam.getRoutes = getRoutes,
        Beam.getTripRoutes = getTripRoutes,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.snapToRoad = snapToRoad,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
