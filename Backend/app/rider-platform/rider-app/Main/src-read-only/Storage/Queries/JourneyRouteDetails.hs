{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyRouteDetails where

import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.JourneyRouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyRouteDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyRouteDetails.JourneyRouteDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyRouteDetails.JourneyRouteDetails] -> m ())
createMany = traverse_ create

findAllBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m [Domain.Types.JourneyRouteDetails.JourneyRouteDetails])
findAllBySearchId searchId = do findAllWithKV [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.JourneyRouteDetails.JourneyRouteDetails -> m (Maybe Domain.Types.JourneyRouteDetails.JourneyRouteDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyRouteDetails.JourneyRouteDetails -> m ())
updateByPrimaryKey (Domain.Types.JourneyRouteDetails.JourneyRouteDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.frequency frequency,
      Se.Set Beam.fromStationId (Kernel.Types.Id.getId <$> fromStationId),
      Se.Set Beam.lineColor lineColor,
      Se.Set Beam.lineColorCode lineColorCode,
      Se.Set Beam.platformNumber platformNumber,
      Se.Set Beam.routeId (Kernel.Types.Id.getId <$> routeId),
      Se.Set Beam.routeLongName routeLongName,
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam.subLegOrder subLegOrder,
      Se.Set Beam.toStationId (Kernel.Types.Id.getId <$> toStationId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.JourneyRouteDetails Domain.Types.JourneyRouteDetails.JourneyRouteDetails where
  fromTType' (Beam.JourneyRouteDetailsT {..}) = do
    pure $
      Just
        Domain.Types.JourneyRouteDetails.JourneyRouteDetails
          { frequency = frequency,
            fromStationId = Kernel.Types.Id.Id <$> fromStationId,
            id = Kernel.Types.Id.Id id,
            lineColor = lineColor,
            lineColorCode = lineColorCode,
            platformNumber = platformNumber,
            routeId = Kernel.Types.Id.Id <$> routeId,
            routeLongName = routeLongName,
            searchId = Kernel.Types.Id.Id searchId,
            subLegOrder = subLegOrder,
            toStationId = Kernel.Types.Id.Id <$> toStationId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyRouteDetails Domain.Types.JourneyRouteDetails.JourneyRouteDetails where
  toTType' (Domain.Types.JourneyRouteDetails.JourneyRouteDetails {..}) = do
    Beam.JourneyRouteDetailsT
      { Beam.frequency = frequency,
        Beam.fromStationId = Kernel.Types.Id.getId <$> fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lineColor = lineColor,
        Beam.lineColorCode = lineColorCode,
        Beam.platformNumber = platformNumber,
        Beam.routeId = Kernel.Types.Id.getId <$> routeId,
        Beam.routeLongName = routeLongName,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.subLegOrder = subLegOrder,
        Beam.toStationId = Kernel.Types.Id.getId <$> toStationId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
