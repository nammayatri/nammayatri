{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyRouteDetails where

import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.JourneyRouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.JourneyLeg.Types
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyRouteDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyRouteDetails.JourneyRouteDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyRouteDetails.JourneyRouteDetails] -> m ())
createMany = traverse_ create

findAllBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m [Domain.Types.JourneyRouteDetails.JourneyRouteDetails])
findAllBySearchId searchId = do findAllWithKV [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]

updateAlternateShortNames :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateAlternateShortNames alternateShortNames searchId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.alternateShortNames alternateShortNames, Se.Set Beam.updatedAt _now] [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]

updateJourneyStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneyLegStatus -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> m ())
updateJourneyStatus journeyStatus searchId subLegOrder = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.journeyStatus journeyStatus, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId),
          Se.Is Beam.subLegOrder $ Se.Eq subLegOrder
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.JourneyRouteDetails.JourneyRouteDetails -> m (Maybe Domain.Types.JourneyRouteDetails.JourneyRouteDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyRouteDetails.JourneyRouteDetails -> m ())
updateByPrimaryKey (Domain.Types.JourneyRouteDetails.JourneyRouteDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.alternateShortNames alternateShortNames,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.fromStationId fromStationCode,
      Se.Set Beam.journeyStatus journeyStatus,
      Se.Set Beam.lineColor lineColor,
      Se.Set Beam.lineColorCode lineColorCode,
      Se.Set Beam.platformNumber platformNumber,
      Se.Set Beam.routeId routeCode,
      Se.Set Beam.routeLongName routeLongName,
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam.subLegOrder subLegOrder,
      Se.Set Beam.toStationId toStationCode,
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
          { alternateShortNames = alternateShortNames,
            frequency = frequency,
            fromStationCode = fromStationId,
            id = Kernel.Types.Id.Id id,
            journeyStatus = journeyStatus,
            lineColor = lineColor,
            lineColorCode = lineColorCode,
            platformNumber = platformNumber,
            routeCode = routeId,
            routeLongName = routeLongName,
            searchId = Kernel.Types.Id.Id searchId,
            subLegOrder = subLegOrder,
            toStationCode = toStationId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyRouteDetails Domain.Types.JourneyRouteDetails.JourneyRouteDetails where
  toTType' (Domain.Types.JourneyRouteDetails.JourneyRouteDetails {..}) = do
    Beam.JourneyRouteDetailsT
      { Beam.alternateShortNames = alternateShortNames,
        Beam.frequency = frequency,
        Beam.fromStationId = fromStationCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyStatus = journeyStatus,
        Beam.lineColor = lineColor,
        Beam.lineColorCode = lineColorCode,
        Beam.platformNumber = platformNumber,
        Beam.routeId = routeCode,
        Beam.routeLongName = routeLongName,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.subLegOrder = subLegOrder,
        Beam.toStationId = toStationCode,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
