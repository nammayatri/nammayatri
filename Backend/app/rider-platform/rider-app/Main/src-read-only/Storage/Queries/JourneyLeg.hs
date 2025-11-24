{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLeg (module Storage.Queries.JourneyLeg, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.FRFSRouteDetails
import qualified Domain.Types.JourneyLeg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.MapsClient
import qualified Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyLeg as Beam
import Storage.Queries.JourneyLegExtra as ReExport

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyLeg.JourneyLeg] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m (Maybe Domain.Types.JourneyLeg.JourneyLeg))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByLegSearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.JourneyLeg.JourneyLeg))
findByLegSearchId legSearchId = do findOneWithKV [Se.Is Beam.legId $ Se.Eq legSearchId]

updateAfterEditLocation ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Prelude.Maybe Kernel.Types.Common.Distance -> Kernel.External.Maps.Google.MapsClient.Types.LatLngV2 -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateAfterEditLocation duration distance endLocation id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.duration duration,
      Se.Set Beam.distance ((.value) <$> distance),
      Se.Set Beam.distanceUnit ((.unit) <$> distance),
      Se.Set Beam.endLocationLat (endLocation & (.latitude)),
      Se.Set Beam.endLocationLon (endLocation & (.longitude)),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDistanceAndDuration ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Distance -> Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateDistanceAndDuration distance duration id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.distance ((.value) <$> distance),
      Se.Set Beam.distanceUnit ((.unit) <$> distance),
      Se.Set Beam.duration duration,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateLegPricingIdByLegSearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateLegPricingIdByLegSearchId legPricingId legSearchId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.legPricingId legPricingId, Se.Set Beam.updatedAt _now] [Se.Is Beam.legId $ Se.Eq legSearchId]

updateLegSearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateLegSearchId legSearchId id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.legId legSearchId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateMode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Common.MultimodalTravelMode -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateMode mode id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.mode mode, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m (Maybe Domain.Types.JourneyLeg.JourneyLeg))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateByPrimaryKey (Domain.Types.JourneyLeg.JourneyLeg {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agencyGtfsId (agency >>= (.gtfsId)),
      Se.Set Beam.agencyName (agency <&> (.name)),
      Se.Set Beam.busLocationData (Just $ toJSON busLocationData),
      Se.Set Beam.changedBusesInSequence changedBusesInSequence,
      Se.Set Beam.distance ((.value) <$> distance),
      Se.Set Beam.distanceUnit ((.unit) <$> distance),
      Se.Set Beam.duration duration,
      Se.Set Beam.endLocationLat (endLocation & (.latitude)),
      Se.Set Beam.endLocationLon (endLocation & (.longitude)),
      Se.Set Beam.estimatedMaxFare estimatedMaxFare,
      Se.Set Beam.estimatedMinFare estimatedMinFare,
      Se.Set Beam.finalBoardedBusNumber finalBoardedBusNumber,
      Se.Set Beam.finalBoardedBusNumberSource finalBoardedBusNumberSource,
      Se.Set Beam.finalBoardedBusServiceTierType finalBoardedBusServiceTierType,
      Se.Set Beam.finalBoardedDepotNo finalBoardedDepotNo,
      Se.Set Beam.finalBoardedScheduleNo finalBoardedScheduleNo,
      Se.Set Beam.finalBoardedWaybillId finalBoardedWaybillId,
      Se.Set Beam.fromArrivalTime fromArrivalTime,
      Se.Set Beam.fromDepartureTime fromDepartureTime,
      Se.Set Beam.fromStopCode (fromStopDetails >>= (.stopCode)),
      Se.Set Beam.fromStopGtfsId ((fromStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode),
      Se.Set Beam.fromStopName (fromStopDetails >>= (.name)),
      Se.Set Beam.fromStopPlatformCode (fromStopDetails >>= (.platformCode)),
      Se.Set Beam.groupCode groupCode,
      Se.Set Beam.isDeleted isDeleted,
      Se.Set Beam.journeyId (Just $ Kernel.Types.Id.getId journeyId),
      Se.Set Beam.legPricingId legPricingId,
      Se.Set Beam.legId legSearchId,
      Se.Set Beam.serviceTypes liveVehicleAvailableServiceTypes,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.mode mode,
      Se.Set Beam.multimodalSearchRequestId multimodalSearchRequestId,
      Se.Set Beam.osmEntrance (osmEntrance >>= Just . Data.Aeson.toJSON),
      Se.Set Beam.osmExit (osmExit >>= Just . Data.Aeson.toJSON),
      Se.Set Beam.sequenceNumber (Just sequenceNumber),
      Se.Set Beam.startLocationLat (startLocation & (.latitude)),
      Se.Set Beam.startLocationLon (startLocation & (.longitude)),
      Se.Set Beam.straightLineEntrance (straightLineEntrance >>= Just . Data.Aeson.toJSON),
      Se.Set Beam.straightLineExit (straightLineExit >>= Just . Data.Aeson.toJSON),
      Se.Set Beam.toArrivalTime toArrivalTime,
      Se.Set Beam.toDepartureTime toDepartureTime,
      Se.Set Beam.toStopCode (toStopDetails >>= (.stopCode)),
      Se.Set Beam.toStopGtfsId ((toStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode),
      Se.Set Beam.toStopName (toStopDetails >>= (.name)),
      Se.Set Beam.toStopPlatformCode (toStopDetails >>= (.platformCode)),
      Se.Set Beam.userBookedBusServiceTierType userBookedBusServiceTierType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
