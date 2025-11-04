{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteDetails (module Storage.Queries.RouteDetails, module ReExport) where

import qualified Domain.Types.RouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteDetails as Beam
import Storage.Queries.RouteDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteDetails.RouteDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteDetails.RouteDetails] -> m ())
createMany = traverse_ create

findAllByJourneyLegId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.RouteDetails.RouteDetails])
findAllByJourneyLegId limit offset journeyLegId = do findAllWithOptionsKV [Se.Is Beam.journeyLegId $ Se.Eq journeyLegId] (Se.Asc Beam.subLegOrder) limit offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails -> m (Maybe Domain.Types.RouteDetails.RouteDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAlternateShortNamesAndRouteIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Prelude.Text] -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Kernel.Prelude.Text -> m ())
updateAlternateShortNamesAndRouteIds alternateShortNames alternateRouteIds journeyLegId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.alternateShortNames alternateShortNames, Se.Set Beam.alternateRouteIds alternateRouteIds, Se.Set Beam.updatedAt _now] [Se.Is Beam.journeyLegId $ Se.Eq journeyLegId]

updateRoute ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateRoute routeGtfsId routeCode routeLongName routeShortName journeyLegId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.routeGtfsId routeGtfsId,
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.routeLongName routeLongName,
      Se.Set Beam.routeShortName routeShortName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.journeyLegId $ Se.Eq journeyLegId]

updateUserBookedRouteShortName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateUserBookedRouteShortName userBookedRouteShortName journeyLegId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.userBookedRouteShortName userBookedRouteShortName, Se.Set Beam.updatedAt _now] [Se.Is Beam.journeyLegId $ Se.Eq journeyLegId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails -> m (Maybe Domain.Types.RouteDetails.RouteDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteDetails.RouteDetails -> m ())
updateByPrimaryKey (Domain.Types.RouteDetails.RouteDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agencyGtfsId agencyGtfsId,
      Se.Set Beam.agencyName agencyName,
      Se.Set Beam.alternateRouteIds alternateRouteIds,
      Se.Set Beam.alternateShortNames alternateShortNames,
      Se.Set Beam.endLocationLat endLocationLat,
      Se.Set Beam.endLocationLon endLocationLon,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.fromArrivalTime fromArrivalTime,
      Se.Set Beam.fromDepartureTime fromDepartureTime,
      Se.Set Beam.fromStopCode fromStopCode,
      Se.Set Beam.fromStopGtfsId fromStopGtfsId,
      Se.Set Beam.fromStopName fromStopName,
      Se.Set Beam.fromStopPlatformCode fromStopPlatformCode,
      Se.Set Beam.journeyLegId journeyLegId,
      Se.Set Beam.legEndTime legEndTime,
      Se.Set Beam.legStartTime legStartTime,
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.routeColorCode routeColorCode,
      Se.Set Beam.routeColorName routeColorName,
      Se.Set Beam.routeGtfsId routeGtfsId,
      Se.Set Beam.routeLongName routeLongName,
      Se.Set Beam.routeShortName routeShortName,
      Se.Set Beam.startLocationLat startLocationLat,
      Se.Set Beam.startLocationLon startLocationLon,
      Se.Set Beam.subLegOrder subLegOrder,
      Se.Set Beam.toArrivalTime toArrivalTime,
      Se.Set Beam.toDepartureTime toDepartureTime,
      Se.Set Beam.toStopCode toStopCode,
      Se.Set Beam.toStopGtfsId toStopGtfsId,
      Se.Set Beam.toStopName toStopName,
      Se.Set Beam.toStopPlatformCode toStopPlatformCode,
      Se.Set Beam.trackingStatus trackingStatus,
      Se.Set Beam.trackingStatusLastUpdatedAt trackingStatusLastUpdatedAt,
      Se.Set Beam.userBookedRouteShortName userBookedRouteShortName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
