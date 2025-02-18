{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLegExtra where

import Domain.Types.FRFSRouteDetails
import Domain.Types.JourneyLeg
import qualified Domain.Types.JourneyLeg as JL
import qualified Domain.Types.RouteDetails as RouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.OrphanInstances.JourneyLeg
import qualified Storage.Queries.RouteDetails as RD

-- Extra code goes here --
create' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
create' = createWithKV

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
create journeyLeg = do
  forM_ (JL.routeDetails journeyLeg) $ \routeDetail -> do
    _now <- getCurrentTime
    newId <- Common.generateGUID
    let fromStopDetails' = fromMaybe (MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.fromStopDetails)
        toStopDetails' = fromMaybe (MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.toStopDetails)
    let routeDetails =
          RouteDetails.RouteDetails
            { routeGtfsId = routeDetail.gtfsId <&> gtfsIdtoDomainCode,
              id = newId,
              routeLongName = routeDetail.longName,
              routeShortName = routeDetail.shortName,
              routeColorName = routeDetail.shortName,
              routeColorCode = routeDetail.color,
              frequency = routeDetail.frequency,
              journeyLegId = journeyLeg.id,
              agencyGtfsId = routeDetail.gtfsId <&> gtfsIdtoDomainCode,
              agencyName = routeDetail.longName,
              subLegOrder = Just routeDetail.subLegOrder,
              --fromStopDetails:
              fromStopCode = fromStopDetails'.stopCode,
              fromStopName = fromStopDetails'.name,
              fromStopGtfsId = fromStopDetails'.gtfsId <&> gtfsIdtoDomainCode,
              fromStopPlatformCode = fromStopDetails'.platformCode,
              --toStopDetails:
              toStopCode = toStopDetails'.stopCode,
              toStopName = toStopDetails'.name,
              toStopGtfsId = toStopDetails'.gtfsId <&> gtfsIdtoDomainCode,
              toStopPlatformCode = toStopDetails'.platformCode,
              --Times --
              fromArrivalTime = routeDetail.fromArrivalTime,
              fromDepartureTime = routeDetail.fromDepartureTime,
              toArrivalTime = routeDetail.toArrivalTime,
              toDepartureTime = routeDetail.toDepartureTime,
              --startLocation:
              startLocationLat = Just routeDetail.startLocation.latLng.latitude,
              startLocationLon = Just routeDetail.startLocation.latLng.longitude,
              --endLocation:
              endLocationLat = Just routeDetail.endLocation.latLng.latitude,
              endLocationLon = Just routeDetail.endLocation.latLng.longitude,
              merchantId = journeyLeg.merchantId,
              merchantOperatingCityId = journeyLeg.merchantOperatingCityId,
              createdAt = _now,
              updatedAt = _now
            }
    RD.create routeDetails

  create' journeyLeg
