module SharedLogic.SimulatedFlow.Ride where

import Environment
import Kernel.External.Encryption
import Kernel.Prelude hiding (HasField)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Types.Booking.Type as DB
import SharedLogic.Types.Ride
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.SimulatedFlow.Driver as CD
import qualified Storage.CachedQueries.SimulatedFlow.SearchRequest as CSSearchRequest
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics

type GetDriverLocResp = MapSearch.LatLong

simulateRide ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    SimluatedCacheFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasField "rideCfg" r RideConfig,
    HasField "simulatedMaxDone" r Int
  ) =>
  Bool ->
  Id Ride ->
  m GetDriverLocResp ->
  m GetDriverLocResp
simulateRide isSimulated rideId nonSimulatedAction =
  if isSimulated
    then do
      bookingId <- CSSearchRequest.getBookingIdByRideId rideId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: booking not found")
      quoteId <- CSSearchRequest.getQuoteIdByBookingId bookingId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Quote: quote not found")
      estimateId <- CSSearchRequest.getEstimateIdByQuoteId quoteId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Estimate: estimate not found")
      selectedDriver <- CD.getLinkedDriverByEstimateId estimateId >>= fromMaybeM (InvalidRequest "SimulatedFlow:RouteInfo: route not found")
      locationInfo <- CSSearchRequest.getSimulatedLocationInfoByRideId rideId
      now <- getCurrentTime
      let driverRoute = selectedDriver.driverRoute
      case locationInfo of
        Nothing -> do
          let newLocationInfo =
                SimulatedLocationInfo
                  { lastCacheTimeStamp = now,
                    indexOfRouteInfo = 0,
                    doneCount = 0
                  }
          CSSearchRequest.cacheSimulatedLocationInfoByRideId rideId newLocationInfo
          return $ head driverRoute.points
        Just info -> do
          let routeDuration = fromMaybe 60 driverRoute.duration
          let indDiff = round (diffUTCTime now info.lastCacheTimeStamp) `div` max 1 routeDuration.getSeconds
              newIndex = indDiff + info.indexOfRouteInfo
          if newIndex < length driverRoute.points
            then do
              let newLocationInfo =
                    SimulatedLocationInfo
                      { lastCacheTimeStamp = now,
                        indexOfRouteInfo = newIndex,
                        doneCount = info.doneCount
                      }
              CSSearchRequest.cacheSimulatedLocationInfoByRideId rideId newLocationInfo
              return $ driverRoute.points !! newIndex
            else do
              let doneCount = info.doneCount + 1
              let newLocationInfo =
                    info
                      { lastCacheTimeStamp = now,
                        doneCount = doneCount
                      }
              thresholdCount <- asks (.simulatedMaxDone)
              if doneCount > thresholdCount
                then do
                  CSSearchRequest.linkBookingStatusBooking DB.CANCELLED bookingId
                  CSSearchRequest.cacheSimulatedLocationInfoByRideId rideId newLocationInfo
                  return $ last driverRoute.points
                else do
                  CSSearchRequest.cacheSimulatedLocationInfoByRideId rideId newLocationInfo
                  return $ last driverRoute.points
    else nonSimulatedAction
