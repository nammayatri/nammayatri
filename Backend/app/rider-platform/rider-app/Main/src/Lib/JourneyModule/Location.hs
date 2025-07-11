module Lib.JourneyModule.Location where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Data.List.NonEmpty as NE
import Domain.Types.Journey
import Domain.Types.Location
import qualified Domain.Types.Station as Station
import Domain.Types.Trip
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps as Maps
import Kernel.Prelude (atan2, intToNominalDiffTime, listToMaybe)
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JLT

locationToLatLng :: Location -> LatLong
locationToLatLng Location {..} = LatLong {..}

makeLocationRedisKey :: Id person -> Text
makeLocationRedisKey driverId = mconcat ["locations", ":", driverId.getId]

addPoint :: (HedisFlow m env) => Id Journey -> ApiTypes.RiderLocationReq -> m ()
addPoint journeyId req = do
  let key = makeLocationRedisKey journeyId
  lPush key $ NE.singleton req
  lTrim key 0 10 -- always keep last 10 points
  Hedis.expire key 21600 -- 6 hours

clearPoints :: HedisFlow m env => Id Journey -> m ()
clearPoints journeyId = do
  let key = makeLocationRedisKey journeyId
  clearList key

getLastThreePoints :: (HedisFlow m env) => Id Journey -> m [ApiTypes.RiderLocationReq]
getLastThreePoints journeyId = do
  currentTime <- getCurrentTime
  points <- lRange (makeLocationRedisKey journeyId) 0 (-1)
  let thirtySecondsAgo = 30
  return (take 3 $ filter (\ApiTypes.RiderLocationReq {..} -> diffUTCTime currentTime currTime <= intToNominalDiffTime thirtySecondsAgo) points)

updateJourneyLegStatus :: MultimodalTravelMode -> [ApiTypes.RiderLocationReq] -> LatLong -> JLT.JourneyLegStatus -> Bool -> Maybe Station.Station -> (Bool, JLT.JourneyLegStatus)
updateJourneyLegStatus travelMode recentLocations endLatLong currentStatus isLastCompleted maybeToStation = do
  let (arrivedThreshold, finishingThreshold) =
        case travelMode of
          Walk -> (50, 100)
          Subway -> (300, 500)
          Metro -> (300, 500)
          Bus -> (100, 300)
          Taxi -> (50, 100)
  let toStationGates = maybe [] (fromMaybe [] . Station.gates) maybeToStation
  let checkAll threshold newStatus toStationGates' =
        fromMaybe (False, currentStatus) $
          checkProximityViaLatest newStatus recentLocations threshold endLatLong
            <|> checkProximityViaGates newStatus recentLocations threshold toStationGates'
            <|> checkProximityViaAverage newStatus recentLocations threshold toStationGates' endLatLong
  case currentStatus of
    JLT.Ongoing -> checkAll finishingThreshold JLT.Finishing toStationGates
    JLT.Finishing -> checkAll arrivedThreshold JLT.Completed toStationGates
    cs | cs `elem` [JLT.InPlan, JLT.Booked] -> if isLastCompleted then (True, bool JLT.OnTheWay JLT.Ongoing (travelMode == Walk)) else (False, currentStatus)
    JLT.Completed -> (False, currentStatus) -- No change once the leg is completed
    _ -> (False, currentStatus)
  where
    checkGates :: JLT.JourneyLegStatus -> HighPrecMeters -> LatLong -> [Station.Gate] -> Maybe (Bool, JLT.JourneyLegStatus)
    checkGates newStatus threshold avgPosition toStationGates' = do
      minDistance <- closestGateDistance avgPosition toStationGates'
      if minDistance <= threshold then Just (True, newStatus) else Nothing

    checkProximityViaLatest :: JLT.JourneyLegStatus -> [ApiTypes.RiderLocationReq] -> HighPrecMeters -> LatLong -> Maybe (Bool, JLT.JourneyLegStatus)
    checkProximityViaLatest newStatus recentLocations' threshold targetLatLong = do
      latestPosition <- listToMaybe recentLocations'
      checkDistance latestPosition.latLong threshold newStatus targetLatLong

    checkProximityViaGates :: JLT.JourneyLegStatus -> [ApiTypes.RiderLocationReq] -> HighPrecMeters -> [Station.Gate] -> Maybe (Bool, JLT.JourneyLegStatus)
    checkProximityViaGates newStatus recentLocations' threshold toStationGates' = do
      latestPosition <- listToMaybe recentLocations'
      minGateDist <- closestGateDistance latestPosition.latLong toStationGates'
      if minGateDist <= threshold
        then Just (True, newStatus)
        else Nothing

    checkProximityViaAverage :: JLT.JourneyLegStatus -> [ApiTypes.RiderLocationReq] -> HighPrecMeters -> [Station.Gate] -> LatLong -> Maybe (Bool, JLT.JourneyLegStatus)
    checkProximityViaAverage newStatus recentLocations' threshold toStationGates' targetLatLong = do
      avgPosition <- averagePosition recentLocations'
      checkDistance avgPosition threshold newStatus targetLatLong <|> checkGates newStatus threshold avgPosition toStationGates'

    closestGateDistance :: LatLong -> [Station.Gate] -> Maybe HighPrecMeters
    closestGateDistance pos gates =
      fmap (minimum . NE.map gateDistance) (NE.nonEmpty gates)
      where
        gateDistance g = distanceBetweenInMeters pos (LatLong g.lat g.lon)

    checkDistance :: LatLong -> HighPrecMeters -> JLT.JourneyLegStatus -> LatLong -> Maybe (Bool, JLT.JourneyLegStatus)
    checkDistance pos threshold newStatus target =
      if distanceBetweenInMeters pos target <= threshold
        then Just (True, newStatus)
        else Nothing

-- Convert latitude and longitude to Cartesian coordinates
latLongToCartesian :: LatLong -> (Double, Double, Double)
latLongToCartesian (LatLong lat lon) =
  let latRad = lat * pi / 180
      lonRad = lon * pi / 180
      x = cos latRad * cos lonRad
      y = cos latRad * sin lonRad
      z = sin latRad
   in (x, y, z)

-- Convert Cartesian coordinates back to latitude and longitude
cartesianToLatLong :: (Double, Double, Double) -> LatLong
cartesianToLatLong (x, y, z) =
  let hyp = sqrt (x * x + y * y)
      lat = atan2 z hyp * 180 / pi
      lon = atan2 y x * 180 / pi
   in LatLong lat lon

-- Calculate the geographic mean of a list of LatLong
averagePosition :: [ApiTypes.RiderLocationReq] -> Maybe LatLong
averagePosition [] = Nothing
averagePosition locations =
  let cartesianCoords = map (latLongToCartesian . (.latLong)) locations
      (sumX, sumY, sumZ) = foldl' (\(sx, sy, sz) (x, y, z) -> (sx + x, sy + y, sz + z)) (0, 0, 0) cartesianCoords
      count = fromIntegral (length locations)
      avgCartesian = (sumX / count, sumY / count, sumZ / count)
   in Just (cartesianToLatLong avgCartesian)
