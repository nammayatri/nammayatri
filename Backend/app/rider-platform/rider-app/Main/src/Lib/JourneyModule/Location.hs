module Lib.JourneyModule.Location where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Data.List.NonEmpty as NE
import Domain.Types.Journey
import Domain.Types.Location
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps as Maps
import Kernel.Prelude (atan2, intToNominalDiffTime)
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

updateJourneyLegStatus :: [ApiTypes.RiderLocationReq] -> LatLong -> JLT.JourneyLegStatus -> Bool -> (Bool, JLT.JourneyLegStatus)
updateJourneyLegStatus recentLocations endLatLang currentStatus isLastJustCompleted = do
  let arrivedThreshold = 50
  let finishingThreshold = 100
  case currentStatus of
    JLT.Ongoing -> checkThreshold finishingThreshold JLT.Finishing
    JLT.Finishing -> checkThreshold arrivedThreshold JLT.Completed
    JLT.InPlan -> if isLastJustCompleted then (True, JLT.Ongoing) else (False, currentStatus)
    JLT.Completed -> (False, currentStatus) -- No change once the leg is completed
    _ -> (False, currentStatus)
  where
    checkThreshold threshold newStatus =
      case averagePosition recentLocations of
        Just avgPosition ->
          if distanceBetweenInMeters avgPosition endLatLang <= threshold
            then (True, newStatus)
            else (False, currentStatus)
        Nothing -> (False, currentStatus)

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
