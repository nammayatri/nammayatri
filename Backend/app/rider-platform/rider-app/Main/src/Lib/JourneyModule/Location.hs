module Lib.JourneyModule.Location where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Data.List.NonEmpty as NE
import Domain.Types.Journey
import Domain.Types.Location
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps as Maps
import Kernel.Prelude (intToNominalDiffTime)
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import Kernel.Utils.Common

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
