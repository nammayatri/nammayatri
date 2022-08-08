module Domain.Action.UI.Search.Common where

import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as Location
import EulerHS.Prelude hiding (state)
import Tools.Metrics (CoreMetrics)
import qualified Types.API.Search as API
import Utils.Common

buildSearchRequest ::
  ( (HasFlowEnv m r ["searchRequestExpiry" ::: Maybe Seconds, "graphhopperUrl" ::: BaseUrl]),
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  DPerson.Person ->
  Location.SearchReqLocation ->
  Maybe Location.SearchReqLocation ->
  Maybe HighPrecMeters ->
  UTCTime ->
  m SearchRequest.SearchRequest
buildSearchRequest person pickup mbDrop mbDistance now = do
  searchRequestId <- generateGUID
  validTill <- getSearchRequestExpiry now
  return
    SearchRequest.SearchRequest
      { id = searchRequestId,
        startTime = now,
        validTill = validTill,
        riderId = person.id,
        fromLocation = pickup,
        toLocation = mbDrop,
        distance = mbDistance,
        merchantId = person.merchantId,
        createdAt = now
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry startTime = do
      searchRequestExpiry <- maybe 7200 fromIntegral <$> asks (.searchRequestExpiry)
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger searchRequestExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

buildSearchReqLoc :: MonadFlow m => API.SearchReqLocation -> m Location.SearchReqLocation
buildSearchReqLoc API.SearchReqLocation {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  return
    Location.SearchReqLocation
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        address = address,
        createdAt = now,
        updatedAt = now
      }
