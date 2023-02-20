module Domain.Action.UI.Search.Common
  ( SearchReqLocation (..),
    buildSearchReqLoc,
    buildSearchRequest,
  )
where

import Domain.Types.LocationAddress
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as Location
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Version
import Kernel.Utils.Common
import Tools.Metrics (CoreMetrics)

buildSearchRequest ::
  ( (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]),
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  DPerson.Person ->
  Location.SearchReqLocation ->
  Maybe Location.SearchReqLocation ->
  Maybe HighPrecMeters ->
  UTCTime ->
  Maybe Version ->
  Maybe Version ->
  m SearchRequest.SearchRequest
buildSearchRequest person pickup mbDrop mbDistance now bundleVersion clientVersion = do
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
        createdAt = now,
        bundleVersion = bundleVersion,
        clientVersion = clientVersion
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry startTime = do
      searchRequestExpiry <- maybe 7200 fromIntegral <$> asks (.searchRequestExpiry)
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger searchRequestExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

data SearchReqLocation = SearchReqLocation
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

buildSearchReqLoc :: MonadFlow m => SearchReqLocation -> m Location.SearchReqLocation
buildSearchReqLoc SearchReqLocation {..} = do
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
