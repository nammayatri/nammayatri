module Domain.Action.Beckn.Search
  ( DSearchReq (..),
    DSearchRes (..),
    LocationReq (..),
    search,
  )
where

import Beckn.Prelude (ToSchema)
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (..))
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Traversable
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Environment
import EulerHS.Prelude hiding (id, state)
import qualified Storage.CachedQueries.Organization as QOrg
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import qualified Tools.Metrics as Metrics

data DSearchReq = DSearchReq
  { messageId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LocationReq,
    pickupTime :: UTCTime,
    mbDropLocation :: Maybe LocationReq
  }

data DSearchRes = DSearchRes
  { searchRequest :: DSR.SearchRequest,
    transporter :: DOrg.Organization,
    fromLocation :: DLoc.SearchReqLocation,
    mbToLocation :: Maybe DLoc.SearchReqLocation,
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

data LocationReq = LocationReq
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, HasCoordinates)

search :: Id DOrg.Organization -> DSearchReq -> Flow DSearchRes
search transporterId req@DSearchReq {..} = do
  transporter <- QOrg.findById transporterId >>= fromMaybeM (OrgDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  let pickupLatLong = getCoordinates pickupLocation
  let mbDropoffLatLong = getCoordinates <$> mbDropLocation
  unlessM (rideServiceableDefault QGeometry.someGeometriesContain pickupLatLong mbDropoffLatLong) $
    throwError RideNotServiceable
  whenJustM
    (QSearchRequest.findByMsgIdAndBapIdAndBppId messageId bapId transporter.id)
    (\_ -> throwError $ InvalidRequest "Duplicate Search request")

  searchMetricsMVar <- Metrics.startSearchMetrics transporter.id

  now <- getCurrentTime
  validity <- getValidTime now pickupTime
  fromLocation <- buildSearchReqLoc now pickupLocation
  mbToLocation <- buildSearchReqLoc now `traverse` mbDropLocation
  searchRequest <- buildSearchRequest req transporter.id now validity fromLocation mbToLocation
  Esq.runTransaction $ do
    --These things are used only for analitics
    QSearchRequest.create searchRequest
  pure DSearchRes {..}

buildSearchReqLoc ::
  MonadGuid m =>
  UTCTime ->
  LocationReq ->
  m DLoc.SearchReqLocation
buildSearchReqLoc now LocationReq {..} = do
  locId <- generateGUID
  return
    DLoc.SearchReqLocation
      { id = locId,
        createdAt = now,
        updatedAt = now,
        ..
      }

getValidTime :: HasFlowEnv m r '["caseExpiry" ::: Maybe Seconds] => UTCTime -> UTCTime -> m UTCTime
getValidTime now startTime = do
  caseExpiry_ <- maybe 7200 fromIntegral <$> asks (.caseExpiry)
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

buildSearchRequest ::
  MonadGuid m =>
  DSearchReq ->
  Id DOrg.Organization ->
  UTCTime ->
  UTCTime ->
  DLoc.SearchReqLocation ->
  Maybe DLoc.SearchReqLocation ->
  m DSR.SearchRequest
buildSearchRequest DSearchReq {..} transporterId now validity fromLocation mbToLocation = do
  uuid <- generateGUID
  pure
    DSR.SearchRequest
      { id = Id uuid,
        messageId = messageId,
        startTime = pickupTime,
        validTill = validity,
        providerId = transporterId,
        fromLocation = fromLocation,
        toLocation = mbToLocation,
        bapId = bapId,
        bapUri = bapUri,
        createdAt = now
      }
