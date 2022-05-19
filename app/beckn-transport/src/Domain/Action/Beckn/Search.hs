module Domain.Action.Beckn.Search where

import App.Types
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Traversable
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude hiding (id, state)
import Product.Location
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Tools.Metrics as Metrics
import Types.Error
import Utils.Common

data DSearchReq = DSearchReq
  { messageId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    mbDropLocation :: Maybe DLoc.SearchReqLocationAPIEntity
  }

data DSearchRes = DSearchRes
  { searchRequest :: DSR.SearchRequest,
    transporter :: DOrg.Organization,
    fromLocation :: DLoc.SearchReqLocation,
    mbToLocation :: Maybe DLoc.SearchReqLocation,
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

search :: Id DOrg.Organization -> DSearchReq -> Flow DSearchRes
search transporterId req@DSearchReq {..} = do
  transporter <- QOrg.findById transporterId >>= fromMaybeM (OrgDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  let pickupLatLong = locationToLatLong pickupLocation
  let mbDropoffLatLong = locationToLatLong <$> mbDropLocation
  unlessM (rideServiceable QGeometry.someGeometriesContain pickupLatLong mbDropoffLatLong) $
    throwError RideNotServiceable
  whenJustM
    (QSearchRequest.findByMsgIdAndBapIdAndBppId messageId bapId transporter.id)
    (\_ -> throwError $ InvalidRequest "Duplicate Search request")

  searchMetricsMVar <- Metrics.startSearchMetrics transporter.id

  now <- getCurrentTime
  validity <- getValidTime now pickupTime
  fromLocation <- buildSearchReqLoc now pickupLocation
  mbToLocation <- buildSearchReqLoc now `traverse` mbDropLocation
  searchRequest <- buildSearchRequest req transporter.id now validity fromLocation.id (mbToLocation <&> (.id))
  Esq.runTransaction $ do
    QLoc.create fromLocation
    whenJust mbToLocation QLoc.create
    QSearchRequest.create searchRequest
  pure DSearchRes {..}

buildSearchReqLoc ::
  MonadGuid m =>
  UTCTime ->
  DLoc.SearchReqLocationAPIEntity ->
  m DLoc.SearchReqLocation
buildSearchReqLoc now DLoc.SearchReqLocationAPIEntity {..} = do
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
  Id DLoc.SearchReqLocation ->
  Maybe (Id DLoc.SearchReqLocation) ->
  m DSR.SearchRequest
buildSearchRequest DSearchReq {..} transporterId now validity fromLocationId mbToLocationId = do
  uuid <- generateGUID
  pure
    DSR.SearchRequest
      { id = Id uuid,
        messageId = messageId,
        startTime = pickupTime,
        validTill = validity,
        providerId = transporterId,
        fromLocationId = fromLocationId,
        toLocationId = mbToLocationId,
        bapId = bapId,
        bapUri = bapUri,
        createdAt = now
      }
