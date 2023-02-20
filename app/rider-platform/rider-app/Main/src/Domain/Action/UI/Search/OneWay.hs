module Domain.Action.UI.Search.OneWay
  ( OneWaySearchReq (..),
    OneWaySearchRes (..),
    DSearch.SearchReqLocation (..),
    oneWaySearch,
  )
where

import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import Storage.Queries.Geometry
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics
import qualified Tools.Metrics as Metrics

data OneWaySearchReq = OneWaySearchReq
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWaySearchRes = OneWaySearchRes
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    --TODO: This supposed to be temporary solution. Check if we still need it
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime
  }

oneWaySearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m,
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  OneWaySearchReq ->
  Maybe Version ->
  Maybe Version ->
  m OneWaySearchRes
oneWaySearch personId req bundleVersion clientVersion = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  toLocation <- DSearch.buildSearchReqLoc req.destination
  now <- getCurrentTime
  distance <-
    (\res -> metersToHighPrecMeters res.distance)
      <$> MapSearch.getDistance merchant.id
        MapSearch.GetDistanceReq
          { origin = req.origin.gps,
            destination = req.destination.gps,
            travelMode = Just MapSearch.CAR
          }
  searchRequest <- DSearch.buildSearchRequest person fromLocation (Just toLocation) (Just distance) now bundleVersion clientVersion
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  DB.runTransaction $ do
    QSearchRequest.create searchRequest
    QPFS.updateStatus personId DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  let dSearchRes =
        OneWaySearchRes
          { origin = req.origin,
            destination = req.destination,
            searchId = searchRequest.id,
            now = now,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill
          }
  return dSearchRes
  where
    validateServiceability geoConfig =
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps (Just req.destination.gps)) $
        throwError RideNotServiceable
