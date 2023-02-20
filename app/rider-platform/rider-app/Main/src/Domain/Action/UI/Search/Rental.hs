module Domain.Action.UI.Search.Rental
  ( RentalSearchReq (..),
    RentalSearchRes (..),
    DSearch.SearchReqLocation (..),
    rentalSearch,
  )
where

import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import Storage.Queries.Geometry
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Metrics
import qualified Tools.Metrics as Metrics

data RentalSearchReq = RentalSearchReq
  { origin :: DSearch.SearchReqLocation,
    startTime :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalSearchRes = RentalSearchRes
  { origin :: DSearch.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    startTime :: UTCTime,
    --TODO: This supposed to be temporary solution. Check if we still need it
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime
  }

rentalSearch ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  RentalSearchReq ->
  m RentalSearchRes
rentalSearch personId bundleVersion clientVersion req = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <-
    QMerchant.findById person.merchantId
      >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest person fromLocation Nothing Nothing now bundleVersion clientVersion
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  DB.runTransaction $ do
    QSearchRequest.create searchRequest
  let dSearchRes =
        RentalSearchRes
          { origin = req.origin,
            searchId = searchRequest.id,
            startTime = req.startTime,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill
          }
  return dSearchRes
  where
    validateServiceability geoConfig = do
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps Nothing) $
        throwError RideNotServiceable
