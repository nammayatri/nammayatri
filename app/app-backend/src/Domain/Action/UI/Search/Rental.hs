module Domain.Action.UI.Search.Rental
  ( RentalSearchReq (..),
    RentalSearchRes (..),
    DSearch.SearchReqLocation (..),
    rentalSearch,
  )
where

import Beckn.Prelude
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSearchReq
import Storage.Queries.Geometry
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Metrics
import qualified Tools.Metrics as Metrics
import Types.Error
import Utils.Common

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
    gatewayUrl :: BaseUrl
  }

rentalSearch ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  RentalSearchReq ->
  m RentalSearchRes
rentalSearch personId req = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <-
    QMerchant.findById person.merchantId
      >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest person fromLocation Nothing Nothing now
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  DB.runTransaction $ do
    QSearchRequest.create searchRequest
  let dSearchRes =
        RentalSearchRes
          { origin = req.origin,
            searchId = searchRequest.id,
            startTime = req.startTime,
            gatewayUrl = merchant.gatewayUrl
          }
  return dSearchRes
  where
    validateServiceability geoConfig = do
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps Nothing) $
        throwError RideNotServiceable
