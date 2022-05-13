module Domain.Action.UI.Search.Rental where

import App.Types
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (id, state)
import Storage.Queries.Geometry
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Tools.Metrics as Metrics
import qualified Types.API.RentalSearch as API
import Types.Error
import Utils.Common

data DSearchReq = DSearchReq
  { origin :: API.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    startTime :: UTCTime
  }

search :: Id Person.Person -> API.SearchReq -> Flow (API.SearchRes, DSearchReq)
search personId req = do
  validateServiceability
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest (getId personId) fromLocation Nothing Nothing now
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  DB.runTransaction $ do
    Location.create fromLocation
    QSearchRequest.create searchRequest
  let dSearchReq =
        DSearchReq
          { origin = req.origin,
            searchId = searchRequest.id,
            startTime = req.startTime
          }
  return (API.SearchRes $ searchRequest.id, dSearchReq)
  where
    validateServiceability = do
      unlessM (rideServiceable someGeometriesContain req.origin.gps Nothing) $
        throwError RideNotServiceable
