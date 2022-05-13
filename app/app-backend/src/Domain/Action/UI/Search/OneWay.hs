module Domain.Action.UI.Search.OneWay where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch
import Beckn.Streaming.MonadProducer
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (id, state)
import Storage.Queries.Geometry
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Tools.Metrics as Metrics
import qualified Types.API.Search as API
import Types.Error
import Utils.Common

data DSearchReq = DSearchReq
  { origin :: API.SearchReqLocation,
    destination :: API.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime
  }

search :: Id Person.Person -> API.SearchReq -> Flow (API.SearchRes, DSearchReq)
search personId req = do
  validateServiceability
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  toLocation <- DSearch.buildSearchReqLoc req.destination
  now <- getCurrentTime
  distance <- (\res -> getDistanceInMeter res.info.distance) <$> MapSearch.getDistance (Just MapSearch.CAR) req.origin.gps req.destination.gps
  searchRequest <- DSearch.buildSearchRequest (getId personId) fromLocation (Just toLocation) (Just distance) now
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  DB.runTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QSearchRequest.create searchRequest
  let dSearchReq =
        DSearchReq
          { origin = req.origin,
            destination = req.destination,
            searchId = searchRequest.id,
            now = now
          }
  return (API.SearchRes $ searchRequest.id, dSearchReq)
  where
    validateServiceability = do
      unlessM (rideServiceable someGeometriesContain req.origin.gps (Just req.destination.gps)) $
        throwError RideNotServiceable

sendPublicTransportSearchRequest ::
  MonadProducer PublicTransportSearch m =>
  Id Person.Person ->
  DSearchReq ->
  m ()
sendPublicTransportSearchRequest personId DSearchReq {..} = do
  producePublicTransportSearchMessage publicTransportSearch
  where
    publicTransportSearch =
      PublicTransportSearch
        { id = getId searchId,
          gps = origin.gps,
          requestorId = getId personId,
          createdAt = now
        }
