module Domain.Action.Search where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch (PublicTransportSearch)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Search as DSearch
import qualified Storage.Queries.Search as QSearch

type SearchReq = PublicTransportSearch

data SearchMessage = SearchMessage
  { searchId :: Id DSearch.Search,
    gps :: LatLong,
    toDate :: UTCTime
  }

search :: EsqDBFlow m r => SearchReq -> m SearchMessage
search req = do
  now <- getCurrentTime
  searchRequest <- buildSearchRequest req now
  _ <- Esq.runTransaction $ QSearch.create searchRequest
  let searchMessage =
        SearchMessage
          { searchId = searchRequest.id,
            gps = req.gps,
            toDate = getToDate now
          }
  return searchMessage
  where
    getToDate = addUTCTime 7200 -- 2 hours

buildSearchRequest ::
  MonadFlow m =>
  SearchReq ->
  UTCTime ->
  m DSearch.Search
buildSearchRequest searchReq now = do
  id <- generateGUID
  return
    DSearch.Search
      { id = id,
        lat = searchReq.gps.lat,
        lon = searchReq.gps.lon,
        requestorId = Id searchReq.requestorId,
        createdAt = now
      }
