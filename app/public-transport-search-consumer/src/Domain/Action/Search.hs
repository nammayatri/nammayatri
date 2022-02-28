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
  let searchRequest = makeSearchRequest now
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
    makeSearchRequest now =
      DSearch.Search
        { id = Id req.id,
          lat = req.gps.lat,
          lon = req.gps.lon,
          requestorId = Id req.requestorId,
          createdAt = now
        }
