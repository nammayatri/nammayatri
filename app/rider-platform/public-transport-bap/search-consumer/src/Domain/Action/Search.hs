module Domain.Action.Search where

import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Search as DSearch
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Streaming.Kafka.Topic.PublicTransportSearch (PublicTransportSearch)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Storage.Queries.Search as QSearch

type SearchReq = PublicTransportSearch

data SearchMessage = SearchMessage
  { searchId :: Id DSearch.Search,
    gps :: LatLong,
    fromDate :: UTCTime,
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
            fromDate = now,
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
