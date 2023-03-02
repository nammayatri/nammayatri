{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

search :: forall m r. EsqDBFlow m r => SearchReq -> m SearchMessage
search req = do
  now <- getCurrentTime
  let searchRequest = makeSearchRequest now
  _ <- Esq.runTransaction $ QSearch.create @m searchRequest
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
