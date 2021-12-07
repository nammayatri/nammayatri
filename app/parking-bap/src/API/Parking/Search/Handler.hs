module API.Parking.Search.Handler where

import qualified API.Parking.Search.Types as Search
import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Common
import Control.Lens ((?~))
import qualified Core.API.Search as Search
import Core.API.Types (BecknReq (BecknReq))
import qualified Core.Context as Context
import qualified Core.Search.Intent as Intent
import qualified Core.Search.Location as Location
import qualified Core.Time as Time
import qualified Domain.Search as DSearch
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Search as QSearch
import Tools.Auth (PersonId)
import Tools.Context (buildContext)
import qualified Tools.Metrics as Metrics

handler :: PersonId -> Search.SearchReq -> FlowHandler Search.SearchRes
handler personId req = withFlowHandlerAPI . withPersonIdLogTag (Id personId) $ do
  now <- getCurrentTime
  let bapPersonId = Id personId
  searchRequest <- buildSearchRequest bapPersonId req now
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  _ <- Esq.runTransaction $ QSearch.create searchRequest
  fork "search" . withRetry $ do
    bapURI <- asks (.config.selfURI)
    context <- buildContext Context.SEARCH txnId bapURI Nothing
    let intent = mkIntent req
    ExternalAPI.search (BecknReq context $ Search.SearchIntent intent)
  return . Search.SearchRes $ searchRequest.id

buildSearchRequest ::
  MonadFlow m =>
  Id DSearch.BAPPerson ->
  Search.SearchReq ->
  UTCTime ->
  m DSearch.Search
buildSearchRequest bapPersonId searchReq now = do
  id <- generateGUID
  return
    DSearch.Search
      { id = id,
        lat = searchReq.location.lat,
        lon = searchReq.location.lon,
        requestorId = bapPersonId,
        fromDate = searchReq.fromDate,
        toDate = searchReq.toDate,
        createdAt = now
      }

mkIntent :: Search.SearchReq -> Intent.Intent
mkIntent req = do
  Intent.emptyIntent
    & #fulfillment
      ?~ ( Intent.emptyFulFillmentInfo
             & #start
               ?~ ( Intent.emptyLocationAndTime
                      & #time
                        ?~ ( Time.emptyTime
                               & #timestamp
                                 ?~ (req.fromDate)
                           )
                  )
             & #end
               ?~ ( Intent.emptyLocationAndTime
                      & #location
                        ?~ ( Location.emptyLocation
                               & #gps
                                 ?~ (req.location)
                           )
                      & #time
                        ?~ ( Time.emptyTime
                               & #timestamp
                                 ?~ (req.toDate)
                           )
                  )
         )
