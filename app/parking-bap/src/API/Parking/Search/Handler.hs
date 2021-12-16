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
import qualified Core.Gps as Gps
import qualified Core.Search.Intent as Intent
import qualified Core.Search.Location as Location
import qualified Core.Time as Time
import qualified Data.Text as T
import qualified Domain.Search as DSearch
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Search as QSearch
import Text.Read (readMaybe)
import Tools.Auth (PersonId)
import Tools.Context (buildContext)
import Tools.Error
import qualified Tools.Metrics as Metrics
import API.Types.Common (Gps)

handler :: PersonId -> Search.SearchReq -> FlowHandler Search.SearchRes
handler personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  now <- getCurrentTime
  gps <- makeGps req.location
  searchRequest <- buildSearchRequest personId req now gps
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  _ <- Esq.runTransaction $ QSearch.create searchRequest
  fork "search" . withRetry $ do
    bapURI <- asks (.config.selfURI)
    context <- buildContext Context.SEARCH txnId bapURI Nothing
    let intent = mkIntent req gps
    ExternalAPI.search (BecknReq context $ Search.SearchIntent intent)
  return . Search.SearchRes $ searchRequest.id

makeGps :: MonadFlow m => Gps -> m Gps.Gps
makeGps location = do
  lat <- readMaybe (T.unpack location.lat) & fromMaybeM (InternalError "Unable to parse lat")
  lon <- readMaybe (T.unpack location.lon) & fromMaybeM (InternalError "Unable to parse lon")
  pure Gps.Gps {..}

buildSearchRequest ::
  MonadFlow m =>
  Id DSearch.Person ->
  Search.SearchReq ->
  UTCTime ->
  Gps.Gps ->
  m DSearch.Search
buildSearchRequest bapPersonId searchReq now gps = do
  id <- generateGUID
  return
    DSearch.Search
      { id = id,
        lat = gps.lat,
        lon = gps.lon,
        requestorId = bapPersonId,
        fromDate = searchReq.fromDate,
        toDate = searchReq.toDate,
        createdAt = now
      }

mkIntent :: Search.SearchReq -> Gps.Gps -> Intent.Intent
mkIntent req gps = do
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
                                 ?~ gps
                           )
                      & #time
                        ?~ ( Time.emptyTime
                               & #timestamp
                                 ?~ (req.toDate)
                           )
                  )
         )
