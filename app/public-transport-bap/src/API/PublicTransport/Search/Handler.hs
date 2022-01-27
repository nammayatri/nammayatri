module API.PublicTransport.Search.Handler where

import API.Types.Common
import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.Migration.Context as Context
import qualified Beckn.Types.Core.Migration.Gps as Gps
import qualified Beckn.Types.Core.Migration.Location as Location
import qualified Beckn.Types.Core.Migration.Time as Time
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Control.Lens ((?~))
import qualified Core.API.Search as Search
import qualified Core.Search.Intent as Intent
import qualified Data.Text as T
import Domain.Outcoming.Search as Search
import Domain.Search as DSearch
import ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Search as QSearch
import Text.Read
import Tools.Auth
import Tools.Context

handler :: PersonId -> Search.SearchReq -> FlowHandler Search.SearchRes
handler personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  now <- getCurrentTime
  gps <- makeGps req.location
  searchRequest <- buildSearchRequest personId req now gps
  let txnId = getId (searchRequest.id)
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
  Id Person ->
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
