module Product.Search where

import API.UI.Search.Types as Search
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.Migration.Gps as Gps
import Beckn.Types.Id
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Utils.Common
import Core.ACL.Handler.Search as SearchHandler
import qualified Domain.Types.Search as Domain
import qualified Storage.Queries.Search as QSearch
import Tools.Auth

searchHandler ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    EsqDBFlow m r,
    HasField "config" r c,
    HasField "gatewayUrl" c BaseUrl,
    HasField "selfURI" c BaseUrl,
    HasField "httpClientOptions" c HttpClientOptions,
    HasField "selfId" c Text
  ) =>
  PersonId ->
  Search.SearchReq ->
  m Search.SearchRes
searchHandler personId req = do
  now <- getCurrentTime
  gps <- SearchHandler.makeGps req.location
  searchRequest <- buildSearchRequest personId req now gps
  let txnId = getId (searchRequest.id)
  _ <- Esq.runTransaction $ QSearch.create searchRequest
  fork "search" . withRetry $ do
    bapURI <- asks (.config.selfURI)
    SearchHandler.searchHandler req gps txnId bapURI
  return . Search.SearchRes $ searchRequest.id

buildSearchRequest ::
  MonadFlow m =>
  Id Person ->
  Search.SearchReq ->
  UTCTime ->
  Gps.Gps ->
  m Domain.Search
buildSearchRequest bapPersonId searchReq now gps = do
  id <- generateGUID
  return
    Domain.Search
      { id = id,
        lat = gps.lat,
        lon = gps.lon,
        requestorId = bapPersonId,
        fromDate = searchReq.fromDate,
        toDate = searchReq.toDate,
        createdAt = now
      }
