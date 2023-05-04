{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}

module API.UI.SearchRetry
  ( SearchRetryReq (..),
    SearchRetryRes (..),
    DSearchCommon.SearchReqLocation (..),
    API,
    handler,
  )
where

import qualified Beckn.ACL.Search as TaxiACL
import Data.Aeson
import Data.OpenApi hiding (Header)
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import Domain.Action.UI.Search.OneWay as OneWay
import Domain.Action.UI.SearchRetry.OneWayRetry as DOneWayRetry
import qualified Domain.Types.Person as Person
import Domain.Types.SearchRequest (SearchRequest)
import Environment
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Search as Search
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.RouteInfo as Redis
import qualified Storage.Queries.Person as Person
import Tools.Auth
import qualified Tools.Maps as Maps
import Tools.Metrics

------ Search Flow --------

type API =
  "rideSearchRetry"
    :> TokenAuth
    :> ReqBody '[JSON] SearchRetryReq
    :> Capture "parentSearchId" (Id SearchRequest)
    :> Header "device" Text
    :> Post '[JSON] SearchRetryRes

handler :: FlowServer API
handler = search

data SearchRetryRes = SearchRetryRes
  { searchId :: Id SearchRequest,
    searchExpiry :: UTCTime,
    routeInfo :: Maybe Maps.RouteInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

search :: Id Person.Person -> SearchRetryReq -> Id SearchRequest -> Maybe Text -> FlowHandler SearchRetryRes
search personId req parentSearchId device = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  Search.checkSearchRateLimit personId
  (searchId, searchExpiry, routeInfo) <- searchRetry personId parentSearchId req device
  return $ SearchRetryRes searchId searchExpiry routeInfo

searchRetry ::
  Id Person.Person ->
  Id SearchRequest ->
  DOneWayRetry.SearchRetryReq ->
  Maybe Text ->
  Flow (Id SearchRequest, UTCTime, Maybe Maps.RouteInfo)
searchRetry personId parentSearchId req device = do
  person <- Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  dSearchRes <- DOneWayRetry.oneWaySearchRetry person merchant req parentSearchId
  mbRouteResponse <- Redis.getRouteInfo parentSearchId
  routeResponse <-
    case mbRouteResponse of
      Just a -> pure a
      Nothing -> getRouteResponse dSearchRes person
  let shortestRouteInfo = Search.getRouteInfoWithShortestDuration routeResponse
  let shortestRouteDistance = (.distance) =<< shortestRouteInfo
  let shortestRouteDuration = (.duration) =<< shortestRouteInfo
  fork "search cabs" . withShortRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchRes device shortestRouteDistance shortestRouteDuration (Just parentSearchId)
    void $ CallBPP.search dSearchRes.gatewayUrl becknTaxiReq
  return (dSearchRes.searchId, dSearchRes.searchRequestExpiry, shortestRouteInfo)

getRouteResponse :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => OneWay.OneWaySearchRes -> Person.Person -> m [Maps.RouteInfo]
getRouteResponse dSearchRes person = do
  let sourceLatlong = dSearchRes.origin.gps
  let destinationLatLong = dSearchRes.destination.gps
  let request =
        Maps.GetRoutesReq
          { waypoints = [sourceLatlong, destinationLatLong],
            calcPoints = True,
            mode = Just Maps.CAR
          }
  Maps.getRoutes person.merchantId request
