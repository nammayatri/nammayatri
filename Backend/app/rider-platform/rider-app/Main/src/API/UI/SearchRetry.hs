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
import qualified Domain.Types.Person as Person
import Domain.Types.SearchRequest (SearchRequest)
import Environment
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version
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
    :> ReqBody '[JSON] OneWay.SearchRetryReq
    :> Servant.Header "x-bundle-version" Version
    :> Servant.Header "x-client-version" Version
    :> Capture "parentSearchId" (Id SearchRequest)
    :> Header "device" Text
    :> Post '[JSON] SearchRetryRes

handler :: FlowServer API
handler = searchRetryRequest

data SearchRetryRes = SearchRetryRes
  { searchId :: Id SearchRequest,
    searchExpiry :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

searchRetryRequest :: Id Person.Person -> OneWay.SearchRetryReq -> Maybe Version -> Maybe Version -> Id SearchRequest -> Maybe Text -> FlowHandler SearchRetryRes
searchRetryRequest personId req mbBundleVersion mbClientVersion parentSearchId device = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  Search.checkSearchRateLimit personId
  (searchId, searchExpiry) <- searchRetry personId parentSearchId req mbBundleVersion mbClientVersion device
  return $ SearchRetryRes searchId searchExpiry

searchRetry ::
  Id Person.Person ->
  Id SearchRequest ->
  OneWay.SearchRetryReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Flow (Id SearchRequest, UTCTime)
searchRetry personId parentSearchId req bundleVersion clientVersion device = do
  person <- Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  distanceAndDuration <- Redis.getdistanceAnndDurationInfo parentSearchId
  dSearchRes <- OneWay.search person merchant (OneWay.SearchRetry req) (Just parentSearchId) bundleVersion clientVersion Nothing device Nothing Nothing
  mbDistanceAndDuration <-
    case distanceAndDuration of
      Just distanceandDuration -> do
        return DistanceAndDuration {shortdistance = distanceandDuration.shortdistance, duration = distanceandDuration.duration, longestditance = distanceandDuration.longestditance}
      Nothing -> getRouteResponse dSearchRes person
  fork "search cabs" . withShortRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchRes device mbDistanceAndDuration.shortdistance mbDistanceAndDuration.duration (Just parentSearchId)
    void $ CallBPP.search dSearchRes.gatewayUrl becknTaxiReq
  return (dSearchRes.searchId, dSearchRes.searchRequestExpiry)

getRouteResponse :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => OneWay.OneWaySearchRes -> Person.Person -> m DistanceAndDuration
getRouteResponse dSearchRes person = do
  let sourceLatlong = dSearchRes.origin.gps
  let destinationLatLong = dSearchRes.destination.gps
  let request =
        Maps.GetRoutesReq
          { waypoints = [sourceLatlong, destinationLatLong],
            calcPoints = True,
            mode = Just Maps.CAR
          }
  resp <- Maps.getRoutes person.merchantId request
  let shortestRouteInfo = Search.getRouteInfoWithShortestDuration resp
  let shortestRouteDistance = (.distance) =<< shortestRouteInfo
  let shortestRouteDuration = (.duration) =<< shortestRouteInfo
  let longestRouteDistance = (.distance) =<< Search.getLongestRouteDistance resp
  Redis.distanceAnndDurationInfo dSearchRes.searchId OneWay.DistanceAndDuration {shortdistance = shortestRouteDistance, duration = shortestRouteDuration, longestditance = longestRouteDistance}
  return DistanceAndDuration {shortdistance = shortestRouteDistance, duration = shortestRouteDuration, longestditance = longestRouteDistance}
