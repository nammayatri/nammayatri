{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}

module API.UI.Search
  ( SearchReq (..),
    SearchRes (..),
    DOneWaySearch.OneWaySearchReq (..),
    DRentalSearch.RentalSearchReq (..),
    DSearchCommon.SearchReqLocation (..),
    API,
    search,
    handler,
  )
where

import qualified Beckn.ACL.Metro.Search as MetroACL
import qualified Beckn.ACL.Search as TaxiACL
import qualified Data.Text as T
import Environment
import Kernel.External.Maps
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Topic.PublicTransportSearch
import Kernel.Streaming.MonadProducer
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.PublicTransport as PublicTransport
import SharedLogic.Search
import qualified SharedLogic.Search.Common as DSearchCommon
import qualified SharedLogic.Search.OneWay as DOneWaySearch
import qualified SharedLogic.Search.Rental as DRentalSearch
import SharedLogic.SimulatedFlow.Search
import qualified SharedLogic.Types.Person as Person
import SharedLogic.Types.SearchRequest (SearchRequest)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.Queries.Person as Person
import Tools.Auth
import qualified Tools.Maps as Maps
import Tools.Metrics

-------- Search Flow --------

type API =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Servant.Header "x-bundle-version" Version
    :> Servant.Header "x-client-version" Version
    :> Header "device" Text
    :> Post '[JSON] SearchRes

handler :: FlowServer API
handler = search

search :: Id Person.Person -> SearchReq -> Maybe Version -> Maybe Version -> Maybe Text -> FlowHandler SearchRes
search personId req mbBundleVersion mbClientVersion device = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  checkSearchRateLimit personId
  updateVersions personId mbBundleVersion mbClientVersion
  interceptWhenSimulated person.isSimulated req personId mbBundleVersion mbClientVersion device $ do
    (searchId, searchExpiry, routeInfo) <- case req of
      OneWaySearch oneWay -> oneWaySearch personId mbBundleVersion mbClientVersion device oneWay
      RentalSearch rental -> rentalSearch personId mbBundleVersion mbClientVersion device rental
    return $ SearchRes searchId searchExpiry routeInfo

oneWaySearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r,
    MonadProducer PublicTransportSearch m
  ) =>
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  DOneWaySearch.OneWaySearchReq ->
  m (Id SearchRequest, UTCTime, Maybe Maps.RouteInfo)
oneWaySearch personId bundleVersion clientVersion device req = do
  person <- Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  let sourceLatlong = req.origin.gps
  let destinationLatLong = req.destination.gps
  let request =
        Maps.GetRoutesReq
          { waypoints = [sourceLatlong, destinationLatLong],
            calcPoints = True,
            mode = Just Maps.CAR
          }
  routeResponse <- Maps.getRoutes person.merchantId request
  let shortestRouteInfo = getRouteInfoWithShortestDuration routeResponse
  let longestRouteDistance = (.distance) =<< getLongestRouteDistance routeResponse
  let shortestRouteDistance = (.distance) =<< shortestRouteInfo
  let shortestRouteDuration = (.duration) =<< shortestRouteInfo
  dSearchRes <- DOneWaySearch.oneWaySearch person merchant req bundleVersion clientVersion longestRouteDistance device shortestRouteDistance shortestRouteDuration
  fork "search cabs" . withShortRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchRes device shortestRouteDistance shortestRouteDuration
    void $ CallBPP.search dSearchRes.gatewayUrl becknTaxiReq
  fork "search metro" . withShortRetry $ do
    becknMetroReq <- MetroACL.buildSearchReq dSearchRes
    CallBPP.searchMetro dSearchRes.gatewayUrl becknMetroReq
  fork "search public-transport" $ PublicTransport.sendPublicTransportSearchRequest personId dSearchRes
  fork "updating search counters" $ do
    merchantConfigs <- CMC.findAllByMerchantId person.merchantId
    SMC.updateSearchSimulationCounters personId merchantConfigs
    mSimulationDetected <- SMC.anySimulationDetected personId person.merchantId merchantConfigs
    whenJust mSimulationDetected $ \mc -> SMC.takeAction personId (Just mc.id) mc.shouldSimulate
  return (dSearchRes.searchId, dSearchRes.searchRequestExpiry, shortestRouteInfo)

rentalSearch ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  DRentalSearch.RentalSearchReq ->
  m (Id SearchRequest, UTCTime, Maybe Maps.RouteInfo)
rentalSearch personId bundleVersion clientVersion device req = do
  dSearchRes <- DRentalSearch.rentalSearch personId bundleVersion clientVersion device req
  fork "search rental" . withShortRetry $ do
    -- do we need fork here?
    becknReq <- TaxiACL.buildRentalSearchReq dSearchRes
    void $ CallBPP.search dSearchRes.gatewayUrl becknReq
  pure (dSearchRes.searchId, dSearchRes.searchRequestExpiry, Nothing)

checkSearchRateLimit ::
  ( Redis.HedisFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadTime m
  ) =>
  Id Person.Person ->
  m ()
checkSearchRateLimit personId = do
  let key = searchHitsCountKey personId
  hitsLimit <- asks (.searchRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.searchRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    msgTemplate <- asks (.searchLimitExceedNotificationTemplate)
    let message = T.replace "{#cust-id#}" (getId personId) msgTemplate
    _ <- SF.postMessage message
    throwError $ HitsLimitError limitResetTimeInSec

searchHitsCountKey :: Id Person.Person -> Text
searchHitsCountKey personId = "BAP:Ride:search:" <> getId personId <> ":hitsCount"

updateVersions :: EsqDBFlow m r => Id Person.Person -> Maybe Version -> Maybe Version -> m ()
updateVersions personId mbBundleVersion mbClientVersion = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  DB.runTransaction $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
