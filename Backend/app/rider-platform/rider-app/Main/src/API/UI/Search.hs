{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

-- import qualified Beckn.ACL.Metro.Search as MetroACL

import qualified Beckn.ACL.Search as TaxiACL
import Data.Aeson
-- import qualified SharedLogic.MerchantConfig as SMC

import qualified Data.HashMap as HM
import Data.OpenApi hiding (Header)
import qualified Data.OpenApi as OpenApi hiding (Header)
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Domain.Types.SearchRequest (SearchRequest)
import Environment
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
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
import Lib.SessionizerMetrics.Types.Event
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.PublicTransport as PublicTransport
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Person as Person
import Tools.Auth
import qualified Tools.JSON as J
import qualified Tools.Maps as Maps
import Tools.Metrics

-------- Search Flow --------

type API =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Servant.Header "x-bundle-version" Version
    :> Servant.Header "x-client-version" Version
    :> Header "x-device" Text
    :> Post '[JSON] SearchRes

handler :: FlowServer API
handler = search

data SearchReq = OneWaySearch DOneWaySearch.OneWaySearchReq | RentalSearch DRentalSearch.RentalSearchReq
  deriving (Generic, Show)

instance ToJSON SearchReq where
  toJSON = genericToJSON fareProductOptions

instance FromJSON SearchReq where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema SearchReq where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = J.fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductSchemaOptions :: OpenApi.SchemaOptions
fareProductSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = J.fareProductTaggedObject,
      OpenApi.constructorTagModifier = fareProductConstructorModifier
    }

data SearchRes = SearchRes
  { searchId :: Id SearchRequest,
    searchExpiry :: UTCTime,
    routeInfo :: Maybe Maps.RouteInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  x -> x

search :: (Id Person.Person, Id Merchant.Merchant) -> SearchReq -> Maybe Version -> Maybe Version -> Maybe Text -> FlowHandler SearchRes
search (personId, _) req mbBundleVersion mbClientVersion mbDevice = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  updateVersions personId mbBundleVersion mbClientVersion
  (searchId, searchExpiry, routeInfo) <- case req of
    OneWaySearch oneWay -> oneWaySearch personId mbBundleVersion mbClientVersion mbDevice oneWay
    RentalSearch rental -> rentalSearch personId mbBundleVersion mbClientVersion mbDevice rental
  return $ SearchRes searchId searchExpiry routeInfo

oneWaySearch ::
  ( EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r ["searchRequestExpiry" ::: Maybe Seconds, "nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasBAPMetrics m r,
    MonadProducer PublicTransportSearch m,
    EventStreamFlow m r
  ) =>
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  DOneWaySearch.OneWaySearchReq ->
  m (Id SearchRequest, UTCTime, Maybe Maps.RouteInfo)
oneWaySearch personId bundleVersion clientVersion device req = do
  dSearchRes <- DOneWaySearch.oneWaySearch personId req bundleVersion clientVersion device
  fork "search cabs" . withShortRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchRes
    becknTaxiReqV2 <- TaxiACL.buildOneWaySearchReqV2 dSearchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.search dSearchRes.gatewayUrl becknTaxiReq
  -- fork "search metro" . withShortRetry $ do
  --   becknMetroReq <- MetroACL.buildSearchReq dSearchRes
  --   CallBPP.searchMetro dSearchRes.gatewayUrl becknMetroReq
  fork "search public-transport" $ PublicTransport.sendPublicTransportSearchRequest personId dSearchRes
  return (dSearchRes.searchId, dSearchRes.searchRequestExpiry, dSearchRes.shortestRouteInfo)

rentalSearch ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r ["searchRequestExpiry" ::: Maybe Seconds, "nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
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
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text]
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

updateVersions :: (CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> Maybe Version -> Maybe Version -> m ()
updateVersions personId mbBundleVersion mbClientVersion = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  -- DB.runTransaction $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
  void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
