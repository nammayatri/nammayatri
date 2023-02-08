module API.UI.Search
  ( SearchReq (..),
    SearchRes (..),
    DOneWaySearch.OneWaySearchReq (..),
    DRentalSearch.RentalSearchReq (..),
    DSearchCommon.SearchReqLocation (..),
    API,
    handler,
  )
where

import qualified Core.ACL.Metro.Search as MetroACL
import qualified Core.ACL.Search as TaxiACL
import Data.Aeson
import Data.OpenApi
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Person as Person
import Domain.Types.SearchRequest (SearchRequest)
import Environment
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
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
import qualified SharedLogic.PublicTransport as PublicTransport
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Person as Person
import Tools.Auth
import qualified Tools.JSON as J
import Tools.Metrics

-------- Search Flow --------

type API =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Servant.Header "x-bundle-version" Version
    :> Servant.Header "x-client-version" Version
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
    searchExpiry :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  x -> x

search :: Id Person.Person -> SearchReq -> Maybe Version -> Maybe Version -> FlowHandler SearchRes
search personId req mbBundleVersion mbClientVersion = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  updateVersions personId mbBundleVersion mbClientVersion
  (searchId, searchExpiry) <- case req of
    OneWaySearch oneWay -> oneWaySearch personId oneWay
    RentalSearch rental -> rentalSearch personId rental
  return $ SearchRes searchId searchExpiry

oneWaySearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds, "gatewayUrl" ::: BaseUrl],
    HasBAPMetrics m r,
    MonadProducer PublicTransportSearch m
  ) =>
  Id Person.Person ->
  DOneWaySearch.OneWaySearchReq ->
  m (Id SearchRequest, UTCTime)
oneWaySearch personId req = do
  dSearchRes <- DOneWaySearch.oneWaySearch personId req
  fork "search cabs" . withShortRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchRes
    void $ CallBPP.search dSearchRes.gatewayUrl becknTaxiReq
  fork "search metro" . withShortRetry $ do
    becknMetroReq <- MetroACL.buildSearchReq dSearchRes
    CallBPP.searchMetro becknMetroReq
  fork "search public-transport" $ PublicTransport.sendPublicTransportSearchRequest personId dSearchRes
  return (dSearchRes.searchId, dSearchRes.searchRequestExpiry)

rentalSearch ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds, "gatewayUrl" ::: BaseUrl],
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  DRentalSearch.RentalSearchReq ->
  m (Id SearchRequest, UTCTime)
rentalSearch personId req = do
  dSearchRes <- DRentalSearch.rentalSearch personId req
  fork "search rental" . withShortRetry $ do
    -- do we need fork here?
    becknReq <- TaxiACL.buildRentalSearchReq dSearchRes
    void $ CallBPP.search dSearchRes.gatewayUrl becknReq
  pure (dSearchRes.searchId, dSearchRes.searchRequestExpiry)

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
