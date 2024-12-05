module SharedLogic.Search where

import API.Types.UI.Search
import qualified BecknV2.OnDemand.Tags as Beckn
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Version
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Person as QP
import qualified Tools.Maps as Maps

data SearchRes = SearchRes
  { origin :: SearchReqLocation,
    stops :: [SearchReqLocation],
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    riderPreferredOption :: SearchRequest.RiderPreferredOption,
    roundTrip :: Bool,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    merchant :: DM.Merchant,
    city :: City,
    device :: Maybe Text,
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    isReallocationEnabled :: Maybe Bool,
    multipleRoutes :: Maybe [Maps.RouteInfo],
    taggings :: Maybe Beckn.Taggings,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }

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

updateVersions :: (CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["version" ::: DeploymentVersion]) => Id Person.Person -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> m ()
updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  deploymentVersion <- asks (.version)
  void $ QP.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion
