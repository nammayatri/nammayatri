module SharedLogic.Search
  ( checkSearchRateLimit,
    getRouteInfoWithShortestDuration,
    searchHitsCountKey,
    getLongestRouteDistance,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Person as Person
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified Tools.Maps as Maps
import Tools.Metrics

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

getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteresult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteresult
getRouteInfoWithShortestDuration [] = Nothing

getLongestRouteDistance :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getLongestRouteDistance [] = Nothing
getLongestRouteDistance (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteresult <- getLongestRouteDistance routeInfoArray
      Just $ comparator' routeInfo restRouteresult
  where
    comparator' route1 route2 =
      if route1.distance > route2.distance
        then route1
        else route2

comparator :: Maps.RouteInfo -> Maps.RouteInfo -> Maps.RouteInfo
comparator route1 route2 =
  if route1.duration < route2.duration
    then route1
    else route2
