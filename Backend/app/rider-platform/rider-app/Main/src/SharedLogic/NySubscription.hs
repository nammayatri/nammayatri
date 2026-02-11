{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DuplicateRecordFields #-}

module SharedLogic.NySubscription where

import qualified API.UI.Search as DSearch
import qualified Data.HashMap.Strict as HM
import Domain.Action.UI.Search (SearchRequestFlow)
import Domain.Types.Location as Location
import qualified Domain.Types.NyRegularSubscription as NyRegularSubscription
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Interface as MapsInterface
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude (HasField (..))
import Kernel.Storage.Esqueleto (EsqDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App (BaseUrl, HasFlowEnv, MonadFlow)
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common (CacheFlow, type (:::))
import Kernel.Utils.Error
import Kernel.Utils.Servant.Client (RetryCfg)
import SharedLogic.Search (OneWaySearchReq (..), SearchReq (..), SearchReqLocation (..))
import Tools.Error
import TransactionLogs.Types

triggerSubscriptionSearch ::
  ( SearchRequestFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    Redis.HedisFlow m r,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  NyRegularSubscription.NyRegularSubscription ->
  m (Id DSearchReq.SearchRequest)
triggerSubscriptionSearch subscription = do
  let oneWaySearchReq =
        OneWaySearchReq
          { origin = toSearchReqLocation subscription.pickupLocation,
            destination = Just $ toSearchReqLocation subscription.dropoffLocation,
            stops = Nothing,
            isSourceManuallyMoved = Nothing,
            isDestinationManuallyMoved = Nothing,
            isSpecialLocation = Nothing,
            startTime = Nothing,
            isReallocationEnabled = Nothing,
            fareParametersInRateCard = Nothing,
            quotesUnifiedFlow = Nothing,
            sessionToken = Nothing,
            placeNameSource = Nothing,
            driverIdentifier = Nothing,
            isMeterRideSearch = Nothing,
            recentLocationId = Nothing,
            platformType = Nothing,
            isReserveRide = Just True,
            subscriptionId = Just subscription.id,
            verifyBeforeCancellingOldBooking = Just True,
            numberOfLuggages = Nothing,
            doMultimodalSearch = Just False
          }
  merchantId <- subscription.merchantId & fromMaybeM (InternalError $ "no merchant id for subscription id :" <> subscription.id.getId)
  searchResp <-
    DSearch.searchTrigger'
      (subscription.userId, merchantId)
      (OneWaySearch oneWaySearchReq)
      Nothing -- backendConfigVersion
      Nothing -- clientBundleVersion
      Nothing -- clientConfigVersion
      Nothing -- clientDevice
      Nothing -- clientId
      Nothing -- clientSdkVersion
      (Just False)
  return searchResp.searchId

toSearchReqLocation :: Location.Location -> SearchReqLocation
toSearchReqLocation loc =
  SearchReqLocation
    { gps = LatLong loc.lat loc.lon,
      address = loc.address
    }
