{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Search
  ( DSearch.SearchReq (..),
    DSearch.SearchRes (..),
    DSearch.SearchResp (..),
    DSearch.OneWaySearchReq (..),
    DSearch.RentalSearchReq (..),
    DSearch.SearchReqLocation (..),
    API,
    search,
    handler,
  )
where

import qualified Beckn.ACL.Search as TaxiACL
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
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
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Person as Person
import Tools.Auth

-------- Search Flow --------

type API =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] DSearch.SearchReq
    :> Servant.Header "x-bundle-version" Version
    :> Servant.Header "x-client-version" Version
    :> Header "x-device" Text
    :> Post '[JSON] DSearch.SearchResp

handler :: FlowServer API
handler = search

search :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Version -> Maybe Version -> Maybe Text -> FlowHandler DSearch.SearchResp
search (personId, _) req mbBundleVersion mbClientVersion mbDevice = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  updateVersions personId mbBundleVersion mbClientVersion
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbDevice
  fork "search cabs" . withShortRetry $ do
    isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
    if isBecknSpecVersion2
      then do
        becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
        let generatedJson = encode becknTaxiReqV2
        logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
        void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2
      else do
        becknTaxiReq <- TaxiACL.buildSearchReqV1 dSearchRes
        void $ CallBPP.search dSearchRes.gatewayUrl becknTaxiReq
  -- fork "search metro" . withShortRetry $ do
  --   becknMetroReq <- MetroACL.buildSearchReq dSearchRes
  --   CallBPP.searchMetro dSearchRes.gatewayUrl becknMetroReq
  fork "search public-transport" $ PublicTransport.sendPublicTransportSearchRequest personId dSearchRes
  return $ DSearch.SearchResp dSearchRes.searchId dSearchRes.searchRequestExpiry dSearchRes.shortestRouteInfo

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
