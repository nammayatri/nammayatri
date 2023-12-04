{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.TestMetro.Search where

import qualified Beckn.ACL.Metro.Search as MetroACL
import Data.Aeson
import Data.OpenApi hiding (Header)
import qualified Data.Text as T
import qualified Domain.Action.UI.TestMetro.Search as DMetroSearch
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Domain.Types.SearchRequest (SearchRequest)
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
import qualified Storage.Queries.Person as Person
import Tools.Auth
import Tools.JSON
import qualified Tools.Maps as Maps

type API =
  "test"
    :> "metroSearch"
    :> TokenAuth
    :> ReqBody '[JSON] MSearchReq
    :> Servant.Header "x-bundle-version" Version
    :> Servant.Header "x-client-version" Version
    :> Header "x-device" Text
    :> Post '[JSON] MetroSearchRes

handler :: FlowServer API
handler = metroSearch

newtype MSearchReq = MSearchReq DMetroSearch.MetroSearchReq
  deriving (Generic, Show)

instance ToJSON MSearchReq where
  toJSON = genericToJSON fareProductOptions

instance FromJSON MSearchReq where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema MSearchReq where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductSchemaOptions :: SchemaOptions
fareProductSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

data MetroSearchRes = MetroSearchRes
  { searchId :: Id SearchRequest,
    searchExpiry :: UTCTime,
    routeInfo :: Maybe Maps.RouteInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

metroSearch :: (Id Person.Person, Id Merchant.Merchant) -> MSearchReq -> Maybe Version -> Maybe Version -> Maybe Text -> FlowHandler MetroSearchRes
metroSearch (personId, _) (MSearchReq req) mbBundleVersion mbClientVersion mbDevice = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  updateVersions personId mbBundleVersion mbClientVersion
  dSearchRes <- DMetroSearch.metroSearch personId req mbBundleVersion mbClientVersion mbDevice
  fork "search metro" . withShortRetry $ do
    becknMetroReq <- MetroACL.buildMetroSearchReq dSearchRes
    void $ CallBPP.searchMetro dSearchRes.gatewayUrl becknMetroReq
  pure $ MetroSearchRes dSearchRes.searchId dSearchRes.searchRequestExpiry dSearchRes.shortestRouteInfo

searchHitsCountKey :: Id Person.Person -> Text
searchHitsCountKey personId = "BAP:Ride:search:" <> getId personId <> ":hitsCount"

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

updateVersions :: (CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> Maybe Version -> Maybe Version -> m ()
updateVersions personId mbBundleVersion mbClientVersion = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  -- DB.runTransaction $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
  void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
