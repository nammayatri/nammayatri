{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Slack.Flow where

import Data.Maybe
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common hiding (Error)
import Lib.Slack.Types
import Servant hiding (throwError)
import Servant.Client

-- | Slack API interface
type SlackConnectAPI =
  Header "Authorization" Text
    :> ReqBody '[JSON] SlackRequest
    :> Post '[JSON] SlackResponse

slackConnectAPI :: Proxy SlackConnectAPI
slackConnectAPI = Proxy

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "slack.com",
      baseUrlPort = 443,
      baseUrlPath = T.unpack "/api/chat.postMessage"
    }

postMessage ::
  ( CoreMetrics m,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig]
  ) =>
  T.Text ->
  m SlackResponse
postMessage message = do
  withLogTag "Slack" $ do
    SlackConfig {..} <- asks (.slackCfg)
    let slackRequest =
          SlackRequest
            { channel = channelName,
              blocks = Just [Block {_type = "section", _text = Block {_type = "plain_text", _text = message}}]
            }
    callSlackAPI
      defaultBaseUrl
      (callSlack slackToken slackRequest)
      "PostMessage"
  where
    callSlack token slackRequest = ET.client slackConnectAPI (Just $ "Bearer " <> token) slackRequest

callSlackAPI :: CallAPI env a
callSlackAPI =
  callApiUnwrappingApiError
    (identity @Error)
    Nothing
    Nothing
