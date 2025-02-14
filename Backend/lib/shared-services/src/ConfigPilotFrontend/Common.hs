{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ConfigPilotFrontend.Common
  ( ConfigPilotFrontendReq (..),
    ConfigPilotFrontendRes (..),
    ConfigPilotFrontendConfig (..),
    callConfigPilotFrontend,
  )
where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

data ConfigPilotFrontendReq = ConfigPilotFrontendReq
  { domain :: Text,
    experiment :: Bool
  }
  deriving (Generic, Read, Show, FromJSON, ToJSON)

newtype ConfigPilotFrontendRes = ConfigPilotFrontendRes
  { message :: Text
  }
  deriving (Generic, Read, Show, FromJSON, ToJSON)

type ConfigPilotFrontendAPI =
  "internal" :> "ConfigPilotFrontend"
    :> Header "x-api-key" Text
    :> ReqBody '[JSON] ConfigPilotFrontendReq
    :> Post '[JSON] ConfigPilotFrontendRes

configPilotFrontendAPI :: Proxy ConfigPilotFrontendAPI
configPilotFrontendAPI = Proxy

configPilotFrontendClient :: Maybe Text -> ConfigPilotFrontendReq -> ET.EulerClient ConfigPilotFrontendRes
configPilotFrontendClient = ET.client configPilotFrontendAPI

data ConfigPilotFrontendConfig = ConfigPilotFrontendConfig
  { url :: BaseUrl,
    apiKey :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

callConfigPilotFrontend ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ConfigPilotFrontendReq ->
  ConfigPilotFrontendConfig ->
  m ConfigPilotFrontendRes
callConfigPilotFrontend req cfg = do
  callAPI cfg.url (configPilotFrontendClient (Just cfg.apiKey) req) "ConfigPilotFrontend" configPilotFrontendAPI
    >>= fromEitherM (ExternalAPICallError (Just "CALL_TO_URL_SHORTNER_FAILED") cfg.url)
