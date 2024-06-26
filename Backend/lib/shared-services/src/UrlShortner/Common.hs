{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module UrlShortner.Common
  ( GenerateShortUrlReq (..),
    GenerateShortUrlRes (..),
    UrlShortnerConfig (..),
    generateShortUrl,
  )
where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.JSON
import Servant hiding (throwError)

type GenerateShortUrlAPI =
  "internal" :> "generateShortUrl"
    :> Header "x-api-key" Text
    :> ReqBody '[JSON] GenerateShortUrlReq
    :> Post '[JSON] GenerateShortUrlRes

generateShortUrlAPI :: Proxy GenerateShortUrlAPI
generateShortUrlAPI = Proxy

generateShortUrlClient :: Maybe Text -> GenerateShortUrlReq -> ET.EulerClient GenerateShortUrlRes
generateShortUrlClient = ET.client generateShortUrlAPI

data GenerateShortUrlReq = GenerateShortUrlReq
  { baseUrl :: Text,
    customShortCode :: Maybe Text,
    shortCodeLength :: Maybe Int,
    expiryInHours :: Maybe Int
  }
  deriving (Generic, Read, Show)

instance FromJSON GenerateShortUrlReq where
  parseJSON = genericParseJSON removeNullFields

instance ToJSON GenerateShortUrlReq where
  toJSON = genericToJSON removeNullFields

data GenerateShortUrlRes = GenerateShortUrlRes
  { shortUrl :: Text,
    urlExpiry :: UTCTime
  }
  deriving (Generic, Read, Show)

instance FromJSON GenerateShortUrlRes where
  parseJSON = genericParseJSON removeNullFields

instance ToJSON GenerateShortUrlRes where
  toJSON = genericToJSON removeNullFields

data UrlShortnerConfig = UrlShortnerConfig
  { url :: BaseUrl,
    apiKey :: Text
  }
  deriving (Generic, Show, FromDhall, FromJSON, ToJSON)

generateShortUrl ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortnerConfig]
  ) =>
  GenerateShortUrlReq ->
  m GenerateShortUrlRes
generateShortUrl req = do
  cfg <- asks (.urlShortnerConfig)
  res <-
    callAPI cfg.url (generateShortUrlClient (Just cfg.apiKey) req) "generateShortUrl" generateShortUrlAPI
      >>= fromEitherM (ExternalAPICallError (Just "CALL_TO_URL_SHORTNER_FAILED") cfg.url)
  validateUrl res.shortUrl
  return res
  where
    validateUrl = void . parseBaseUrl
