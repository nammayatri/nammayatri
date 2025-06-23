module ExternalBPP.ExternalAPI.Subway.CRIS.ChangeDevice where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import EulerHS.Prelude hiding (find, readMaybe)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (encryptPayload)
import Kernel.External.Encryption
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant.API

-- Encrypted request/response types for API
data EncryptedRequest = EncryptedRequest
  { app :: Text,
    data_ :: Text
  }
  deriving (Generic, Show)

instance ToJSON EncryptedRequest where
  toJSON EncryptedRequest {..} =
    object ["app" .= app, "data" .= data_]

data ChangeDeviceResponse = ChangeDeviceResponse
  { respCode :: Int,
    respMessage :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- API type using encrypted types
type ChangeDeviceAPI =
  "t" :> "uts.cris.in" :> "VBCU" :> "1" :> "request-device-change"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[JSON] EncryptedRequest
    :> Post '[JSON] ChangeDeviceResponse

changeDeviceAPI :: Proxy ChangeDeviceAPI
changeDeviceAPI = Proxy

-- Business logic types
data CRISChangeDeviceRequest = CRISChangeDeviceRequest
  { mob :: Text,
    appCode :: Text,
    imei :: Text,
    osType :: Text,
    sessionId :: Int,
    agentAccountId :: Int,
    otp :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Main function that handles business logic
changeDevice ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  CRISChangeDeviceRequest ->
  m ChangeDeviceResponse
changeDevice config request = do
  logInfo $ "Change device request object: " <> show request

  -- 1. Construct and encrypt the request
  let jsonStr = constructChangeDeviceJson request
  encryptionKey <- decrypt config.encryptionKey -- Decrypt the key first
  encryptedPayload <- encryptPayload jsonStr encryptionKey

  -- 2. Make API call with encrypted request
  let encReq =
        EncryptedRequest
          { app = config.appCode,
            data_ = encryptedPayload
          }

  -- 3. Make API call and return response
  callCRISAPI config changeDeviceAPI (eulerClientFn encReq) "changeDevice"
  where
    eulerClientFn encReq token =
      let client = ET.client changeDeviceAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") encReq

-- Helper function to construct JSON string
constructChangeDeviceJson :: CRISChangeDeviceRequest -> Text
constructChangeDeviceJson request = do
  let changeDeviceRequest =
        object
          [ "mob" .= request.mob,
            "appCode" .= request.appCode,
            "imei" .= request.imei,
            "osType" .= request.osType,
            "sessionID" .= request.sessionId,
            "agentAccountID" .= request.agentAccountId,
            "otp" .= request.otp
          ]
  decodeUtf8 $ LBS.toStrict $ encode changeDeviceRequest
