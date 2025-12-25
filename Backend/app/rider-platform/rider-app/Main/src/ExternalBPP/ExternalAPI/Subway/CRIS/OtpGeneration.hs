module ExternalBPP.ExternalAPI.Subway.CRIS.OtpGeneration where

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

data OtpGenerationResponse = OtpGenerationResponse
  { respCode :: Int,
    respMessage :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- API type using encrypted types
type OtpGenerationAPI =
  "t" :> "uts.cris.in" :> "VBCU" :> "1" :> "generate-otp"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[JSON] EncryptedRequest
    :> Post '[JSON] OtpGenerationResponse

otpGenerationAPI :: Proxy OtpGenerationAPI
otpGenerationAPI = Proxy

-- Business logic types
data CRISOtpGenerationRequest = CRISOtpGenerationRequest
  { mob :: Text,
    imei :: Text,
    otpType :: Int,
    osType :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Main function that handles business logic
generateOtp ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  CRISOtpGenerationRequest ->
  m OtpGenerationResponse
generateOtp config request = do
  logInfo $ "OTP generation request object: " <> show request

  -- 1. Construct and encrypt the request
  let jsonStr = constructOtpJson request
  encryptionKey <- decrypt config.encryptionKey -- Decrypt the key first
  encryptedPayload <- encryptPayload jsonStr encryptionKey

  -- 2. Make API call with encrypted request
  let encReq =
        EncryptedRequest
          { app = config.appCode,
            data_ = encryptedPayload
          }

  -- 3. Make API call and return response
  callCRISAPI config otpGenerationAPI (eulerClientFn encReq) "generateOtp"
  where
    eulerClientFn encReq token =
      let client = ET.client otpGenerationAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") encReq

-- Helper function to construct JSON string
constructOtpJson :: CRISOtpGenerationRequest -> Text
constructOtpJson request = do
  let otpRequest =
        object
          [ "mob" .= request.mob,
            "imei" .= request.imei,
            "otpType" .= request.otpType,
            "osType" .= request.osType
          ]
  decodeUtf8 $ LBS.toStrict $ encode otpRequest
