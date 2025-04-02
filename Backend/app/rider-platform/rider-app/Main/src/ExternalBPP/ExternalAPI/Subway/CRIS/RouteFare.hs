module ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare where

-- import qualified Crypto.Cipher.AES as AES
-- import qualified Crypto.Cipher.Types as CT
-- import qualified Crypto.Error as CE
import Data.Aeson
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Encoding as TLE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData, encryptPayload)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.API

data EncryptedResponse = EncryptedResponse
  { responseCode :: Text,
    responseData :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data CRISFareRequest = CRISFareRequest
  { -- | mandatory
    tpAccountId :: Int,
    -- | mandatory
    mobileNo :: Int,
    -- | mandatory
    imeiNo :: Text,
    -- | mandatory (Already Shared)
    appCode :: Text,
    -- | mandatory
    appSession :: Text,
    -- | mandatory
    sourceCode :: Text,
    -- | "1 blank space"
    changeOver :: Text,
    -- | mandatory
    destCode :: Text,
    -- | mandatory
    ticketType :: Text,
    -- | mandatory
    sourceZone :: Text,
    -- | "1 blank space"
    via :: Text,
    -- New fields
    trainType :: Maybe Text,
    routeId :: Int,
    clusterId :: Int,
    suburbanFlag :: Int,
    comboFlag :: Int,
    cacheFlag :: Int,
    modeOfBook :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data RouteFareRes = RouteFareRes
  { fare :: Double,
    distance :: Double,
    currency :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

type RouteFareAPI =
  "t" :> "uts.cris.in" :> "tputs" :> "V" :> "get_route_fare_details"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] EncryptedResponse

routeFareAPI :: Proxy RouteFareAPI
routeFareAPI = Proxy

-- Helper function to transform numbers to strings in JSON
transformNumbers :: Value -> Value
transformNumbers (Object obj) = Object $ fmap transformNumbers obj
transformNumbers (Array array) = Array $ fmap transformNumbers array
transformNumbers (Number n) = String $ T.pack $ show n -- Convert numbers to strings
transformNumbers other = other

-- Update CRISFareResponse to match the exact format
data CRISFareResponse = CRISFareResponse
  { routeFareDetailsList :: [RouteFareDetails]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- RouteFareDetails is already correct with its fields
data RouteFareDetails = RouteFareDetails
  { routeId :: Int,
    fareDtlsList :: [FareDetails],
    maximumValuesList :: [MaximumValues],
    allowedValuesList :: [AllowedValues]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data FareDetails = FareDetails
  { ticketFare :: Text,
    distance :: Int,
    ticketTypeCode :: Text,
    trainTypeCode :: Text,
    classCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data MaximumValues = MaximumValues
  { item :: Text,
    value :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data AllowedValues = AllowedValues
  { ticketTypeCode :: Text,
    ticketTypeName :: Text,
    trainTypeCode :: Text,
    trainTypeDescription :: Text,
    classCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data DecryptedResponse = DecryptedResponse
  { routeFareDetailsList :: [RouteFareDetails]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- Update getRouteFare to handle this format
getRouteFare ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  CRISFareRequest ->
  m CRISFareResponse
getRouteFare config request = do
  logInfo $ "Request object: " <> show request

  let jsonStr =
        "{"
          <> "\"tpAccountId\":\""
          <> show (tpAccountId request)
          <> "\","
          <> "\"mobileNo\":\""
          <> show (mobileNo request)
          <> "\","
          <> "\"imeiNo\":\""
          <> imeiNo request
          <> "\","
          <> "\"appCode\":\""
          <> appCode request
          <> "\","
          <> "\"appSession\":\""
          <> appSession request
          <> "\","
          <> "\"sourceCode\":\""
          <> sourceCode request
          <> "\","
          <> "\"changeOver\":\""
          <> changeOver request
          <> "\","
          <> "\"destCode\":\""
          <> destCode request
          <> "\","
          <> "\"ticketType\":\""
          <> ticketType request
          <> "\","
          <> "\"sourceZone\":\""
          <> sourceZone request
          <> "\","
          <> "\"via\":\""
          <> via request
          <> "\","
          <> "\"trainType\":"
          <> maybe "null" (\t -> "\"" <> t <> "\"") (trainType request)
          <> ","
          <> "\"routeId\":"
          <> show (request.routeId)
          <> ","
          <> "\"clusterId\":"
          <> show (clusterId request)
          <> ","
          <> "\"suburbanFlag\":"
          <> show (suburbanFlag request)
          <> ","
          <> "\"comboFlag\":"
          <> show (comboFlag request)
          <> ","
          <> "\"cacheFlag\":"
          <> show (cacheFlag request)
          <> ","
          <> "\"modeOfBook\":"
          <> show (modeOfBook request)
          <> "}"

  payload <- encryptPayload jsonStr config

  encryptedResponse <- callCRISAPI config routeFareAPI (eulerClientFn payload) "getRouteFare"

  logInfo $ "Encrypted response: " <> show encryptedResponse

  -- Fix the encoding chain
  case eitherDecode (encode encryptedResponse) of
    Left err -> throwError (InternalError $ "Failed to parse encrypted response: " <> T.pack (show err))
    Right encResp -> do
      logInfo $ "Got response code: " <> responseCode encResp
      logInfo $ "Encrypted response data length: " <> show (T.length $ responseData encResp)

      case decryptResponseData (responseData encResp) config of
        Left err -> throwError (InternalError $ "Failed to decrypt response: " <> T.pack err)
        Right decryptedJson -> do
          logInfo $ "Decrypted JSON: " <> decryptedJson
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
            Left err -> throwError (InternalError $ "Failed to parse decrypted JSON: " <> T.pack (show err))
            Right fareResponse -> pure fareResponse
  where
    eulerClientFn payload token =
      let client = ET.client routeFareAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") payload

sampleRequest :: CRISFareRequest
sampleRequest =
  CRISFareRequest
    { tpAccountId = 3700001,
      mobileNo = 9962013253,
      imeiNo = "ed409d8d764c04f7",
      appCode = "CUMTA",
      appSession = "10",
      sourceCode = "PER",
      changeOver = " ",
      destCode = "TBM",
      ticketType = "J",
      sourceZone = "SR",
      via = " ",
      trainType = Nothing,
      routeId = 0,
      clusterId = -1,
      suburbanFlag = 0,
      comboFlag = 0,
      cacheFlag = 0,
      modeOfBook = 2
    }
