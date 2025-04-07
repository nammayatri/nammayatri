module ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import Domain.Types.MerchantOperatingCity
import EulerHS.Prelude hiding (find, readMaybe)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData, encryptPayload)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.API
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier

data EncryptedResponse = EncryptedResponse
  { responseCode :: Text,
    responseData :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data CRISFareRequest = CRISFareRequest
  { mobileNo :: Text,
    imeiNo :: Text,
    appSession :: Int,
    sourceCode :: Text,
    changeOver :: Text,
    -- | "1 blank space"
    destCode :: Text,
    ticketType :: Text,
    via :: Text,
    trainType :: Maybe Text,
    routeId :: Text
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
    maximumValuesList :: Maybe [MaximumValues],
    allowedValuesList :: Maybe [AllowedValues]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data FareDetails = FareDetails
  { ticketFare :: Text,
    distance :: Maybe Int,
    ticketTypeCode :: Maybe Text,
    trainTypeCode :: Maybe Text,
    classCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data MaximumValues = MaximumValues
  { item :: Maybe Text,
    value :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data AllowedValues = AllowedValues
  { ticketTypeCode :: Maybe Text,
    ticketTypeName :: Maybe Text,
    trainTypeCode :: Maybe Text,
    trainTypeDescription :: Maybe Text,
    classCode :: Maybe Text
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
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  Id MerchantOperatingCity ->
  CRISFareRequest ->
  m [FRFSUtils.FRFSFare]
getRouteFare config merchantOperatingCityId request = do
  logInfo $ "Request object: " <> show request
  let clusterId :: Int = -1
      suburbanFlag :: Int = 0
      comboFlag :: Int = 0
      cacheFlag :: Int = 0
      modeOfBook :: Int = 2
  let jsonStr =
        "{"
          <> "\"tpAccountId\":\""
          <> show config.tpAccountId
          <> "\","
          <> "\"mobileNo\":\""
          <> mobileNo request
          <> "\","
          <> "\"imeiNo\":\""
          <> imeiNo request
          <> "\","
          <> "\"appCode\":\""
          <> config.appCode
          <> "\","
          <> "\"appSession\":\""
          <> show (appSession request)
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
          <> config.sourceZone
          <> "\","
          <> "\"via\":\""
          <> via request
          <> "\","
          <> "\"trainType\":"
          <> maybe "null" (\t -> "\"" <> t <> "\"") (trainType request)
          <> ","
          <> "\"routeId\":"
          <> request.routeId
          <> ","
          <> "\"clusterId\":"
          <> show clusterId
          <> ","
          <> "\"suburbanFlag\":"
          <> show suburbanFlag
          <> ","
          <> "\"comboFlag\":"
          <> show comboFlag
          <> ","
          <> "\"cacheFlag\":"
          <> show cacheFlag
          <> ","
          <> "\"modeOfBook\":"
          <> show modeOfBook
          <> "}"

  clientKey <- decrypt config.clientKey
  payload <- encryptPayload jsonStr clientKey

  encryptedResponse <- callCRISAPI config routeFareAPI (eulerClientFn payload) "getRouteFare"

  logInfo $ "Encrypted response: " <> show encryptedResponse

  -- Fix the encoding chain
  decryptedResponse :: CRISFareResponse <- case eitherDecode (encode encryptedResponse) of
    Left err -> throwError (InternalError $ "Failed to parse encrypted response: " <> T.pack (show err))
    Right encResp -> do
      logInfo $ "Got response code: " <> responseCode encResp
      logInfo $ "Encrypted response data length: " <> show (T.length $ responseData encResp)

      case decryptResponseData (responseData encResp) clientKey of
        Left err -> throwError (InternalError $ "Failed to decrypt response: " <> T.pack err)
        Right decryptedJson -> do
          logInfo $ "Decrypted JSON: " <> decryptedJson
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
            Left err -> throwError (InternalError $ "Failed to parse decrypted JSON: " <> T.pack (show err))
            Right fareResponse -> pure fareResponse
  let firstRouteFareDetails = listToMaybe decryptedResponse.routeFareDetailsList
  logInfo $ "FRFS Subway Fare: " <> show firstRouteFareDetails
  let fares = maybe [] (.fareDtlsList) firstRouteFareDetails
  fares `forM` \fare -> do
    let mbFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.ticketFare
    fareAmount <- mbFareAmount & fromMaybeM (InternalError $ "Failed to parse fare amount: " <> show fare.ticketFare)
    classCode <- fare.classCode & fromMaybeM (InternalError $ "Failed to parse class code: " <> show fare.classCode)
    serviceTiers <- QFRFSVehicleServiceTier.findByProviderCode classCode merchantOperatingCityId
    serviceTier <- serviceTiers & listToMaybe & fromMaybeM (InternalError $ "Failed to find service tier: " <> show classCode)
    return $
      FRFSUtils.FRFSFare
        { price =
            Price
              { amountInt = round fareAmount,
                amount = fareAmount,
                currency = INR
              },
          discounts = [],
          vehicleServiceTier =
            FRFSUtils.FRFSVehicleServiceTier
              { serviceTierType = serviceTier._type,
                serviceTierProviderCode = serviceTier.providerCode,
                serviceTierShortName = serviceTier.shortName,
                serviceTierDescription = serviceTier.description,
                serviceTierLongName = serviceTier.longName
              }
        }
  where
    eulerClientFn payload token =
      let client = ET.client routeFareAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") payload

-- sampleRequest :: CRISFareRequest
-- sampleRequest =
--   CRISFareRequest
--     { mobileNo = 9962013253,
--       imeiNo = "ed409d8d764c04f7",
--       appSession = "10",
--       sourceCode = "PER",
--       changeOver = " ",
--       destCode = "TBM",
--       ticketType = "J",
--       via = " ",
--       trainType = Nothing,
--       routeId = 0
--     }
