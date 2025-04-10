module ExternalBPP.ExternalAPI.Subway.CRIS.RouteFareV1
  ( getRouteFareV1,
    CRISFareRequestV1 (..),
    CRISFareResponseV1 (..),
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import Domain.Types.FRFSSearch as DFRFSSearch
import Domain.Types.MerchantOperatingCity
import EulerHS.Prelude hiding (find, readMaybe, whenJust)
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
import Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier

data EncryptedResponse = EncryptedResponse
  { responseCode :: Text,
    responseData :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- Request type with updated fields
data CRISFareRequestV1 = CRISFareRequestV1
  { mobileNo :: Text,
    imeiNo :: Text,
    appSession :: Int,
    sourceCode :: Text,
    changeOver :: Text,
    destCode :: Text,
    via :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Response types
data CRISFareResponseV1 = CRISFareResponseV1
  { routeFareDetailsList :: [RouteFareDetails],
    sdkData :: Text -- Added sdkData field
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data RouteFareDetails = RouteFareDetails
  { routeId :: Int,
    fareDtlsList :: [FareDetails],
    maximumValuesList :: [MaximumValues],
    allowedValuesList :: [AllowedValues]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareDetails = FareDetails
  { ticketFare :: Text,
    distance :: Int,
    via :: Text, -- Added via field
    ticketTypeCode :: Text,
    trainTypeCode :: Text,
    classCode :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data MaximumValues = MaximumValues
  { item :: Text,
    value :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AllowedValues = AllowedValues
  { ticketTypeCode :: Text,
    ticketTypeName :: Text,
    trainTypeCode :: Text,
    trainTypeDescription :: Text,
    classCode :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- API type with updated endpoint
type RouteFareAPI =
  "t" :> "uts.cris.in" :> "tputs" :> "V" :> "get_route_fare_details_v1"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[JSON] Text
    :> Post '[JSON] EncryptedResponse

-- Main function
getRouteFareV1 ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  Id MerchantOperatingCity ->
  CRISFareRequestV1 ->
  Maybe (Id DFRFSSearch.FRFSSearch) ->
  m [FRFSUtils.FRFSFare]
getRouteFareV1 config merchantOperatingCityId request mbFrfsSearchId = do
  logInfo $ "Request object: " <> show request
  let typeOfBooking :: Int = 0
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
          <> "\"sourceZone\":\""
          <> config.sourceZone
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
          <> config.ticketType
          <> "\","
          <> "\"via\":\""
          <> request.via
          <> "\","
          <> "\"typeOfBooking\":"
          <> show typeOfBooking
          <> "}"

  clientKey <- decrypt config.clientKey
  payload <- encryptPayload jsonStr clientKey

  encryptedResponse <- callCRISAPI config routeFareAPI (eulerClientFn payload) "getRouteFare"

  logInfo $ "Encrypted response: " <> show encryptedResponse

  -- Fix the encoding chain
  decryptedResponse :: CRISFareResponseV1 <- case eitherDecode (encode encryptedResponse) of
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

  whenJust mbFrfsSearchId $ \frfsSeacrhId -> QFRFSSearch.updateSdkTokenById frfsSeacrhId (Just decryptedResponse.sdkData)

  let firstRouteFareDetails = listToMaybe decryptedResponse.routeFareDetailsList
  logInfo $ "FRFS Subway Fare: " <> show firstRouteFareDetails
  let fares = maybe [] (.fareDtlsList) firstRouteFareDetails
  fares `forM` \fare -> do
    let mbFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.ticketFare
    fareAmount <- mbFareAmount & fromMaybeM (InternalError $ "Failed to parse fare amount: " <> show fare.ticketFare)
    classCode <- pure fare.classCode & fromMaybeM (InternalError $ "Failed to parse class code: " <> show fare.classCode)
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

routeFareAPI :: Proxy RouteFareAPI
routeFareAPI = Proxy
