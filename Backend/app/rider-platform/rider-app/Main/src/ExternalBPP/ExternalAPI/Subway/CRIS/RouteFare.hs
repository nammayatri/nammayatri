module ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare
  ( getRouteFare,
    CRISFareRequest (..),
    CRISFareResponse (..),
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import qualified Domain.Types.FRFSQuote as Quote
import Domain.Types.MerchantOperatingCity
import EulerHS.Prelude hiding (concatMap, find, null, readMaybe, whenJust)
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

-- Request type with updated fields
data CRISFareRequest = CRISFareRequest
  { mobileNo :: Maybe Text,
    imeiNo :: Text,
    appSession :: Int,
    sourceCode :: Text,
    changeOver :: Text,
    destCode :: Text,
    via :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Response types
data CRISFareResponse = CRISFareResponse
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
  { adultFare :: Text,
    childFare :: Text,
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
  "t" :> "uts.cris.in" :> "VCU" :> "1" :> "get_route_fare_details_v3"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] EncryptedResponse

-- Main function
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
  let typeOfBooking :: Int = 0
  let mobileNo = request.mobileNo >>= (readMaybe @Int . T.unpack)
  let fareRequest =
        object
          [ "tpAccountId" .= (config.tpAccountId :: Int), -- Explicitly mark as Int
            "mobileNo" .= (fromMaybe 9999999999 mobileNo),
            "imeiNo" .= imeiNo request,
            "appCode" .= config.appCode,
            "appSession" .= appSession request,
            "sourceZone" .= config.sourceZone,
            "sourceCode" .= sourceCode request,
            "changeOver" .= changeOver request,
            "destCode" .= destCode request,
            "ticketType" .= config.ticketType,
            "via" .= request.via,
            "typeOfBooking" .= typeOfBooking
          ]
  let jsonStr = decodeUtf8 $ LBS.toStrict $ encode fareRequest

  logInfo $ "JSON string: " <> jsonStr

  encryptionKey <- decrypt config.encryptionKey
  decryptionKey <- decrypt config.decryptionKey
  payload <- encryptPayload jsonStr encryptionKey

  encryptedResponse <- callCRISAPI config routeFareAPI (eulerClientFn payload) "getRouteFare"

  logInfo $ "Encrypted response: " <> show encryptedResponse

  -- Fix the encoding chain
  decryptedResponse :: CRISFareResponse <- case eitherDecode (encode encryptedResponse) of
    Left err -> throwError (InternalError $ "Failed to parse encrypted response: " <> T.pack (show err))
    Right encResp -> do
      logInfo $ "Got response code: " <> responseCode encResp

      if encResp.responseCode == "0"
        then do
          case decryptResponseData (responseData encResp) decryptionKey of
            Left err -> throwError (InternalError $ "Failed to decrypt response: " <> T.pack err)
            Right decryptedJson -> do
              logInfo $ "Decrypted JSON: " <> decryptedJson
              case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
                Left err -> throwError (InternalError $ "Failed to decode decrypted JSON: " <> T.pack (show err))
                Right fareResponse -> pure fareResponse
        else throwError (InternalError $ "Non-zero response code in routeFare: " <> encResp.responseCode <> " " <> encResp.responseData)

  let routeFareDetails = decryptedResponse.routeFareDetailsList

  logInfo $ "FRFS Subway Fare: " <> show routeFareDetails
  frfsDetails <-
    routeFareDetails `forM` \routeFareDetail -> do
      let allFares = routeFareDetail.fareDtlsList
      let routeId = routeFareDetail.routeId
      let onlySelectedViaFares = filter (\fare -> fare.via == request.changeOver) allFares
      let fares = if null onlySelectedViaFares || request.changeOver == " " then allFares else onlySelectedViaFares
      fares `forM` \fare -> do
        let mbFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.adultFare
            mbChildFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.childFare
        fareAmount <- mbFareAmount & fromMaybeM (InternalError $ "Failed to parse fare amount: " <> show fare.adultFare)
        childFareAmount <- mbChildFareAmount & fromMaybeM (InternalError $ "Failed to parse fare amount: " <> show fare.childFare)
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
              childPrice =
                Just $
                  Price
                    { amountInt = round childFareAmount,
                      amount = childFareAmount,
                      currency = INR
                    },
              discounts = [],
              farePolicyId = Nothing,
              fareDetails =
                Just
                  Quote.FRFSFareDetails
                    { providerRouteId = show routeId,
                      distance = Meters fare.distance,
                      via = fare.via,
                      ticketTypeCode = fare.ticketTypeCode,
                      trainTypeCode = fare.trainTypeCode,
                      sdkToken = decryptedResponse.sdkData,
                      appSession = request.appSession
                    },
              vehicleServiceTier =
                FRFSUtils.FRFSVehicleServiceTier
                  { serviceTierType = serviceTier._type,
                    serviceTierProviderCode = serviceTier.providerCode,
                    serviceTierShortName = serviceTier.shortName,
                    serviceTierDescription = serviceTier.description,
                    serviceTierLongName = serviceTier.longName
                  }
            }
  return $ concat frfsDetails
  where
    eulerClientFn payload token =
      let client = ET.client routeFareAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") payload

routeFareAPI :: Proxy RouteFareAPI
routeFareAPI = Proxy
