module ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare where

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
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISError (..))
import ExternalBPP.ExternalAPI.Subway.CRIS.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.API
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier

-- API type with updated endpoint
type RouteFareAPI =
  "t" :> "uts.cris.in" :> "VCU" :> "1" :> "get_route_fare_details_v4"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] EncryptedResponse

mkRouteFareKey :: Text -> Text -> Text -> Text
mkRouteFareKey startStopCode endStopCode searchReqId = "CRIS:" <> searchReqId <> "-" <> startStopCode <> "-" <> endStopCode

mkRouteFareCacheKey :: Text -> Text -> Text -> Text
mkRouteFareCacheKey startStopCode endStopCode changeOver = "CRIS:" <> startStopCode <> "-" <> endStopCode <> "-" <> changeOver

getCachedFaresAndRecache ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  CRISFareRequest ->
  m CRISFareResponse
getCachedFaresAndRecache config request = do
  let redisKey = mkRouteFareCacheKey request.sourceCode request.destCode request.changeOver
  redisResp <- Hedis.safeGet redisKey
  case redisResp of
    Just fares -> do
      fork "fetchAndCacheFares for suburban" $ void fetchAndCacheFares
      return fares
    Nothing -> do
      fetchAndCacheFares
  where
    fetchAndCacheFares = do
      let typeOfBooking :: Int = 0
      let mobileNo = request.mobileNo >>= (readMaybe @Int . T.unpack)
      let fareRequest =
            object
              [ "tpAccountId" .= (config.tpAccountId :: Int), -- Explicitly mark as Int
                "mobileNo" .= (fromMaybe 9999999999 mobileNo),
                "imeiNo" .= request.imeiNo,
                "appCode" .= config.appCode,
                "appSession" .= request.appSession,
                "sourceZone" .= config.sourceZone,
                "sourceCode" .= sourceCode request,
                "changeOver" .= request.changeOver,
                "destCode" .= request.destCode,
                "ticketType" .= config.ticketType,
                "via" .= request.via,
                "typeOfBooking" .= typeOfBooking
              ]
      let jsonStr = decodeUtf8 $ LBS.toStrict $ encode fareRequest

      logInfo $ "getRouteFare Req: " <> jsonStr

      encryptionKey <- decrypt config.encryptionKey
      decryptionKey <- decrypt config.decryptionKey
      payload <- encryptPayload jsonStr encryptionKey

      let eulerClientFn payload' token =
            let client = ET.client routeFareAPI
             in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") payload'
      encryptedResponse <- callCRISAPI config routeFareAPI (eulerClientFn payload) "getRouteFare"

      logInfo $ "getRouteFare Resp: " <> show encryptedResponse

      -- Fix the encoding chain
      decryptedResponse :: CRISFareResponse <- case eitherDecode (encode encryptedResponse) of
        Left err -> throwError (CRISError $ "Failed to parse encrypted getRouteFare Resp: " <> T.pack (show err))
        Right encResp -> do
          logInfo $ "getRouteFare Resp Code: " <> responseCode encResp
          if encResp.responseCode == "0"
            then do
              case decryptResponseData (responseData encResp) decryptionKey of
                Left err -> throwError (CRISError $ "Failed to decrypt getRouteFare Resp: " <> T.pack err)
                Right decryptedJson -> do
                  logInfo $ "getRouteFare Decrypted Resp: " <> decryptedJson
                  case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
                    Left err -> throwError (CRISError $ "Failed to decode getRouteFare Resp: " <> T.pack (show err))
                    Right fareResponse -> pure fareResponse
            else throwError (CRISError $ "Non-zero response code in getRouteFare Resp: " <> encResp.responseCode <> " " <> encResp.responseData)
      let fareCacheKey = mkRouteFareCacheKey request.sourceCode request.destCode request.changeOver
      Hedis.setExp fareCacheKey decryptedResponse 3600
      return decryptedResponse

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
  decryptedResponse <- getCachedFaresAndRecache config request

  let routeFareDetails = decryptedResponse.routeFareDetailsList

  frfsDetails <-
    routeFareDetails `forM` \routeFareDetail -> do
      let allFares = routeFareDetail.fareDtlsList
      let routeId = routeFareDetail.routeId
      let onlySelectedViaFares = filter (\fare -> fare.via == request.changeOver) allFares
      let fares = if null onlySelectedViaFares || request.changeOver == " " then allFares else onlySelectedViaFares
      fares `forM` \fare -> do
        let mbFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.adultFare
            mbChildFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.childFare
        fareAmount <- mbFareAmount & fromMaybeM (CRISError $ "Failed to parse fare amount: " <> show fare.adultFare)
        childFareAmount <- mbChildFareAmount & fromMaybeM (CRISError $ "Failed to parse fare amount: " <> show fare.childFare)
        classCode <- pure fare.classCode & fromMaybeM (CRISError $ "Failed to parse class code: " <> show fare.classCode)
        serviceTiers <- QFRFSVehicleServiceTier.findByProviderCode classCode merchantOperatingCityId
        serviceTier <- serviceTiers & listToMaybe & fromMaybeM (CRISError $ "Failed to find service tier: " <> show classCode)
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
              categories = [],
              farePolicyId = Nothing,
              fareDetails =
                Just
                  Quote.FRFSFareDetails
                    { providerRouteId = show routeId,
                      distance = kilometersToMeters $ Kilometers fare.distance,
                      via = fare.via,
                      ticketTypeCode = fare.ticketTypeCode,
                      trainTypeCode = fare.trainTypeCode,
                      appSession = request.appSession
                    },
              vehicleServiceTier =
                FRFSUtils.FRFSVehicleServiceTier
                  { serviceTierType = serviceTier._type,
                    serviceTierProviderCode = serviceTier.providerCode,
                    serviceTierShortName = serviceTier.shortName,
                    serviceTierDescription = serviceTier.description,
                    serviceTierLongName = serviceTier.longName,
                    isAirConditioned = serviceTier.isAirConditioned
                  }
            }
  return $ concat frfsDetails

routeFareAPI :: Proxy RouteFareAPI
routeFareAPI = Proxy
