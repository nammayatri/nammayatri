module ExternalBPP.ExternalAPI.Subway.CRIS.RouteFareV3 where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import qualified Domain.Types.FRFSQuote as Quote
import Domain.Types.FRFSQuoteCategoryType
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
  "t" :> "uts.cris.in" :> "VCU" :> "1" :> "get_route_fare_details_v3"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] EncryptedResponse

mkRouteFareKey :: Text -> Text -> Text -> Text
mkRouteFareKey startStopCode endStopCode searchReqId = "CRIS:" <> searchReqId <> "-" <> startStopCode <> "-" <> endStopCode

mkRouteFareCacheKey :: Text -> Text -> Text -> Bool -> Text
mkRouteFareCacheKey startStopCode endStopCode changeOver getAllFares = "CRIS:" <> startStopCode <> "-" <> endStopCode <> "-" <> changeOver <> "-" <> T.pack (show getAllFares)

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
  Bool ->
  Bool ->
  m ([FRFSUtils.FRFSFare], Maybe Text)
getRouteFare config merchantOperatingCityId request useCache getAllFares = do
  let redisKey = mkRouteFareCacheKey request.sourceCode request.destCode request.changeOver getAllFares
  redisResp <- Hedis.safeGet redisKey
  case (redisResp, useCache) of
    (Just fares, True) -> do
      logDebug $ "[V3] getCachedFaresAndRecache: fares found in cache" <> show fares
      fork "[V3] fetchAndCacheFares for suburban" $ void fetchAndCacheFares
      return (fares, Nothing)
    (_, _) -> do
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

      logInfo $ "[V3] getRouteFare Req: " <> jsonStr

      encryptionKey <- decrypt config.encryptionKey
      decryptionKey <- decrypt config.decryptionKey
      payload <- encryptPayload jsonStr encryptionKey

      let eulerClientFn payload' token =
            let client = ET.client routeFareAPI
             in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") payload'
      encryptedResponse <- callCRISAPI config routeFareAPI (eulerClientFn payload) "getRouteFare"

      logInfo $ "[V3] getRouteFare Resp: " <> show encryptedResponse

      -- Fix the encoding chain
      decryptedResponse :: CRISFareResponse <- case eitherDecode (encode encryptedResponse) of
        Left err -> throwError (CRISError $ "[V3] Failed to parse encrypted getRouteFare Resp: " <> T.pack (show err))
        Right encResp -> do
          logInfo $ "[V3] getRouteFare Resp Code: " <> responseCode encResp
          if encResp.responseCode == "0"
            then do
              case decryptResponseData (responseData encResp) decryptionKey of
                Left err -> throwError (CRISError $ "[V3] Failed to decrypt getRouteFare Resp: " <> T.pack err)
                Right decryptedJson -> do
                  logInfo $ "[V3] getRouteFare Decrypted Resp: " <> decryptedJson
                  case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
                    Left err -> throwError (CRISError $ "[V3] Failed to decode getRouteFare Resp: " <> T.pack (show err))
                    Right fareResponse -> pure fareResponse
            else throwError (CRISError $ "[V3] Non-zero response code in getRouteFare Resp: " <> encResp.responseCode <> " " <> encResp.responseData)
      let routeFareDetails = decryptedResponse.routeFareDetailsList

      frfsDetails <-
        routeFareDetails `forM` \routeFareDetail -> do
          let allFares = routeFareDetail.fareDtlsList
          let routeId = routeFareDetail.routeId
          let validStations = T.splitOn "-" request.via
          let fares' = if getAllFares then allFares else filter (\fare -> fare.via `Kernel.Prelude.elem` validStations) allFares
          let fares'' = filter (\fare -> fare.via == " ") allFares
          let fares =
                if request.changeOver == " "
                  then if null fares'' || getAllFares then fares' else fares''
                  else filter (\fare -> fare.via == request.changeOver) allFares
          fares `forM` \fare -> do
            let mbFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.adultFare
                mbChildFareAmount = readMaybe @HighPrecMoney . T.unpack $ fare.childFare
            fareAmount <- mbFareAmount & fromMaybeM (CRISError $ "[V3] Failed to parse fare amount: " <> show fare.adultFare)
            childFareAmount <- mbChildFareAmount & fromMaybeM (CRISError $ "[V3] Failed to parse fare amount: " <> show fare.childFare)
            classCode <- pure fare.classCode & fromMaybeM (CRISError $ "[V3] Failed to parse class code: " <> show fare.classCode)
            serviceTiers <- QFRFSVehicleServiceTier.findByProviderCode classCode merchantOperatingCityId
            serviceTier <- serviceTiers & listToMaybe & fromMaybeM (CRISError $ "[V3] Failed to find service tier: " <> show classCode)
            return $
              FRFSUtils.FRFSFare
                { categories =
                    [ FRFSUtils.FRFSTicketCategory
                        { category = ADULT,
                          code = "ADULT",
                          title = "Adult General Ticket",
                          description = "Adult General Ticket",
                          tnc = "Terms and conditions apply for adult general ticket",
                          price =
                            Price
                              { amountInt = round fareAmount,
                                amount = fareAmount,
                                currency = INR
                              },
                          offeredPrice =
                            Price
                              { amountInt = round fareAmount,
                                amount = fareAmount,
                                currency = INR
                              },
                          eligibility = True
                        },
                      FRFSUtils.FRFSTicketCategory
                        { category = CHILD,
                          code = "CHILD",
                          title = "Child General Ticket",
                          description = "Child General Ticket",
                          tnc = "Terms and conditions apply for child general ticket",
                          price =
                            Price
                              { amountInt = round childFareAmount,
                                amount = childFareAmount,
                                currency = INR
                              },
                          offeredPrice =
                            Price
                              { amountInt = round childFareAmount,
                                amount = childFareAmount,
                                currency = INR
                              },
                          eligibility = True
                        }
                    ],
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
      let fareCacheKey = mkRouteFareCacheKey request.sourceCode request.destCode request.changeOver getAllFares
      let fares = concat frfsDetails
      Hedis.setExp fareCacheKey fares 3600
      return $ (fares, decryptedResponse.sdkData)

routeFareAPI :: Proxy RouteFareAPI
routeFareAPI = Proxy
