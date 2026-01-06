{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.GetFare where

import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Text as T
import Domain.Types.FRFSQuoteCategorySpec
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.Queries.FRFSTicketCategoryMetadataConfig as QFRFSTicketCategoryMetadataConfig

data GetFareReq = GetFareReq
  { operatorNameId :: Int,
    fromStationId :: T.Text,
    toStationId :: T.Text,
    ticketTypeId :: Int,
    merchantId :: T.Text,
    travelDatetime :: T.Text,
    fareTypeId :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GetFareResult = GetFareResult
  { fare :: Maybe HighPrecMoney,
    discountedFare :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GetFareRes = GetFareRes
  { statusCode :: Int,
    message :: T.Text,
    result :: Maybe GetFareResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type GetFareAPI =
  "api" :> "qr" :> "v1" :> "fare" :> "getfare"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] GetFareReq
    :> Post '[JSON] GetFareRes

getFareAPI :: Proxy GetFareAPI
getFareAPI = Proxy

getFare :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, EsqDBFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> CMRLV2Config -> GetFareReq -> m [FRFSUtils.FRFSFare]
getFare integrationBPPConfig config fareReq = do
  logInfo $ "[CMRLV2:GetFare] Getting fare from: " <> fareReq.fromStationId <> " to: " <> fareReq.toStationId
  logDebug $ "[CMRLV2:GetFare] Request params - operatorNameId: " <> show fareReq.operatorNameId <> ", ticketTypeId: " <> show fareReq.ticketTypeId <> ", fareTypeId: " <> show fareReq.fareTypeId
  let cacheKey = "cmrlv2-fare-" <> T.pack (show fareReq.operatorNameId) <> "-" <> fareReq.fromStationId <> "-" <> fareReq.toStationId <> "-" <> T.pack (show fareReq.ticketTypeId)
  mbCachedFares <- Hedis.get cacheKey
  case mbCachedFares of
    Just cachedFares -> do
      logDebug $ "[CMRLV2:GetFare] Cache HIT for key: " <> cacheKey
      return cachedFares
    Nothing -> do
      logDebug $ "[CMRLV2:GetFare] Cache MISS for key: " <> cacheKey <> ", fetching from API"
      logDebug $ "[CMRLV2:GetFare] Request payload: " <> T.pack (show fareReq)
      let eulerClient = \accessToken -> ET.client getFareAPI (Just $ "Bearer " <> accessToken) fareReq
      fareRes <- callCMRLV2API config eulerClient "getFare" getFareAPI
      logDebug $ "[CMRLV2:GetFare] API Response: statusCode=" <> show fareRes.statusCode <> ", message=" <> fareRes.message
      ticketCategoryMetadataConfig <- QFRFSTicketCategoryMetadataConfig.findByCategoryVehicleAndCity ADULT (becknVehicleCategoryToFrfsVehicleCategory integrationBPPConfig.vehicleCategory) integrationBPPConfig.merchantOperatingCityId
      fares <-
        case ((fareRes.result >>= (.discountedFare)) <|> (fareRes.result >>= (.fare))) of
          Just amount -> do
            let offeredPrice = amount
                originalPrice =
                  case (ticketCategoryMetadataConfig <&> (.domainCategoryValue)) of
                    Just domainCategoryValue ->
                      case domainCategoryValue of
                        FixedAmount discountAmount -> offeredPrice + discountAmount
                        Percentage discountPercentage -> HighPrecMoney $ offeredPrice.getHighPrecMoney / (1 - (toRational discountPercentage / 100))
                    Nothing -> offeredPrice
            return $
              [ FRFSUtils.FRFSFare
                  { categories =
                      [ FRFSUtils.FRFSTicketCategory
                          { category = ADULT,
                            price =
                              Price
                                { amountInt = round originalPrice,
                                  amount = originalPrice,
                                  currency = INR
                                },
                            offeredPrice =
                              Price
                                { amountInt = round amount,
                                  amount = amount,
                                  currency = INR
                                },
                            eligibility = True
                          }
                      ],
                    fareDetails = Nothing,
                    farePolicyId = Nothing,
                    vehicleServiceTier =
                      FRFSUtils.FRFSVehicleServiceTier
                        { serviceTierType = Spec.ORDINARY,
                          serviceTierProviderCode = "ORDINARY",
                          serviceTierShortName = "ORDINARY",
                          serviceTierDescription = "ORDINARY",
                          serviceTierLongName = "ORDINARY",
                          isAirConditioned = Just False
                        },
                    fareQuoteType = Nothing
                  }
              ]
          Nothing -> do
            logDebug "[CMRLV2:GetFare] No fare found in response"
            return []
      logInfo $ "[CMRLV2:GetFare] Caching fares, count: " <> show (length fares)
      Hedis.setExp cacheKey fares 86400
      return fares
