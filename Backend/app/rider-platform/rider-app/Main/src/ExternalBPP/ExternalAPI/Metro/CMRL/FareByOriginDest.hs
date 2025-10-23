{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.FareByOriginDest where

import qualified BecknV2.FRFS.Enums as Spec
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Text as T
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FRFSUtils as FRFSUtils

data FareByOriginDestReq = FareByOriginDestReq
  { origin :: T.Text,
    destination :: T.Text,
    ticketType :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareByOriginDestResInner = FareByOriginDestResInner
  { ticketTotalFare :: Maybe HighPrecMoney,
    ticketDiscountFare :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareByOriginDestAPIRes = FareByOriginDestAPIRes
  { statusCode :: Int,
    message :: T.Text,
    result :: Maybe FareByOriginDestResInner
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type FareByOriginDestAPI =
  "CmrlThirdParty" :> "farebyod"
    :> Header "Authorization" T.Text
    :> MandatoryQueryParam "origin" T.Text
    :> MandatoryQueryParam "destination" T.Text
    :> MandatoryQueryParam "ticketType" T.Text
    :> MandatoryQueryParam "appType" T.Text
    :> Get '[JSON] FareByOriginDestAPIRes

fareByOriginDestAPI :: Proxy FareByOriginDestAPI
fareByOriginDestAPI = Proxy

getFareByOriginDest :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> FareByOriginDestReq -> m [FRFSUtils.FRFSFare]
getFareByOriginDest config fareReq = do
  let cacheKey = "cmrl-fare-" <> fareReq.origin <> "-" <> fareReq.destination <> "-" <> fareReq.ticketType
  mbCachedFares <- Hedis.get cacheKey
  case mbCachedFares of
    Just cachedFares -> do
      logDebug $ "Retrieved fares from cache for key: " <> cacheKey
      return cachedFares
    Nothing -> do
      logDebug $ "Cache miss for key: " <> cacheKey <> ", fetching from API"
      let eulerClient = \accessToken -> ET.client fareByOriginDestAPI (Just $ "Bearer " <> accessToken) (getStationCode fareReq.origin) (getStationCode fareReq.destination) fareReq.ticketType cmrlAppType
      fareByODRes <- callCMRLAPI config eulerClient "getFareByOriginDest" fareByOriginDestAPI
      logDebug $ "CMRL Get Fares API Response : " <> show fareByODRes
      let fares = case ((fareByODRes.result >>= (.ticketDiscountFare)) <|> (fareByODRes.result >>= (.ticketTotalFare))) of
            Just amount ->
              [ FRFSUtils.FRFSFare
                  { categories =
                      [ FRFSUtils.FRFSTicketCategory
                          { category = ADULT,
                            code = "ADULT",
                            title = "Adult General Ticket",
                            description = "Adult General Ticket",
                            tnc = "Terms and conditions apply for adult general ticket",
                            price =
                              Price
                                { amountInt = round amount,
                                  amount = amount,
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
                        }
                  }
              ]
            Nothing -> []
      -- Cache the fares with 24-hour TTL (86400 seconds)
      Hedis.setExp cacheKey fares 86400
      return fares
  where
    getStationCode :: Text -> Text
    getStationCode stationCode = fromMaybe stationCode (listToMaybe $ T.splitOn "|" stationCode)
