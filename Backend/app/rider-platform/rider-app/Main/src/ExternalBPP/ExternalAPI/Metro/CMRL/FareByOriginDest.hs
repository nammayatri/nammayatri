{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.FareByOriginDest where

import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant hiding (route)
import qualified SharedLogic.FRFSUtils as FRFSUtils
import Tools.Error

data FareByOriginDestReq = FareByOriginDestReq
  { route :: T.Text,
    origin :: T.Text,
    destination :: T.Text,
    ticketType :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareByOriginDestResInner = FareByOriginDestResInner
  { result :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareByOriginDestAPIRes = FareByOriginDestAPIRes
  { statusCode :: Int,
    message :: T.Text,
    result :: Maybe FareByOriginDestResInner
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type FareByOriginDestAPI =
  "cumta" :> "farebyod"
    :> Header "Authorization" T.Text
    :> MandatoryQueryParam "origin" T.Text
    :> MandatoryQueryParam "destination" T.Text
    :> MandatoryQueryParam "ticketType" T.Text
    :> Get '[JSON] FareByOriginDestAPIRes

fareByOriginDestAPI :: Proxy FareByOriginDestAPI
fareByOriginDestAPI = Proxy

getFareByOriginDest :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> FareByOriginDestReq -> m [FRFSUtils.FRFSFare]
getFareByOriginDest config fareReq = do
  accessToken <- getAuthToken config
  fareByODRes <-
    callAPI config.networkHostUrl (ET.client fareByOriginDestAPI (Just $ "Bearer " <> accessToken) (getStationCode fareReq.origin) (getStationCode fareReq.destination) fareReq.ticketType) "getFareByOriginDest" fareByOriginDestAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_FARE_BY_ORIGIN_DEST_API") config.networkHostUrl)
  logDebug $ "CMRL Get Fares API Response : " <> show fareByODRes
  case fareByODRes.result >>= (.result) of
    Just amount ->
      return $
        [ FRFSUtils.FRFSFare
            { price =
                Price
                  { amountInt = round amount,
                    amount = amount,
                    currency = INR
                  },
              discounts = [],
              vehicleServiceTier =
                FRFSUtils.FRFSVehicleServiceTier
                  { serviceTierType = Spec.ORDINARY,
                    serviceTierProviderCode = "ORDINARY",
                    serviceTierShortName = "ORDINARY",
                    serviceTierDescription = "ORDINARY",
                    serviceTierLongName = "ORDINARY"
                  }
            }
        ]
    Nothing -> return []
  where
    getStationCode :: Text -> Text
    getStationCode stationCode = fromMaybe stationCode (listToMaybe $ T.splitOn "|" stationCode)
