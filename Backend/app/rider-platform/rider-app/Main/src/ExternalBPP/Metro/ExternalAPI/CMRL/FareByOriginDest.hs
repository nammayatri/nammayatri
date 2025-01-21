{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.Metro.ExternalAPI.CMRL.FareByOriginDest where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.Metro.ExternalAPI.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import Tools.Error

data FareByOriginDestReq = FareByOriginDestReq
  { origin :: T.Text,
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

getFareByOriginDest :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> FareByOriginDestReq -> m (Maybe HighPrecMoney)
getFareByOriginDest config fareReq = do
  accessToken <- getAuthToken config
  fareByODRes <-
    callAPI config.networkHostUrl (ET.client fareByOriginDestAPI (Just $ "Bearer " <> accessToken) fareReq.origin fareReq.destination fareReq.ticketType) "getFareByOriginDest" fareByOriginDestAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_FARE_BY_ORIGIN_DEST_API") config.networkHostUrl)
  return (fareByODRes.result >>= (.result))
