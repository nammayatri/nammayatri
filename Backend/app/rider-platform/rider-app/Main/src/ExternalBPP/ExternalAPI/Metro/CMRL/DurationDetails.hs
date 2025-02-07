{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.DurationDetails where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant
import Tools.Error

data DurationDetailsReq = DurationDetailsReq
  { originStationId :: T.Text,
    destinationStationId :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DurationDetailsResult = DurationDetailsResult
  { noOfStops :: Int,
    distance :: Double,
    timeDuration :: Int,
    via :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DurationDetailsRes = DurationDetailsRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [DurationDetailsResult]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type DurationDetailsAPI =
  "cumta" :> "traveldistanceduration"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] DurationDetailsReq
    :> Get '[JSON] DurationDetailsRes

durationDetailsAPI :: Proxy DurationDetailsAPI
durationDetailsAPI = Proxy

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> DurationDetailsReq -> m [DurationDetailsResult]
getDurationDetails config req = do
  accessToken <- getAuthToken config
  response <-
    callAPI config.networkHostUrl (ET.client durationDetailsAPI (Just $ "Bearer " <> accessToken) req) "getDurationDetails" durationDetailsAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_DURATION_DETAILS_API") config.networkHostUrl)
  return response.result
