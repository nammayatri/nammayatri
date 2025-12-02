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

data DurationDetailsReq = DurationDetailsReq
  { originStationId :: T.Text,
    destinationStationId :: T.Text,
    appType :: T.Text
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
  "CmrlThirdParty" :> "traveldistanceduration"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] DurationDetailsReq
    :> Get '[JSON] DurationDetailsRes

durationDetailsAPI :: Proxy DurationDetailsAPI
durationDetailsAPI = Proxy

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLConfig -> DurationDetailsReq -> m [DurationDetailsResult]
getDurationDetails config req = do
  let eulerClient = \accessToken -> ET.client durationDetailsAPI (Just $ "Bearer " <> accessToken) req
  response <- callCMRLAPI config eulerClient "getDurationDetails" durationDetailsAPI
  return response.result
