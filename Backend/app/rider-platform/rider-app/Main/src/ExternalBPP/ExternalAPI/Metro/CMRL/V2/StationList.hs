{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.StationList where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data Station = Station
  { stationId :: Text,
    stationCode :: Text,
    stationName :: Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data StationListResponse = StationListResponse
  { statusCode :: Int,
    message :: Text,
    result :: [Station]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type StationListAPI =
  "api" :> "qr" :> "v1" :> "stations" :> "list"
    :> Header "Authorization" Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> Get '[JSON] StationListResponse

stationListAPI :: Proxy StationListAPI
stationListAPI = Proxy

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m [Station]
getStationList config = do
  logInfo $ "[CMRLV2:StationList] Fetching station list for operatorNameId: " <> show config.operatorNameId
  let eulerClient = \accessToken -> ET.client stationListAPI (Just $ "Bearer " <> accessToken) config.operatorNameId
  response <- callCMRLV2API config eulerClient "getStationList" stationListAPI
  logDebug $ "[CMRLV2:StationList] API Response: statusCode=" <> show response.statusCode <> ", message=" <> response.message
  logInfo $ "[CMRLV2:StationList] Fetched " <> show (length response.result) <> " stations"
  return response.result
