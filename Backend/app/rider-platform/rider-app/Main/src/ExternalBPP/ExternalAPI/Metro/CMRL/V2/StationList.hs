{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.StationList where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data Station = Station
  { stationName :: Text,
    stationShortName :: Text,
    stationUniqueid :: Text,
    lineId :: Maybe Int,
    lineName :: Maybe Text,
    latitude :: Maybe Text,
    longitude :: Maybe Text,
    sequenceNo :: Maybe Int,
    stationNameTamil :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

newtype StationListResponse = StationListResponse
  { stations :: [Station]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type StationListAPI =
  "api" :> "qr" :> "v1" :> "stations" :> "list"
    :> Header "Authorization" Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> Get '[JSON] StationListResponse

stationListAPI :: Proxy StationListAPI
stationListAPI = Proxy

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLV2Config -> m [Station]
getStationList config = do
  logInfo $ "[CMRLV2:StationList] Fetching station list for operatorNameId: " <> show config.operatorNameId
  let eulerClient = \accessToken -> ET.client stationListAPI (Just $ "Bearer " <> accessToken) config.operatorNameId
  response <- callCMRLV2API config eulerClient "getStationList" stationListAPI
  logInfo $ "[CMRLV2:StationList] Fetched " <> show (length response.stations) <> " stations"
  return response.stations
