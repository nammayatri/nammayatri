{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.StationList where

import Data.Aeson
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data Station = Station
  { id :: Int,
    lineId :: Text,
    stationId :: Text,
    code :: Text,
    name :: Text,
    taName :: Maybe Text,
    address :: Text,
    latitude :: Double,
    longitude :: Double,
    sequenceNo :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data StationListResponse = StationListResponse
  { result :: [Station]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type StationListAPI =
  "CmrlThirdParty" :> "stations"
    :> Header "Authorization" Text
    :> MandatoryQueryParam "appType" Text
    :> Get '[JSON] StationListResponse

stationListAPI :: Proxy StationListAPI
stationListAPI = Proxy

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLConfig -> m [Station]
getStationList config = do
  let eulerClient = \accessToken -> ET.client stationListAPI (Just $ "Bearer " <> accessToken) cmrlAppType
  response <- callCMRLAPI config eulerClient "getStationList" stationListAPI
  return response.result
