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
import Tools.Error

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
  "cumta" :> "stations"
    :> Header "Authorization" Text
    :> Get '[JSON] StationListResponse

stationListAPI :: Proxy StationListAPI
stationListAPI = Proxy

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> m [Station]
getStationList config = do
  accessToken <- getAuthToken config
  response <-
    callAPI config.networkHostUrl (ET.client stationListAPI (Just $ "Bearer " <> accessToken)) "getStationList" stationListAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_STATION_LIST_API") config.networkHostUrl)
  return response.result
