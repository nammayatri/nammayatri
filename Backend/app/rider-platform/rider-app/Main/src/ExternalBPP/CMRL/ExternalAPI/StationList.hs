{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.CMRL.ExternalAPI.StationList where

import Data.Aeson
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
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
  deriving (Generic, Show, Eq)

instance FromJSON Station where
  parseJSON = genericParseJSON constructorsWithSnakeCase

data StationListResponse = StationListResponse
  { result :: [Station]
  }
  deriving (Generic, Show)

instance FromJSON StationListResponse where
  parseJSON = genericParseJSON constructorsWithSnakeCase

type StationListAPI =
  "cumta" :> "stations"
    :> Header "Authorization" Text
    :> Get '[JSON] StationListResponse

stationListAPI :: Proxy StationListAPI
stationListAPI = Proxy

getStationList :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> m [Station]
getStationList accessToken host = do
  response <-
    callAPI host (ET.client stationListAPI (Just $ "Bearer " <> accessToken) ()) "getStationList" stationListAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_STATION_LIST_API") host)
  return response.result
