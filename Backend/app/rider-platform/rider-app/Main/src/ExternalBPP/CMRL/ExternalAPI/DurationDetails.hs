{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.CMRL.ExternalAPI.DurationDetails where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import GHC.Generics (Generic)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Servant
import Tools.Error

data DurationDetailsReq = DurationDetailsReq
  { originStationId :: T.Text,
    destinationStationId :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON DurationDetailsReq where
  toJSON = genericToJSON defaultOptions

data DurationDetailsResult = DurationDetailsResult
  { noOfStops :: Int,
    distance :: Double,
    timeDuration :: Int,
    via :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON DurationDetailsResult where
  parseJSON = genericParseJSON defaultOptions

data DurationDetailsRes = DurationDetailsRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [DurationDetailsResult]
  }
  deriving (Generic, Show)

instance FromJSON DurationDetailsRes where
  parseJSON = genericParseJSON defaultOptions

type DurationDetailsAPI =
  "cumta" :> "traveldistanceduration"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] DurationDetailsReq
    :> Get '[JSON] DurationDetailsRes

durationDetailsAPI :: Proxy DurationDetailsAPI
durationDetailsAPI = Proxy

getDurationDetails :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> DurationDetailsReq -> m [DurationDetailsResult]
getDurationDetails host accessToken req = do
  response <-
    callAPI host (ET.client durationDetailsAPI (Just $ "Bearer " <> accessToken) req) "getDurationDetails" durationDetailsAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_DURATION_DETAILS_API") host)
  return response.result
