{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.CMRL.ExternalAPI.FareMatrix where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import GHC.Generics (Generic)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Servant
import Tools.Error

data FareMatrixRes = FareMatrixRes
  { sourceStationId :: T.Text,
    destinationStationId :: T.Text,
    fareValue :: Int,
    actualFareValue :: Maybe Int,
    mediaTypeId :: Maybe Int,
    travelType :: Int,
    vendorId :: Maybe Int,
    passengerTypeId :: Maybe Int
  }
  deriving (Generic, Show)

instance FromJSON FareMatrixRes where
  parseJSON = genericParseJSON defaultOptions

data FareMatrixAPIRes = FareMatrixAPIRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [FareMatrixRes]
  }
  deriving (Generic, Show)

instance FromJSON FareMatrixAPIRes where
  parseJSON = genericParseJSON defaultOptions

type FareMatrixAPI =
  "cumta" :> "farematrix"
    :> Header "Authorization" T.Text
    :> Get '[JSON] FareMatrixAPIRes

fareMatrixAPI :: Proxy FareMatrixAPI
fareMatrixAPI = Proxy

getFareMatrix :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> m [FareMatrixRes]
getFareMatrix host accessToken = do
  fareMatrixRes <-
    callAPI host (ET.client fareMatrixAPI (Just $ "Bearer " <> accessToken)) "getFareMatrix" fareMatrixAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_FARE_MATRIX_API") host)
  return fareMatrixRes.result
