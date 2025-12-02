{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.FareMatrix where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

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
  deriving (Generic, Show, ToJSON, FromJSON)

data FareMatrixAPIRes = FareMatrixAPIRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [FareMatrixRes]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type FareMatrixAPI =
  "CmrlThirdParty" :> "farematrix"
    :> Header "Authorization" T.Text
    :> MandatoryQueryParam "appType" T.Text
    :> Get '[JSON] FareMatrixAPIRes

fareMatrixAPI :: Proxy FareMatrixAPI
fareMatrixAPI = Proxy

getFareMatrix :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLConfig -> m [FareMatrixRes]
getFareMatrix config = do
  let eulerClient = \accessToken -> ET.client fareMatrixAPI (Just $ "Bearer " <> accessToken) cmrlAppType
  fareMatrixRes <- callCMRLAPI config eulerClient "getFareMatrix" fareMatrixAPI
  return fareMatrixRes.result
