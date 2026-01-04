{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.BusinessHour where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data CommonParam = CommonParam
  { paramId :: Int,
    paramName :: T.Text,
    paramValue :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data BusinessHourRes = BusinessHourRes
  { returnCode :: T.Text,
    returnMessage :: T.Text,
    commonParamList :: [CommonParam]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type BusinessHourAPI =
  "api" :> "qr" :> "v1" :> "operations" :> "business-date-hour"
    :> Header "Authorization" T.Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> Get '[JSON] BusinessHourRes

businessHourAPI :: Proxy BusinessHourAPI
businessHourAPI = Proxy

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m BusinessHourRes
getBusinessHour config = do
  logInfo $ "[CMRLV2:BusinessHour] Fetching business hours for operatorNameId: " <> show config.operatorNameId
  let eulerClient = \accessToken -> ET.client businessHourAPI (Just $ "Bearer " <> accessToken) config.operatorNameId
  response <- callCMRLV2API config eulerClient "getBusinessHour" businessHourAPI
  logDebug $ "[CMRLV2:BusinessHour] API Response - returnCode: " <> response.returnCode <> ", returnMessage: " <> response.returnMessage
  logInfo $ "[CMRLV2:BusinessHour] Fetched " <> show (length response.commonParamList) <> " business hour params"
  return response
