{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.BusinessHour where

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

data BusinessHourResult = BusinessHourResult
  { qrBookingStartTime :: T.Text,
    qrBookingEndTime :: T.Text,
    businessStartTime :: T.Text,
    businessEndTime :: T.Text,
    qrTicketRestrictionStartTime :: T.Text,
    qrTicketRestrictionEndTime :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data BusinessHourRes = BusinessHourRes
  { statusCode :: Int,
    message :: T.Text,
    result :: BusinessHourResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type BusinessHourAPI =
  "CmrlThirdParty" :> "businesshour"
    :> Header "Authorization" T.Text
    :> MandatoryQueryParam "appType" T.Text
    :> Get '[JSON] BusinessHourRes

businessHourAPI :: Proxy BusinessHourAPI
businessHourAPI = Proxy

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLConfig -> m BusinessHourResult
getBusinessHour config = do
  let eulerClient = \accessToken -> ET.client businessHourAPI (Just $ "Bearer " <> accessToken) cmrlAppType
  response <- callCMRLAPI config eulerClient "getBusinessHour" businessHourAPI
  return response.result
