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
import Tools.Error

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
  "cumta" :> "businesshour"
    :> Header "Authorization" T.Text
    :> Get '[JSON] BusinessHourRes

businessHourAPI :: Proxy BusinessHourAPI
businessHourAPI = Proxy

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> m BusinessHourResult
getBusinessHour config = do
  accessToken <- getAuthToken config
  response <-
    callAPI config.networkHostUrl (ET.client businessHourAPI (Just $ "Bearer " <> accessToken)) "getBusinessHour" businessHourAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_BUSINESS_HOUR_API") config.networkHostUrl)
  return response.result
