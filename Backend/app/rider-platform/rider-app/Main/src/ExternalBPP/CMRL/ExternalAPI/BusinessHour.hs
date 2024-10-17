{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.CMRL.ExternalAPI.BusinessHour where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import GHC.Generics (Generic)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
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
  deriving (Generic, Show)

instance FromJSON BusinessHourResult where
  parseJSON = genericParseJSON defaultOptions

data BusinessHourRes = BusinessHourRes
  { statusCode :: Int,
    message :: T.Text,
    result :: BusinessHourResult
  }
  deriving (Generic, Show)

instance FromJSON BusinessHourRes where
  parseJSON = genericParseJSON defaultOptions

type BusinessHourAPI =
  "cumta" :> "businesshour"
    :> Header "Authorization" T.Text
    :> Get '[JSON] BusinessHourRes

businessHourAPI :: Proxy BusinessHourAPI
businessHourAPI = Proxy

getBusinessHour :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> m BusinessHourResult
getBusinessHour accessToken host = do
  response <-
    callAPI host (ET.client businessHourAPI (Just "Bearer " <> accessToken) ()) "getBusinessHour" businessHourAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_BUSINESS_HOUR_API") host)
  return response.result
