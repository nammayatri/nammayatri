{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.CRUT.FetchFare where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.CRUT.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant
import Tools.Error

data FetchFareReq = FetchFareReq
  { city :: T.Text,
    routeId :: T.Text,
    startStopId :: T.Text,
    endStopId :: T.Text,
    filters :: FareFilters,
    passengerDetails :: [PassengerDetail]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareFilters = FareFilters
  { configurationId :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PassengerDetail = PassengerDetail
  { id :: T.Text,
    count :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FetchFareRes = FetchFareRes
  { finalPayableAmount :: Int,
    payableAmount :: Int,
    passengerDetails :: [PassengerFareDetail]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PassengerFareDetail = PassengerFareDetail
  { id :: T.Text,
    name :: T.Text,
    count :: Int,
    payableAmount :: Int,
    fareBreakup :: [FareBreakup]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareBreakup = FareBreakup
  { name :: T.Text,
    value :: Int,
    operation :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type FetchFareAPI =
  "mticketing" :> "v2" :> "partner" :> "ticket" :> "fare"
    :> Header "Authorization" T.Text
    :> Header "userid" T.Text
    :> ReqBody '[JSON] FetchFareReq
    :> Post '[JSON] FetchFareRes

fetchFareAPI :: Proxy FetchFareAPI
fetchFareAPI = Proxy

fetchFare :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CRUTConfig -> FetchFareReq -> m FetchFareRes
fetchFare config req = do
  authToken <- getAuthToken config
  response <-
    callAPI config.networkHostUrl (ET.client fetchFareAPI (Just $ "Basic " <> authToken) (Just config.userId) req) "fetchFare" fetchFareAPI
      >>= fromEitherM (ExternalAPICallError (Just "CRUT_FETCH_FARE_API") config.networkHostUrl)
  return response
