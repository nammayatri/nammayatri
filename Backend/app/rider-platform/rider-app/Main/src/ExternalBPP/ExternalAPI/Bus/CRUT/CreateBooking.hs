{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.CRUT.CreateBooking where

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

data CreateBookingReq = CreateBookingReq
  { amount :: Int,
    configId :: T.Text,
    passengerDetails :: [PassengerDetail],
    tripDetail :: TripDetail,
    metaData :: BookingMetaData,
    customerInfo :: CustomerInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PassengerDetail = PassengerDetail
  { id :: T.Text,
    count :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TripDetail = TripDetail
  { endStopId :: T.Text,
    routeId :: T.Text,
    startStopId :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data BookingMetaData = BookingMetaData
  { buyerAppId :: T.Text,
    buyerPlatformId :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CustomerInfo = CustomerInfo
  { mobileNumber :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CreateBookingRes = CreateBookingRes
  { bookingId :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type CreateBookingAPI =
  "mticketing" :> "v2" :> "partner" :> "ticket" :> "booking"
    :> Header "Authorization" T.Text
    :> Header "userid" T.Text
    :> ReqBody '[JSON] CreateBookingReq
    :> Post '[JSON] CreateBookingRes

createBookingAPI :: Proxy CreateBookingAPI
createBookingAPI = Proxy

createBooking :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CRUTConfig -> CreateBookingReq -> m CreateBookingRes
createBooking config req = do
  authToken <- getAuthToken config
  response <-
    callAPI config.networkHostUrl (ET.client createBookingAPI (Just $ "Basic " <> authToken) (Just config.userId) req) "createBooking" createBookingAPI
      >>= fromEitherM (ExternalAPICallError (Just "CRUT_CREATE_BOOKING_API") config.networkHostUrl)
  return response
