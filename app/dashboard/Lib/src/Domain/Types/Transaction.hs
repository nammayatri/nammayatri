module Domain.Types.Transaction where

import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified "dashboard-helper-api" Dashboard.Common.Merchant as Common
import qualified "dashboard-helper-api" Dashboard.Common.Message as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Customer as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id

-- request is raw Text here, because if some field will be changed, we can't parse it
data Transaction = Transaction
  { id :: Id Transaction,
    requestorId :: Id DP.Person,
    merchantId :: Maybe (Id DM.Merchant), -- will be Nothing for admin apis
    endpoint :: Endpoint, -- Text?
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }

data Endpoint
  = RideAPI Common.RideEndpoint
  | BookingAPI Common.BookingEndpoint
  | DriverAPI Common.DriverEndpoint
  | DriverRegistrationAPI Common.DriverRegistrationEndpoint
  | MerchantAPI Common.MerchantEndpoint
  | CustomerAPI Common.CustomerEndpoint
  | MessageAPI Common.MessageEndpoint
  deriving (Show, Read)
