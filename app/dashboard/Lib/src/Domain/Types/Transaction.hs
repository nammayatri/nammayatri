module Domain.Types.Transaction where

import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Driver as Common
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Driver.Registration as Common
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Ride as Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Merchant as Common
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
  | DriverAPI Common.DriverEndpoint
  | DriverRegistrationAPI Common.DriverRegistrationEndpoint
  | MerchantAPI Common.MerchantEndpoint
  deriving (Show, Read)
