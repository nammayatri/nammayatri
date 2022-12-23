module Domain.Types.Transaction where

import Beckn.Prelude
import Beckn.Types.Id
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Ride as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP

-- request is raw Text here, because if some field will be changed, we can't parse it
data Transaction = Transaction
  { id :: Id Transaction,
    personId :: Id DP.Person,
    merchantId :: Maybe (Id DM.Merchant), -- will be Nothing for admin apis
    endpoint :: Endpoint, -- Text?
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    errorCode :: Maybe Text,
    createdAt :: UTCTime
  }

data Endpoint
  = RideEndpoint Common.RideEndpoint
  | DriverEndpoint Common.DriverEndpoint
  | DriverRegistrationEndpoint Common.DriverRegistrationEndpoint
  deriving (Show, Read)
