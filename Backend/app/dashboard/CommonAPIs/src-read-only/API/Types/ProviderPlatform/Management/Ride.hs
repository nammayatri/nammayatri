{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Ride where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data BookingStatus = UPCOMING | UPCOMING_6HRS | ONGOING | ONGOING_6HRS | COMPLETED | CANCELLED deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Read, Kernel.Prelude.ToParamSchema)

data RideListItem = RideListItem
  { bookingStatus :: API.Types.ProviderPlatform.Management.Ride.BookingStatus,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fareDiff :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    fareDiffWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    rideCreatedAt :: Kernel.Prelude.UTCTime,
    rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    rideShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride,
    tripCategory :: API.Types.ProviderPlatform.Management.Ride.TripCategory,
    vehicleNo :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RideListRes = RideListRes {rides :: [API.Types.ProviderPlatform.Management.Ride.RideListItem], summary :: Dashboard.Common.Summary, totalItems :: Kernel.Prelude.Int}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TripCategory = OneWay | Rental | RideShare | InterCity | CrossCity | Ambulance deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

type API = ("ride" :> GetRideList)

type GetRideList =
  ( "list" :> QueryParam "bookingStatus" API.Types.ProviderPlatform.Management.Ride.BookingStatus :> QueryParam "currency" Kernel.Types.Common.Currency
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "fareDiff"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "rideShortId"
           (Kernel.Types.Id.ShortId Dashboard.Common.Ride)
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Ride.RideListRes
  )

newtype RideAPIs = RideAPIs {getRideList :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.RideListRes}

mkRideAPIs :: (Client EulerHS.Types.EulerClient API -> RideAPIs)
mkRideAPIs rideClient = (RideAPIs {..})
  where
    getRideList = rideClient
