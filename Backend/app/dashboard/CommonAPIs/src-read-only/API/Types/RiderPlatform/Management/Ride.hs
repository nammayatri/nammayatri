{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Ride where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data BookingStatus
  = UPCOMING
  | UPCOMING_6HRS
  | ONGOING
  | ONGOING_6HRS
  | RCOMPLETED
  | RCANCELLED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Location = Location
  { id :: Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Ride.Location,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    address :: API.Types.RiderPlatform.Management.Ride.LocationAddress,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAddress = LocationAddress
  { street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride,
    rideCreatedAt :: Kernel.Prelude.UTCTime,
    rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    bookingStatus :: API.Types.RiderPlatform.Management.Ride.BookingStatus,
    nextStopLocation :: Kernel.Prelude.Maybe API.Types.RiderPlatform.Management.Ride.Location,
    rideScheduledAt :: Kernel.Prelude.UTCTime,
    fareProductType :: Domain.Types.FareProductType,
    tripCategory :: Domain.Types.TripCategory,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListRes = RideListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, rides :: [API.Types.RiderPlatform.Management.Ride.RideListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("ride" :> GetRideList)

type GetRideList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "bookingStatus"
           API.Types.RiderPlatform.Management.Ride.BookingStatus
      :> QueryParam "rideShortId" ((Kernel.Types.Id.ShortId Dashboard.Common.Ride))
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "driverPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           API.Types.RiderPlatform.Management.Ride.RideListRes
  )

newtype RideAPIs = RideAPIs {getRideList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.RiderPlatform.Management.Ride.BookingStatus) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId Dashboard.Common.Ride)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Ride.RideListRes)}

mkRideAPIs :: (Client EulerHS.Types.EulerClient API -> RideAPIs)
mkRideAPIs rideClient = (RideAPIs {..})
  where
    getRideList = rideClient

data RideEndpointDSL
  = GetRideListEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON RideEndpointDSL where
  toJSON (GetRideListEndpoint) = Data.Aeson.String "GetRideListEndpoint"

instance FromJSON RideEndpointDSL where
  parseJSON (Data.Aeson.String "GetRideListEndpoint") = pure GetRideListEndpoint
  parseJSON _ = fail "GetRideListEndpoint expected"
