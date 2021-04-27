module Types.API.DriverInformation
  ( ActiveDriversResponse (..),
    DriverRidesInformation (..),
    DriverInformationResponse (..),
    GetRideInfoRes (..),
    RideInfo (..),
    ListDriverRes (..),
    DriverEntityRes (..),
  )
where

import Beckn.Types.Id
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Types.Storage.Person (Person)
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import Beckn.Types.Storage.Vehicle (Vehicle)
import Data.Aeson
import Data.Time
import EulerHS.Prelude
import qualified Types.API.Person as PersonAPI
import Types.Storage.DriverInformation

newtype ActiveDriversResponse = ActiveDriversResponse
  { active_drivers :: [DriverRidesInformation]
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverRidesInformation = DriverRidesInformation
  { driver_id :: Id Person,
    completed_rides_over_time :: Int,
    earnings_over_time :: Float
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverInformationResponse = DriverInformationResponse
  { transporter :: Organization.Organization,
    person :: PersonAPI.PersonEntityRes,
    driverInformation :: DriverInformation
  }
  deriving (Generic, ToJSON, FromJSON)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data RideInfo = RideInfo
  { productInstanceId :: Id ProductInstance,
    pickupLoc :: Loc.Location,
    dropLoc :: Loc.Location,
    etaForPickupLoc :: Maybe Integer,
    distanceToPickupLoc :: Maybe Float,
    notificationExpiryTime :: UTCTime,
    estimatedPrice :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

newtype ListDriverRes = ListDriverRes
  {drivers :: [DriverEntityRes]}
  deriving (Generic, ToJSON, FromJSON)

data DriverEntityRes = DriverEntityRes
  { id :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe Vehicle,
    active :: Bool,
    onRide :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)
